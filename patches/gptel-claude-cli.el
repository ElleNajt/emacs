;;; gptel-claude-cli.el --- Claude CLI support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Claude Code
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for using the Claude Code CLI (`claude -p`)
;; as a backend for gptel instead of the Anthropic API.
;;
;; This is useful if you want to:
;; - Use your existing Claude subscription instead of API credits
;; - Leverage Claude Code's local prompt caching and session management
;; - Use MCP servers configured in Claude Code
;; - Avoid API rate limits
;;
;; Prerequisites:
;; - Install Claude Code CLI: https://docs.claude.com/en/docs/claude-code
;; - Authenticate with: claude setup-token
;;
;; Usage:
;;
;; (require 'gptel-claude-cli)
;;
;; ;; Basic setup with default model
;; (gptel-make-claude-cli "Claude CLI")
;;
;; ;; Custom setup with specific model
;; (gptel-make-claude-cli "Claude CLI"
;;   :model "claude-sonnet-4-5-20250929"
;;   :stream nil)  ;; streaming not yet supported
;;
;; ;; Set this as your default backend:
;; (setq gptel-backend (gptel-make-claude-cli "Claude CLI"))
;;
;; ;; Or select it interactively:
;; M-x gptel-menu  then press 'b' to select backend
;;
;; Limitations:
;; - Streaming is not currently supported
;; - Tool use / function calling not tested
;; - Each request starts a new Claude CLI process (no session persistence)

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'cl-lib)
(require 'json)

(defvar gptel--claude-cli-models
  '(claude-sonnet-4-5-20250929
    claude-haiku-4-5-20251001
    claude-opus-4-5-20250514)
  "List of Claude models available via Claude CLI.")

;;; Backend struct

;; Note: We inherit from gptel-openai because Claude CLI uses
;; the same message format (role/content structure) and we want
;; to reuse the buffer parsing logic from gptel-openai.
(cl-defstruct (gptel-claude-cli (:constructor gptel--make-claude-cli)
                                (:copier nil)
                                (:include gptel-openai))
  "Backend struct for Claude CLI.

Since Claude CLI handles authentication and API calls internally,
we don't need host, endpoint, or key fields."
  (binary "claude" :documentation "Path to claude binary")
  (cli-model nil :documentation "Model to use (e.g., claude-sonnet-4-5-20250929)")
  (working-dir nil :documentation "Working directory for claude process"))

;;;###autoload
(cl-defun gptel-make-claude-cli
    (name &key
          (binary "claude")
          (model "claude-sonnet-4-5-20250929")
          (working-dir default-directory)
          (stream nil)
          (models gptel--claude-cli-models))
  "Register a Claude CLI backend for gptel with NAME.

Keyword arguments:

BINARY: Path to the claude binary (default: \"claude\")
MODEL: Model to use (default: claude-sonnet-4-5-20250929)
WORKING-DIR: Working directory for claude process (default: `default-directory')
STREAM: Streaming is not yet supported for Claude CLI backend
MODELS: List of available models"
  (declare (indent 1))
  (let ((backend (gptel--make-claude-cli
                  :name name
                  :binary binary
                  :cli-model model
                  :working-dir working-dir
                  :stream stream
                  :models (gptel--process-models models))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

;;; Request handling

(cl-defmethod gptel--request-data ((_backend gptel-claude-cli) prompts)
  "Format PROMPTS for Claude CLI.

Claude CLI takes plain text input via stdin, so we convert the
structured PROMPTS into a simple text conversation."
  ;; Convert structured prompts to plain text
  ;; Each message in the form: "Role: content"
  (let ((text-prompts
         (mapconcat
          (lambda (msg)
            (let ((role (plist-get msg :role))
                  (content (plist-get msg :content)))
              (cond
               ;; Handle string content
               ((stringp content)
                (format "%s: %s"
                        (capitalize (if (symbolp role) (symbol-name role) role))
                        content))
               ;; Handle array of content blocks
               ((vectorp content)
                (mapconcat
                 (lambda (block)
                   (let ((type (plist-get block :type))
                         (text (plist-get block :text)))
                     (if (equal type "text")
                         (format "%s: %s"
                                 (capitalize (if (symbolp role) (symbol-name role) role))
                                 text)
                       "")))
                 content
                 "\n\n"))
               (t ""))))
          prompts
          "\n\n")))
    ;; Return as plist with text key for consistency
    (list :text text-prompts)))

(cl-defmethod gptel--parse-response ((_backend gptel-claude-cli) response _info)
  "Parse Claude CLI JSON RESPONSE and extract the result text.

RESPONSE is the parsed JSON output from `claude -p --output-format json`."
  ;; The JSON response has a "result" field with the text
  (plist-get response :result))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-claude-cli) _info)
  "Streaming is not yet supported for Claude CLI backend."
  (error "Streaming is not supported for Claude CLI backend"))

;;; Override transport for Claude CLI

;; We need to intercept the normal curl/url transport for Claude CLI backends.
;; The cleanest way is to advise the transport selection.

(defun gptel-claude-cli--dispatch (orig-fun fsm)
  "Dispatch to Claude CLI transport if backend is gptel-claude-cli.

Otherwise call ORIG-FUN with FSM."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (if (gptel-claude-cli-p backend)
        (gptel-claude-cli--get-response-fsm fsm)
      (funcall orig-fun fsm))))

(defun gptel-claude-cli--get-response-fsm (fsm)
  "Get response from Claude CLI for state machine FSM.

This is called instead of curl/url transport for Claude CLI backends."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (binary (gptel-claude-cli-binary backend))
         (model (or (gptel-claude-cli-cli-model backend)
                   (gptel--model-name gptel-model)))
         (prompts (gptel--parse-buffer backend
                                      (and gptel--num-messages-to-send
                                           (* 2 gptel--num-messages-to-send))))
         (data (gptel--request-data backend prompts))
         (input-text (plist-get data :text))
         (callback (or (plist-get info :callback) #'gptel--insert-response))
         (output-buffer (generate-new-buffer " *gptel-claude-cli*"))
         (error-buffer (generate-new-buffer " *gptel-claude-cli-error*"))
         process)

    ;; Set up default callback if not provided
    (unless (plist-get info :callback)
      (plist-put info :callback callback))

    ;; Create the process
    (condition-case err
        (progn
          (setq process
                (make-process
                 :name "gptel-claude-cli"
                 :buffer output-buffer
                 :stderr error-buffer
                 :command (list binary "-p"
                               "--output-format" "json"
                               "--model" model)
                 :connection-type 'pipe
                 :sentinel
                 (lambda (proc _event)
                   (when (eq (process-status proc) 'exit)
                     (let ((exit-code (process-exit-status proc)))
                       (if (zerop exit-code)
                           ;; Success - parse response
                           (with-current-buffer output-buffer
                             (goto-char (point-min))
                             ;; Skip any non-JSON output (like warnings)
                             (when (re-search-forward "^{" nil t)
                               (beginning-of-line)
                               (let* ((json-object-type 'plist)
                                      (json-key-type 'keyword)
                                      (json-array-type 'vector)
                                      (response (condition-case parse-err
                                                    (json-read)
                                                  (error
                                                   (message "gptel-claude-cli: JSON parse error: %s" parse-err)
                                                   nil)))
                                      (response-text
                                       (when response
                                         (gptel--parse-response backend response info))))
                                 (plist-put info :status "finished")
                                 (when callback
                                   (funcall callback response-text info))))
                             (kill-buffer output-buffer)
                             (when (buffer-live-p error-buffer)
                               (kill-buffer error-buffer)))
                         ;; Error - get error text
                         (let ((error-text
                                (with-current-buffer error-buffer
                                  (buffer-string))))
                           (plist-put info :status (format "Error: %s" error-text))
                           (when callback
                             (funcall callback nil info))
                           (kill-buffer output-buffer)
                           (when (buffer-live-p error-buffer)
                             (kill-buffer error-buffer)))))))

                 :filter
                 (lambda (_proc string)
                   ;; Just accumulate output in buffer
                   (when (buffer-live-p output-buffer)
                     (with-current-buffer output-buffer
                       (goto-char (point-max))
                       (insert string))))))

          ;; Send input to process
          (process-send-string process input-text)
          (process-send-eof process)

          ;; Store process in request alist for cleanup
          (setf (alist-get process gptel--request-alist)
                (cons fsm
                      #'(lambda ()
                          (when (process-live-p process)
                            (delete-process process))
                          (when (buffer-live-p output-buffer)
                            (kill-buffer output-buffer))
                          (when (buffer-live-p error-buffer)
                            (kill-buffer error-buffer)))))

          ;; Return the process
          process)

      (error
       ;; Handle process creation errors
       (when (buffer-live-p output-buffer)
         (kill-buffer output-buffer))
       (when (buffer-live-p error-buffer)
         (kill-buffer error-buffer))
       (signal (car err) (cdr err))))))

;; Install advice to intercept transport for Claude CLI backends
(advice-add 'gptel-curl-get-response :around #'gptel-claude-cli--dispatch)

(provide 'gptel-claude-cli)
;;; gptel-claude-cli.el ends here
