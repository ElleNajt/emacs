;;; computers/mcp.el -*- lexical-binding: t; -*-



;;; MCP
;; taken from https://github.com/lizqwerscott/mcp.el



;; ("mcp_nixos" . (:command "/home/elle/code/mcp_servers/.venv/bin/python" :args ("-m" "mcp_nixos")))

(setq mcp-hub-servers
      '(
        ("emacs_introspection" . (:command "npx" :args ("-y" "@lnajt/emacs-introspection-mcp")) )
        ("nixos" . (:command "uvx" :args ("--install-deps" "mcp-nixos")))
        ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/elle/code/ai_agents/")))))

(mcp-hub-restart-all-server)
(add-hook 'after-init-hook
          #'mcp-hub-start-all-server)

(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))


(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(gptel-mcp-register-tool)

(gptel-mcp-use-tool)


(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

;; (gptel-mcp-close-use-tool)
(setq mysecretnumber "42.00")
