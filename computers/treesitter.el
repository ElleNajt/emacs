;;; computers/treesitter.el -*- lexical-binding: t; -*-


;;; Tree sitter
;;  Links to code downloaded from git
;; (setq combobulate-source-code-path "~/Documents/GitHub/combobulate")
;; (setq tsfold-source-code-path "~/Documents/GitHub/ts-fold")

;; (load! "../vendored/combobulate-config")
;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)))

;; ;; Enable tree-sitter globally
;; (global-tree-sitter-mode)

;; ;; Enable tree-sitter-based highlighting
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; ;; Load language grammars
;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (global-treesit-auto-mode))


(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package! tree-sitter-langs
  :after tree-sitter)

;; (use-package ts-fold
;;   :load-path tsfold-source-code-path)

;;;; Evil Object Tree sitter
;;;;; Evil text objects
;; Docs: You can also bind multiple items and we will match the first one we can find

(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
;; (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-function--class.outer ))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
(define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
(define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ( "assignment.outer")))
(define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ( "assignment.inner")))

;;;;; Goto text object
(defun goto-text-object (group &optional previous end query)
  (interactive)
  (evil-textobj-tree-sitter-goto-textobj group previous end query))
;; Some comments here


(map!
 :desc "Next Parameter" :nv "] x" (lambda () (interactive) (goto-text-object "parameter.inner"))
 :desc "Previous Parameter" :nv "[ x" (lambda () (interactive) (goto-text-object "parameter.inner" t))

 :desc "Next Return" :nv "] r" (lambda () (interactive) (goto-text-object "return.outer"))
 :desc "Previous Return" :nv "[ r" (lambda () (interactive) (goto-text-object "return.outer" t))
 :desc "Next Return" :nv "] R" (lambda () (interactive) (goto-text-object "return.inner"))
 :desc "Previous Return" :nv "[ R" (lambda () (interactive) (goto-text-object "return.inner" t))

 ;;  Next applicatioN
 :desc "Next Call" :nv "] n" (lambda () (interactive) (goto-text-object "call.outer"))
 :desc "Previous Call" :nv "[ n" (lambda () (interactive) (goto-text-object "call.outer" t))

 :desc "Next loop (Inner)" :nv "] l" (lambda () (interactive) (goto-text-object "loop.inner"))
 :desc "Previous loop (Inner)" :nv "[ l" (lambda () (interactive) (goto-text-object "loop.inner" t))
 :desc "Next loop (Outer)" :nv "] l" (lambda () (interactive) (goto-text-object "loop.outer"))
 :desc "Previous loop (Outer)" :nv "[ l" (lambda () (interactive) (goto-text-object "loop.outer" t))

 :desc "Next if (Inner)" :nv "] i" (lambda () (interactive) (goto-text-object "conditional.inner"))
 :desc "Previous if (Inner)" :nv "[ i" (lambda () (interactive) (goto-text-object "conditional.inner" t))
 :desc "Next if (Outer)" :nv "] i" (lambda () (interactive) (goto-text-object "conditional.outer"))
 :desc "Previous if (Outer)" :nv "[ i" (lambda () (interactive) (goto-text-object "conditional.outer" t))

 ;;  Mostly I want to move to outer assignments, it's easy to get to the inner assignment from them with WW
 :desc "Next Assignment" :nv "] a" (lambda () (interactive) (goto-text-object "assignment.outer"))
 :desc "Previous Assignment" :nv "[ a" (lambda () (interactive) (goto-text-object "assignment.outer" t))
 :desc "Next Assignment" :nv "] A" (lambda () (interactive) (goto-text-object "assignment.inner"))
 :desc "Previous Assignment" :nv "[ A" (lambda () (interactive) (goto-text-object "assignment.inner" t))

 :desc "Next class (Outer)" :nv "] c" (lambda () (interactive) (goto-text-object "class.outer"))
 :desc "Previous class (Outer)" :nv "[ c" (lambda () (interactive) (goto-text-object "class.outer" t))
 :desc "Next class (Inner)" :nv "] C" (lambda () (interactive) (goto-text-object "class.inner"))
 :desc "Previous class (Inner)" :nv "[ C" (lambda () (interactive) (goto-text-object "class.inner" t))

 ;;  undecided about this binding because Next/Previous function is cool, and [ m ] m feels kinda clunky
 :desc "Next function (Inner)" :nv "] F" (lambda () (interactive) (goto-text-object "function.inner"))
 :desc "Previous function (Inner)" :nv "[ F" (lambda () (interactive) (goto-text-object "function.inner" t))
 :desc "Next function (Outer)" :nv "] f" (lambda () (interactive) (goto-text-object "function.outer"))
 :desc "Previous function (Outer)" :nv "[ f" (lambda () (interactive) (goto-text-object "function.outer" t)))

;;;;; top / bottom of text object

;; Can these be made into motions?
(map!

 :desc "Begging of Function (Inner)" :nv "SPC f ["
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--function.inner ))))
 :desc "End of Function (Inner)" :nv "SPC f ]"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--function.inner ))))
 :desc "End of Function (Outer)" :nv "SPC f {"
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--function.outer ))))
 :desc "End of Function (Outer)" :nv "SPC f }"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--function.outer ))))

 :desc "Begging of Class (Inner)" :nv "SPC c ["
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--class.inner ))))
 :desc "End of Class (Inner)" :nv "SPC c ]"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--class.inner ))))
 :desc "End of Class (Outer)" :nv "SPC c {"
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--class.outer ))))
 :desc "End of Class (Outer)" :nv "SPC c }"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--class.outer ))))

 :desc "Begging of Loop (Inner)" :nv "SPC l ["
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--loop.inner ))))
 :desc "End of Loop (Inner)" :nv "SPC l ]"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--loop.inner ))))
 :desc "End of Loop (Outer)" :nv "SPC l {"
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--loop.outer ))))
 :desc "End of Loop (Outer)" :nv "SPC l }"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--loop.outer ))))

 :desc "Begging of Conditional (Inner)" :nv "SPC i ["
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--conditional.inner ))))
 :desc "End of Conditional (Inner)" :nv "SPC i ]"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--conditional.inner ))))
 :desc "End of Conditional (Outer)" :nv "SPC i {"
 (lambda () (interactive) (goto-char (car  (evil-textobj-tree-sitter-function--conditional.outer ))))
 :desc "End of Conditional (Outer)" :nv "SPC i }"
 (lambda () (interactive) (goto-char (cadr  (evil-textobj-tree-sitter-function--conditional.outer ))))

 )
