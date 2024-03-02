;;; experimental/treesitterexperiments.el -*- lexical-binding: t; -*-

(defun testing/AST-parent ()
  "Return parent node of the tree-sitter node at point."
  (interactive)
  (let ((node (tree-sitter-node-at-pos)))
    (if node
	(message "%s" (tsc-node-type (tsc-get-parent (tsc-get-parent node))))
      (message "No parent node at point"))))
