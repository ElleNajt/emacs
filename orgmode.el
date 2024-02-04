;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (setq org-hierarchical-todo-statistics nil
        +org-capture-todo-file  (concat org-directory "/inbox.org")
        +org-capture-journal-file (concat org-directory "/journal.org")
        ;; (concat org-directory "/foodjournal.org")
        org-capture-templates '(("a" "Todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i%U" )
                                ("t" "Todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i%U" )
                                ("T" "Todo with link" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i\n%a%U" )
                                ;; ("n" "Inbox-Note" entry
                                ;;  (file+headline +org-capture-todo-file  "Inbox Note")
                                ;; "* %?\n%i%T" )
                                ("f" "Food journal entry" entry
                                 (file+olp+datetree  "~/Documents/Notes/foodjournal.org")
                                 "* %U %?\n%i" :prepend t)
                                ("j" "Journal entry" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i" :prepend t)
                                ("J" "Journal entry with link" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i\n%a" :prepend t))
        org-archive-location (concat org-directory "/trash::* from %s")
        org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d)" "RUNNING(r)")
                            (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
        org-log-done 't
        org-agenda-hide-tags-regexp "journal\\|tag2\\|tags3"
        org-agenda-custom-commands
        '(("i" "Inbox" tags "inbox")
          ("p" . "Person...")
          ("pa" "Aspen" tags-todo "aspen"))))

;; (with-eval-after-load 'org
;;   (add-to-list 'org-modules 'org-habit t))
