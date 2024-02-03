;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (setq org-hierarchical-todo-statistics nil
        +org-capture-todo-file  (concat org-directory "/inbox.org")
        +org-capture-journal-file (concat org-directory "/journal.org")
        org-default- (concat org-directory "/inbox.org")
        org-capture-templates '(("a" "Todo" entry
                                 (file+headline +org-capture-todo-file  "Inbox")
                                 "* TODO %?\n%i%T" )
                                ("t" "Todo" entry
                                 (file+headline +org-capture-todo-file  "Inbox")
                                 "* TODO %?\n%i%T" )
                                ("T" "Todo with link" entry
                                 (file+headline +org-capture-todo-file  "Inbox")
                                 "* TODO %?\n%i\n%a%T" )
                                ;; ("n" "Inbox-Note" entry
                                ;;  (file+headline +org-capture-todo-file  "Inbox Note")
                                ;; "* %?\n%i%T" )
                                ("j" "Journal entry" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i" :prepend t)
                                ("J" "Journal entry with link" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i\n%a" :prepend t))
        org-archive-location (concat org-directory "/trash::* from %s")))

(setq org-log-done 't)
