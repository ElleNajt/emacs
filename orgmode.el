;;; -*- lexical-binding: t; -*-


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
                              ("l" "Linked todo" entry
                               (file+headline +org-capture-todo-file  "Inbox")
                               "* TODO %?\n%i\n%a%T" )
                              ;; ("n" "Inbox-Note" entry
                              ;;  (file+headline +org-capture-todo-file  "Inbox Note")
                              ;; "* %?\n%i%T" )
                              ("j" "Journal" entry
                               (file+olp+datetree +org-capture-journal-file)
                               "* %U %?\n%i\n%a" :prepend t))
      org-archive-location (concat org-directory "/trash::* from %s"))

(setq org-log-done 't)
