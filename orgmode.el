;;; -*- lexical-binding: t; -*-


(setq org-hierarchical-todo-statistics nil
      org-default-notes-file (concat org-directory "inbox.org")
      org-capture-templates '(("a" "Todo" entry
                               (file+headline +org-capture-notes-file "Inbox")
                               "* TODO %?\n%i%T" )
                              ("t" "Todo" entry
                               (file+headline +org-capture-notes-file "Inbox")
                               "* TODO %?\n%i%T" )
                              ("n" "Note" entry
                               (file+headline +org-capture-notes-file "Inbox")
                               "* %?\n%i%T" )
                              ("l" "Linked todo" entry
                               (file+headline +org-capture-notes-file "Inbox")
                               "* TODO %?\n%i\n%a%T" ))
      org-archive-location (concat org-directory "trash::* from %s"))

(setq org-log-done 't)
