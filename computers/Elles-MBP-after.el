;;; -*- lexical-binding: t; -*-


(with-eval-after-load 'org
  (add-to-list 'org-capture-templates
               '("f" "Food journal entry" entry
                 (file+olp+datetree  "~/Documents/Notes/foodjournal.org")
                 "* %U %?\n%i" :prepend t))

  (add-to-list 'org-agenda-custom-commands '("pa" "Aspen" tags-todo "aspen")))
