;;; -*- lexical-binding: t; -*-


(add-to-list 'org-capture-templates
             '("f" "Food journal entry" entry
               (file+olp+datetree  "~/Documents/Notes/foodjournal.org")
               "* %U %?\n%i" :prepend t))
