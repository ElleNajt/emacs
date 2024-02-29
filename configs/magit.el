;;; configs/magit.el -*- lexical-binding: t; -*-

(evil-set-command-property 'smerge-next :repeat nil)
(evil-set-command-property 'smerge-prev :repeat nil)

(map!
 (:leader
  (:desc "previous Git conflict" :n "[ n" #'smerge-prev)
  (:desc "next Git conflict" :n "] n" #'smerge-next)
  (:desc "smerge" :prefix "m"
   :desc "Keep Current" :n "SPC" #'smerge-keep-current
   :desc "Keep All"     :n "a" #'smerge-keep-all
   :desc "Keep Upper"   :n "u" #'smerge-keep-upper
   :desc "Keep Lower"   :n "l" #'smerge-keep-lower)))

