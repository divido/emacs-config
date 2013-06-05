;; Adds "mockable" to the list of keywords. This is used in practice as a #define
;; in many of my projects.
(font-lock-add-keywords
 'c++-mode
 '(("\\<mockable\\>" . font-lock-keyword-face)))

(provide 'c++-extra-keywords)
