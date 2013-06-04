(define-skeleton c++-class-skeleton
  "Makes a C++ Class"
  "Name of Class: "
  "class " str "\n"
  "{\n"
  "public:\n"
  str "();\n"
  "virtual ~" str "();\n\n"
  "private:\n"
  "struct Internals;\n"
  "Internals *intern;\n\n"
  str "(const " str " &);\n"
  str " &operator = (const " str " &);\n"
  "};"
  '(c-indent-defun))

(define-skeleton c++-stl-foreach
  "Makes a C++ for loop using STL style iterators"
  ""
  "for (" (setq iter (skeleton-read "Name of Iterator: "))
  " = " (setq container (skeleton-read "Name of Container: ")) ".begin(); "
  iter " != " container ".end(); "
  iter "++)")

(provide 'c++-skeletons)
