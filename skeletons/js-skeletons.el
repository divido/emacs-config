(define-skeleton js-console-log-var
  "Creates a console.log for a single variable"
  "Variable: "
  "console.log('" str " =', " str ");")

(define-skeleton js-console-log-json-var
  "Creates a console.log for a single variable, passing it through JSON.stringify"
  "Variable: "
  "console.log('" str " =', JSON.stringify(" str ", null, 2));")

(provide 'js-skeletons)
