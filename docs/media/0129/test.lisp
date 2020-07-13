(in-package poftheday)

(defun process-even-clause (value clause)
  (trivial-with-current-source-form:with-current-source-form (clause)
    (destructuring-bind (number &rest body) clause
      (unless (evenp number)
        (error "Only even numbers are allowed."))
      `((= ,value ,number)
        ,@body))))

(defmacro even-number-case (value &body clauses)
  "Like `cl:case' but each key has to be an even number."
  (alexandria:once-only (value)
    `(cond ,@(mapcar
              (alexandria:curry #'process-even-clause
                                value)
              clauses))))

(defun app-code ()
  (even-number-case 100
    (2 :two)
    (4 :three)
    (6 :four)))
