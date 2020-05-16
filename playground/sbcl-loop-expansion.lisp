;; In SBCL this simple loop

(loop for idx below 10
      collect idx)

;; Expands into:

(block nil
  (let ((idx 0))
    (declare (type (and number real) idx))
    (let* ((#:loop-list-head-746 (list nil))
           (#:loop-list-tail-747 #:loop-list-head-746))
      (tagbody
       sb-loop::next-loop
         (when (>= idx '10) (go sb-loop::end-loop))
         (rplacd #:loop-list-tail-747 (setq #:loop-list-tail-747 (list idx)))
         (setq idx (1+ idx))
         (go sb-loop::next-loop)
       sb-loop::end-loop
         (return-from nil (cdr #:loop-list-head-746))))))

;; It uses rplacd to replace the tail of the list with a new tail.

;; Here how it works:

POFTHEDAY> (defvar *head* (list nil))
*HEAD*
POFTHEDAY> (defvar *tail* *head*)
*TAIL*
POFTHEDAY> (car *head*)
NIL
POFTHEDAY> (cdr *head*)
NIL
POFTHEDAY> *head*
(NIL)
POFTHEDAY> (rplacd *tail*
                   (setf *tail*
                         (list 1)))
(NIL 1)
POFTHEDAY> *head*
(NIL 1)
POFTHEDAY> *tail*
(1)
POFTHEDAY> (cdr *tail*)
NIL
POFTHEDAY> (rplacd *tail*
                   (setf *tail*
                         (list 2)))
(1 2)
POFTHEDAY> (rplacd *tail*
                   (setf *tail*
                         (list 3)))
(2 3)
POFTHEDAY> *head*
(NIL 1 2 3)
POFTHEDAY> *tail*
(3)
POFTHEDAY> (cdr *head*)
(1 2 3)
