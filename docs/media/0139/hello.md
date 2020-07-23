    (defpackage #:hello-world
      (:use :cl :named-readtables))
    (in-package #:hello-world)
    
    (in-readtable :papyrus)

# Hello world with Papyrus

As you probably know, every programmer starts his learning of the
new programming language from the "hello world" program.

Simplest hello world program outputs a text "Hello World!" in console and exit.

Here is how we can output this program in Common Lisp:

```lisp
(defun main ()
    (princ "Hello World!")
    (terpri))
```
