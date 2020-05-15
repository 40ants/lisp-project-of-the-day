(defpackage #:fast-read-lines
  (:use #:cl))
(in-package fast-read-lines)


;; This is experimental code for #0069 post.


(defun read-lines (filename &key (separator #\Newline))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  
  (let ((fd (osicat-posix:open filename
                               osicat-posix::o-rdonly))
        (current-string "")
        (separator (char-code separator)))
    (uiop:while-collecting (collect-string)
      (unwind-protect
           (cffi:with-foreign-pointer (buf 1024 buf-size)
             (flet ((collect-lines (num-bytes)
                      (loop with begin of-type fixnum = 0
                            for offset of-type fixnum from 0 below num-bytes
                            for char = (cffi:mem-ref buf :unsigned-char offset)
                              
                            when (= char separator)
                              do (let ((part (cffi:foreign-string-to-lisp buf
                                                                          :offset begin
                                                                          :count (- offset begin))))
                                   (setf begin (1+ offset))
                                     
                                   (cond ((zerop (length current-string))
                                          (collect-string part))
                                         (t
                                          (collect-string
                                           (concatenate 'string part))
                                          (setf current-string ""))))
                            finally (unless (= offset num-bytes)
                                      (setf current-string
                                            (concatenate 'string
                                                         current-string
                                                         (cffi:foreign-string-to-lisp
                                                          buf
                                                          :offset begin
                                                          :count (- offset begin))))))))
               (loop for num-bytes of-type fixnum = (osicat-posix:read fd buf buf-size)
                     while (not (zerop num-bytes))
                     do (collect-lines num-bytes)
                     finally (when current-string
                               (collect-string current-string))))
             )
        (osicat-posix:close fd)))))


(defun make-utf-8-decoder ()
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  
  (let* ((mapping (cffi::lookup-mapping cffi::*foreign-string-mappings* :utf-8))
         (max-chars (1- array-total-size-limit))
         (decoder (babel-encodings:decoder mapping))
         (code-point-counter (babel-encodings:code-point-counter mapping)))
    (describe decoder)
    (lambda (pointer offset count)
      (declare (optimize (speed 3) (debug 0) (safety 0)))
      (declare (type fixnum offset count))
      (multiple-value-bind (size new-end)
          (funcall code-point-counter
                   pointer offset (+ offset count) max-chars)
        (let ((string (make-string size :element-type 'babel:unicode-char)))
          (funcall decoder pointer offset new-end string 0)
          string)))))


(defun optimized-read-lines (filename &key (separator #\Newline))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  
  (let ((fd (osicat-posix:open filename
                               osicat-posix::o-rdonly))
        (current-string "")
        (separator (char-code separator))
        (utf8-decoder (make-utf-8-decoder)))
    
    (uiop:while-collecting (collect-string)
      (unwind-protect
           (cffi:with-foreign-pointer (buf 1024 buf-size)
             (flet ((collect-lines (num-bytes)
                      (loop with begin of-type fixnum = 0
                            for offset of-type fixnum from 0 below num-bytes
                            for char = (cffi:mem-ref buf :unsigned-char offset)
                              
                            when (= char separator)
                              do (let ((part (funcall utf8-decoder
                                                      buf
                                                      begin
                                                      (- offset begin))))
                                   (setf begin (1+ offset))
                                     
                                   (cond ((zerop (length current-string))
                                          (collect-string part))
                                         (t
                                          (collect-string
                                           (concatenate 'string part))
                                          (setf current-string ""))))
                            finally (unless (= offset num-bytes)
                                      (setf current-string
                                            (concatenate 'string
                                                         current-string
                                                         (funcall utf8-decoder
                                                                  buf
                                                                  begin
                                                                  (- offset begin))))))))
               (loop for num-bytes of-type fixnum = (osicat-posix:read fd buf buf-size)
                     while (not (zerop num-bytes))
                     do (collect-lines num-bytes)
                     finally (when current-string
                               (collect-string current-string))))
             )
        (osicat-posix:close fd)))))


(declaim (ftype (function (cffi:foreign-pointer
                           fixnum
                           fixnum
                           simple-string
                           fixnum)
                          fixnum)
                utf-8-decoder))
(defun utf-8-decoder (src start end dest d-start)
  (declare (type cffi:foreign-pointer src)
           (type simple-string dest)
           (fixnum start end d-start))
  (let ((u2 0) (u3 0) (u4 0) (u5 0) (u6 0))
    (declare (type babel-encodings::ub8 u2 u3 u4 u5 u6))
    (loop for di fixnum from d-start
          for i fixnum from start below end
          for u1 of-type babel-encodings::ub8 = (cffi:mem-aref src :uchar i) do
            ;; Note: CONSUME-OCTET doesn't check if I is being
            ;; incremented past END.  We're assuming that END has
            ;; been calculated with the CODE-POINT-POINTER above that
            ;; checks this.
            (macrolet
                ((consume-octet ()
                   `(let ((next-i (incf i)))
                      (if (= next-i end)
                          ;; FIXME: data for this error is incomplete.
                          ;; and signalling this error twice
                          (return-from setter-block
                            (babel-encodings::decoding-error
                             nil :utf-8 src i
                             babel-encodings::+repl+
                             'end-of-input-in-character))
                          (cffi:mem-aref src :uchar next-i))))
                 (handle-error (n &optional (c 'character-decoding-error))
                   `(babel-encodings::decoding-error
                     (vector ,@(subseq '(u1 u2 u3 u4 u5 u6) 0 n))
                     :utf-8 src (1+ (- i ,n)) babel-encodings::+repl+ ',c))
                 (handle-error-if-icb (var n)
                   `(when (not (< #x7f ,var #xc0))
                      (decf i)
                      (return-from setter-block
                        (handle-error ,n invalid-utf8-continuation-byte)))))
              (setf
               (aref dest di)
               (block setter-block
                 (cond
                   ((< u1 #x80) u1)     ; 1 octet
                   ((< u1 #xc0)
                    (handle-error 1 invalid-utf8-starter-byte))
                   (t
                    (setq u2 (consume-octet))
                    (handle-error-if-icb u2 1)
                    (cond
                      ((< u1 #xc2)
                       (handle-error 2 overlong-utf8-sequence))
                      ((< u1 #xe0)      ; 2 octets
                       (logior (babel-encodings::f-ash
                                (babel-encodings::f-logand #x1f u1) 6)
                               (babel-encodings::f-logxor u2 #x80)))
                      (t
                       (setq u3 (consume-octet))
                       (handle-error-if-icb u3 2)
                       (cond
                         ((and (= u1 #xe0) (< u2 #xa0))
                          (handle-error 3 overlong-utf8-sequence))
                         ((< u1 #xf0)   ; 3 octets
                          (let ((start (babel-encodings::f-logior
                                        (babel-encodings::f-ash
                                         (babel-encodings::f-logand u1 #x0f)
                                         12)
                                        (babel-encodings::f-ash
                                         (babel-encodings::f-logand u2 #x3f)
                                         6))))
                            (if (<= #xd800 start #xdfc0)
                                (handle-error 3 character-out-of-range)
                                (logior start (babel-encodings::f-logand
                                               u3 #x3f)))))
                         (t             ; 4 octets
                          (setq u4 (consume-octet))
                          (handle-error-if-icb u4 3)
                          (cond
                            ((and (= u1 #xf0) (< u2 #x90))
                             (handle-error 4 overlong-utf8-sequence))
                            ((< u1 #xf8)
                             (if (or (> u1 #xf4) (and (= u1 #xf4) (> u2 #x8f)))
                                 (handle-error 4 character-out-of-range)
                                 (babel-encodings::f-logior
                                  (babel-encodings::f-ash
                                   (babel-encodings::f-logand u1 7) 18)
                                  (babel-encodings::f-ash
                                   (babel-encodings::f-logxor u2 #x80) 12)
                                  (babel-encodings::f-ash
                                   (babel-encodings::f-logxor u3 #x80) 6)
                                  (babel-encodings::f-logxor u4 #x80))))
                            ;; from here on we'll be getting either
                            ;; invalid continuation bytes or overlong
                            ;; 5-byte or 6-byte sequences.
                            (t
                             (setq u5 (consume-octet))
                             (handle-error-if-icb u5 4)
                             (cond
                               ((and (= u1 #xf8) (< u2 #x88))
                                (handle-error 5 overlong-utf8-sequence))
                               ((< u1 #xfc)
                                (handle-error 5 character-out-of-range))
                               (t
                                (setq u6 (consume-octet))
                                (handle-error-if-icb u6 5)
                                (cond
                                  ((and (= u1 #xfc) (< u2 #x84))
                                   (handle-error 6 overlong-utf8-sequence))
                                  (t
                                   (handle-error 6 character-out-of-range))))))))))))))))
          finally (return (the fixnum (- di d-start))))))


(defun optimized-read-lines2 (filename &key (separator #\Newline))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  
  (let ((fd (osicat-posix:open filename
                               osicat-posix::o-rdonly))
        (current-string ""))
    
    (uiop:while-collecting (collect-string)
      (unwind-protect
           (cffi:with-foreign-pointer (buf 1024 buf-size)
             (flet ((collect-lines (num-bytes)
                      (loop with begin of-type fixnum = 0
                            for offset of-type fixnum from 0 below num-bytes
                            for char = (cffi:mem-ref buf :unsigned-char offset)
                              
                            when (= char separator)
                              do (let ((part (funcall utf8-decoder
                                                      buf
                                                      begin
                                                      (- offset begin))))
                                   (setf begin (1+ offset))
                                     
                                   (cond ((zerop (length current-string))
                                          (collect-string part))
                                         (t
                                          (collect-string
                                           (concatenate 'string part))
                                          (setf current-string ""))))
                            finally (unless (= offset num-bytes)
                                      (setf current-string
                                            (concatenate 'string
                                                         current-string
                                                         (funcall utf8-decoder
                                                                  buf
                                                                  begin
                                                                  (- offset begin))))))))
               (loop for num-bytes of-type fixnum = (osicat-posix:read fd buf buf-size)
                     while (not (zerop num-bytes))
                     do (collect-lines num-bytes)
                     finally (when current-string
                               (collect-string current-string)))))
        (osicat-posix:close fd)))))
