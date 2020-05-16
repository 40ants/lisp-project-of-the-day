(in-package poftheday)


;; This is experimental code for #0069 post.

(defun read-file-1 (filename &key (separator #\Newline))
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  
  (let ((fd (osicat-posix:open filename
                               osicat-posix::o-rdonly))
        (current-string "")
        (separator (char-code separator)))
    (unwind-protect
         (cffi:with-foreign-pointer (buf (* 1024 10) buf-size)
           (loop for num-bytes of-type fixnum = (osicat-posix:read fd buf buf-size)
                 while (not (zerop num-bytes))
                 summing 1))
      (osicat-posix:close fd))))


(defun read-lines-1 (filename &key (separator #\Newline))
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  
  (let ((fd (osicat-posix:open filename
                               osicat-posix::o-rdonly))
        (current-string "")
        (separator (char-code separator)))
    (uiop:while-collecting (collect-string)
      (unwind-protect
           (cffi:with-foreign-pointer (buf (* 1024 10) buf-size)
             (flet ((collect-lines (num-bytes)
                      (loop with begin of-type fixnum = 0
                            for offset of-type fixnum from 0 below num-bytes
                            for char = (cffi:mem-ref buf :unsigned-char offset)
                              
                            when (= char separator)
                              do (let ((part (cffi:foreign-string-to-lisp buf
                                                                          :offset begin
                                                                          :count (- offset begin)
                                                                          :encoding :utf-8)))
                                   (setf begin (1+ offset))
                                     
                                   (cond ((zerop (length current-string))
                                          (collect-string part))
                                         (t
                                          (collect-string
                                           (concatenate 'string current-string part))
                                          (setf current-string ""))))
                            finally (unless (= offset num-bytes)
                                      (setf current-string
                                            (concatenate 'string
                                                         current-string
                                                         (cffi:foreign-string-to-lisp
                                                          buf
                                                          :offset begin
                                                          :count (- offset begin)
                                                          :encoding :utf-8)))))))
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
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  
  (let* ((mapping (cffi::lookup-mapping cffi::*foreign-string-mappings* :utf-8))
         (max-chars (1- array-total-size-limit))
         (decoder (babel-encodings:decoder mapping))
         (code-point-counter (babel-encodings:code-point-counter mapping)))
    (lambda (pointer offset count)
      (declare (optimize (speed 3) (debug 0) (safety 0)))
      (declare (type fixnum offset count))
      (multiple-value-bind (size new-end)
          (funcall code-point-counter
                   pointer offset (+ offset count) max-chars)
        (let ((string (make-string size :element-type 'babel:unicode-char)))
          (funcall decoder pointer offset new-end string 0)
          string)))))


(defun read-lines-2 (filename &key (separator #\Newline))
  "This version moves out out the loop few Babel's calls, creates a decoder
   and funcall it in the loop."
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
B                                                      begin
                                                      (- offset begin))))
                                   (setf begin (1+ offset))
                                     
                                   (cond ((zerop (length current-string))
                                          (collect-string part))
                                         (t
                                          (collect-string
                                           (concatenate 'string
                                                        current-string
                                                        part))
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


;;

(defun read-lines-3 (filename)
  "A version suggested by Luís Oliveira (https://twitter.com/luismbo):
   https://twitter.com/luismbo/status/1261635583799169025"
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))


;; This code is by christophejunke
;; https://gist.github.com/christophejunke/a0458d87ff7bd48c344c8deeade9c767
;;
;; * not much faster.
;; * if I have to copy strings then it slower.
;; * It does not decode from utf-8 to lisp characters.

(defparameter *max-line-size* 65536)
(defparameter *default-buffer-size* 16384)

(declaim (inline map-lines-on-shared-buffer))


(define-condition internal-buffer-overflow (error)
  ((buffer :reader internal-buffer-overflow/buffer :initarg :buffer)
   (variable :reader internal-buffer-overflow/variable  :initarg :variable)
   (size :reader internal-buffer-overflow/size :initarg :size)))


(defun map-lines-on-shared-buffer
    (stream function &key (buffer-size *default-buffer-size*))
  "Call FUNCTION for each line read from STREAM, using an internal buffer.
FUNCTION should accept a single argument, a string which content is
only available while the callback is processing it. The same
underlying buffer is used for successive invocations of FUNCTION."
  (check-type stream stream)
  (check-type function (or function symbol))
  (check-type buffer-size fixnum)
  (when (symbolp function)
    (setf function (symbol-function function)))
  (let ((view (make-array 0
                          :element-type 'character
                          :adjustable t))
        (buffer (make-array buffer-size :element-type 'character))
        (buffer-start 0) ;; where to start filling the buffer from stream
        (buffer-end 0)   ;; where buffer filling from stream ended
        (start 0)        ;; start of current line within buffer (w.r.t. #\newline)
        (end 0))         ;; end of current line within buffer
    (declare (type function function)
             (type string buffer)
             (type fixnum buffer-end buffer-start start end)
             (dynamic-extent buffer-end buffer-start start end))
    (flet ((callback (start end)
             (funcall function
                      (adjust-array view
                                    (- end start)
                                    :displaced-to buffer
                                    :displaced-index-offset start))))
      (declare (inline callback))
      (block nil
        (tagbody

         :buffer

           ;; Fill buffer with characters from the stream. When
           ;; nothing is read, we can exit the state machine. It is
           ;; however still possible that there are are characters
           ;; left between position zero and buffer-start: if so,
           ;; process that region with the callback function.

           (setf buffer-end
                 (read-sequence buffer stream :start buffer-start))
           (when (= buffer-end buffer-start)
             (when (plusp buffer-start)
               (callback 0 buffer-start))
             (return))

         :next-line

           ;; Try to find the end of line. If we reach the end of the
           ;; buffer, go instead to :COPY, which shifts the content on
           ;; the "left" to make room for more characters to buffer.

           (setf end
                 (or (position #\newline
                               buffer
                               :test #'char=
                               :start start
                               :end buffer-end)
                     (go :copy)))

           ;; The whole line fits in the buffer, from start to end.
           (callback start end)
           (setf start (1+ end))
           (go :next-line)

         :copy

           ;; not enough room, copy the latest line fragment (the one
           ;; being currently read) at the beginning of the buffer. If
           ;; we already are at the beginning (ZEROP START), then we
           ;; try to extend to buffer instead.

           (when (zerop start)
             (go :extend))

           (replace buffer buffer :start2 start :end2 buffer-end)
           (setf buffer-start (- buffer-end start))
           (setf start 0)

           ;; We shifted the current line on the left, because we did
           ;; not yet found a newline. Buffer more characters.
           (go :buffer)

         :extend

           (let ((size (array-total-size buffer)))
             (setf  buffer
                    (adjust-array
                     buffer
                     (if (>= size *max-line-size*)
                         (restart-case
                             (error 'internal-buffer-overflow
                                    :buffer buffer
                                    :variable '*max-line-size*
                                    :size size)
                           (ignore ()
                             :report "Ignore the limit and extend the buffer."
                             (* size 2)))
                         (min *max-line-size* (* size 2)))))
             (setf buffer-start size)
             (go :buffer)))))))


(defun read-lines-4 (filename)
  (with-open-file (in filename)
    (uiop:while-collecting (collect-string)
      (map-lines-on-shared-buffer
       in
       (lambda (s)
         (collect-string (copy-seq s)))))))



;; ======================================
;; The code below is not finished yet.
;; ======================================

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
