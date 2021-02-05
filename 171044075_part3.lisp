; CAN DUYAR - 171044075

; this is my flatten function from question-1. I also used it to solve this question.
(defun flattenFunc (to_flatten)
  (cond ((null to_flatten) nil)
        ((atom to_flatten) (list to_flatten))
        (t (loop for x in to_flatten appending (flattenFunc x))))
)

; this function reads the "boundries.txt" file.
(defun read_from_file (&optional (filename "integer_inputs.txt"))
    (with-open-file (stream (make-pathname :name filename) :direction :input)
        (loop for line = (read-line stream nil nil)
            while line
             collect (read-from-string (concatenate 'string "(" line ")")))))

;this function prints collatz sequence on the file which is given as parameter
(defun collatz_sequence(col stream)
	(progn
	  (format stream " ~D" col)
	  (cond
	    ((= col 1) col)
	    ((= (mod col 2) 0) (collatz_sequence(/ col 2) stream))
	    ((= (mod col 2) 1) (collatz_sequence(+ 1 (* col 3)) stream)))))

; this function creates a file named as "collatz_outputs.txt" and writes the results on it.
(defun write_data  (&optional(filename "collatz_outputs.txt"))
	(setq list1 (flattenFunc(read_from_file)))
	(setq len (- (length list1) 1))

	(if (>= len 5) ; value of len should be max 5 because I read at most 5 integers (according to pdf instructions).
		(setq len 4))
			
	(if(/= len -1) ; if file is not empty then create a file named as "collatz_outputs.txt" and write everything on it.			
    (with-open-file  (stream filename :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)

	(loop for a from 0 to len
		do (format stream "~S :" (nth a list1))	; print on file			
			(collatz_sequence(nth a list1) stream)(terpri stream))
	)
)
	(if (= len -1) ; if file is empty
		(print "Your file is empty!!!"))
)

(write_data)