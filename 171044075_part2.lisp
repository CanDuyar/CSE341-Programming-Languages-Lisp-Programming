
; CAN DUYAR - 171044075

;it returns T(true) if number which is given as parameter is a semiprime number, otherwise returns nil(false).
(defun is_semiprime (param &optional (x 2))
  (cond ((> x (isqrt param)) nil)
        ((zerop (rem param x)) (and (is_prime x) (is_prime (/ param x))))
        (t (is_semiprime param (+ x 1)))))

;it returns T(true) if number which is given as parameter is a prime number, otherwise returns nil(false).
(defun is_prime (param &optional (x 2))
  (cond ((> x (isqrt param)) t)
        ((zerop (rem param x)) nil)
        (t (is_prime param (+ x 1)))))

; this is my flatten function from question-1. I also used it to solve this question.
(defun flattenFunc (to_flatten)
  (cond ((null to_flatten) nil)
        ((atom to_flatten) (list to_flatten))
        (t (loop for x in to_flatten appending (flattenFunc x))))
  )


; this function reads the "boundries.txt" file.
(defun read_from_file (&optional (filename "boundries.txt"))

    (with-open-file (stream (make-pathname :name filename) :direction :input)
        (loop for line = (read-line stream nil nil)
            while line
             collect (read-from-string (concatenate 'string "(" line ")")))))


; this function creates a file named as "primedistribution.txt" and writes the results on it.
(defun write_data  (&optional(filename "primedistribution.txt"))
    (with-open-file  (stream filename :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)

        (setq list_keep (flattenFunc(read_from_file))) ; list_keep keeps the value which comes from read_from_file function
        (setq num_max (max (car list_keep) (car(cdr list_keep)))) ; max number
        (setq num_min (min (car list_keep) (car(cdr list_keep)))) ; min number
  

        (loop for x from num_min to num_max
   			do (when (is_prime x)
   					(format stream "~S " x)(format stream "is Prime ~%"))

   				(when (is_semiprime x)
   					(format stream "~S " x)(format stream "is Semi-prime ~%"))
   		)
   )
)

; this is my main function named as primecrawler
(defun primecrawler()
  (write_data)
)

(primecrawler) ; calling primecrawler function to see results

