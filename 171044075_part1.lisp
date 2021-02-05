  
; CAN DUYAR - 171044075

(setf (readtable-case *readtable*) :invert) ;to make available for any test scenarios
; for example  ' 1 2 3 (G t U) should be equal to (1 2 3 G t U) so I used readtable-case to prevent possible mistakes.


; this function reads the "nested_list.txt" file.
(defun read_from_file (&optional (file "nested_list.txt"))
  (with-open-file (data file)   
      (loop :for line = (read-line data nil nil)
            :while line
            :collect (read-from-string (concatenate 'string "(" line ")")))))

; this is my flatten function to convert from nested lists to a specific flatten list.
(defun flattenFunc (to_flatten)
  (cond ((null to_flatten) nil)
        ((atom to_flatten) (list to_flatten))
        (t (loop for x in to_flatten appending (flattenFunc x))))
)

; this function creates a file named as "flatten_list.txt" and writes the flatten list on it.
(defun write_data (new_list)
    (with-open-file  (stream "flattened_list.txt" :direction :output :if-exists :supersede 
                                                   :if-does-not-exist :create)
    (format stream "~A " (flattenFunc new_list)))
)

; flattener is my main function.
(defun flattener()
	(write_data(read_from_file))
)

(flattener) ;calling flattener function
