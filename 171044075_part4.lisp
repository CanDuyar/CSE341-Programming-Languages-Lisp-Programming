
; CAN DUYAR - 171044075

(defstruct treeNode ;node of huffman tree structure 
  (frequency 0 :type number) ;it keeps frequency of letter
  (huffman_code nil :type (or bit-vector null)) ;to keep huffman_codes of each nodes
  (letter nil :type t) ;to keep letters of file
  (left nil :type (or treeNode null)) ;left node
  (right nil :type (or treeNode null))) ;right node
 
; this function sorts to obtain triangular shape in file
(defun comparison (s)
(sort s (lambda (alfa beta) (<(length(write-to-string(car alfa))) (length(write-to-string(car beta))))))s) 

#|this function sorts huffman codes with the help of my "comparison" function. I used it because codes in 
the files should be ordered according to their length. Shorter codes should appear at the top of the file
|#
(defun triangularShape (nodes &optional)
  (setq gtu (make-hash-table))
  (loop for node being each hash-value of nodes
        do (setf(gethash (parse-integer (remove #\*(remove (aref (write-to-string (treeNode-huffman_code node) ) 0)
         (write-to-string (treeNode-huffman_code node) )))) gtu)(remove #\\(remove (aref (write-to-string (treeNode-letter node) ) 0) (write-to-string (treeNode-letter node) ))))
  )
  (setq pl nil)
  (maphash (lambda (key val)(push (cons key val) pl)) gtu)
  (setf triangular pl)
  (comparison triangular)
  )

;this function reads the file named as "paragraph.txt"
(defun read-file (&optional (filename "paragraph.txt"))
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;this is my main function to define structure of huffman tree
(defun huffman_structure (structure &key)

	(multiple-value-bind (param_nodes order)(let* ((length (length structure))(increment (/ 1 length))
  (param_nodes (make-hash-table :size length))(order '()))(map nil #'(lambda (letter)
  (multiple-value-bind (node presentp) (gethash letter param_nodes)
    (if presentp(incf (treeNode-frequency node) increment)
      (let ((node (make-treeNode :frequency increment :letter letter)))
      (setf (gethash letter param_nodes) node order (list* node order)))))) structure)
      (values param_nodes (sort order '< :key 'treeNode-frequency))
    )
    (do () ((endp (cdr order)) (values param_nodes (first order)))
		
    (destructuring-bind (le ri &rest order-rest) order (let ((nod (make-treeNode :left le :right ri
    :frequency (+ (treeNode-frequency le)(treeNode-frequency ri)))))
		(setf order (merge 'list (list nod) order-rest '< :key 'treeNode-frequency))))
    )
  )
)

 ;this function does huffman encoding operation to give new binary values with the help of left-right operations
(defun huffmanEncoding (structure_param &key)
	(multiple-value-bind (param_nodes tree)
    (huffman_structure structure_param)
    (labels ((hc (node length bits)
                (let ((left (treeNode-left node))
                     (right (treeNode-right node)))
               	(cond
                  	((and (null left) (null right))
                   	(setf (treeNode-huffman_code node)
                         (make-array length :element-type 'bit :initial-contents (reverse bits))))
                    (t (hc left (1+ length) (list* 0 bits))
                     (hc right (1+ length) (list* 1 bits)))))))
    (hc tree 0 '()) param_nodes)
  )
)
 
 ;this function writes huffman codes on file named as "huffman_codes.txt"
(defun write_HuffmanFile(&optional(filename "huffman_codes.txt"))
  (with-open-file (stream filename :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)  
    (triangularShape(huffmanEncoding (read-file)))
    (loop for (alfa . beta) in triangular 
      do (format stream "~a: ~10t ~a~%" beta alfa))         
    )    
)

; this is my main function
(defun huffmanAlgorithm()
  (write_HuffmanFile)
)

(huffmanAlgorithm) ; calling huffmanAlgorithm function to run entire program