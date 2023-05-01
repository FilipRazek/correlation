(defun push-point-to-lists (l point)
  (if (null l)
      nil
      (nconc
      (list  (nconc (list (car point)) (car l)))   
       (push-point-to-lists (cdr l) (cdr point)))
      ))
              
(defun listify (l) (map 'list 'list l))

(defun invert (point &rest points)
  (if (= (length points) 0)
    (listify point)
     (push-point-to-lists (apply 'invert points) point)
      ))


(defun mean (l) (/ (apply '+ l) (length l)))
(defun mult (X Y) (map 'list '* X Y))
(defun dot (X Y) (apply '+ (mult X Y)))
(defun add-constant (l c) (map 'list (lambda (x) (+ x c)) l))
(defun remove-mean (l) (add-constant l (- (mean l))))
(defun covar (X Y) (/ (dot (remove-mean X) (remove-mean Y)) (length X)))
(defun var (X) (- (/ (dot X X) (length X)) (expt (mean X) 2)))
(defun s (X) (sqrt (var X)))
(defun corr (X Y) (/ (covar X Y) (* (s X) (s Y))))


(defun build-triangle (fn values)
  (if (<= (length values) 1)
      (list (list (apply fn (list (car values) (car values)))))
      (nconc
       (build-triangle fn (cdr values))
      (list (reverse (map 'list
		  (lambda
		     (temp)
		    (apply fn (list temp (car values))))
		  values))))
       
      ))

(defun print-table (table)
  (if (null table)
      nil
      (nconc (print (car table)) (print-table (cdr table)))
      ))

;https://stackoverflow.com/questions/3813895/how-can-i-read-the-contents-of-a-file-into-a-list-in-lisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
(defun delimiterp (c) (position c " ,.;/"))
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))


(defun main () (nconc 
(print-table (build-triangle 'corr (apply 'invert (map 'list (lambda (str) (map 'list 'parse-integer (my-split str))) (get-file (read-line))))))
(read)))
