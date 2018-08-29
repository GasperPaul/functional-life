(defun apply_rule (world rule neighbourhood cell)
    (let* ((state (getf cell :state))
           (position (getf cell :position))
           (neighbours (funcall neighbourhood world position)))
          (list :state (funcall rule state neighbours) :position position)))

(defun step_time (world rule neighbourhood)
    (mapcar (lambda (cell) (apply_rule world rule neighbourhood cell)) world))

;----------------

(deftype life-states () '(member :alive :dead))

(defun rule_life (state neighbours)
    (labels ((is-alive (cell) (eq (getf cell :state) :alive)))
            (let ((count (count-if #'is-alive neighbours)))
                 (cond 
                     ((and (eq state :alive) (or (< count 2) (> count 3))) :dead)
                     ((and (eq state :dead) (= count 3)) :alive)
                     (t state)))))

(defun neighbourhood_life (world pos)
    (labels ((is-near (c1 c2) (<= (abs (- c1 c2)) 1))
             (filter-neighbours (cell) (let ((p (getf cell :position))) 
                                            (and (every #'identity (mapcar #'is-near p pos)) (not (eq p pos))))))
            (remove-if-not #'filter-neighbours world)))

;----------------

(defun generate_world (str)
    (labels ((generate-state (ch) (cond ((eq ch #\# ) :alive) (t :dead))))
            (reduce #'append (loop for n = 0 then (incf m)
                                   for i = 0 then (incf i)
                                   as j = -1
                                   as m = (position #\Newline str :start n)
                                   collect (mapcar (lambda (ch) (list :state (generate-state ch) :position (list i (incf j))))
                                                   (coerce (subseq str n m) 'list))
                                   while m))))

(defvar initial_state (generate_world "
0#00000000
00#0000000
###0000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000"))

(defun print_world (world)
    (let ((len (reduce #'max (mapcar (lambda (c) (car (last (getf c :position)))) world))))
         (labels ((print-cell (state) (cond ((eq state :alive) "#") (t "0")))
                  (to-str (cell) (let ((s (getf cell :state))
                                       (p (getf cell :position)))
                                      (concatenate 'string (print-cell s) (cond ((= (car (last p)) len) (string #\Newline)) (t ""))))))
                 (print (apply #'concatenate 'string (mapcar #'to-str world))))))

(defun main () 
    (print_world (step_time initial_state #'rule_life #'neighbourhood_life)))

(main)