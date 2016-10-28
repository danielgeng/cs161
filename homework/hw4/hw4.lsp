(defun split-line (line)
  (if (equal line :eof)
    :eof
    (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; checks if n or its negation is in l
(defun is_in (n l)
  (or (> (count n l) 0) (> (count (* n -1) l) 0))
)

; checks if l has any contradictions
(defun is_valid (l)
  (cond
    ((null l) t)
    ((> (count (* (car l) -1) l) 0) nil)
    (t (is_valid (cdr l)))
  )
)

; handles unit clauses
; input: delta (list of clauses), sol (solution), ndelta (null list)
; output: list of new of clauses and new solution
(defun unit_clauses (delta sol ndelta)
  (let ((nd (if (null ndelta) delta ndelta)))
  (cond
    ((null delta) (list ndelta sol))
    ((= (length (car delta)) 1) ; unit clause found
      (unit_clauses
        (cdr delta)
        (append sol (list (car (car delta)))) ; update solution
        (rm_literal nd (car (car delta))) ; update delta
      )
    )
    (t (unit_clauses (cdr delta) sol nd))
  )
  )
)

; remove all clauses containing n, remove -n from all clauses
(defun rm_literal (delta n)
  (cond
    ((null delta) nil)
    ; remove -n from all clauses
    ((> (count (* n -1) (car delta)) 0)
      (append 
        (list (rm_literal_helper (car delta) (* n -1)))
        (rm_literal (cdr delta) n)
      )
    )
    ; remove all clauses containing n
    ((> (count n (car delta)) 0) (rm_literal (cdr delta) n))
    (t (append (list (car delta)) (rm_literal (cdr delta) n)))
  )
)

; removes literal n from clause l
(defun rm_literal_helper (l n)
  (cond
    ((null l) nil)
    ((= (car l) n) (cdr l))
    (t (append (list (car l)) (rm_literal_helper (cdr l) n)))
  )
)

; count number of occurrences of n in delta
(defun get_counts (delta n)
  (cond
    ((null delta) 0)
    (t (+ (count n (car delta)) (get_counts (cdr delta) n)))
  )
)

; handles pure literals
; input: delta (list of clauses), sol (solution), n (# of vars)
; output: list of new of clauses and new solution
(defun pure_literals (delta sol n)
  (cond
    ((= n 0) (list delta sol))
    ; move on if this literal is accounted for
    ((is_in n sol) (pure_literals delta sol (- n 1)))
    (t (let (
      (ct_l (get_counts delta n)) ; count of n
      (ct_nl (get_counts delta (* n -1)))) ; count of -n
      (cond
        ; both non-zero: no action
        ((and (not (= ct_l 0)) (not (= ct_nl 0))) 
          (pure_literals delta sol (- n 1))
        )
        ; both zero: just add n to solution
        ((and (= ct_l 0) (= ct_nl 0))
          (pure_literals delta (append sol (list n)) (- n 1)))
        ; no n: remove all clauses with -n, add -n to solution
        ((= ct_l 0)
          (pure_literals
            (rm_pure_lit delta (* n -1))
            (append sol (list (* n -1)))
            (- n 1)
          )
        )
        ; no -n: remove all clauses with n, add n to solution
        (t
          (pure_literals
            (rm_pure_lit delta n)
            (append sol (list n))
            (- n 1)
          )
        )
      )
    ))
  )
)

; removes clauses that contain n
(defun rm_pure_lit (delta n)
  (cond
    ((null delta) nil)
    ((> (count n (car delta)) 0) (rm_pure_lit (cdr delta) n))
    (t (append (list (car delta)) (rm_pure_lit (cdr delta) n)))
  )
)

; returns a literal not in the solution
(defun find_lit (sol n)
  (cond
    ((= n 0) 0)
    ((not (is_in n sol)) n)
    (t (find_lit sol (- n 1)))
  )
)

(defun sat? (n delta)
  (let ((init (unit_clauses delta nil nil)))
    (if (is_valid (cadr init)) (sat_helper n (car init) (cadr init)))
  )
)

(defun sat_helper (n delta sol)
  ; contradiction in solution: terminate immediately
  (if (not (is_valid sol)) nil
  ; correct length, only valid if delta is empty
  (if (= (length sol) n) (if (null delta) sol)
  ; handle unit clauses
  (let* (
    (unit (unit_clauses delta sol nil)) (nd (car unit)) (ns (cadr unit)))
  ; handle pure literals
  (let* (
    (pure (pure_literals nd ns n)) (nd2 (car pure)) (ns2 (cadr pure)))
  (if
    (= (length ns2) (length sol))
    (let* (
      ; find a literal to split on
      (lit (find_lit ns2 n))
      (d1 (rm_literal nd2 lit))
      (d2 (rm_literal nd2 (* lit -1))))
      (or
        (sat_helper n d1 (append ns2 (list lit)))
        (sat_helper n d2 (append ns2 (list (* lit -1))))
      )
    )
    (sat_helper n nd2 ns2)
  )
  ))))
)

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
