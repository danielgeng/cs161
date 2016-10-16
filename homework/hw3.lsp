;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
)

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp")
)

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
)

(defun isWall (v)
  (= v wall)
)

(defun isBox (v)
  (= v box)
)

(defun isKeeper (v)
  (= v keeper)
)

(defun isStar (v)
  (= v star)
)

(defun isBoxStar (v)
  (= v boxstar)
)

(defun isKeeperStar (v)
  (= v keeperstar)
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
  (t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
      col
      (getKeeperColumn (cdr r) (+ col 1))
  )))
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
  (t (let ((x (getKeeperColumn (car s) 0)))
    (if x ; keeper is in this row
      (list x row)
      (getKeeperPosition (cdr s) (+ row 1)) ; else move on
    )
  )))
)

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond 
    ((null L) nil)
    (t (let (
      (cur (car L))
      (res (cleanUpList (cdr L)))
    )
      (if cur
        (cons cur res)
        res
      )
    ))
  )
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond
    ((null s) t)
    (t (and (goal-test_helper (car s)) (goal-test (cdr s))))
  )
)

; helper function for iterating rows
(defun goal-test_helper (s)
  (cond
    ((null s) t)
    ((isBox (car s)) nil)
    (t (goal-test_helper (cdr s)))
  )
)

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
(defun next-states (s)
  (cleanUpList (list
    (try_move s 'up)
    (try_move s 'down)
    (try_move s 'left)
    (try_move s 'right)
  ))
)

; returns the contents at the specified coordinates
(defun get_square (s r c)
  (cond
    ((null s) wall)
    ((or (< r 0) (< c 0) (> r (max_row s)) (> c (max_col s))) wall)
    ((> r 0) (get_square (cdr s) (- r 1) c))
    ((> c 0) (get_square (list (cdr (car s))) r (- c 1)))
    (t (car (car s)))
  )
)

; returns the max row index of the state
(defun max_row (s)
  (- (length s) 1)
)

; returns the max column index of the state
(defun max_col (s)
  (- (length (car s)) 1)
)

; returns the size of the grid
(defun grid_size (s)
  (* (length s) (length (car s)))
)

; sets the contents at the specified coordinates to v
(defun set_square (s r c v)
  (cond
    ((> r 0) (cons (car s) (set_square (cdr s) (- r 1) c v)))
    (t (cons (set_square_helper (car s) c v) (cdr s)))
  )
)

; helper function for setting item once row is found
(defun set_square_helper (s c v)
  (cond
    ((> c 0) (cons (car s) (set_square_helper (cdr s) (- c 1) v)))
    (t (cons v (cdr s)))
  )
)

; returns the state when moving in the current direction if the move is valid
; else returns nil
(defun try_move (s d)
  (let* (
    (pos (getKeeperPosition s 0))
    (r (cadr pos))
    (c (car pos)))
  (cond
    ((equal d 'up) (make_move s r c (- r 1) c (- r 2) c))
    ((equal d 'down) (make_move s r c (+ r 1) c (+ r 2) c))
    ((equal d 'left) (make_move s r c r (- c 1) r (- c 2)))
    ((equal d 'right) (make_move s r c r (+ c 1) r (+ c 2)))
    (t nil)
  ))
)

; returns a status code for moving from (r, c) to (r1, c1)
; 0 -> invalid move
; 1 -> move keeper
; 2 -> move keeper and block
; input parameters:
; (r, c) -> original position
; (r1, c1) -> position when moving to given direction by 1
; (r2, c2) -> position when moving to given direction by 2
(defun move_status (s r c r1 c1 r2 c2)
  (cond
    ; can't move into a wall
    ((isWall (get_square s r1 c1)) 0)
    ((has_box (get_square s r1 c1))
      (cond
        ; can't push box into a square with a wall or another box
        ((or (has_box (get_square s r2 c2)) (isWall (get_square s r2 c2))) 0)
        (t 2) ; valid move: move box and keeper
      )
    )
    (t 1) ; valid move: keeper only
  )
)

; attempts to make a move from (r, c) to (r1, c1)
; returns new state on success, else nil
(defun make_move (s r c r1 c1 r2 c2)
  ; check if the move is valid
  (let ((status (move_status s r c r1 c1 r2 c2)))
  (if (= status 0)
    nil
    (or
      ; set orig square
      (let ((s1
        (if
          (isKeeperStar (get_square s r c))
          (set_square s r c star)
          (set_square s r c blank)
        )
      ))
      ; set square keeper is moving into
      (let ((s2
        (if
          (has_star (get_square s r1 c1))
          (set_square s1 r1 c1 keeperstar)
          (set_square s1 r1 c1 keeper)
        )
      ))
      ; set box square if needed
      (if (= status 2)
        (if 
          (isStar (get_square s r2 c2))
          (set_square s2 r2 c2 boxstar)
          (set_square s2 r2 c2 box)
        )
        s2
      )
    ))
  )))
)

; checks if the square has a box
(defun has_box (v)
  (or (isBox v) (isBoxStar v))
)

; checks if the square has a star
(defun has_star (v)
  (or (isStar v) (isBoxStar v))
)

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s) 
  0
)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;
(defun h1 (s)
  (cond
    ((null s) 0)
    ((atom s) (if (isBox s) 1 0))
    (t (+ (h1 (car s)) (h1 (cdr s))))
  )
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; finds manhattan distance between each box to the nearest goal
; doesn't account for unpassable squares
(defun h504588536 (s)
  (h_helper (box_cntr s 0) (goal_cntr s 0) s)
)

; gets a list of positions for all boxes
(defun box_cntr (s r)
  (cond
    ((null s) nil)
    (t (append 
      (box_cntr_helper (car s) r 0)
      (box_cntr (cdr s) (+ r 1))
    ))
  )
)

; helper function for each row
(defun box_cntr_helper (s r c)
  (cond
    ((null s) nil)
    ((atom s) (if (isBox s) (list (list r c))))
    (t (append
      (box_cntr_helper (car s) r c)
      (box_cntr_helper (cdr s) r (+ c 1))
    ))
  )
)

; gets a list of positions for all goals
(defun goal_cntr (s r)
  (cond
    ((null s) nil)
    (t (append 
      (goal_cntr_helper (car s) r 0)
      (goal_cntr (cdr s) (+ r 1))
    ))
  )
)

(defun goal_cntr_helper (s r c)
  (cond
    ((null s) nil)
    ((atom s) (if (isStar s) (list (list r c))))
    (t (append
      (goal_cntr_helper (car s) r c)
      (goal_cntr_helper (cdr s) r (+ c 1))
    ))
  )
)

; returns manhattan distance between (r, c) and (r1, c1)
(defun dist (r c r1 c1)
  (let* (
    (delta_r (- r r1))
    (delta_c (- c c1))
    (dr (if (< delta_r 0) (* delta_r -1) delta_r))
    (dc (if (< delta_c 0) (* delta_c -1) delta_c)))
  (+ dr dc)
  )
)

; returns total distance of boxes to nearest goals (in manhattan distance)
(defun h_helper (boxes goals s)
  (cond
    ((null boxes) 0)
    ((null goals) 0)
    (t (+ (box_dist (car boxes) goals nil s) (h_helper (cdr boxes) goals s)))
  )
)

; returns distance from box to nearest goal (in manhattan distance)
(defun box_dist (box goals dists s)
  (cond
    ((null goals) (get_min dists (grid_size s)))
    (t 
      (box_dist
        box
        (cdr goals)
        (append 
          dists
          (list (
            dist (car box) (cadr box) (car (car goals)) (cadr (car goals)))
          )
        )
        s
      )
    )
  )
)

; returns the minimum from a list
(defun get_min (l m)
  (cond
    ((null l) m)
    ((< (car l) m) (get_min (cdr l) (car l)))
    (t (get_min (cdr l) m))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
   (1 0 3 0 0 1)
   (1 0 2 0 0 1)
   (1 1 0 1 1 1)
   (1 0 0 0 0 1)
   (1 0 0 0 4 1)
   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
   (1 0 0 0 0 0 1) 
   (1 0 0 0 0 0 1) 
   (1 0 0 2 1 4 1) 
   (1 3 0 0 1 0 1)
   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
   (1 0 0 0 1 0 0 0 1)
   (1 0 0 0 2 0 3 4 1)
   (1 0 0 0 1 0 0 0 1)
   (1 0 0 0 1 0 0 0 1)
   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
   (0 0 0 0 0 1 4)
   (0 0 0 0 0 0 0)
   (0 0 1 1 1 0 0)
   (0 0 1 0 0 0 0)
   (0 2 1 0 0 0 0)
   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
   (1 1 0 0 1 1)
   (1 0 0 0 0 1)
   (1 4 2 2 4 1)
   (1 0 0 0 0 1)
   (1 1 3 1 1 1)
   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
   (1 0 0 0 0 0 4 1)
   (1 0 0 0 2 2 3 1)
   (1 0 0 1 0 0 4 1)
   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
   (0 0 1 1 1 1 0 0 0 3)
   (0 0 0 0 0 1 0 0 0 0)
   (0 0 0 0 0 1 0 0 1 0)
   (0 0 1 0 0 1 0 0 1 0)
   (0 2 1 0 0 0 0 0 1 0)
   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
   (1 4 0 0 4 1)
   (1 0 2 2 0 1)
   (1 2 0 1 0 1)
   (1 3 0 0 4 1)
   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
   (1 1 1 0 0 1 1 1 1) 
   (1 0 0 0 0 0 2 0 1) 
   (1 0 1 0 0 1 2 0 1) 
   (1 0 4 0 4 1 3 0 1) 
   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
    (1 0 0 0 1 1 0)
    (1 3 2 0 0 1 1)
    (1 1 0 2 0 0 1)
    (0 1 1 0 2 0 1)
    (0 0 1 1 0 0 1)
    (0 0 0 1 1 4 1)
    (0 0 0 0 1 4 1)
    (0 0 0 0 1 4 1)
    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
    (1 4 0 0 0 4 1)
    (1 0 2 2 1 0 1)
    (1 0 2 0 1 3 1)
    (1 1 2 0 1 0 1)
    (1 4 0 0 4 0 1)
    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
    (1 1 1 1 1 0 0 0 1 1 1 1)
    (1 0 0 0 2 0 0 0 0 0 0 1)
    (1 3 0 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 2 1 1 1 0 0 0 1)
    (1 0 0 0 0 1 0 1 4 0 4 1)
    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
    (1 4 0 0 0 0 0 2 0 1)
    (1 0 2 0 0 0 0 0 4 1)
    (1 0 3 0 0 0 0 0 2 1)
    (1 0 0 0 0 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 4 1)
    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
    (0 2 1 4 0 0 0)
    (0 2 0 4 0 0 0)
    (3 2 1 1 1 0 0)
    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
    (1 0 0 0 0 0 1)
    (1 0 0 2 2 0 1)
    (1 0 2 0 2 3 1)
    (1 4 4 1 1 1 1)
    (1 4 4 1 0 0 0)
    (1 1 1 1 0 0 0)
    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
    (1 0 0 0 1 0 0 0)
    (1 2 1 0 1 1 1 1)
    (1 4 0 0 0 0 0 1)
    (1 0 0 5 0 5 0 1)
    (1 0 5 0 1 0 1 1)
    (1 1 1 0 3 0 1 0)
    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
    (1 3 0 0 1 0 0 0 4 1)
    (1 0 2 0 2 0 0 4 4 1)
    (1 0 2 2 2 1 1 4 4 1)
    (1 0 0 0 0 1 1 4 4 1)
    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
    (0 0 0 1 0 0 0 0 1 0 0 0)
    (0 0 0 1 0 0 0 0 1 0 0 0)
    (1 1 1 1 0 0 0 0 1 1 1 1)
    (0 0 0 0 1 0 0 1 0 0 0 0)
    (0 0 0 0 0 0 3 0 0 0 2 0)
    (0 0 0 0 1 0 0 1 0 0 0 4)
    (1 1 1 1 0 0 0 0 1 1 1 1)
    (0 0 0 1 0 0 0 0 1 0 0 0)
    (0 0 0 1 0 0 0 0 1 0 0 0)
    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
    (1 1 1 1 0 0 1 1 0)
    (1 0 0 0 2 0 0 1 0)
    (1 0 0 5 5 5 0 1 0)
    (1 0 0 4 0 4 0 1 1)
    (1 1 0 5 0 5 0 0 1)
    (0 1 1 5 5 5 0 0 1)
    (0 0 1 0 2 0 1 1 1)
    (0 0 1 0 3 0 1 0 0)
    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
    (1 1 1 0 0 1 1 1 1 0)
    (1 0 0 2 0 0 0 1 1 0)
    (1 3 2 0 2 0 0 0 1 0)
    (1 1 0 2 0 2 0 0 1 0)
    (0 1 1 0 2 0 2 0 1 0)
    (0 0 1 1 0 2 0 0 1 0)
    (0 0 0 1 1 1 1 0 1 0)
    (0 0 0 0 1 4 1 0 0 1)
    (0 0 0 0 1 4 4 4 0 1)
    (0 0 0 0 1 0 1 4 0 1)
    (0 0 0 0 1 4 4 4 0 1)
    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* (
    (k1 (getKeeperPosition s1 0))
    (k2 (getKeeperPosition s2 0))
    (deltaX (- (car k2) (car k1)))
    (deltaY (- (cadr k2) (cadr k1)))
  )
  (cond (
    (= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
    (t (if (> deltaX 0) 'RIGHT 'LEFT))
  ))
)

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond (
    (null m) nil)
    ((= 1 (length m)) (list 'END))
    (t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
  )
)

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond (
    (= s blank) (format t " "))
    ((= s wall) (format t "#"))
    ((= s box) (format t "$"))
    ((= s keeper) (format t "@"))
    ((= s star) (format t "."))
    ((= s boxstar) (format t "*"))
    ((= s keeperstar) (format t "+"))
    (t (format t "|"))
  )
)

;
; Print a row
;
(defun printRow (r)
  (dolist
    (cur r)
    (printSquare cur)
  )
)

;
; Print a state
;
(defun printState (s)
  (progn
  (dolist
    (cur s)
    (printRow cur)
    (format t "~%")
  ))
)

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist
    (cur sl)
    (printState cur)
    (sleep delay)
  )
)
