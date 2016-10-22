; 1) depth-first search of a tree
; input: tree
; output: list of terminal nodes in the order visited by depth-first search
; -----------------------------------------------------------------------------
; case 1: empty tree, return nil
; case 2: tree is an atom, make it a list and return it
; else: visit left child, then visit the rest of the nodes and append results
(defun DFS (tree)
    (cond
        ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (DFS (car tree)) (DFS (cdr tree))))
    )
)

; 2) iterative deepening depth-first search
; input: tree, depth (max depth to be searched)
; output: list of terminal nodes in the order visited by IDDFS
; -----------------------------------------------------------------------------
; base case: stop when depth limit is reached
; else: call DFID to move a level deeper, use helper to traverse current level,
; append results together
(defun DFID (tree depth)
    (cond
        ((< depth 1) nil)
        (t (append
            (DFID tree (- depth 1))
            (DFID_helper tree depth)
        ))
    )
)

; helper function is the same as above DFS function, but it has an additional
; base case that stops the recursion past a certain depth
(defun DFID_helper (tree depth)
    (cond
        ((null tree) nil)
        ((atom tree) (list tree)) ; before depth check to return single atom
        ((< depth 1) nil)
        (t (append
            (DFID_helper (car tree) (- depth 1))
            (DFID_helper (cdr tree) depth)
        ))
    )
)

; 3) missionary-cannibal problem

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
    (equal s '(3 3 nil))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    (cond
        ((> (+ m c) 2) nil) ; can't move more than 2 ppl at a time
        ((< (car s) m) nil) ; can't move more M than what's available
        ((< (cadr s) c) nil) ; can't move more C than what's available
        (t (let* 
            (
                ; # of M and C on the side the boat starts on after moving
                (this_m (- (car s) m))
                (this_c (- (cadr s) c))
                ; # of M and C on the other side after moving
                (other_m (- 3 this_m))
                (other_c (- 3 this_c))
            )
            (cond
                ; must be less C than M on both sides
                ((and (> this_m 0) (> this_c this_m)) nil)
                ((and (> other_m 0) (> other_c other_m)) nil)
                ; valid state given move
                (t (list (list other_m other_c (not (caddr s)))))
            )
        ))
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
    ; use next-state to generate all the valid states using possible moves
    (append
        (next-state s 0 1)
        (next-state s 1 0)
        (next-state s 0 2)
        (next-state s 2 0)
        (next-state s 1 1)
    )
)

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
    (cond
        ((null states) nil) ; no more states, end search
        ; found final state, append the state to path and end search
        ((final-state (car states)) (append path (list (car states))))
        ; search deeper on current state
        ((single-dfs (car states) path (- depth 1)))
        ; search other states on curr level
        (t (mult-dfs (cdr states) path depth))
    )
)

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.
(defun single-dfs (s path depth)
    (cond
        ((< depth 1) nil) ; exceed curr search depth, end search
        ; call mult-dfs on valid successor states
        (t (mult-dfs (succ-fn s) (append path (list s)) depth))
    )
)

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
    (cond
        ; try current depth
        ((single-dfs s nil depth))
        ; if no solution, try next depth level
        (t (id-dfs s (+ depth 1)))
    )
)
