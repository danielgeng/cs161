(setq tests
  (list
    "cnfs/sat/cnf_10.cnf"
    "cnfs/sat/cnf_20.cnf"
    "cnfs/sat/cnf_30.cnf"
    "cnfs/sat/cnf_50.cnf"
    "cnfs/unsat/cnf_12.cnf"
    "cnfs/unsat/cnf_20.cnf"
    "cnfs/unsat/cnf_30.cnf"
    "cnfs/unsat/cnf_42.cnf"
  )
)

(defun test ()
  (loop for x in tests
    do (print x)
    do (print (time (solve-cnf x)))
  )
  t
)
