
(ns mrsudoku.rules)

(defsystem arith-simpl
           [and-contraire (and ?x (not ?x))->false
            and-true-l (and true ?x) ->?x
            and-true-r (and ?x true) ->?x]
           with (top-down (or-else and-contraire
                                   and-true-l
                                   and-true-r)))

[and-idem (and ?x ?x) -> ?x
 and-false-l (and false ?x) -> false
 and-false-r (and false ?x) -> false
 (or ?x (not ?x)) -> true
 (or true ?x) -> true
 (or (not (not ?x)))-> ?x
 (not true)-> false
 (not false)-> true
 (==> true ?x)->?x
 (==> false ?x)->true
 (==> ?x true)->true
 (==> ?x false)-> (not ?x)
 (<=> ?x ?x)->true
 (<=> true ?x)->?x
 (<=> false ?x)-> (not ?x)]
[suppr-impl (==> ?x ?y)-> (or (not ?x)?y)
 suppr-equiv (?x ?y)-> (and (==> ?x ?y) (==> ?y ?x))]
;;and-or-not
with top-down (or-else suppr-impl
                       suppr-equiv
                       success)



;;nnf
[nnf-not (not (not ?x))->?x
 nnf-and (not (and ?x ?y))-> (or (not ?x ) (not ?y))
 nnf-or (not (or ?x ?y))-> (and (not ?x) (not ?y))]
with top-down (or-else nnf-not
                       nnf-and
                       nnf-or
                       success)

;;forme normale conjonctive
[cnf-l (or (and ?x ?y)?z)-> (and (or ?x ?z) (or ?y ?z))
 cnf-r (or ?x (and ?y ?z)) -> (and (or ?x ?y) (or ?x ?z))]
with top-down (or-else cnf-l
                       cnf-r
                       success)

(defn literal? [phi]
  (or (symbol? phi)
      (= (first phi) 'not)))

(defn dcnf-algo [phi]
  (if (literal? phi)
    [phi,phi]
    (let [[conn, g,d]phi
          [vg,g'] (dcnf-algo g)
          [vd,d'] (dcnf-algo d)
          v (gensym "x")]
      [v,(list 'and (cnf (list '<=> v (list conn vg vd))) (list 'and g' d'))])))

(defn dcnf [phi]
  (let [[v1,phi'] (dcnf-algo phi)]
    (list 'and v phi')))