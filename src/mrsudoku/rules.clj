(ns mrsudoku.rules
  (:use midje.sweet)
  (:require [rescribe.system :refer [defsystem]]
            [rescribe.strategy :refer [bottom-up top-down or-else success]]))

;;This section of code  was written by  https://github.com/fredokun
;;https://github.com/fredokun/rescribe/blob/master/test/rescribe/examples/proplog.clj
;;begin

(defsystem simplify
           [;; negation
            simpl-not-true (not true) -> false
            simpl-not-false (not false) -> true
            simpl-not-not (not (not ?X)) -> ?X
            ;; conjunction
            simpl-and-absurd-l (and ?X (not ?X)) -> false
            simpl-and-absurd-r (and (not ?X) ?X) -> false
            simpl-and-true-l (and true ?X) -> ?X
            simpl-and-true-r (and ?X true) -> ?X
            simpl-and-false-l (and false ?X) -> false
            simpl-and-false-r (and ?X false) -> false
            ;; disjunction
            simpl-or-exclude-l (or ?X (not ?X)) -> true
            simpl-or-exclude-r (or (not ?X) ?X) -> true
            simpl-or-true-l (or true ?X) -> true
            simpl-or-true-r (or ?X true) -> true
            simpl-or-false-l (or false ?X) -> ?X
            simpl-or-false-r (or ?X false) -> ?X
            ;; implication
            simpl-impl-refl (==> ?X ?X) -> true
            simpl-impl-true-l (==> true ?X) -> ?X
            simpl-impl-true-r (==> ?X true) -> true
            simpl-impl-false-l (==> false ?X) -> true
            simpl-impl-false-r (==> ?X false) -> (not ?X)
            ;; equivalence
            simpl-equiv-refl (<=> ?X ?X) -> true
            simpl-equiv-true-l (<=> true ?X) -> ?X
            simpl-equiv-true-r (<=> ?X true) -> ?X
            simpl-equiv-false-l (<=> false ?X) -> (not ?X)
            simpl-equiv-false-r (<=> ?X false) -> (not ?X)
            ] with (bottom-up (or-else simpl-not-true
                                       simpl-not-false
                                       simpl-not-not
                                       simpl-and-absurd-l
                                       simpl-and-absurd-r
                                       simpl-and-true-l
                                       simpl-and-true-r
                                       simpl-and-false-l
                                       simpl-and-false-r
                                       simpl-or-exclude-l
                                       simpl-or-exclude-r
                                       simpl-or-true-l
                                       simpl-or-true-r
                                       simpl-or-false-l
                                       simpl-or-false-r
                                       simpl-impl-refl
                                       simpl-impl-true-l
                                       simpl-impl-true-r
                                       simpl-impl-false-l
                                       simpl-impl-false-r
                                       simpl-equiv-refl
                                       simpl-equiv-true-l
                                       simpl-equiv-true-r
                                       simpl-equiv-false-l
                                       simpl-equiv-false-r
                                       success)))

(defsystem and-or-form
           [and-or-impl (==> ?X ?Y) -> (or (not ?X) ?Y)
            and-or-equiv (<=> ?X ?Y) -> (and (==> ?X ?Y) (==> ?Y ?X))]
           with (bottom-up (or-else and-or-impl
                                    and-or-equiv
                                    success)))



(defsystem negation-normal-form
           [nnf-not-not (not (not ?X)) -> ?X
            nnf-morgan-and (not (and ?X ?Y)) -> (or (not ?X) (not ?Y))
            nnf-morgan-or (not (or ?X ?Y)) -> (and (not ?X) (not ?Y))
            ] with (top-down (or-else nnf-not-not
                                      nnf-morgan-and
                                      nnf-morgan-or
                                      success)))


(defn nnf [prop]
  (-> prop
      (simplify)
      (and-or-form)
      (negation-normal-form)))

(fact "Some formulas put in NNF."

      (nnf '(==> x (==> (==> x y) y)))
      => '(or (not x) (or (and x (not y)) y)))

;;{

;; ## Conjunctive normal forms

;;}

(defsystem conjunctive-normal-form
           [cnf-not-not (not (not ?X)) -> ?X
            cnf-morgan-and (not (and ?X ?Y)) -> (or (not ?X) (not ?Y))
            cnf-morgan-or (not (or ?X ?Y)) -> (and (not ?X) (not ?Y))
            cnf-distrib-l (or (and ?X ?Y) ?Z) -> (and (or ?X ?Z) (or ?Y ?Z))
            cnf-distrib-r (or ?X (and ?Y ?Z)) -> (and (or ?X ?Y) (or ?X ?Y))
            ] with (top-down (or-else cnf-not-not
                                      cnf-morgan-and
                                      cnf-morgan-or
                                      cnf-distrib-l
                                      cnf-distrib-r
                                      success)))


;;end
(defn cnf [prop]
  (-> prop
      (conjunctive-normal-form)))

