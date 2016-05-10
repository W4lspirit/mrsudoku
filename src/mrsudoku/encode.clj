(ns mrsudoku.encode)
(defn literal? [phi]
  (or (symbol? phi)
      (= (first phi) `not)))

(defn dcnf-algo [phi]
  (if (literal? phi)
    [phi, phi]
    (let [[conn, g, d] phi
          [vg, g'] (dcnf-algo g)
          [vd, d'] (dcnf-algo d)
          v (gensym "x")]
      [v, (list `and (mrsudoku.rules/cnf (list `<=> v (list conn vg vd))) (list `and g' d'))])))

(defn dcnf [phi]
  (let [[v, phi'] (dcnf-algo phi)]
    (list 'and v phi')))

(defn clause-set [phi]
  (if (literal? phi)
    #{phi}
    (let [[_, g, d] phi]
      (clojure.set/union (clause-set g) (clause-set d)))))
(defn and-formula? [phi]
  (and (sequential? phi)
       (= (first phi) `and)))
(defn cnf-set [phi]
  (if (and-formula? phi)
    (let [[_, g, d] phi]
      (clojure.set/union (cnf-set g) (cnf-set d)))
    #{(clause-set phi)}))
(defn bits [n]
  (loop [n n, res `()]
    (if (> n 0)
      (recur (quot n 2) (cons (rem n 2) res))
      res)))
(defn pad-seq
  [s e n]
  (loop [len (count s), res s]
    (if (< ln n)
      (recur (inc len) (cons e res))
      res)))
(defn make-var [cx cy b]
  (symbol (str "l" cy "c" cx "b" b)))

(defn encode-num [cx cy n]
  (loop [s (pad-seq (bits n) 0 4)
         bit 0
         phi true]
    (if (seq s)
      (recur (rest s) (inc bit) (list `and
                                      (if (= (first s) 0)
                                        (list `not (make-var cx cy bit))
                                        (make-var cx cy bit))
                                      phi))
      phi)))
(defn encode-nums [grille]
  (mrsudoku.grid/reduce-grid (fn [phi cx cy cell]
                               &(if (= (:status cell) :empty)
                                 phi
                                 (list `and (encode-num cx cy (:value cell)) phi))) true grille))
(defn encode-empty [cx cy]
  (loop [k,phi false]
    (if (<= k 9)
      (recur (inc k) (list `or (encode-num cx cy k) phi))
      phi)))


;;TODO Check if it's true or false
(defn encode-empties [grille]
  (mrsudoku.grid/reduce-grid (fn [phi cx cy cell]
                               (if (= (:status cell) :empty)
                                 phi
                                 (list `and (encode-empty cx cy ) phi))) true grille))

(defn distinc-cells [cx cy cx2 cy2]
  (loop [bit 0, phi false]
    (if (<= bit 3)
      (recur (inc bit) (list `or
                             (list`or
                                  (list `and
                                        (make-var cx cy bit)
                                        (list `not (make-var cx2 cy2 bit))))
                             (list `and
                                   (list `not (make-var cx cy bit))
                                   (make-var cx2 cy2 bit))
                             phi))
      phi)))
(defn cell-pairs [v]
  (loop [s v , res []]
    (if (seq s)
      (recur (rest s) (into [] (concat res
                                       (reduce (fn [v e]
                                                 (conj v [(first s)e]))
                                               []
                                               (rest s)))))
      res)))
(defn coll-cells [cx]
  (into []
        (for [cy (range 1 10)]
          [cx cy])))


;;TODO something
(defn distinc-cell-pairs [phi])
(distinc-colls []
               (loop [cx 1,phi true]
                 (if (<= cx 9)
                   (recur (inc cx) (list `and
                                         (distinc-cell-pairs (cell-pairs (coll-cells cx)))
                                         phi))
                   phi )))
