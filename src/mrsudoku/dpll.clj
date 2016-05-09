(ns mrsudoku.dpll)

(defn find-1-literal "trouve un literal seul dans sa clause ou nil"
  [phi]
  (if (seq phi)
    (if (= (count (first phi)) 1)
      (first (first phi))
      (recur (rest phi)))
    nil))
    
(defn make-literal-true
  [phi x]
  (reduce (fn [phi' clause]
            (cond
              (contains? clause x) phi'
              (contains? clause (list `not x)) (if (= (count clause) 1)
                                                 (reduced nil)
                                                 (conj phi' (disj clause (list `not x))))
              :else (conj phi' clause))) #{} phi))
              
(defn make-literal-false
  [phi x]
  (reduce (fn [phi' clause]
            (cond
              (contains? clause x) (if (= (count clause) 1)
                                     (reduced nil)
                                     (conj phi' (disj clause x)))
              (contains? clause (list `not x)) phi'
              :else (conj phi' clause))) #{} phi))

(defn rule-1-literal
  [phi]
  (when-let [l (find-1-literal phi)]
    (if (symbol? l)
      [(make-literal-true phi l) l true]
      [(make-literal-false phi (second l)) (second l) false])))

(defn clause-positive-literals
  [clause]
  (loop [s clause res #{}]
    (if (seq s)
      (if (symbol? (first s))
        (recur (rest s) (conj res (first s)))
        (recur (rest s) res))
      res)))

(defn clause-negative-literals
  [clause]
  (loop [s clause res #{}]
    (if (seq s)
      (let [e (first s)]
        (if (= 2 (count e)) ;; (= '(not _) e)
          (recur (rest s) (conj res (second e)))
          (recur (rest s) res)))
        res)))
      
(defn positive-literals
  "Ensemble des littéraux positifs dans phi"
  [phi]
  ((reduce clojure.set/union (map clause-positive-literals phi))))

(defn negative-literals
  "Ensemble des littéraux négatifs dans phi"
  [phi]
  ((reduce clojure.set/union (map clause-negative-literals phi))))

(defn find-affirmative-negative
  "[only-affirmatives only-negatives]"
  [phi]
  (let [affirmatives (positive-literals phi)
        negatives (negative-literals phi)
        only-affirmatives (clojure.set/difference affirmatives negatives)
        only-negatives (clojure.set/difference negatives affirmatives)]
    [only-affirmatives only-negatives]))

(defn make-affirmative
  "Instantiation positive des vars dans phi"
  [phi vars]
  (reduce make-literal-true phi vars))

(defn make-negative 
  "Instantiation négative des vars dans phi"
  [phi vars]
  (reduce make-literal-false phi vars))

(defn make-inst
  [a n]
  (merge
    (reduce (fn [mres e] (assoc mres e true)) {} a)
    (reduce (fn [mres e] (assoc mres e false)) {} n)))

(defn rule-affirmative-negative
  [phi]
  (let [[affirmatives negatives] (find-affirmative-negative phi)
        phi' (make-affirmative phi affirmatives)
        phi'' (make-negative phi' negatives)]
    [phi'' (make-inst affirmatives negatives)]))

(defn first-splitter [phi]
  (let [l (first (first phi))]
    (if (symbol? l)
      l
      (second l))))
      
(defn seq-positives [phi] "renvoie une sequence de symboles positif"
  (reduce (fn [seq clause]
            (concat (filter symbol? clause) seq))
          `() phi))
          
(defn max-true-split [phi]
  (let [freqs (frequencies (seq-positives phi))]
    (key (apply max-key val freqs))))

(defn rule-split [phi splitter]
            (let [x (splitter phi)]
              [[(make-literal-true x phi) x true]
               [(make-literal-false x phi) x false]]))

(defn dpll [phi inst splitter]
  (if (empty? phi)
    inst
    (if-let [[phi' x xval] (rule-1-literal phi)]
      (recur phi' (assoc inst x xval) splitter)
      (if-let [[phi' aux-inst] (rule-affirmative-negative phi)]
        (recur phi' (merge inst aux-inst) splitter)
        (let [[[phi-true x-true x-val-true], [phi-false x-false x-val-false]] (rule-split phi splitter)]
          (or (dpll phi-true (assoc inst x-true x-val-true) splitter)
              (dpll phi-false (assoc inst x-false x-val-false) splitter)
              nil))))))