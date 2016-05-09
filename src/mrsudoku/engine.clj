(ns mrsudoku.engine
  (:use midje.sweet)
  (:require [mrsudoku.grid :as g]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))

(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]
  (reduce (fn [acc s] (if 
                        (or (= :set (get s :status)) (= :init (get s :status)))
                        (conj acc (get s :value))
                        acc))
          #{}
          cells))
  

(fact
 (values (g/block sudoku-grid 1)) => #{5 3 6 9 8})

(fact
 (values (g/row sudoku-grid 1)) => #{5 3 7})

(fact
 (values (g/col sudoku-grid 1)) => #{5 6 8 4 7})

(fact
 (values (g/block sudoku-grid 8)) => #{4 1 9 8})

(fact
 (values (g/row sudoku-grid 8)) => #{6 4 1 9 5})

(fact
 (values (g/col sudoku-grid 8)) => #{6 8 7})

(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cellss except]
  {:pre [(<= 1 except (count cellss))]}
  (loop [cells cellss acc #{}]
    (if (seq cells)
      (if (and 
            (or 
              (= :set (get (first cells) :status))
              (= :init (get (first cells) :status))) 
            (not= except (inc(- (count cellss) (count cells)))))
        (recur (rest cells) (conj acc (get (first cells) :value)))
        (recur (rest cells) acc))
      acc)))

(fact
 (values-except (g/block sudoku-grid 1) 1) => #{3 9 6 8})

(fact
 (values-except (g/block sudoku-grid 1) 4) => #{3 9 5 8})

(defn mk-conflict [kind  value]
  {:status :conflict
   :kind kind
   :value value})

(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (set? kind1) (set? kind2)) (clojure.set/union kind1 kind2)
    (set? kind1) (conj kind1 kind2)
    (set? kind2) (conj kind2 kind1)
    (= kind1 kind2) kind1
    :else (hash-set kind1 kind2)))

(fact
 (merge-conflict-kind :row :row) => :row)

(fact
 (merge-conflict-kind :row :block) => #{:row :block})

(fact
 (merge-conflict-kind :row #{:row :block}) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} :block) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} #{:block :col}) => #{:row :block :col})


(defn merge-conflict [conflict1 conflict2]
  (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts)) 

(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                            value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))


(defn trans-temp-seq 
  "reduce row into vector [index (none empty cell)]"
  [row] 
  (reduce 
    (fn [[cpt temp] c] 
      (if (= (get c :status) :empty) 
         [(inc cpt) temp]
        (vector (inc cpt) (conj temp (vector cpt c)))))
         [1 []] row))

        
(defn conflict? 
  "check if cell1 is in conflict with cell2"
  [cell1 cell2]
  (and 
    (= (get cell1 :status) :set )
    (= (get cell1 :value) (get cell2 :value))))

    
(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  ;;; à compléter
  ;; diff entre set et init 
  ;;key cx cy |val mk-conflict kind cx cy value 
  (let [temp (second (trans-temp-seq row))]
  
    (reduce (fn [conflicts cell]
     
              (let [[index c] cell] 
                (loop [cells temp , conf conflicts]
                  (if (seq cells)
                    (if (and 
                          (not= index (first (first cells))) 
                          (conflict? c (second (first cells))));;conflicts detected ok
                      (recur (rest cells) 
                             (assoc conf (vector index cy) (mk-conflict :row (get c :value))))
                      (recur (rest cells) conf ));;ko  ArityException Wrong number of args (0) passed to: PersistentArrayMap 
                    conf))))
            {} temp)))



(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [4 1] {:status :conflict, :kind :row, :value 1}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[6 4] {:status :conflict, :kind :row, :value 6}})

(defn rows-conflicts [grid]
  (reduce merge-conflicts {}
          (map (fn [r] (row-conflicts (g/row grid r) r)) (range 1 10))))

(defn col-conflicts
  "Returns a map of conflicts in a `col`."
  [col cx]
  (let [temp (second (trans-temp-seq col))]
  
    (reduce (fn [conflicts cell]
     
              (let [[index c] cell] 
                (loop [cells temp , conf conflicts]
                  (if (seq cells)
                    (if (and 
                          (not= index (first (first cells))) 
                          (conflict? c (second (first cells))));;conflicts detected ok
                      (recur (rest cells) 
                             (assoc conf (vector cx index) (mk-conflict :row (get c :value))))
                      (recur (rest cells) conf ));;ko  ArityException Wrong number of args (0) passed to: PersistentArrayMap 
                    conf))))
            {} temp)))

;;; Ecrire les 'fact'  nécessaires...
(fact
 (col-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [1 4] {:status :conflict, :kind :row, :value 1}})
 

(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))

(defn quick-fix [temp block b]
  (let [soluce (g/reduce-block (fn [acc index cx cy cell]
                               (assoc acc index [cx,cy])) {} block b)]
  (reduce (fn [res [k,v]]
            (assoc res (get soluce (second k)) v)) {} temp)))
            
(defn block-conflicts
  [block b]
  (let [temp (col-conflicts block b)]
    (quick-fix temp block b)))
(fact
 (block-conflicts [{:status :init, :value 5} {:status :init, :value 3} {:status :empty} {:status :set, :value 3} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 9} {:status :init, :value 8}] 1)
 => {[1 2] {:status :conflict, :kind :row, :value 3}})
;;; Ecrire les 'fact' nécessaires...

(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))