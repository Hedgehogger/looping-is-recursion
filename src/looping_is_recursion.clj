(ns looping-is-recursion)

; (defn find-first-index [pred a-seq]
;   (loop [acc 1
;          n down-from]
;     (if (pred? (get a-seq n))
;       (get a-seq n)
;       (recur (* acc n) (dec n))
;     )
;   )
; )

(defn accumulating-factorial-helper [acc n]
  (if (zero? n)
    acc
    (accumulating-factorial-helper (* acc n) (dec n))))

(defn accumulating-factorial [n]
  (accumulating-factorial-helper 1 n))

(defn recur-factorial [number]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc n) (dec n))))]
    (helper 1 number)))

(defn loopy-factorial [down-from]
  (loop [acc 1
         n down-from]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))

; Expand the helper to 3 arguments, 1 extra argument that keeps the base start value
; that is used to multiply each iteration
(defn power [base exp]
  (let [helper (fn [curr base exp]
    (cond
   	  (== 1 exp)
        curr
      (== 0 exp)
        1
      :else
        (recur (* curr base) base (dec exp))))]
    (helper base base exp)
  )
)

; (helper 5   5 4)
; (helper 25  5 3)
; (helper 125 5 2)
; (helper 625 5 1)

(defn singleton? [coll]
  (and (not (empty? coll)) ; coll should not be empty, i.e. not []
       (empty? (rest coll))) ; the rest of coll should be empty, i.e. []
)

(defn last-element [a-seq]
  (cond
   	(singleton? a-seq)
      (first a-seq)
    (== 1 (count a-seq))
      (first a-seq)
    (empty? a-seq)
      nil
    :else
      (recur (rest a-seq))
  )
)

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
   	(not (== (count seq1) (count seq2))) ; check length
      false
    (and (singleton? seq1) (singleton? seq2))
    	(if (== (first seq1) (first seq2))
       	  true
          false 
        )  
    (== (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else
      false
  )
)

(defn find-first-index-helper [pred? a-seq acc]
  (cond
   (== (count a-seq) acc)
     nil                    
   (== (count a-seq) 0)
     nil 
   :else
      (if (pred? (get a-seq acc))
        acc
        (find-first-index-helper pred? a-seq (inc acc))
      )
  )
)

(defn find-first-index-withhelper [pred? a-seq]
  (find-first-index-helper pred? a-seq 0)
)

(defn find-first-index [pred? a-seq]
  (loop [prd? pred?
         b-seq a-seq
         acc 0]
    (cond
      (== (count b-seq) acc)
        nil                    
      (== (count b-seq) 0)
        nil 
      :else
        (if (prd? (get b-seq acc))
          acc
          (recur prd? b-seq (inc acc))
        )
    )
  )
)

(defn avg [a-seq]
  (loop [coll a-seq
         currsum 0
         ind 0]
    (cond  
      (== (count coll) ind)
        (/ currsum ind)               
      (== (count coll) 0)
        0
      :else
        (recur coll (+ currsum (get coll ind)) (inc ind))
    )
  )
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn parity [a-seq]
  (loop [origseq a-seq
         currseq []
         ind 0]
    (cond  
      (== (count origseq) ind)
        (set currseq)             
      (== (count origseq) 0)
        (set currseq)
      :else
        (recur origseq (toggle (set currseq) (get origseq ind)) (inc ind))
    )
  )
)

(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (+ (fib (- n 1)) 
         (fib (- n 2)))
    )
  )
)

(defn fast-fibo [n]
  (loop [n n
         fibn 1
         fibn-1 0
         ind 1]
    (cond
      (== n 0)
        0  
      (== n ind)
        fibn            
      :else
        (recur n (+ fibn fibn-1) fibn (inc ind))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq)) 
  )
)


(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         new-seq []
         ind 0]
    (cond
      (== (count a-seq) 0)
        new-seq
      (== (count a-seq) ind)
        new-seq        
      (> (count new-seq) (count (set new-seq)))
        (drop-last new-seq)
      :else
        (recur a-seq (conj new-seq (get a-seq ind)) (inc ind))
    )
  )
)

