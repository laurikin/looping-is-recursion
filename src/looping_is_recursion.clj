(ns looping-is-recursion)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn power [base exp]
  (loop [acc 1 n exp]
      (if (= 0 n)
        acc
        (recur (* acc base) (dec n)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (recur (rest a-seq))))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (not (empty? b-seq)))
      false
    (and (empty? b-seq) (not (empty? a-seq)))
      false
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (recur (rest a-seq) (rest b-seq))
    :else
      false))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn find-first-index [pred a-seq]
  (loop [n 0
         rest-of-seq a-seq]
    (cond
      (empty? rest-of-seq)
        nil
      (pred (first rest-of-seq))
        n
      :else
        (recur (inc n) (rest rest-of-seq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [acc (first a-seq)
           n 1
           rest-of-seq (rest a-seq)]
      (if (empty? rest-of-seq)
        (/ acc n)
        (recur (+ acc (first rest-of-seq)) (inc n) (rest rest-of-seq))))))


(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         ros a-seq]
    (if (empty? ros)
      acc
      (recur (toggle acc (first ros)) (rest ros)))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn fast-fibo [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (loop [fib-prev 1N
             acc 1N
             curr 2]
        (if (= curr n)
          acc
          (recur acc (+ acc fib-prev) (inc curr))))))


(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8
(fast-fibo 120) ;=> 8

(defn cut-at-repetition [a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (singleton? a-seq)
      a-seq
    :else
      (loop [acc [(first a-seq)]
             olds #{(first a-seq)}
             ros (rest a-seq)]
        (cond
          (empty? ros)
            acc
          (contains? olds (first ros))
            acc
          :else
            (recur
              (conj acc (first ros))
              (conj olds (first ros))
              (rest ros))))))

(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]

