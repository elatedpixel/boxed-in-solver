(ns box-solver.core
  (:require [box-solver.data :as d])
  (:gen-class))

(def switch
  (zipmap "rbgy" "RBGY"))

(def direction
  {:up    [-1 0]
   :down  [1 0]
   :left  [0 -1]
   :right [0 1]})

(defn target [d {:keys [robot]}]
  (map + robot (direction d)))

(defn target-of-target [d {:keys [robot]}]
  (map + robot (direction d) (direction d)))

(defn valid-move? [t tt]
  (or ((set " rgby*@") t)              ; robot moves into position
      (and (= \+ t)                    ; robot pushes crate
           ((set " rgby@") tt))))      ; crate moves into valid space

(defn move
  [d {:keys [robot world origin switches] :as state}]
  (let [t  (get-in world (target d state))
        ro (get-in origin robot)
        to (get-in origin (target d state))
        tt (get-in world (target-of-target d state))]
    (when (valid-move? t tt)
      (let [world'
            (cond-> world
              (= \+ t)
              (assoc-in (target-of-target d state) \+)

              (and (= \+ t) ((set "rgby") tt))
              (assoc-in (switches (switch tt)) \space)

              ((set "rgby") to)
              (assoc-in (switches (switch to)) (switch to))

              :always
              (-> (assoc-in (target d state) \p)
                  (assoc-in robot (get-in origin robot))))
            hash' (hash world')]
        (cond-> state
          (= \@ t)
          (assoc :exited? true)

          (= \@ ro)
          (assoc :exited? false)

          (= \* t)
          (assoc :gears? (.contains (apply str (flatten world')) "*"))

          :always
          (-> (assoc :world world')
              (assoc :robot (target d state))
              (update :steps conj d))
                                        ;
          )))))

(def select-values (comp vals select-keys))

(defn seq-graph [data-structure children-fn initial-state]
  ((fn walk [explored frontier]
     (lazy-seq
      (when (seq frontier)
        (let [state (peek frontier)
              children (children-fn state)
              hashes (zipmap (map (comp hash :world) children) children)]
          (cons state
                (walk (into explored (keys hashes))
                      (into (pop frontier) (select-values hashes (remove explored (keys hashes))))))))))
   #{(hash (:world initial-state))} (conj data-structure initial-state)))

(def bfs (partial seq-graph clojure.lang.PersistentQueue/EMPTY))

(defn next-moves [state]
  (keep #(move % state) (keys direction)))

#_(defn bfs [queue]
  (lazy-seq
   (when (seq queue)
     (let [state (peek queue)]
       (cons state
             (bfs (into (pop queue) (next-moves state))))))))

(def won? #(= [false true] ((juxt :gears? :exited?) %)))

(defn solve [state]
  (first (drop-while (complement won?)
                     (bfs next-moves state))))

(defmacro with-timeout [n & body]
  `(let [future# (future ~@body)
         return# (deref future# ~n ::timeout)]
     (when (= ::timeout return#)
       (future-cancel future#))
     return#))

(defn solver [n & {:keys [timeout]}]
  (let [level    (d/levels n)
        solution (if (nil? timeout)
                   (time (solve (d/make-state level)))
                   (with-timeout timeout (time (solve (d/make-state level)))))]
    solution))

(defn print-solver [n & {:keys [timeout]}]
  (do
    (println)
    (println "solving level" n "with" timeout "millisecond timeout.")
    (d/display (d/levels n))
    (let [solution (solver n :timeout timeout)]
      (println (:steps solution))
      (println (count (:steps solution)) "steps")
      (println)
      solution)))

(comment
  (print-solver 5)
                                        ;
  )

(defn -main [& args]
  (let [timeout (Integer/parseInt (or (System/getenv "TIMEOUT") "30000"))]
    (doall
     (for [n (sort (keys d/levels))]
       (print-solver n timeout)))))
