(ns box-solver.solver
  (:require [box-solver.data :as d]))

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
  (or (#{\space \@ \b \g \* \r \y} t)
      (and (= \+ t)
           (#{\space \@ \b \g \r \y} tt))))

(def won? #(= [false true] ((juxt :gears? :exited?) %)))

(defn move
  [{:keys [robot world origin switches] :as state} d]
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
                  (assoc-in robot (get-in origin robot))))]
        (cond-> state
          (= \@ to)
          (assoc :exited? true)

          (= \@ ro)
          (assoc :exited? false)

          (= \* t)
          (assoc :gears? (.contains (apply str (flatten world')) "*"))

          :always
          (-> (assoc :world world')
              (assoc :hash (hash world'))
              (assoc :robot (target d state))
              (update :steps conj d)))))))

(defn seq-graph [data-structure children-fn initial-state]
  ((fn walk [explored frontier]
     (lazy-seq
      (when (seq frontier)
        (let [state    (peek frontier)
              children (children-fn state)]
          (cons state
                (walk (into explored (map :hash children))
                      (into (pop frontier) (remove #(explored (:hash %)) children))))))))
   #{(:hash initial-state)} (conj data-structure initial-state)))

(def bfs (partial seq-graph clojure.lang.PersistentQueue/EMPTY))

(defn next-moves [state]
  (keep (partial move state) (keys direction)))

(defn solve [state]
  (some #(when (won? %) %) (bfs next-moves state)))

(defmacro with-timeout [n & body]
  `(let [future# (future ~@body)
         return# (deref future# ~n ::timeout)]
     (when (= ::timeout return#)
       (future-cancel future#))
     return#))

(defn solver [n & {:keys [timeout]}]
  (let [level    (d/levels n)
        solution (if (nil? timeout)
                   (solve (d/make-state level))
                   (with-timeout timeout (solve (d/make-state level))))]
    solution))

(defn print-solver [{:keys [timeout level]}]
  (do
    (println)
    (println "solving level" level "with" (or timeout "no") "millisecond timeout.")
    (d/display (d/levels level))
    (let [solution (time (solver level :timeout timeout))]
      (println (:steps solution))
      (println (count (:steps solution)) "steps")
      (println)
      solution)))
