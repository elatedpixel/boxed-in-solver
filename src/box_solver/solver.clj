(ns box-solver.solver
  (:require [box-solver.data :as d]
            [clojure.core.async :as async])
  (:use [clojure.pprint :only (pprint)]))

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

(defn bfs [children-fn initial-state]
  ((fn walk [explored frontier]
     (lazy-seq
      (when (seq frontier)
        (let [state    (peek frontier)
              children (children-fn state)]
          (cons state
                (walk (into explored (map :hash children))
                      (into (pop frontier) (remove (comp explored :hash) children))))))))
   #{(:hash initial-state)} (conj clojure.lang.PersistentQueue/EMPTY initial-state)))

(defn next-moves [state]
  (keep (partial move state) (keys direction)))

(defn all-mins [f]
  (fn
    ([] [])
    ([acc] acc)
    ([acc state]
     (if (or (zero? (f acc))
             (= (count (f state)) (f acc)))
       (-> acc
           (assoc :steps (count (f state)))
           (update :values conj state))
       (reduced acc)))))

(defn solve [state]
  #_(transduce
   (comp (filter won?))
   (all-mins :steps)
   {:steps 0 :values []}
   (bfs next-moves state))
  #_(-> (split-with (complement won?) (bfs next-moves state))
      ((fn [[l w]] {:wins (count w) :losses (count l)})))
  (some #(when (won? %) %) (bfs next-moves state)))

(defmacro wait [n & body]
  `(async/alts!!
    [(async/timeout ~n)
     (async/go ~@body)]))

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
                   (wait timeout (solve (d/make-state level))))]
    (do
      (shutdown-agents)
      solution)))

(defn print-solver [{:keys [timeout level]}]
  (do
    (println)
    (println "solving level" level "with" (or timeout "no") "millisecond timeout.")
    (d/display (d/levels level))
    (let [solution (time (solver level :timeout timeout))]
      ;; (println (:steps solution))
      ;; (println (count (:values solution)) "solutions found.")
      ;; (println (count (:steps solution)) "steps")
      (pprint solution)
      (println)
      solution)))
