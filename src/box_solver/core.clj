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

(defmulti move
  (fn [d state] (get-in (:world state) (target d state))))
(defmethod move \space [d {:keys [robot world origin seen] :as state}]
  (let [world' (-> world
                   (assoc-in (target d state) \p)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc-in [:robot] (target d state))
          (assoc-in (cons :world (target d state)) \p)
          (assoc-in (cons :world robot) (get-in origin robot))
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \@ [d {:keys [robot world origin seen] :as state}]
  (let [world' (-> world
                   (assoc-in (target d state) \p)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc :exited? true)
          (assoc :robot (target d state))
          (assoc :world world')
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \+ [d {:keys [robot world origin seen switches] :as state}]
  (let [tt (get-in world (target-of-target d state))
        to (get-in origin (target d state))]
    (when ((set " rbgyp") tt)
      (let [world' (cond-> world
                     ((set "rgby") tt) (assoc-in (switches (switch tt)) \space)
                     ((set "rgby") to) (assoc-in (switches (switch to)) (switch to))
                     :always (-> (assoc-in (target d state) \p)
                                 (assoc-in (target-of-target d state) \+)
                                 (assoc-in robot (get-in origin robot))))]
        (when (not (seen (hash world')))
          (-> state
              (assoc :robot (target d state))
              (assoc :world world')
              (update :steps conj d)
              (update :seen conj (hash world'))))))))
(defmethod move \* [d {:keys [robot world origin seen] :as state}]
  (let [world' (-> world
                   (assoc-in (target d state) \p)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc :robot (target d state))
          (assoc :world world')
          (assoc :gears? (.contains (apply str (flatten world')) "*"))
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \p [d state] (do
                               (println "Direction:" d)
                               (throw (Exception. "illegal state?"))))
(defmethod move :default [_ _] nil)

(defn next-moves [state]
  (keep #(move % state) (keys direction)))

(defn bfs [queue]
  (lazy-seq
   (when (seq queue)
     (let [state (peek queue)]
       (cons state
             (bfs (into (pop queue) (next-moves state))))))))

(def won? #(= [false true] ((juxt :gears? :exited?) %)))

(defn solve [state]
  (first (drop-while (complement won?)
                     (bfs (conj clojure.lang.PersistentQueue/EMPTY state)))))

(defn -main []
  (let [level (d/levels 2)]
    (time (do
            (println (solve (d/make-state level)))
            (d/display level)))))
