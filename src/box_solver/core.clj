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
  [d {:keys [robot world origin seen switches] :as state}]
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
        (when (not (seen hash'))
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
                (update :steps conj d)
                (update :seen conj hash'))
                                        ;
            )
                                        ;
          )))))

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
  (let [level (d/levels 4)]
    (time (let [solution (solve (d/make-state level))]
            (do
              (println solution)
              (println (count (:steps solution)) "steps.")
              (d/display level))))))
