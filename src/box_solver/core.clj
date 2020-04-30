(ns box-solver.core)

(defn coords [x s width]
  (let [index (.indexOf s (str x))]
    (vector (quot index width) (rem index width))))

(defrecord State [robot world origin switches gears? exited? seen steps])
(defn make-state [{:keys [width height level]}]
  (let [robot (coords \1 level width)
        world (mapv vec (partition width level))]
    (->State robot
             world
             (->> level
                  (replace (zipmap "1c" (repeat \space)))
                  (partition width)
                  (mapv vec))
             (zipmap "RGBY" (map #(coords % level width) "RGBY"))
             (.contains level "*")
             false
             #{(hash world)}
             [])))

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
                   (assoc-in (target d state) \1)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc-in [:robot] (target d state))
          (assoc-in (cons :world (target d state)) \1)
          (assoc-in (cons :world robot) (get-in origin robot))
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \e [d {:keys [robot world origin seen] :as state}]
  (let [world' (-> world
                   (assoc-in (target d state) \1)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc :exited? true)
          (assoc :robot (target d state))
          (assoc :world world')
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \c [d {:keys [robot world origin seen switches] :as state}]
  (let [tt (get-in world (target-of-target d state))
        to (get-in origin (target d state))]
    (when ((set " rbgye") tt)
      (let [world' (-> world
                       (assoc-in (target d state) \1)
                       (assoc-in (target-of-target d state) \c)
                       (assoc-in robot (get-in origin robot)))]
        (when (not (seen (hash world')))
          (-> state
              (assoc :robot (target d state))
              (assoc :world
                     (cond-> world'
                       ((set "rgby") tt)
                       (assoc-in (switches (switch tt)) \space)
                       ((set "rgby") to)
                       (assoc-in (switches (switch to)) (switch to))))
              (update :steps conj d)
              (update :seen conj (hash world'))))))))
(defmethod move \* [d {:keys [robot world origin seen] :as state}]
  (let [world' (-> world
                   (assoc-in (target d state) \1)
                   (assoc-in robot (get-in origin robot)))]
    (when (not (seen (hash world')))
      (-> state
          (assoc :robot (target d state))
          (assoc :world world')
          (assoc :gears? (.contains (apply str (flatten world')) "*"))
          (update :steps conj d)
          (update :seen conj (hash world'))))))
(defmethod move \1 [d state] (do
                               (println "Direction:" d)
                               (clojure.pprint/pprint state)
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

(def level {:width 6
            :height 7
            :level (str "xxexxx"
                        "xxRxxx"
                        "xr   x"
                        "xxx  x"
                        "x1c  x"
                        "x   *x"
                        "xxxxxx")})

(time (clojure.pprint/pprint (solve (make-state level))))
