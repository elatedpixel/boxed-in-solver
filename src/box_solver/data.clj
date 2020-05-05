(ns box-solver.data)

(defn- coords [x s width]
  (let [index (.indexOf s (str x))]
    (vector (quot index width) (rem index width))))

(defrecord State [robot world origin hash switches gears? exited? steps])
(defn make-state [{:keys [width height level]}]
  (let [robot (coords \p level width)
        world (mapv vec (partition width level))]
    (->State robot
             world
             (->> level
                  (replace (zipmap "p+*RBGY" (repeat \space)))
                  (partition width)
                  (mapv vec))
             (hash world)
             (zipmap "RGBY" (map #(coords % level width) "RGBY"))
             (.contains level "*")
             false
             [])))

(def level-regex #"(?<width>\d+)\|(?<height>\d+)\|\d+\|(?<level>[x' BRGY@*+rgpby]+)")

(defn display [level]
  (->> (:level level)
       (partition (:width level))
       (map #(apply str %))
       (clojure.string/join "\n")
       (println)))

(let
    [data (-> (clojure.java.io/resource "Levels.txt")
              (slurp)
              (clojure.string/split #","))]
  (def levels
    (->> data
         (map #(or (re-matches level-regex %) %))
         (map rest)
         (map (fn [[width height level]]
            {:width  (Integer/parseInt width)
             :height (Integer/parseInt height)
             :level  level}))
         (zipmap (rest (range))))))
