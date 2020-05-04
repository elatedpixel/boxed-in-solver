(ns box-solver.core
  (:require [box-solver.data :as d]
            [clojure.tools.cli :refer [parse-opts]])
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

(def won? #(= [false true] ((juxt :gears? :exited?) %)))

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

(defn seq-graph [data-structure children-fn stop-pred initial-state]
  ((fn walk [explored frontier]
     (lazy-seq
      (when (seq frontier)
        (let [state (peek frontier)
              children (children-fn state)
              hashes (zipmap (map (comp hash :world) children) children)]
          (cons state
                (if-let [winner (some #(when (stop-pred %) %) children)]
                  (list winner)
                  (walk (into explored (keys hashes))
                       (into (pop frontier) (select-values hashes (remove explored (keys hashes)))))))))))
   #{(hash (:world initial-state))} (conj data-structure initial-state)))

(def bfs (partial seq-graph clojure.lang.PersistentQueue/EMPTY))

(defn next-moves [state]
  (keep #(move % state) (keys direction)))

(defn solve [state]
  (last (bfs next-moves won? state)))

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

(comment
  (print-solver 9 :timeout (* 1000 60 3))
                                        ;
  )

(def cli-options
  ;; An option with a required argument
  [["-t" "--timeout TIMEOUT" (str "Execution timeout in milliseconds (0-" 0x10000000000 ")")
    :default nil
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 0x10000000000) (str "Must be a number between 0 and " 0x10000000000)]]
   ["-l" "--level LEVEL" "Which level number to solve (1-150)"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 151) "Must be a number between 1 and 150"]]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (cond
      (:help options)
      (do
        (println "Boxed-in solver (corona v1.0)")
        (println "Options:")
        (println summary))

      errors
      (do
        (println "Boxed-in solver (corona v1.0)")
        (println "Options:")
        (println summary)
        (println errors))

      :else
      (print-solver options))
    ))
