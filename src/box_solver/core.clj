(ns box-solver.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [box-solver.solver :as solver])
  (:gen-class))

(def cli-options
  ;; An option with a required argument
  [["-t" "--timeout TIMEOUT" (str "Execution timeout in milliseconds (0-" 0x10000000000 ")")
    :default nil
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 0x10000000000) (str "Must be a number between 0 and " 0x10000000000)]]
   ["-l" "--level LEVEL" "Which level number to solve (1-150)"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 152) "Must be a number between 1 and 150"]]
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
      (solver/print-solver options))
    ))
