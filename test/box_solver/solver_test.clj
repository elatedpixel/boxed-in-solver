(ns box-solver.solver-test
  (:require [clojure.test :refer :all]
            [box-solver.data :as data]
            [box-solver.solver :refer :all]))

(deftest test-moving-pieces
  (testing "robot can step on exit"
    (let [level (data/make-state {:width 2 :height 1 :level "p@"})]
      (is (= [:right] (:steps (solve level))))))
                                        ;
  (testing "robot can pick up gear"
    (let [level (data/make-state {:width 3 :height 1 :level "p*@"})]
      (is (= [:right :right] (:steps (solve level))))))
                                        ;
  (testing "robot can push crate over exit"
    (let [level (data/make-state {:width 4 :height 1 :level "p+@ "})]
      (is (= [:right :right] (:steps (solve level))))))
                                        ;
  (testing "robot can open force-field with crate on switch"
    (let [level (data/make-state {:width 5 :height 1 :level "r+pR@"})]
      (is (= [:left :right :right :right] (:steps (solve level))))))
                                        ;
  )
