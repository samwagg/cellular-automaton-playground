(ns ca-playground.grid-test 
  (:require
   [ca-playground.grid :as grid]
   [clojure.test :refer :all]))

(deftest vec-grid-test
  (testing "invalid input"
    (is (thrown? java.lang.AssertionError
                 (grid/vec-grid [[1]
                                 [3 4]]))))
  (testing "empty grid"
    (is (= [] (seq (grid/vec-grid []))))))

(deftest vec-grid-of-test
  (testing "basic happy path"
    (is (= (grid/vec-grid [[1 2] [3 4]])
           (grid/vec-grid-of [1 2 3 4] 2 2))))
  (testing "infinite lazy seq"
    (is (= (grid/vec-grid [[0 1 2]
                           [3 4 5]
                           [6 7 8]])
           (grid/vec-grid-of (range) 3 3))))
  (testing "coll not large enough"
    (is (thrown? java.lang.AssertionError
                 (grid/vec-grid-of [1 2] 2 2)))))

(deftest VecGrid-tests
  (testing "dims"
    (is (= [5 5] (-> (grid/vec-grid-of (range) 5 5)
                     grid/dims))))
  (testing "gget"
    (testing "happy path"
      (is (= 13 (-> (grid/vec-grid-of (range) 5 5)
                    (grid/gget [2 3])))))
    (testing "coordinate not in grid"
      (is (= nil (-> (grid/vec-grid-of (range) 5 5)
                     (grid/gget [0 5]))))))
  (testing "gassoc"
    (testing "happy path"
      (is (= (grid/vec-grid [[1 2]])
             (-> (grid/vec-grid [[1 1]])
                 (grid/gassoc [0 1] 2)))))
    (testing "coordinate not in grid"
      (is (thrown? java.lang.AssertionError
                   (-> (grid/vec-grid [[1 1]])
                       (grid/gassoc [0 2] 1))))))
  (testing "subgrid"
    (testing "happy path"
      (is (= (grid/vec-grid [[12 13 14]
                             [17 18 19]])
            (-> (grid/vec-grid-of (range) 5 5)
                (grid/subgrid [2 2] [3 4])))))
    (testing "subgrid outside of grid"
      (is (= (grid/vec-grid [])
             (-> (grid/vec-grid-of (range) 5 5)
                 (grid/subgrid [5 5] [6 6])))))
    (testing "subgrid partially outside of grid"
      (is (= (grid/vec-grid [[18 19]
                             [23 24]])
             (-> (grid/vec-grid-of (range) 5 5)
                 (grid/subgrid [3 3] [6 6])))))))
