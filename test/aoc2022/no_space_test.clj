(ns aoc2022.no-space-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.no-space :refer :all]))

(deftest start-with-empty-root
         (is (= (start-fs) {["/"] []})))

(deftest adding-directory
         (let [starting-directories (start-fs)
               changed-directories (add-dir starting-directories ["/"] "x")]
              (is  (= {["/"] [] ["/" "x"] []}
                      changed-directories))))

(deftest adding-file
         (let [filesystem (add-dir (start-fs) ["/"] "x")]
              (is (= {["/"] [] ["/" "x"] [(file "a" "23")]}
                     (add-file filesystem ["/" "x"] {:name "a" :size 23})))))

(deftest simple-directory-size
         (let [fs (add-dir (start-fs) ["/"] "x")
               fs2 (add-file fs ["/" "x"] (file "a" "23"))
               fs3 (add-file fs2 ["/" "x"] (file "b" "1"))]
              (is (= 24 (dir-size fs3 ["/" "x"])))))

(deftest nested-directory-size
         (let [fs (-> (start-fs)
                      (add-dir ["/"] "x")
                      (add-file ["/" "x"] (file "a" "5"))
                      (add-file ["/"] (file "b" "7")))]
              (is (= 12 (dir-size fs ["/"])))))

(deftest test-subdirs
         (let [fs (-> (start-fs)
                      (add-dir ["/"] "x")
                      (add-dir ["/" "x"] "y")
                      (add-dir ["/"] "z"))]
              (are [dir expected]
                   (= expected (sub-dirs fs dir))
                   ["/"] (set [["/"] ["/" "x"] ["/" "x" "y"] ["/" "z"]])
                   ["/" "x"] (set [["/" "x"] ["/" "x" "y"]]))))

(deftest test-cd-to-root
     (is (= ["/"] (cd "/" ["/" "x"]))))

(deftest test-cd-to-parent
         (is (= ["/" "x"] (cd ".." ["/" "x" "y"]) )))

(deftest test-cd-to-absolute-path
         (is (= ["/" "x"] (cd "/x" ["/" "p" "q"]))))

(deftest test-cd-to-relative-path
         (is (= ["/" "x" "y" "z"] (cd "y/z" ["/" "x"]))))

(deftest test-cd-to-relative-path-with-parent
         (is (= ["/" "x" "z"] (cd "y/../z" ["/" "x"]))))

(deftest test-cd-to-relative-path-with-multiple-parent
         (is (= ["/"] (cd "../.." ["/" "x" "y"]))))

(deftest dir-command-adds-dir
         (is (= {["/"] [] ["/" "mydir"] []}
                (:fs (do-command (start-state) "dir mydir")))))