(ns safe.core-test
  (:require [clojure.test :refer :all]
            [safe.core :refer :all]))

(defn throw* [& args]
  (throw (RuntimeException. "Test Exception")))

(deftest demonstration
  (testing ""
    (is (value? (bind "123")))
    (is (err? (bind (throw*))))
    (is (= 24 (fmap+ (bind 23) inc)))
    (is (err? (fmap+ (bind (throw*)) inc)))
    (is (= "Test Exception" @(fmap- (bind (throw*)) #(bind (.getMessage %)))))
    (is (= 3 @(try-> 1 (inc) (inc))))
    (is (= "Test Exception" @(fmap- (try-> 1 (throw*) (inc)) #(bind (.getMessage %)))))))