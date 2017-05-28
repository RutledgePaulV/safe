(ns safe.core
  (:import (clojure.lang IDeref IRecord IPersistentMap)
           (java.util NoSuchElementException)))


(declare success failure)

(defprotocol Try

  (value? [this] "Does the try represent a successful execution?")

  (err? [this] "Does the try represent a failed execution?")

  (filter+ [this pred] "Filter on the success value.")

  (filter- [this pred] "Filter on the exception.")

  (fmap+ [this f] "Flatmap against the success value.")

  (fmap- [this f] "Flatmap against the exception.")

  (map+ [this f] "Map against the success value.")

  (map- [this f] "Map against the exception."))

(defrecord Success [value]

  IDeref
  (deref [_] value)

  Try

  (value? [_] true)
  (err? [_] false)
  (filter- [this _] this)
  (fmap- [this _] this)
  (map- [this _] this)

  (filter+ [this pred]
    (if (pred value) this
      (failure (NoSuchElementException.))))

  (fmap+ [_ f]
    (try
      (f value)
      (catch Exception e
        (failure e))))

  (map+ [_ f]
    (try
      (success (f value))
      (catch Exception e
        (failure e)))))

(defrecord Failure [exception]

  IDeref
  (deref [_] (throw exception))

  Try

  (value? [_] false)
  (err? [_] true)
  (filter+ [this _] this)
  (fmap+ [this _] this)
  (map+ [this _] this)

  (filter- [this pred]
    (if (pred exception) this
     (failure (NoSuchElementException.))))

  (fmap- [this f]
    (try
      (f exception)
      (catch Exception e
        (failure e))))

  (map- [this f]
    (try
      (success (f exception))
      (catch Exception e
        (failure e)))))

(prefer-method print-method IRecord IDeref)

(prefer-method print-method IPersistentMap IDeref)

(defn success [v] (->Success v))

(defn failure [e] (->Failure e))

(defmacro try* [& body]
  `(try
     (success ~@body)
     (catch Exception e#
       (failure e#))))

(defmacro bind [& body]
  `(try
     (let [value# ~@body]
       (if-not (satisfies? Try value#)
         (success value#)
         value#))
     (catch Exception e#
       (failure e#))))

(defmacro try->
  "Like some-> but for try monads."
  [expr & forms]
  (let [g (gensym)
        steps
        (map
          (fn [step]
            `(let [result# (bind ~g)]
               (fmap+ result# #(bind (-> % ~step))))) forms)]
    `(let [~g (bind ~expr) ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro try->>
  "Like some->> but for try monads."
  [expr & forms]
  (let [g (gensym)
        steps
        (map
          (fn [step]
            `(let [result# (bind ~g)]
               (fmap+ result# #(bind (->> % ~step))))) forms)]
    `(let [~g (bind ~expr) ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))