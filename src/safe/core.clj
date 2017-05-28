(ns safe.core
  (:import (clojure.lang IDeref IRecord IPersistentMap)
           (java.util NoSuchElementException)))


(defprotocol TryMonad

  (+? [this] "Does the try represent a successful execution?")

  (-? [this] "Does the try represent a failed execution?")

  (+filter [this pred] "Filter on the success value.")

  (-filter [this pred] "Filter on the exception.")

  (+fmap [this f] "Flatmap against the success value.")

  (+map [this f] "Map against the success value.")

  (-fmap [this f] "Flatmap against the exception.")

  (-map [this f] "Map against the exception."))


(defrecord Try [value exception]

  TryMonad
  
  (+? [_] (nil? exception))

  (-? [_] (some? exception))

  (+filter [_ pred]
    (if (pred value)
      _ (map->Try {:exception (NoSuchElementException.)})))

  (-filter [_ pred]
    (if (pred exception)
      _ (map->Try {:exception (NoSuchElementException.)})))

  (+fmap [_ f]
    (if (+? _)
      (try
        (f value)
        (catch Exception e
          (map->Try {:exception e}))) _))

  (+map [_ f]
    (if (+? _)
      (try
        (map->Try {:value (f value)})
        (catch Exception e
          (map->Try {:exception e}))) _))

  (-fmap [_ f]
    (if (-? _)
      (try
        (f exception)
        (catch Exception e
          (map->Try {:exception e}))) _))

  (-map [_ f]
    (if (-? _)
      (try
        (map->Try {:value (f exception)})
        (catch Exception e
          (map->Try {:exception e}))) _))

  IDeref
  (deref [_]
    (if (+? _) value (throw exception))))

(prefer-method print-method IRecord IDeref)

(prefer-method print-method IPersistentMap IDeref)

(defmacro try* [& body]
  `(try
     (map->Try {:value ~@body})
     (catch Exception e#
       (map->Try {:exception e#}))))

(defn success* [v] (map->Try {:value v}))

(defn failure* [v] (map->Try {:exception v}))

(defmacro bind* [& body]
  `(try
     (let [value# ~@body]
       (if-not (instance? Try value#)
         (map->Try {:value value#})
         value#))
     (catch Exception e#
       (map->Try {:exception e#}))))

(defmacro try->
  "Like some-> but for try monads."
  [expr & forms]
  (let [g (gensym)
        steps
        (map
          (fn [step]
            `(let [result# (bind* ~g)]
               (+fmap result# #(bind* (-> % ~step))))) forms)]
    `(let [~g (bind* ~expr) ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro try->>
  "Like some->> but for try monads."
  [expr & forms]
  (let [g (gensym)
        steps
        (map
          (fn [step]
            `(let [result# (bind* ~g)]
               (+fmap result# #(bind* (->> % ~step))))) forms)]
    `(let [~g (bind* ~expr) ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))