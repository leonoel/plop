(ns plop.jvm)

(defn fail! []
  (throw (js/Error. "JVM target not supported.")))

(defn genstruct [slots]
  (fail!))

(defn walk [f s]
  (fail!))