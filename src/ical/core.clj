(ns ical.core
  (:use clojure.string)
  (:refer-clojure :exclude [replace reverse println])
  (:require [clj-time.format :as format]))

(def ^{:doc "The media-type of all iCalendar objects, as declared in
            section 3.1"}
  media-type "text/calendar")

(def ^{:dynamic true} *fold-column* 75)

(defn println [s]
  (letfn [(fold [s n]
            (let [lines (partition n n nil s)]
              (->>
                (cons (first lines)
                      ;; Each subsequent line to begin with a space
                      (map (partial cons " ") (rest lines)))
                (map #(concat % "\r\n")) ; Add CRLFs
                ;; Convert each line back to a string
                (map (partial reduce str))
                (reduce str))))]
    (doall (print (fold s *fold-column*)))))

(defn forbid-quote-in-parameter-value [x]
  (if (some #{\"} x)
    (throw (Exception. "Parameter value cannot contain a double-quote")))
  x)

;; 4.1.1 Property parameters with values containing a COLON, a
;; SEMICOLON or a COMMA character MUST be placed in quoted text.
(defn quote-if-necessary [x]
  (if (some #{\: \; \,} x) (str "\"" x "\"") x))

(defn is-quoted? [x]
  (let [s (seq x)]
    (and
      (= \" (first s))
      (not (some #{\"} (butlast (rest s)))))))

(defmulti format-value class :default :other)

(defmethod format-value String [s] s)
(defmethod format-value org.joda.time.LocalDate [ld]
  (.print (clj-time.format/formatters :basic-date) ld))
(defmethod format-value org.joda.time.DateTime [dt]
  (.print (clj-time.format/formatters :basic-date-time-no-ms) dt))
(defmethod format-value :other [s] s)


(defmulti format-parameter-value class :default :other)

(defmethod format-parameter-value Boolean [x]
  (upper-case (str x)))

(defmethod format-parameter-value org.joda.time.LocalDate [x]
  (format-value x))

(defmethod format-parameter-value org.joda.time.DateTime [x]
  (format-value x))

(defmethod format-parameter-value :other [x]
  (cond
    (is-quoted? x) x
    :otherwise ((comp quote-if-necessary
                      upper-case
                      forbid-quote-in-parameter-value) x)))

(defmulti serialize-param coll?)
(defmethod serialize-param false [x]
  (format-parameter-value x))
(defmethod serialize-param true [x]
  (->> x
       (map format-parameter-value)
       (interpose ",")
       (reduce str)))

;; ## Writing objects

(defn write-object
  "Write an iCalendar object."
  [obj]
  (letfn [(serialize-params [params]
            (map (fn [[k v]]
                   (format "%s=%s"
                           (upper-case (name k))
                           (serialize-param v))) params))]
    (assert (keyword? (first obj)))
    (let [n (upper-case (name (first obj)))
          snd (second obj)
          params (if (map? snd) snd)
          values (drop (if params 2 1) obj)
          parent? (true?
                    (some #(and (vector? %) (keyword? (first %)))
                          values))]
      (if parent?
        (do
          (println (str "BEGIN:" n))
          (doall (for [e values] (write-object e)))
          (println (str "END:" n)))
        (println
          (str
            (reduce str (interpose ";" (cons n (serialize-params params))))
            ":"
            (reduce str (interpose "," (map format-value (flatten values))))))))))
