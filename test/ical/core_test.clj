(ns ical.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [ical.core :as core]))

(deftest test-ical
  (letfn
    [(to-ical [input] (with-out-str (core/write-object input)))]

    (testing "Properties"
      (is "VERSION:2.0" (to-ical [:version "2.0"])))
    (testing "Multiple parameters"
      (is "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:MAILTO:bubu@mail.com"
          (to-ical [:attendee {:rsvp true :role "req-participant"} "MAILTO:bubu@mail.com"])))

    (testing "Object structure"
      (let [result (to-ical [:vcalendar [:vevent [:summary "Bubu Master Boss"]]])]
        (is (= ["BEGIN:VCALENDAR\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:Bubu Master Boss\r"
                "END:VEVENT\r"
                "END:VCALENDAR\r"]
               (str/split result #"\n")
               ))))))
