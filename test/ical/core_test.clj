(ns ical.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [ical.core :as core]))

(deftest test-ical
  (letfn
    [(to-ical [input] (with-out-str (core/write-object input)))
     (absences-to-ical [input] (with-out-str (core/absences->ical-object input)))]

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
               ))))

    (testing "Object structure with multiple values"
      (let [result (to-ical [:vcalendar [:vevent [:summary "Bubu Master Boss"]]
                                        [:vevent [:summary "CHUCK"]]])]
        (is (= ["BEGIN:VCALENDAR\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:Bubu Master Boss\r"
                "END:VEVENT\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:CHUCK\r"
                "END:VEVENT\r"
                "END:VCALENDAR\r"]
               (str/split result #"\n")))))

    (testing "absences->ical-object"
      (let [result (absences-to-ical [{:start-date "2015-11-15"
                                       :end-date "2015-11-20"}
                                      {:start-date "2015-12-15"
                                       :end-date "2015-12-20"}])]
        (is (= ["BEGIN:VCALENDAR\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:holidays\r"
                "DTSTART:2015-11-15\r"
                "DTEND:2015-11-20\r"
                "END:VEVENT\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:holidays\r"
                "DTSTART:2015-12-15\r"
                "DTEND:2015-12-20\r"
                "END:VEVENT\r"
                "END:VCALENDAR\r"]
               (str/split result #"\n")))))))
