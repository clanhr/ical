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
      (let [result (absences-to-ical [{:_id 1
                                       :start-date "20151210"
                                       :end-date "20151212"
                                       :status "cancelled"}
                                      {:_id 2
                                       :start-date "20151215"
                                       :end-date "20151220"
                                       :status "approved"}])]
        (is (= ["BEGIN:VCALENDAR\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:holidays\r"
                "UID:1\r"
                "DTSTART:20151210\r"
                "DTEND:20151212\r"
                "STATUS:cancelled\r"
                "END:VEVENT\r"
                "BEGIN:VEVENT\r"
                "SUMMARY:holidays\r"
                "UID:2\r"
                "DTSTART:20151215\r"
                "DTEND:20151220\r"
                "STATUS:approved\r"
                "END:VEVENT\r"
                "END:VCALENDAR\r"]
               (str/split result #"\n")))))))
