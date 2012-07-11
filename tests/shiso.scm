;;; Shiso - Barebones unit testing for Scheme
;;; Written by Rob Sayers - rsayers@robsayers.com
;;; Based on http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html
;;;
;;; Run your tests as:
;;;
;;; (check
;;;  (list
;;;   '(case1)
;;;   '(case2)
;;;   '(case3)))
;;;
;;;A simple yet contrived example would be
;;;
;;;(check
;;; (list
;;;  '(eq? 1 1)))
;;;
;;;Which would return:
;;;
;;;pass ... (eq? 1 1)
;;;
(use-modules (ice-9 format))


(define (check cases)
  (map report-result cases))


(define (report-result form)
  (format #t "~:[FAIL~;pass~] ... ~a~%" (eval form (interaction-environment)) form))