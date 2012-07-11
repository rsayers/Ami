(load "shiso.scm")

;;; cgi-lib tests
(load "../lib/cgi-lib.scm")
(use-modules (srfi srfi-1))
(setenv "QUERY_STRING" "foo=bar&name=rob")
(setenv "CONTENT_TYPE" "text/plain")
(setenv "HTTP_COOKIE" "cookie=true")

(define test-env (read-parse))

(check (list 
	'(string=? "text/plain" (test-env 'CONTENT_TYPE))
        '(string=? "true" (cadr (assoc "cookie" (test-env 'COOKIES))))
	'(string=? "bar" (cadr (assoc "foo" (test-env 'GET_VARS))))))


;;; ami tests
(load "../ami.scm")

;; URL params
(get "/hello/:name" (lambda (name) (string-join (list "Hello " name) "")))


;; Access GET vars from an action
(get "/getvars" (lambda () (get$ "name")))

;; Access a cookie from an action
(get "/getcookie" (lambda () (cookie$ "cookie")))

(get "/template" (lambda () (template "count.html")))
(check (list
       '(string=? "\nCounting: 10-9-8-7-6-5-4-3-2-1-" (template "count.html"))
       '(string=? "\nCounting: 10-9-8-7-6-5-4-3-2-1-" (dispatch "GET" "/template"))
       '(string=? "Hello rob" (dispatch "GET" "/hello/rob"))
       '(string=? "true" (dispatch "GET" "/getcookie"))
       '(string=? "The resouce you requested is not found" (dispatch "GET" "/doesNotExist"))
       '(string=? "rob" (dispatch "GET" "/getvars"))))



