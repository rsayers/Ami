#!/usr/bin/guile --debug 
!#

(load "../ami.scm")

(get "/" (lambda ()
           (let ((count (default (cookie$ "clicks") "0")))
             (set-cookie "clicks" (+ 1 (string->number count)) "path=/;")
              (set-cookie "foo" (+ 1 (string->number count)) "path=/;") 
              (string-append "You've been here " count " times, hit reload and go for the high score!"))))
             ;; (env 'COOKIES))))
            

(run)
