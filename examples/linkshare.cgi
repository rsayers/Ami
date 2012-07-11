#!/usr/bin/guile --debug 
!#

(load "../ami.scm")
(use-modules (srfi srfi-1))

;;; Config vars
(define *posts-per-page* 10)
(define *datafile* "posts.txt")
(define *data* '())
(define *baseurl* "http://localhost/~rsayers/amidev/examples/linkshare.cgi")
;;; Functions for routes
(define (index)
  (template "template/posts.html"))

(define (submit-link)
  (load-data)
  (add-link (post$ "name") (post$ "url"))
  (index))

(define (upvote id)
  (vote (string->number id) 1)
  (index))

(define (downvote id)
  (vote  (string->number id) -1)
  (index))

;;; Routes
(get "/newLink" (lambda () (template "linkForm.html")))
(post "/newLink" submit-link)
(get "/upvote/:id" upvote)
(get "/downvote/:id" downvote)
(get "/" index)

;;; Helper functions

(define (load-data)
 (set! *data* (call-with-input-file *datafile*
    (lambda (p)
      (let loop ((line (read p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read p) (cons line result)))))))

 (cond
  ((null? *data*) '()) ;; this case does nothing really
  (else (set! *data* (car *data*)))))

(define (save-data)
  (call-with-output-file *datafile*
    (lambda (p)
      (write *data* p))))

(define (add-link title url)
  (set! *data* (cons (list title url (+ 1 (max-id)) 1) *data*))
  (save-data))

(define (get-record id)
  (false->nil (find (lambda (r) (eq? id (third r))) *data*)))

(define (update-record id record)
  (set! *data* (let ((data (filter (lambda (r) (not (equal? id (third r)))) *data*)))
    (cons record data))))
          
(define (vote id mod)
  (load-data)
  (let ((rec (get-record id)))
    (update-record id (list (first rec) (second rec)  id (+ mod (fourth rec))))
    (save-data)))

(define (max-in-list l)
  (cond
   ((null? l) 0)
   ((eq? 1 (length l)) (car l))
   ((eq? 2 (length l)) (max (car l) (cadr l)))
   (else (max-in-list (cons (max (car l) (cadr l)) (cddr l))))))

(define (max-id)
    (max-in-list (map (lambda (r) (third r)) *data*)))

(define (sort-data n)
  (sort n (lambda (a b) (> (fourth a) (fourth b)))))

(define (format-link link)
  (display (string-concatenate (list "<tr><td>" (number->string (fourth link)) "</td><td><a href='" *baseurl* "/upvote/" (number->string (third link)) "'>+</a> <a href='" *baseurl* "/downvote/" (number->string (third link)) "'>-</a></td><td><a href='" (second link) "'>" (first link) "</a></td></tr>"))))
(load-data)
(run)