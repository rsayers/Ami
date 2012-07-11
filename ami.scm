;;; Ami - A friendly little webapp framework in Scheme
;;; Copyright (C) 2012 Robert Sayers. MIT License
;;; http://ami.robsayers.com

(use-modules (srfi srfi-1)
              (ice-9 rdelim))

(load "lib/irregex.scm")
(load "lib/cgi-lib.scm")
(define env (read-parse))
(define *dispatch-table* '())
(define *headers* '())
(define *params* '())
(define *get-vars '())
(define *post-vars '())
(define *server-vars* '())
(define *cookies* '())
(define *output-buffer* '())

;;****f* Ami/p
;; NAME
;; 	p - alias to display,  to make templates more compact
;;******
(define p display)

;;****f* Ami/get$
;; NAME
;; 	get$ - retrieves an HTTP_GET_VAR
;; INPUTS
;; 	key - String value of the key you want to find
;;
;; RESULT
;;	String value if found, otherwise Nil
;;******

(define (get$ key)
  (find-var 'GET_VARS key))

;;****f* Ami/post$
;; NAME
;;      post$ - retrieves an HTTP_POST_VAR
;; INPUTS
;;      key - String value of the key you want to find
;;
;; RESULT
;;      String value if found, otherwise Nil
;;******
(define (post$ key) 
   (find-var 'POST_VARS key))

;;****f* Ami/cookie$
;; NAME
;;      cookie$ - retrieves a cookie
;; INPUTS
;;      key - String value of the key you want to find
;;
;; RESULT
;;      String value if found, otherwise Nil
;;******
(define (cookie$ key) 
  (find-var 'COOKIES key))

;;****f* Ami/find-var
;; NAME
;;      find-var - Searches a specified association list for a key
;; INPUTS
;;      collection - Symbol of 'POST_VARS, 'GET_VARS, or 'COOKIES
;;      key - String value of the key you want to find
;; RESULT
;;      String value if found, otherwise Nil
;; NOTES
;;	This is wrapped by the get$ post$ and cookie$ functions and will probably not be called by the user
;;******
(define (find-var collection key)
   (let ((res (assoc key (env collection))))
    (cond ((eq? #f res) '())
          (else (cadr res)))))

;;****f* Ami/set-cookie
;; NAME
;;      set-cookie - sets a cookie
;; INPUTS
;;      key - The String name of the cookie
;;      val - the value of the cookie
;;      extra - (Optional) A list of extra strings to append to the cookie, use this to pass a path or expiration date
;;
;; RESULT
;;      Nil
;;******
(define (set-cookie key val . extra)
  (set! val
        (cond
         ((number? val) (number->string val))
         (else val)))
  (set! *cookies* (cons (list key val extra) *cookies*)))

(define (send-cookies)
  (map
   (lambda (c)
     (add-header/allowdup (string-concatenate (list "Set-Cookie: " (car c) "=" (cadr c) ";" (string-concatenate (caddr c)))))) *cookies*))
  
;;****f* Ami/error
;; NAME
;;      error - displays an error message
;; INPUTS
;;      code - The string of the error code
;;
;; RESULT
;;      The string containing the error message
;; EXAMPLE
;;	(error "404") -> "The Resource you requested is not found"
;; NOTES
;; 	Currently only 404 is implemented
;;******

(define (error code)
  (cond
   ((equal? code "404")  "The resouce you requested is not found")))

;; thanks little schemer
(define (rember a lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) a) (cdr lat))
   (else (cons (car lat)
               (rember a (cdr lat))))))

;;****f* Ami/add-header
;; NAME
;;      add-header - adds a new header to send to the browser
;; INPUTS
;;      header - String of the complete header to send
;; EXAMPLE
;;      (add-header "Location: http://google.com")
;; NOTES
;;      Any header with an existing name will be removed before the new one is added
;;	If you need duplicates, use addheader/allowdup
;;******

(define (add-header header)
  (set! *headers* (cons header (rember header *headers*))))

;;****f* Ami/add-header/allowdup
;; NAME
;;      add-header/allowup - Same as add-header, but allows duplicates
;;******

(define (add-header/allowdup header)
  (set! *headers* (cons header  *headers*)))

(define (display-headers headers)
  (cond
   ((null? headers) (display "\n"))
   (else (begin
           (display (car headers))
           (newline)
           (display-headers (cdr headers))))))

(define (return-args uri pattern)
  (letrec
      ((get-arg-list (lambda (matches idx l)
                       (cond
                        ((eq? #f matches) '())
                        ((not (irregex-match-valid-index? matches idx)) l)
                        (else (get-arg-list matches (+ 1 idx) (cons (irregex-match-substring matches idx) l)))))))
    
    (get-arg-list (irregex-match  pattern uri) 1 '())))

;;****f* Ami/get
;; NAME
;;      get - defines a route for the GET method
;; INPUTS
;;	req - the request pattern to match
;;      resp - the function to call when the request is matched      
;; EXAMPLE
;;      (get "/hello" (lambda () "Hello World")) -> URL /hello will display "Hello World" in the browser
;;      (get "/say/:name" (lambda (name) (string-append "Hello! " name))) -> URL /say/steve will display "Hello! steve"
;;      (get "/id/(\d+)" (lambda (id) (string-append "Accessing #" id))) -> URL /id/3 will match, but /id/test would not
;;******

(define (get req resp)
  (set! *dispatch-table* (cons (list "GET" (irregex-replace ":.+" req "(.+)") resp) *dispatch-table*)))

;;****f* Ami/post
;; NAME
;;      post - defines a route for the POST method
;; INPUTS
;;      req - the request pattern to match
;;      resp - the function to call when the request is matched
;; EXAMPLE
;;      (post "/hello" (lambda () "Hello World")) -> URL /hello will display "Hello World" in the browser
;;      (post "/say/:name" (lambda (name) (string-append "Hello! " name))) -> URL /say/steve will display "Hello! steve"
;;      (post "/id/(\d+)" (lambda (id) (string-append "Accessing #" id))) -> URL /id/3 will match, but /id/test would not
;;******

(define (post req resp)
  (set! *dispatch-table* (cons (list "POST" (irregex-replace ":.+" req "(.+)") resp) *dispatch-table*)))

;;****f* Ami/default
;; NAME
;;      default - set a default value for any expression
;; INPUTS
;;      var - the expression to test
;;      val - the desired default value
;; RESULT
;;	if var is #f, '() or "",  val is returned, otherwise, var is
;;******
(define (default var val)
  (cond
   ((not var) val)
   ((null? var) val)
   ((equal? "" var) val)
   (else var)))

;;****f* Ami/root
;; NAME
;;      root - returns the root of a uri path
;; INPUTS
;;      url - a url path 
;; EXAMPLE
;;      (root "/home/test") -> "home"
;;      (root "/") -> "/"
;;      (root "") -> "/"
;;******

(define (root url)
  (let ((parts (str-split url #\/)))
    (cond
     ((not (pair? parts)) "/") ;; If we dont get a pair, that means the url is "/"
     (else (car parts)))))


(define (dispatch method uri)
  (let ((handler
         (find
          (lambda (n)
            (and
             (equal? method (car n))
             (equal? (root uri) (root (cadr n)))
             (irregex-search (cadr n) uri))) *dispatch-table*)))
    
    (cond
     ((or (eq? #f handler) (null? handler)) (error "404"))
     (else (apply (caddr handler) (return-args uri (cadr handler)))))))

(define (run)
  ;; This is the default content type to send if no other is set

  (add-header "Content-type: text/html")
 
  (add-to-buffer (dispatch (default (env 'REQUEST_METHOD) "GET")  (default (env 'PATH_INFO) "/")))
  (send-cookies)
  (display-headers *headers*)
  (display-headers *output-buffer*))

(define (drain-output port)
  (let loop ((chars '())
             (next (read-char port)))
    (if (eof-object? next)
        ; Modified to not return last 'line' with newline
        (list->string (reverse! (cdr chars)))
        (loop (cons next chars)
              (read-char port)))))

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (drain-output p))))
   
;;****f* Ami/template
;; NAME
;;      template - compile a template file
;; INPUTS
;;      filename - file containing the template
;; NOTES
;;	This is implemented so that your template file is compiled and executed in the same
;;      scope as the environment from which it was called,  so your template will have access
;;      to any symbols defined in its calling function
;;******
(define-macro (template filename)
  `(with-output-to-string (lambda ()(current-output-port)(eval-string (compile-template (readlines ,filename))))))
  

;;****f* Ami/add-to-output
;; NAME
;;      add-to-ouput - adds a string to the output buffer
;; INPUTS
;;      data - the string to add to the buffer
;; NOTES
;;	This is not a general usage function, as the proper way to create output is to
;;	return it from your routes function.  Add-to-output is used internally, but may prove
;;	usefull for some developers
;;******
(define (add-to-buffer data)
  (set! *output-buffer* (cons data *output-buffer*)))

(define (compile-template body)
  (set! body (irregex-replace/all "\\%>" body "(display \""))
  (set! body (irregex-replace/all "<\\%" body "\") "))
  (string-append "(display \"" body "\")"))



(define (false->nil n)
  (cond
   ((not n) '())
   (else n)))

(define (false->string n)
  (cond
   ((not n) "")
   (else n)))


