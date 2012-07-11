;;; cgi-lib.scm - An r5rs library for handling cgi-scripts
;;; Rob Sayers - rsayers@robsayers.com -- http://www.robsayers.com
;;; cgi-lib.pl was used for referece
;;; This is woefully incomplete, but complete enough for my needs
;;; Feel free to hack on it yourself



;;; str-split stolen shamelessly from http://schemecookbook.org/Cookbook/StringSplit
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
	((split
	  (lambda (a b)
	    (cond
	     ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
	     ((char=? ch (string-ref str b)) (if (= a b)
						 (split (+ 1 a) (+ 1 b))
						 (cons (substring str a b) (split b b))))
	     (else (split a (+ 1 b)))))))
      (split 0 0))))


;;; This is the worlds dumbest urlencode/urldecode... 
(load "irregex.scm")
(define encoding-table
  (list
   '("+" " ")
   '("%25" "%")
   '("%20" " ")
   '("%09" " ")
   '("%21" "!")
   '("%22" "\"")
   '("%23" "#")
   '("%24" "$")
   '("%26" "&")
   '("%27" "'")
   '("%28" "(")
   '("%29" ")")
   '("%2A" "*")
   '("%2B" "+")
   '("%2C" ",")
   '("%2D" "-")
   '("%2E" ".")
   '("%2F" "/")
   '("%3A" ":")
   '("%3b" ";")
   '("%3d" "=")
   '("%3e" " ")
   '("%3f" "?")
   '("%40" "@")
   '("%5b" "[")
   '("%5c" "\\")
   '("%5d" "]")
   '("%5e" "^")
   '("%5f" "_")
   '("%60" "`")
   '("%7b" "{")
   '("%7c" "|")
   '("%7d" "}")
   '("%7e" "~")
   '("%09" "\t")))

(define (url-decode s)
  (for-each
   (lambda (t)
     (set! s (irregex-replace/all (sre->irregex (car t) 'i) s (cadr t))))
   encoding-table)
  s)

(define (url-encode s)
  (for-each
   (lambda (t)
     (set! s (irregex-replace/all (sre->irregex (cadr t) 'i) s (car t))))
   encoding-table)
  s)
  

;; The fact that a missing env key returns #f causes problems elsewhere
;; lets make it return a blank string instead
(define (str-getenv k)
  (let ((e (getenv k)))
        (cond
         ((string? e) e)
         (else ""))))

(define (parse-querystring string)
  (cond 
   ((string? string)
    (map (lambda (p) (str-split p #\=)) (str-split (url-decode string) #\&)))
   (else '())))

(define (parse-cookiestring string)
  (cond 
   ((string? string)
    (map (lambda (p) (str-split p #\=))  (irregex-split ";" (irregex-replace/all "[' ]" string ""))))
   (else '())))

(define (read-parse)
  
  (define CONTENT_TYPE (str-getenv "CONTENT_TYPE"))
  (define CONTENT-LENGTH (str-getenv "CONTENT_LENGTH"))
  (define REQUEST_METHOD (str-getenv "REQUEST_METHOD"))
  (define GET_VARS (parse-querystring (str-getenv "QUERY_STRING")))
  (define COOKIES (parse-cookiestring (str-getenv "HTTP_COOKIE")))
  
  (define POST_VARS (cond
                     ((equal? (str-getenv "REQUEST_METHOD") "POST") (parse-querystring (symbol->string (read))))
                     (else '())))
  (define DOCUMENT_ROOT (str-getenv "DOCUMENT_ROOT"))
  (define HTTP_REFERER (str-getenv "HTTP_REFERER"))
  (define HTTP_USER_AGENT (str-getenv "HTTP_USER_AGENT"))
  (define PATH_INFO (str-getenv "PATH_INFO"))
  (define PATH_TRANSLATED (str-getenv "PATH_TRANSLATED"))
  (define QUERY_STRING (str-getenv "QUERY_STRING"))
  (define REMOTE_ADDR (str-getenv "REMOTE_ADDR"))
  (define REMOTE_HOST (str-getenv "REMOTE_HOST"))
  (define SCRIPT_NAME (str-getenv "SCRIPT_NAME"))
  (define SERVER_NAME (str-getenv "SERVER_NAME"))
  (define SERVER_PORT (str-getenv "SERVER_PORT"))
  
  (lambda (key)
    (case key
      ((CONTENT_LENGTH) CONTENT_LENGTH)
      ((CONTENT_TYPE) CONTENT_TYPE)
      ((REQUEST_METHOD) REQUEST_METHOD)
      ((DOCUMENT_ROOT) DOCUMENT_ROOT)
      ((HTTP_REFERER) HTTP_REFERER)
      ((HTTP_USER_AGENT) HTTP_USER_AGENT)
      ((PATH_INFO) PATH_INFO)
      ((PATH_TRANSLATED) PATH_TRANSLATED)
      ((QUERY_STRING) QUERY_STRING)
      ((REMOTE_ADDR) REMOTE_ADDR)
      ((REMOTE_HOST) REMOTE_HOST)
      ((SCRIPT_NAME) SCRIPT_NAME)
      ((SERVER_NAME) SERVER_NAME)
      ((SERVER_PORT) SERVER_PORT)
      ((GET_VARS) GET_VARS)
      ((POST_VARS) POST_VARS)
      ((COOKIES) COOKIES)
      (else '()))))