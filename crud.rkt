#lang racket/base

(require racket/list
        web-server/http
        web-server/servlet-env
        web-server/dispatch
        json)

(define database (make-hasheq))

(define (response
    #:code    [code/kw 200]
    #:message [message/kw "OK"]
    #:seconds [seconds/kw (current-seconds)]
    #:mime    [mime/kw #f]
    #:headers [headers/kw empty]
    #:body    [body/kw empty])
 (define mime
   (cond ((string? mime/kw)
      (string->bytes/utf-8 mime/kw))
         ((bytes? mime/kw)
          mime/kw)
     (else
      #f)))
 (define message
   (cond ((bytes? message/kw)
      message/kw)
     ((string? message/kw)
      (string->bytes/utf-8 message/kw))
         (else
          #f)))
 (define body
   (cond ((string? body/kw)
      (list (string->bytes/utf-8 body/kw)))
     ((bytes? body/kw)
      (list body/kw))
     ((list? body/kw)
          body/kw)
     (#t
      body/kw)))
 (response/full
  code/kw
  message
  seconds/kw
  mime
  headers/kw
  body))

  (define (symbolify x)
  (string->symbol (format "~a" x)))

(define (put! req key)
  (define key/symbol (symbolify key))
  (define present? (hash-has-key? database key/symbol))
  (let ([message (if present?
		     "Resource updated"
		     "Resource created")]
	[data/bytes (request-post-data/raw req)])
    (with-handlers ([exn:fail:contract bad-request])
      (let ([data (bytes->string/utf-8 data/bytes)])
	(hash-set! database key/symbol data)
	(response #:code (if present? 201 204)
		  #:message "No Content")))))

(define (get-catalog-item req x)
  (define key (symbolify x))
  (if (hash-has-key? database key)
      (response #:body (format "~a" (hash-ref database key))
		#:mime "text/plain")
      (response #:code 404
		#:message "Not Found")))

(define (db-as-json/bytes)
  (jsexpr->bytes database))

(define (get-whole-catalog req)
  (response #:body (db-as-json/bytes)
	    #:mime "application/json"))

(define (delete! req x)
  (hash-remove! database x)
  (response))

(define (not-allowed req)
  (response #:code 405
	    #:message "Method Not Allowed"))

(define (not-found req)
  (response #:code 404
	    #:message "Not Found"))

(define (bad-request)
  (response #:code 400
            #:message "Bad Request"))

(define (internal-server-error)
  (response #:code 500
            #:message "Internal Server Error"))

(define-values (go _)
  (dispatch-rules
   [("catalog" (integer-arg)) #:method "put" put!]
   [("catalog" (integer-arg)) #:method "get" get-catalog-item]
   [("catalog")               #:method "get" get-whole-catalog]
   [("catalog" (integer-arg)) #:method "delete" delete!]
   [("catalog")               #:method (regexp ".*") not-allowed]
   [else not-found]))

(module+ main
  (serve/servlet
   go
   #:port 4000
   #:command-line? #t
   #:servlet-regexp #rx""))
