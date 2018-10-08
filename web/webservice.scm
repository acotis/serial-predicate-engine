#!/usr/bin/guile
!#


(use-modules (web server)
             (web request)
             (web response)
             (web uri))

(use-modules (ice-9 regex))

(load "../api/api.scm") ;; Serial predicate engine API impl


;; Get the path of a request

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

;; Get the query (if any) of a request

(define (request-query request)
  (uri-query (request-uri request)))

;; Build the response to a parse request

(define (parse-route query)
  (format #t "Query: \"~a\"~%" query)

  (let ((input (regexp-substitute/global
                #f "%20" query 'pre " " 'post)))
    (format #t "Input: \"~a\"~%" input)
  
    (values '((content-type . (text/plain)))
            (car (api-parse input)))))

;; Handler

(define (spe-handler request request-body)
  (let ((path (request-path-components request))
        (query (request-query request)))
    
    (if (equal? '("query") path)
        (parse-route query)
        
        (values '((content-type . (text/plain)))
                "Page not found"))))


(api-preload)

(run-server spe-handler
            'http
            '(#:host "0.0.0.0"))