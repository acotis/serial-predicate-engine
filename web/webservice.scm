#!/usr/bin/guile
!#


(use-modules (web server)
             (web request)
             (web response)
             (web uri))

(load "../api/api.scm") ;; Serial predicate engine API impl


(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (spe-handler request request-body)
  (let ((input (request-path-components request)))
    (values '((content-type . (text/plain)))
            (car (api-parse (car input))))))


(api-preload)

(run-server spe-handler
            'http
            '(#:host "0.0.0.0"))