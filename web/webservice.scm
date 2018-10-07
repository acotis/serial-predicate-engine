#!/usr/bin/guile
!#


(use-modules (web server)
             (web request)
             (web response)
             (web uri))

(load "../api/api.scm") ;; Serial predicate engine API impl


(define (append-lines lines)
  (if (= 0 (length lines))
      ""
      (string-append (car lines)
                     "\n"
                     (append-lines (cdr lines)))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (hello-hacker-handler request request-body)
  (let ((input (request-path-components request)))
    (values '((content-type . (text/plain)))
            (append-lines (api-parse (car input))))))


(api-preload)

(run-server hello-hacker-handler)