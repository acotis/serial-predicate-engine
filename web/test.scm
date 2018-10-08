#!/usr/bin/guile
!#


(use-modules (web server)
             (web request)
             (web response)
             (sxml simple))

(define (templatize title body)
  `(html (head (title ,title))
         (body ,@body)))

(define* (respond #:optional body #:key
                  (status 200)
                  (title "Hello world!")
                  (doctype "<!DOCTYPE html>\n")
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (templatize title body))))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (if sxml
                (begin
                  (if doctype (display doctype port))
                  (sxml->xml sxml port))))))

(define (debug-page request body)
  (respond
   `((h1 "Hello world!")
     (table
      (tr (th "header") (th "value"))
      ,@(map (lambda (pair)
               `(tr (td (tt ,(with-output-to-string
                               (lambda ()
                                 (display (car pair))))))
                    (td (tt ,(with-output-to-string
                               (lambda ()
                                 (write (cdr pair))))))))
             (request-headers request))))))

(define (form-page request body)
  (respond `((h1 "hello!"))))

(run-server form-page)