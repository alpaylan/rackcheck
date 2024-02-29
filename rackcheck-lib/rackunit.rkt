#lang racket/base
(require racket/string)

(require (for-syntax racket/base
                     racket/format
                     syntax/parse)
         racket/format
         racket/port
         (except-in rackunit check)
         (only-in rackunit/private/check-info current-check-info)
         "prop.rkt"
         (submod "prop.rkt" private))

(provide
 (rename-out [check-property* check-property]))

(define with-time (lambda (f)
                    (let ([start (current-inexact-milliseconds)])
                      (let ([result (f)])
                        (let ([end (current-inexact-milliseconds)])
                          (cons result (- end start)))))))



(define (zip a b)
  (apply map list (list a b)))



(define-check (check-property p c)
  (define res-with-time (with-time (lambda () (check c p))))
  (define res (car res-with-time))
  (define time (cdr res-with-time)) 
  (case (result-status res)
    [(falsified)
     (define e (result-e res))

     (define (display-args args)
       (for ([arg-id (in-list (prop-arg-ids (result-prop res)))]
             [arg (in-list args)])
         (display (format "~a: ~s," arg-id arg))))

    (define (args-to-string args)
       (string-join (map 
            (lambda (pair) (format "~a: ~s" (car pair) (cdr pair)))
            (zip (prop-arg-ids (result-prop res)) args)
       ) ", "))



     (define message
       (with-output-to-string
         (lambda ()
           (display (format "[|{ \"time\": ~a, \"foundbug\": true, \"passed\": ~a, \"counterexample\": ~s}|]" time (result-tests-run res) (args-to-string (result-args res))))

           (when (and (result-e res) (not (exn:test:check? (result-e res))))
             (parameterize ([current-error-port (current-output-port)])
               (newline)
               (displayln "Exception:")
               ((error-display-handler)
                (exn-message (result-e res))
                (result-e res)))))))

     (parameterize ([current-check-info (if (exn:test:check? e)
                                            (exn:test:check-stack e)
                                            (current-check-info))])
       (fail-check message))]

    [(timed-out)
     (fail-check (format "Timed out after ~a tests." (result-tests-run res)))]

    [(passed)
     (define labels (sort (hash->list (result-labels res)) > #:key cdr))
     (define num-labels (length labels))
     (displayln (format "  ✓ property ~a passed ~a tests." (prop-name p) (result-tests-run res)))
     (unless (null? labels)
       (displayln "  Labels:")
       (for ([(pair idx) (in-indexed labels)])
         (define lbl (car pair))
         (define pct (* 100 (/ (cdr pair) (result-tests-run res))))
         (displayln (~a "  "
                        (if (= idx (sub1 num-labels))
                            "└"
                            "├")
                        " "
                        (~r #:min-width 5
                            #:precision '(= 2)
                            #:pad-string " "
                            pct)
                        "% "
                        lbl))))]))


(define-syntax (check-property* stx)
  (syntax-parse stx
    [(_ (~optional c:expr) p:expr)
     #:with location (datum->syntax stx (list 'list
                                              (let ([source (syntax-source stx)])
                                                (cond
                                                  [(path? source) source]
                                                  [else (~a source)]))
                                              (syntax-line stx)
                                              (syntax-column stx)
                                              (syntax-position stx)
                                              (syntax-span stx)))
     #'(let ([conf (~? c (make-config))])
         (with-check-info*
           (list
            (make-check-location location)
            (make-check-info 'name (prop-name p))
            (make-check-info 'seessd (config-seed conf)))
           (lambda ()
             (check-property p conf))))]))
