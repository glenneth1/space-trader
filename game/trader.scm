;; game/trader.scm
(define-module (game trader)
  #:export (create-player display-status))

(define (create-player)
  ;; Add fuel and time attributes to the player
  (list 'credits 500 'cargo '() 'fuel 100 'time 0)) ;; Starting fuel and time

(define (display-status player)
  (web-output (string-append "You have " (number->string (cadr player)) " credits."))
  (web-output (string-append "Fuel: " (number->string (cadddr player)) " units"))
  (web-output (string-append "Elapsed time: " (number->string (car (cddddr player))) " days"))
  (web-output (string-append "Cargo: " (number->string (length (cadddr (cdr player)))))))
