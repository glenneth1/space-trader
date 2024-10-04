;; game/trader.scm
(define-module (game trader)
  #:export (create-player display-status))

(define (create-player)
  ;; Add fuel and time attributes to the player
  (list 'credits 500 'cargo '() 'fuel 100 'time 0)) ;; Starting fuel and time

(define (display-status player)
  (format #t "You have ~a credits.\n" (cadr player))
  (format #t "Fuel: ~a units\n" (cadddr player))
  (format #t "Elapsed time: ~a days\n" (car (cddddr player))) ;; Time in days
  (format #t "Cargo: ~a\n" (cadddr (cdr player))))
