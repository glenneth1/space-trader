;; game/mission.scm
(define-module (game mission)
  #:export (list-missions accept-mission))

(define missions '((earth . "Deliver food to Mars")
                   (mars . "Transport water to Jupiter")))

(define (list-missions planet)
  (let ((mission (assoc planet missions)))
    (if mission
        (format #t "Mission available on ~a: ~a\n" planet (cdr mission))
        (format #t "No missions available on ~a.\n" planet))))

(define (accept-mission planet)
  (let ((mission (assoc planet missions)))
    (if mission
        (format #t "Mission accepted: ~a\n" (cdr mission))
        (format #t "No missions available to accept on ~a.\n" planet))))
