;; game/mission.scm
(define-module (game mission)
  #:export (list-missions accept-mission display-missions initialize-missions))

;; Example mission data
(define missions '((earth . "Deliver food to Mars")
                   (mars . "Transport water to Jupiter")))

;; Function to initialize missions
(define (initialize-missions)
  missions)

;; Display available missions for a given planet
(define (list-missions planet)
  (let ((mission (assoc planet missions)))
    (if mission
        (format #t "Mission available on ~a: ~a\n" planet (cdr mission))
        (format #t "No missions available on ~a.\n" planet))))

;; Display missions for the current planet
(define (display-missions planet)
  (list-missions planet))

;; Accept a mission
(define (accept-mission planet)
  (let ((mission (assoc planet missions)))
    (if mission
        (format #t "Mission accepted: ~a\n" (cdr mission))
        (format #t "No missions available to accept on ~a.\n" planet))))
