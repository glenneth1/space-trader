;; main.scm
(use-modules (game market)
             (game trader)
             (game travel)
             (game mission)
             (game world)
             (srfi srfi-1)
             (hoot web)) ;; Import hoot-web module for browser interaction

(define player (create-player))

;; Function to output to the web (HTML element)
(define (web-output msg)
  (js "document.getElementById('game-output').innerHTML = ~a" msg))

;; Add option to earn funds while on Earth
(define (earn-funds player)
  (let ((earnings (+ 50 (random 101))))
    (set-car! (cdr player) (+ (cadr player) earnings)) ;; Add earnings to credits
    (web-output (string-append "You earned " (number->string earnings) " credits on Earth."))))

;; Refuel and repair options when on Earth or another planet
(define (refuel player)
  (let ((fuel-cost (* 5 (- 100 (cadddr player))))) ;; Assume fuel costs 5 credits per unit
    (if (>= (cadr player) fuel-cost)
        (begin
          (set-car! (cdr player) (- (cadr player) fuel-cost)) ;; Deduct credits
          (set-car! (cdddr player) 100) ;; Set fuel to 100
          (web-output (string-append "You refueled your ship for " (number->string fuel-cost) " credits.")))
        (web-output "You don't have enough credits to refuel."))))

(define (repair player damage)
  (if (> damage 0)
      (let ((repair-cost (* 2 damage))) ;; Repairs cost 2 credits per unit of damage
        (if (>= (cadr player) repair-cost)
            (begin
              (set-car! (cdr player) (- (cadr player) repair-cost)) ;; Deduct repair cost
              (web-output (string-append "Your ship was repaired for " (number->string repair-cost) " credits.")))
            (web-output "You don't have enough credits to repair your ship.")))
      (web-output "No repairs needed.")))

;; Regular service check after 30 days
(define (regular-service player)
  (let ((time (car (cddddr player))))
    (if (>= time 30) ;; Require service every 30 days
        (web-output "Your ship's engines need regular service. Please service your ship.")
        (web-output "Your ship is still within service intervals."))))

;; Main game loop - Accepts input from the browser form
(define (runGame input)
  ;; Instead of reading input directly, take input from the browser form
  (let ((user-command input))
    ;; Process user-command like in the original game loop
    (cond ((string=? user-command "earn funds") (earn-funds player))
          ((string=? user-command "refuel") (refuel player))
          ((string=? user-command "repair") (repair player 0))
          ((string=? user-command "travel to mars") (travel-to-planet player 'mars planets))
          ((string=? user-command "travel to jupiter") (travel-to-planet player 'jupiter planets))
          ((string=? user-command "travel to earth") (travel-to-planet player 'earth planets))
          (else (web-output "Unknown command.")))))
