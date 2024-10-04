;; main.scm
(use-modules (game market)
             (game trader)
             (game travel)
             (game mission)
             (game world) ;; Import the planets data
             (srfi srfi-1)) ;; For list functions

(define player (create-player))

;; Add option to earn funds while on Earth
(define (earn-funds player)
  (let ((earnings (+ 50 (random 101)))) ;; Earn between 50-150 credits randomly
    (set-car! (cdr player) (+ (cadr player) earnings)) ;; Add earnings to credits
    (format #t "You earned ~a credits on Earth.\n" earnings)))

;; Refuel and repair options when on Earth or another planet
(define (refuel player)
  (let ((fuel-cost (* 5 (- 100 (cadddr player))))) ;; Assume fuel costs 5 credits per unit
    (if (>= (cadr player) fuel-cost)
        (begin
          (set-car! (cdr player) (- (cadr player) fuel-cost)) ;; Deduct credits
          (set-car! (cdddr player) 100) ;; Set fuel to 100
          (format #t "You refueled your ship for ~a credits.\n" fuel-cost))
        (format #t "You don't have enough credits to refuel.\n"))))

(define (repair player damage)
  (if (> damage 0)
      (let ((repair-cost (* 2 damage))) ;; Repairs cost 2 credits per unit of damage
        (if (>= (cadr player) repair-cost)
            (begin
              (set-car! (cdr player) (- (cadr player) repair-cost)) ;; Deduct repair cost
              (format #t "Your ship was repaired for ~a credits.\n" repair-cost))
            (format #t "You don't have enough credits to repair your ship.\n")))
      (format #t "No repairs needed.\n")))

;; Regular service check after 30 days
(define (regular-service player)
  (let ((time (car (cddddr player))))
    (if (>= time 30) ;; Require service every 30 days
        (format #t "Your ship's engines need regular service. Please service your ship.\n")
        (format #t "Your ship is still within service intervals.\n"))))

;; Main game loop
(define (game-loop)
  (display-status player)
  (if (eq? 'earth (car player)) ;; Check if player is on Earth
      (begin
        (format #t "You are on Earth. Do you want to earn funds? (y/n)\n")
        (let ((choice (read-line)))
          (if (string=? choice "y")
              (earn-funds player)
              (format #t "No funds earned.\n")))
        ;; Refuel and repair options on Earth
        (refuel player)
        (repair player 0))) ;; Assume repairs happen on return to Earth
  ;; Continue with travel, market, and mission interactions...
  (format #t "Where would you like to go? (earth/mars/jupiter)\n")
  (let ((planet (string->symbol (read-line))))
    (travel-to-planet player planet planets) ;; Travel and handle fuel/damage
    (regular-service player) ;; Check if engine service is required
    (display-market planet (cdr (assoc planet planets)))
    (list-missions planet)
    (format #t "Do you want to accept a mission? (y/n)\n")
    (let ((choice (read-line)))
      (if (string=? choice "y")
          (accept-mission planet)
          (format #t "No mission accepted.\n")))
    (game-loop)))

;; Start the game
(game-loop)
