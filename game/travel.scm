;; game/travel.scm
(define-module (game travel)
  #:export (travel-to-planet random-damage))

;; Define distances and fuel costs (distance units, fuel cost, time in days)
(define planet-distances
  '((earth mars . (10 2))     ;; 10 fuel, 2 days travel time
    (earth jupiter . (25 5))  ;; 25 fuel, 5 days travel time
    (mars jupiter . (15 3)))) ;; 15 fuel, 3 days travel time

;; Function to calculate both fuel cost and travel time
(define (calculate-fuel-and-time planet1 planet2)
  (let ((distance-info (assoc planet1 planet2 planet-distances)))
    (if distance-info
        (cdr distance-info)   ;; Returns a list of (fuel cost, travel time)
        '(10 1))))            ;; Default: 10 fuel, 1 day travel time

;; Function to simulate a random event
(define (random-event events)
  (if (null? events)
      '()
      (list-ref events (random (length events)))))

;; Function to simulate random ship damage
(define (random-damage)
  (if (< (random 100) 30) ;; 30% chance of damage
      (+ 10 (random 41))   ;; Damage between 10 and 50 units
      0)) ;; No damage

(define (travel-to-planet player planet planets)
  (let ((planet-data (cdr (assoc planet planets))))
    (if planet-data
        (let* ((events (cdr (assoc 'events planet-data)))
               (travel-info (calculate-fuel-and-time 'earth planet)) ;; Earth as start
               (fuel-cost (car travel-info))
               (time-cost (cadr travel-info))
               (damage (random-damage))) ;; Check if player gets damage
          ;; Check if player has enough fuel
          (if (>= (cadddr player) fuel-cost)
              (begin
                ;; Deduct fuel and add travel time
                (set-car! (cdddr player) (- (cadddr player) fuel-cost)) ;; Deduct fuel
                (set-car! (cddddr player) (+ (car (cddddr player)) time-cost)) ;; Add travel time
                (if (> damage 0)
                    (web-output (string-append "You traveled to " (symbol->string planet) ". Your ship sustained " (number->string damage) " units of damage."))
                    (web-output (string-append "You traveled to " (symbol->string planet) ". No damage to your ship.")))
                ;; Check for random events
                (let ((event (random-event events)))
                  (if event
                      (web-output (string-append "Event: " event))
                      (web-output "No events on this trip."))))
              (web-output (string-append "Not enough fuel to travel to " (symbol->string planet) ". You need at least " (number->string fuel-cost) " units.")))
        (web-output (string-append (symbol->string planet) " is not a valid destination.")))))
