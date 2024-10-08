;; game/trader.scm
(define-module (game trader)
  #:export (create-player player-fuel set-player-fuel! player-time
                          set-player-time! player-credits set-player-credits!
                          player-cargo add-to-cargo remove-from-cargo display-player-status))

;; Create player with initial attributes
(define (create-player)
  (list '(credits . 500)
        '(fuel . 100)
        '(time . 0)
        '(cargo . ())))

;; Display player status
(define (display-player-status player)
  (display (string-append "Credits: " (number->string (player-credits player)) "\n"))
  (display (string-append "Fuel: " (number->string (player-fuel player)) "\n"))
  (display (string-append "Time: " (number->string (player-time player)) "\n"))
  (display (string-append "Cargo: " (if (null? (player-cargo player))
                                        "Empty"
                                        (cargo-list-string (player-cargo player))) "\n")))

;; Helper function to create a string representing the cargo list
(define (cargo-list-string cargo)
  (string-join
   (map (lambda (item)
          (string-append (symbol->string (car item)) ": " (number->string (cdr item))))
        cargo)
   ", "))

;; Accessor and setter functions...

;; Accessor for player fuel
(define (player-fuel player)
  (cdr (assoc 'fuel player)))

;; Setter for player fuel
(define (set-player-fuel! player new-fuel)
  (set-cdr! (assoc 'fuel player) new-fuel))

;; Accessor for player time
(define (player-time player)
  (cdr (assoc 'time player)))

;; Setter for player time
(define (set-player-time! player new-time)
  (set-cdr! (assoc 'time player) new-time))

;; Accessor for player credits
(define (player-credits player)
  (cdr (assoc 'credits player)))

;; Setter for player credits
(define (set-player-credits! player new-credits)
  (set-cdr! (assoc 'credits player) new-credits))

;; Accessor for player cargo
(define (player-cargo player)
  (cdr (assoc 'cargo player)))

;; Add item to player cargo, increasing the quantity if it already exists
(define (add-to-cargo player item amount)
  (let ((cargo (player-cargo player))
        (existing (assoc item cargo)))
    (if existing
        ;; Increase the amount of the existing item
        (set-cdr! existing (+ (cdr existing) amount))
        ;; Otherwise, add the item to cargo
        (set! (cdr (assoc 'cargo player)) (cons (cons item amount) cargo)))))

;; Remove item from player cargo, ensuring the amount doesn't go negative
(define (remove-from-cargo player item amount)
  (let ((cargo (player-cargo player))
        (existing (assoc item cargo)))
    (if (and existing (>= (cdr existing) amount))
        (if (= (cdr existing) amount)
            ;; Remove the item if all of it is sold
            (set! (cdr (assoc 'cargo player)) (assq-delete-all item cargo))
            ;; Otherwise, reduce the quantity
            (set-cdr! existing (- (cdr existing) amount)))
        (display "You don't have enough of this item to remove.\n"))))
