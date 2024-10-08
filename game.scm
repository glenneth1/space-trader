;; game.scm

;; Import necessary modules for Space Trader
(use-modules (game trader)
             (game travel)
             (game mission)
             (game world)
             (ice-9 rdelim)    ;; Import for `read-line`
             (system repl server))  ;; This adds the REPL for an interactive loop

;; Define placeholder functions for event handling
(define (keyboard-event-code event)
  (read-line)) ;; Read user input from command line as a simulation

;; Game state
(define *player* (create-player))  ;; Initialize the player
(define *current-planet* 'earth)   ;; Starting planet
(define *missions* (initialize-missions)) ;; Initialize missions
(define *planets* (initialize-planets))   ;; Planets and data

;; Display current status
(define (display-status)
  (let ((planet (get-planet *current-planet* *planets*)))
    (display-player-status *player*)   ;; Display player status
    (display-planet-info planet)       ;; Display current planet info
    (display-missions *current-planet*))) ;; Display missions for the current planet

;; Functions for user actions
(define (travel-to-planet planet)
  (if (can-travel? *player* planet)
      (begin
        (travel *player* planet)
        (set! *current-planet* planet)
        (display-status))
      (display-error "Not enough fuel to travel.")))

(define (trade-goods planet goods action)
  (if (can-trade? *player* planet goods action)
      (begin
        (trade *player* planet goods action)
        (display-status))
      (display-error "Cannot complete trade.")))

(define (accept-mission mission)
  (if (available-mission? mission *missions*)
      (begin
        (add-mission *player* mission)
        (display-status))
      (display-error "Mission not available.")))

;; Placeholder input functions
(define (prompt-user-for-planet)
  (display "Enter a planet to travel to: ")
  (let ((planet (string->symbol (read-line))))
    (travel-to-planet planet)))

(define (prompt-user-for-trade)
  (display "Enter goods and action (buy/sell): ")
  ;; Add code to handle trading goods based on input
  (display "Trade functionality goes here."))

(define (prompt-user-for-mission)
  (display "Mission selection: ")
  ;; Add code to handle mission acceptance
  (display "Mission functionality goes here."))

;; Game loop
(define (game-loop)
  (display-status)
  (let loop ()
    (display "Press 't' to travel, 'r' to trade, 'm' for mission: ")
    (let ((input (keyboard-event-code 'event)))  ;; Replace this with real input
      (cond
       ((string=? input "t") (prompt-user-for-planet))
       ((string=? input "r") (prompt-user-for-trade))
       ((string=? input "m") (prompt-user-for-mission))
       (else (display "Invalid input.\n"))))
    (loop)))

;; Start game loop
(game-loop)
