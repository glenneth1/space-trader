;; game/world.scm

(define-module (game world)
  #:export (planets))

;; Define planets with special items, population, and events
(define planets
  '((earth . ((food . 100) (water . 50) (tech . 200) (special-item . "Quantum Core")
              (population . 8000000000) (government . "Democracy") (events . '())))
    (mars . ((food . 50) (water . 75) (tech . 150) (special-item . "Martian Dust")
             (population . 1000000) (government . "Colony") (events . '("Pirate attack" "Tech boom"))))
    (jupiter . ((food . 150) (water . 30) (tech . 300) (special-item . "Energy Crystal")
                (population . 50000) (government . "Monarchy") (events . '("Civil unrest" "Trade surge"))))))
