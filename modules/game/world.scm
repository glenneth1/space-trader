;; game/world.scm
(define-module (game world)
  #:export (initialize-planets get-planet display-planet-info))

;; Planets data structure
(define planets '((earth . "A peaceful planet.")
                  (mars . "The red planet with harsh conditions.")
                  (jupiter . "A gas giant with numerous moons.")))

;; Function to initialize planets (returns planet data)
(define (initialize-planets)
  planets)

;; Function to retrieve planet information
(define (get-planet planet planets)
  (let ((planet-info (assoc planet planets)))
    (if planet-info
        planet-info
        #f))) ;; Return #f if the planet is not found

;; Function to display planet information
(define (display-planet-info planet)
  (let ((planet-info (cdr planet))) ;; Get the planet description
    (if planet-info
        (format #t "Planet Info: ~a\n" planet-info)
        (format #t "Unknown planet.\n"))))
