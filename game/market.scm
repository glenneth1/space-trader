;; game/market.scm
(define-module (game market)
  #:export (display-market buy-goods sell-goods))

(define (display-market planet goods)
  (let ((special-item (cdr (assoc 'special-item goods))))
    (web-output (string-append "Market at " (symbol->string planet) ":"))
    ;; Display goods for sale
    (for-each (lambda (item)
                (unless (eq? (car item) 'special-item)
                  (web-output (string-append (symbol->string (car item)) ": " (number->string (cdr item)) " credits"))))
              goods)
    ;; Display special item
    (web-output (string-append "Special item: " special-item))))

(define (buy-goods player goods item amount)
  (let ((price (cdr (assoc item goods))))
    (if (and price (>= (cadr player) (* price amount)))
        (begin
          ;; Deduct cost and add item to player's cargo
          (set-car! (cdr player) (- (cadr player) (* price amount)))
          (set-car! (cdddr player) (cons (cons item amount) (cadddr player)))
          (web-output (string-append "You bought " (number->string amount) " units of " (symbol->string item) "."))
          )
        (web-output (string-append "Not enough credits to buy " (number->string amount) " units of " (symbol->string item) "."))))

(define (sell-goods player item amount)
  (let ((cargo (cadddr player))
        (price (* 100 amount))) ;; Selling price (simplified)
    (if (assoc item cargo)
        (begin
          ;; Remove item from cargo and add credits
          (set-car! (cdddr player) (delete item cargo))
          (set-car! (cdr player) (+ (cadr player) price))
          (web-output (string-append "You sold " (number->string amount) " units of " (symbol->string item) " for " (number->string price) " credits."))
          )
        (web-output (string-append "You don't have " (symbol->string item) " to sell.")))))
