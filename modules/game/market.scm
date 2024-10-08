;; game/market.scm
(define-module (game market)
  #:export (display-market buy-goods sell-goods))

;; Display the market and its goods
(define (display-market planet goods)
  (let ((special-item (cdr (assoc 'special-item goods))))
    (display (string-append "Market at " (symbol->string planet) ":\n"))
    ;; Display goods for sale
    (for-each (lambda (item)
                (unless (eq? (car item) 'special-item)
                  (display (string-append (symbol->string (car item)) ": " (number->string (cdr item)) " credits\n"))))
              goods)
    ;; Display special item if it exists
    (when special-item
      (display (string-append "Special item: " special-item "\n")))))

;; Buy goods from the market
(define (buy-goods player goods item amount)
  (let ((price (cdr (assoc item goods))))
    (if (and price (>= (player-credits player) (* price amount)))  ;; Checking player's credits
        (begin
          ;; Deduct cost and add item to player's cargo
          (set-player-credits! player (- (player-credits player) (* price amount)))  ;; Deduct credits
          (add-to-cargo player item amount)  ;; Add item to player's cargo
          (display (string-append "You bought " (number->string amount) " units of " (symbol->string item) ".\n")))
        (display (string-append "Not enough credits to buy " (number->string amount) " units of " (symbol->string item) ".\n")))))

;; Sell goods to the market
(define (sell-goods player item amount)
  (let ((cargo (assoc item (player-cargo player)))
        (price (* 100 amount))) ;; Selling price (simplified)
    (if (and cargo (>= (cdr cargo) amount))  ;; Check if player has enough of the item
        (begin
          ;; Remove item from cargo and add credits
          (remove-from-cargo player item amount)  ;; Remove the sold amount from cargo
          (set-player-credits! player (+ (player-credits player) price))  ;; Add credits
          (display (string-append "You sold " (number->string amount) " units of " (symbol->string item) " for " (number->string price) " credits.\n")))
        (display (string-append "You don't have enough " (symbol->string item) " to sell.\n")))))
