;; game/market.scm

(define-module (game market)
  #:export (display-market buy-goods sell-goods))

(define (display-market planet goods)
  (let ((special-item (cdr (assoc 'special-item goods))))
    (format #t "Market at ~a:\n" planet)
    ;; Display goods for sale
    (for-each (lambda (item)
                (unless (eq? (car item) 'special-item)
                  (format #t "~a: ~a credits\n" (car item) (cdr item))))
              goods)
    ;; Display special item
    (format #t "Special item: ~a\n" special-item)))

(define (buy-goods player goods item amount)
  (let ((price (cdr (assoc item goods))))
    (if (and price (>= (cadr player) (* price amount)))
        (begin
          ;; Deduct cost and add item to player's cargo
          (set-car! (cdr player) (- (cadr player) (* price amount)))
          (set-car! (cdddr player) (cons (cons item amount) (cadddr player)))
          (format #t "You bought ~a units of ~a.\n" amount item))
        (format #t "Not enough credits to buy ~a.\n" amount))))

(define (sell-goods player item amount)
  (let ((cargo (cadddr player))
        (price (* 100 amount))) ;; Selling price (simplified)
    (if (assoc item cargo)
        (begin
          ;; Remove item from cargo and add credits
          (set-car! (cdddr player) (delete item cargo))
          (set-car! (cdr player) (+ (cadr player) price))
          (format #t "You sold ~a units of ~a for ~a credits.\n"
                  amount item price))
        (format #t "You don't have ~a to sell.\n" item))))
