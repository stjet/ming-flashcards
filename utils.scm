(define-record-type theme-info (fields top background border-left-top border-right-bottom text top-text alt-background alt-text alt-secondary))

;s-string->theme-info
(define s-string->theme-info (lambda (str)
  ;split by : and then \x1F
  (display "placeholder")
))

(define s-bool->string (lambda (b)
  (if b "true" "false")
))

(define join-string (lambda (li j-str)
  (string-append (car li)
    (fold-left (lambda (a x)
        (string-append a x)
      )
      ""
      (map (lambda (e) (string-append j-str e)) (cdr li))
    )
  )
))

(define s-list->string (lambda (li)
  (join-string (map number->string li) "\x1F;")
))

;max should be 0 if no max is desired
(define split-string (lambda (str split-char max)
  (define split-string-tail (lambda (chars current splitted)
    (if (or (= (length chars) 0) (= (+ (length splitted) 1) max))
      (reverse (cons current splitted))
      (let (
        [c (car chars)]
      ) (if (char=? c split-char)
        (split-string-tail (cdr chars) "" (cons current splitted))
        (split-string-tail (cdr chars) (string-append current (string c)) splitted)
      ))
    )
  ))
  (split-string-tail (string->list str) "" '())
))

(define s-string->list (lambda (str)
  (split-string str #\x1F 0)
))