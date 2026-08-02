(define line->question (lambda (line)
  ;question and answer separated by #\x1F, I guess
  (let ([parts (split-string line #\x1F -1)])
    (cons (car parts) (cdr parts)) ;return pair. list would be funnier though
  )
))

(define-record-type theme-info (fields top background border-left-top border-right-bottom text top-text alt-background alt-text alt-secondary))

;s-string->theme-info
(define s-string->theme-info (lambda (str)
  ;split by : then \x1F
  (apply make-theme-info (map (lambda (s)
      (map string->number (split-string s #\x1F -1))
    )
    (split-string str #\: -1)
  ))
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

;todo: generalise to more types
(define s-list->string (lambda (li)
  (join-string (map (lambda (t)
    (if (string? t)
      t
      (number->string t)
    )
  ) li) "\x1F;")
))

;max should be -1 if no max is desired
(define split-string (lambda (str split-char max)
  (define split-string-tail (lambda (chars current splitted)
    ;(= (length splitted) max)
    (if (= (length chars) 0)
      (reverse (cons current splitted))
      (let (
        [c (car chars)]
      ) (if (and (char=? c split-char) (not (= (- max 1) (length splitted))))
        (split-string-tail (cdr chars) "" (cons current splitted))
        (split-string-tail (cdr chars) (string-append current (string c)) splitted)
      ))
    )
  ))
  (split-string-tail (string->list str) "" '())
))

(define string-starts-with? (lambda (str starts-str)
  (define string-starts-with-tail (lambda (index max)
    (if (= index max)
      #t
      (if (char=? (string-ref str index) (string-ref starts-str index))
        (string-starts-with-tail (+ index 1) max)
        #f
      )
    )
  ))
  (if (< (string-length str) (string-length starts-str))
    #f
    (string-starts-with-tail 0 (string-length starts-str))
  )
))

(define s-string->list (lambda (str)
  (split-string str #\x1F -1)
))

(define s-option->string (lambda (opt)
  (if (not opt)
    ;None
    "N"
    ;Some
    (string-append "S" (number->string opt))
  )
))

(define draw-instructions-text (lambda (point fonts text colour bg-colour option-horiz-spacing option-mono-width)
  ;Text(Point, Vec<String>, String, RGBColor, RGBColor, Option<usize>, Option<u8>), //font and text
  (string-append "Text/" (s-list->string point) "\x1E;" (s-list->string fonts) "\x1E;" text "\x1E;" (s-list->string colour) "\x1E;" (s-list->string bg-colour) "\x1E;" (s-option->string option-horiz-spacing) "\x1E;" (s-option->string option-mono-width))
))

(define is-escape (lambda (c)
  (string=? c "𐘃")
))

(define is-enter (lambda (c)
  (string=? c "𐘂")
))

(define is-backspace (lambda (c)
  (string=? c "𐘁")
))
