(source-directories '("./" "./ming-flashcards"))

(load "ipc.scm")

(define-record-type flashcards (fields
  (mutable dimensions)
  (mutable input)
  ;questions to ask, to be removed when correctly answered
  (mutable questions)
  ;current question
  (mutable current)
  ;correct answer(s)
  (mutable answers)
  ;total questions correctly asked
  (mutable correct)
  ;total questions asked
  (mutable total)
  ;if false, command mode
  (mutable answer-mode)
))

(define handle-message (lambda (self whole-message)
  ;placeholder
  (let* (
    [message (car whole-message)]
    [args (if (= (length whole-message) 1)
      '("")
      (cdr whole-message)
    )]
    [arg (join-string args "/")]
  ) (cond
    [
      (or (string=? message "Init") (string=? message "ChangeDimensions"))
      (begin
        (flashcards-dimensions-set! self (map string->number (s-string->list (car args))))
        (list self "JustRedraw")
      )
    ]
    [
      (string=? message "KeyPress")
      ;arg is the char
      (cond
        [
          (is-escape arg)
          (let* (
            [answer-mode (flashcards-answer-mode self)]
            [change (if answer-mode
              #t
              (= (length questions) 0) ;if no questions, can't switch to answer mode
            )]
          ) (if change
            (begin
              (flashcards-answer-mode-set! self (not answer-mode))
              (cons self '("JustRedraw"))
            )
            (list self "DoNothing")
          ))
        ]
        [
          (is-enter arg)
          ;process flashcards.input
          (if (flashcards-answer-mode self)
            ;check to see if answer is correct
            (display "placeholder")
            ;
            ;process command
            (display "placeholder")
            ;
          )
        ]
        [
          (is-backspace arg)
          (if (= (length (flashcards-input self)) 0)
            '(self '("DoNothing"))
            (let ([input (flashcards-input self)])
              (flashcards-input-set! self (substring input 0 (- (length input) 1)))
              (list self "JustRedraw")
            )
          )
        ]
        [
          else
          (begin
            ;add char to input
            (flashcards-input-set! self (string-append (flashcards-input self) arg))
            (list self "JustRedraw")
          )
        ]
      )
    ]
    ;
    [
      else
      (list self "DoNothing")
    ]
  ))
))

(define draw (lambda (self ti)
  ;placeholder
  (let* (
    [coords (flashcards-dimensions self)]
    [width (car coords)]
    [height (car (cdr coords))]
  ) (list
      (draw-instructions-text (list 5 (- height 15)) '("nimbus-roman") (string-append (if (flashcards-answer-mode self)
        "ANS: "
        "CMD: "
      ) (flashcards-input self)) (theme-info-text ti) (theme-info-background ti) #f #f)
      "b"
    )
  )
))

(define title (lambda ()
  "Flashcards"
))

(define resizable (lambda ()
  #t
))

(define ideal-dimensions (lambda (_)
  '(420 300)
))

(listen (make-flashcards '(0 0) "" '() "" '() 0 0 #f) handle-message draw title resizable "Window" ideal-dimensions)
