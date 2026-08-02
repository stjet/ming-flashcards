(source-directories '("./" "./ming-flashcards"))

(load "ipc.scm")
(load "storage.scm")

(define-record-type flashcards (fields
  (mutable dimensions)
  (mutable input)
  ;questions to ask, to be removed when correctly answered
  (mutable questions)
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
            [questions (flashcards-questions self)]
            [answer-mode (flashcards-answer-mode self)]
            [change (if answer-mode
              #t
              (= (length questions) 0) ;if no questions, can't switch to answer mode
            )]
          ) (if change
            (begin
              (flashcards-answer-mode-set! self (not answer-mode))
              ;and reset input
              (flashcards-input-set! self "")
              (list self "JustRedraw")
            )
            (list self "DoNothing")
          ))
        ]
        [
          (is-enter arg)
          ;process flashcards.input
          (if (flashcards-answer-mode self)
            ;check to see if answer is correct
            (let ([ans (cadar (flashcards-questions self))])
              (if (string=? (flashcards-input self) ans)
                (begin
                  (flashcards-input-set! self "")
                  (flashcards-questions-set! self (cdr (flashcards-questions self)))
                  (list self "JustRedraw")
                )
                (begin
                  (flashcards-input-set! self ans)
                  (list self "JustRedraw")
                )
              )
            )
            (cond
              [
                (string-starts-with? (flashcards-input self) "load ")
                (let* (
                  [parts (split-string (flashcards-input self) #\space 2)]
                  [name (list-ref parts 1)]
                  [questions (load-questions name)]
                ) (begin
                  (flashcards-questions-set! self questions)
                  ;set answer mode to true, reset input
                  (flashcards-answer-mode-set! self #t)
                  (flashcards-input-set! self "")
                  (list self "JustRedraw")
                ))
              ]
              [
                else
                (list self "DoNothing")
              ]
            )
          )
        ]
        [
          (is-backspace arg)
          (if (= (string-length (flashcards-input self)) 0)
            (list self "DoNothing")
            (let ([input (flashcards-input self)])
              (flashcards-input-set! self (substring input 0 (- (string-length input) 1)))
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
    [questions (flashcards-questions self)]
  ) (list
      (draw-instructions-text (list 5 5) '("nimbus-roman") (if (> (length questions) 0)
        (caar questions)
        "No questions loaded"
      ) (theme-info-text ti) (theme-info-background ti) #f #f)
      (draw-instructions-text (list 5 (- height 15)) '("nimbus-roman") (string-append (if (flashcards-answer-mode self)
        "ANS: "
        "CMD: "
      ) (flashcards-input self)) (theme-info-text ti) (theme-info-background ti) #f #f)
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

(if (file-exists? "~/.local/share/ming-wm/logs.txt")
  ;rude, yeah. later, fix to append
  (delete-file "~/.local/share/ming-wm/logs.txt")
)

(display (guard
  (x [else (condition-message x)])
  (listen (make-flashcards '(0 0) "" '() #f) handle-message draw title resizable "Window" ideal-dimensions)
) (open-output-file "~/.local/share/ming-wm/logs.txt"))
