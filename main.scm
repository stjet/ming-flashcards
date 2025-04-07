(source-directories '("./" "./ming-flashcards"))

(load "ipc.scm")

(define handle-message (lambda (message)
  ;placeholder
  ;return either ('DoNothing) ('JustRedraw) or ('Request "string") clipboard copy request
  (cons (string->symbol "JustRedraw") '())
))

(define draw (lambda (theme-info)
  ;placeholder
  ""
))

(define title (lambda ()
  "Flashcards"
))

(define resizable (lambda ()
  #t
))

(define ideal-dimensions (lambda (_)
  '(300 300)
))

(listen handle-message draw title resizable "Window" ideal-dimensions)