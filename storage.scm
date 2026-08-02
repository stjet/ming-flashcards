(load "utils.scm")

(define load-questions (lambda (name)
  ;load and parse files
  (define load-questions-tail (lambda (file questions)
    (let ([line (get-line file)])
      (if (eof-object? line)
        questions
        (load-questions-tail file (append questions (list (line->question line))))
      )
    )
  ))
  (load-questions-tail (open-input-file (string-append "~/.local/share/ming-wm/flashcards/" name ".flashq")) '())
))
