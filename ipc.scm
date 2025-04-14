(load "utils.scm")

;since it will be the wm talking to us, invalid inputs won't happen
(define listen (lambda (window handle-message draw title resizable subtype ideal-dimensions)
  (let (
    [pair (let* (
      [_input (get-line (current-input-port))]
      [parts (split-string _input #\space 2)]
      [command (car parts)]
      [rest (if (= (length parts) 2)
        (car (cdr parts))
        "" ;if this is the case `rest` won't be used
      )]
    ) (cond
      [
        (string=? command "handle_message")
        (let ([resp (handle-message window (split-string rest #\/ -1))])
          (cons (car resp) (join-string (cdr resp) "/"))
        )
      ]
      ;draw
      [
        (string=? command "draw")
        (cons window (join-string (draw window (s-string->theme-info rest)) "\x1D;"))
      ]
      [
        (string=? command "title")
        (cons window (title))
      ]
      [
        (string=? command "resizable")
        (cons window (s-bool->string (resizable)))
      ]
      [
        (string=? command "subtype")
        (cons window subtype)
      ]
      [
        (string=? command "ideal_dimensions")
        (cons window (s-list->string (ideal-dimensions (s-string->list rest))))
      ]
      [
        else
        (cons window "invalid")
      ]
    ))]
  ) (display (cdr pair)) (newline) (listen (car pair) handle-message draw title resizable subtype ideal-dimensions)
  )
))