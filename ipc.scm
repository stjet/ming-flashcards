(load "utils.scm")

;since it will be the wm talking to us, invalid inputs won't happen
(define listen (lambda (handle-message draw title resizable subtype ideal-dimensions)
  (display (let* (
    [_input (get-line (current-input-port))]
    [parts (split-string _input #\space 2)]
    [command (car parts)]
    [rest (if (= (length parts) 2)
      (car (cdr parts))
      "" ;if this is the case `rest` won't be used
    )]
  ) (cond;
    [
      (string=? command "handle_message")
      (symbol->string (car (handle-message ""))) ;doesn't handle clipboard copy request yet... or parse the message, for that matter
    ]
    ;draw
    [
      (string=? command "draw")
      (draw "") ;placeholder, doesn't parse theme info or serialise
    ]
    [
      (string=? command "title")
      (title)
    ]
    [
      (string=? command "resizable")
      (s-bool->string (resizable))
    ]
    [
      (string=? command "subtype")
      subtype
    ]
    [
      (string=? command "ideal_dimensions")
      ;placeholder
      (s-list->string (ideal-dimensions (s-string->list rest)))
    ]
    [
      else
      "invalid"
    ]
  )))
  (newline)
  (listen handle-message draw title resizable subtype ideal-dimensions)
))