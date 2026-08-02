(display "Name?")
(let ([name (get-line (current-input-port))])
  (display "Question?")
  (let ([question (get-line (current-input-port))])
    (display "Answer?")
    (let ([answer (get-line (current-input-port))])
      (display answer)
      ;append to file
      ;
    )
  )
)