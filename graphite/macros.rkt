#lang racket

(require (for-syntax syntax/parse))


(provide multi-command-line)

(define-syntax (multi-command-line stx)
  (syntax-parse stx
    [(_ #:program big-name args ... #:subcommands [name:str subargs ...] ...)
     #'(let ([true-name big-name])
         (command-line
          #:program true-name
          args ...
          #:args (type . rest)
          (match type
            [name
             (multi-command-line
              #:program (format "~a ~a" true-name name)
              #:argv rest
              args ...
              subargs ...)] ...)))]
    [(_ args ... #:subcommands [name:str subargs ...] ...)
     #'(command-line
        args ...
        #:args (type . rest)
        (match type
          [name
           (multi-command-line
            #:argv rest
            args ...
            subargs ...)] ...))]
    [(_ args ...)
     #'(command-line args ...)]))


