(module inspector mzscheme

  (define-syntax (with-public-inspector stx)
    (syntax-case stx ()
      [(_ defns ...)
       (with-syntax ([(old-inspector) (generate-temporaries #'(old-inspector))])
         #'(begin (define old-inspector (current-inspector))
                  (current-inspector (make-inspector))
                  defns ...
                  (current-inspector old-inspector)))]))

  (provide with-public-inspector))
