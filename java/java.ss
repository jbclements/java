(module java mzscheme

  (define current-classpath
    (make-parameter (path-list-string->path-list (or (getenv "CLASSPATH") "")
                                                 '())))

  (define current-sourcepath
    (make-parameter '()))

  (provide current-classpath current-sourcepath))
