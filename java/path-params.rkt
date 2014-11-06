#lang racket

(provide current-classpath current-sourcepath)

(define current-classpath
  (make-parameter (path-list-string->path-list (or (getenv "CLASSPATH") "")
                                               '())))

(define current-sourcepath
  (make-parameter '()))
