(module tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1)))
  (require (lib "class.ss"))
  (require "../../java.ss")
  (require "../../syntax/parser.ss")
  (require "../../semantics/semantic-object.ss")
  (require "../../semantics/class-resolver.ss")
  (require "../../semantics/standard-resolver.ss")

  (define (parser-test str)
    (make-test-case (substring str 0 (min 25 (string-length str)))
      (assert-not-exn (lambda () (parse-string str)))))

  (define (parser-fail-test str)
    (make-test-case (string-append "Fail: " (substring str 0 (min 25 (string-length str))))
      (assert-exn (lambda x #t) (lambda () (parse-string str)))))

  (define parser-tests
    (make-test-suite "Tests for the parser"
                     (parser-test "")
                     (parser-test "class C {}")
                     (parser-fail-test "class C")
                     (parser-fail-test "class C {String x = \"foo;}")
                     (parser-test "class C {String x = \"foo\";}")
                     ))

  (current-class-resolver (new class-resolver%))

  (define java.lang.Object
    (lookup-type (build-type-name '(java lang Object))))
  (define java.lang.String
    (lookup-type (build-type-name '(java lang String))))

  )
