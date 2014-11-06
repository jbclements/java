(module javap mzscheme
  (require (lib "list.ss" "srfi" "1"))
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "class.ss"))
  (require "../semantics/class-resolver.ss")
  (require "../semantics/standard-resolver.ss")
  (require "../semantics/semantic-object.ss")

  (current-class-resolver (new class-resolver%))

  (define (constructor%? x) (is-a? x constructor%))
  (define (method%? x) (is-a? x method%))
  (define (field%? x) (is-a? x field%))

  (define (print-fields type)
    (for-each
     (lambda (field)
       (printf "    ~a~a ~a;~n"
               (access-flags->string (send field get-modifiers))
               (type-name->string (send field get-type))
               (send field get-name)))
     (filter field%? (send type get-elements))))

  (define (print-constructors class)
    (for-each
     (lambda (constructor)
       (printf "    ~a~a(~a)~a;~n"
               (access-flags->string (send constructor get-modifiers))
               (send constructor get-name)
               (string-join (map type-name->string (send constructor get-formals))
                            ", ")
               (let ([exns (send constructor get-exceptions)])
                 (if (null? exns)
                     ""
                     (string-append " throws " (string-list (map type-name->string exns)))))))
     (filter constructor%? (send class get-elements))))

  (define (print-methods class)
    (for-each
     (lambda (method)
       (printf "    ~a~a ~a(~a)~a;~n"
               (access-flags->string (send method get-modifiers))
               (type-name->string (send method get-return-type))
               (send method get-name)
               (string-join (map type-name->string (send method get-formals)) ", ")
               (let ([exns (send method get-exceptions)])
                 (if (null? exns)
                     ""
                     (string-append " throws " (string-list (map type-name->string exns)))))))
     (filter method%? (send class get-elements))))

  (define (access-flags->string flags)
    (string-join (map symbol->string flags) " " 'suffix))

  (define (string-list alos)
    (string-join alos ", " 'infix))

  (define (interfaces ifaces)
    (string-append "implements "
                   (string-list (map type-name->string ifaces))
                   " "))

  (define (superclass class)
    (string-append "extends "
                   (type-name->string class)
                   " "))

  (define (superinterfaces ifaces)
    (string-append "extends "
                   (string-list (map type-name->string ifaces))
                   " "))

  (define (print-class-header class)
    (printf "~aclass ~a ~a{~n"
            (access-flags->string (send class get-modifiers))
            (type-name->string (send class get-type-name))
            (cond
              [(and (not (send class get-superclass))
                    (null? (send class get-interfaces)))
               ""]
              [(not (send class get-superclass))
               (interfaces (send class get-interfaces))]
              [(null? (send class get-interfaces))
               (superclass (send class get-superclass))]
              [else
               (string-append (superclass (send class get-superclass))
                              (interfaces (send class get-interfaces)))])))

  (define (print-interface-header interface)
    (printf "~ainterface ~a ~a{~n"
            (access-flags->string
             (lset-difference eq?
                              (send interface get-modifiers)
                              '(abstract)))
            (type-name->string (send interface get-type-name))
            (if (null? (send interface get-interfaces))
                ""
                (superinterfaces (send interface get-interfaces)))))

  (define (javap class-name)
    (cond
      [(lookup-type (build-type-name class-name))
       => (lambda (class)
            (cond
              [(is-a? class class%) (print-class-header class)]
              [(is-a? class interface%) (print-interface-header class)]
              [else (printf "??? ~a {~n" (type-name->string (build-type-name class-name)))])
            (print-fields class)
            (print-constructors class)
            (print-methods class)
            (printf "}~n"))]
      [else #f]))

  (provide javap))
