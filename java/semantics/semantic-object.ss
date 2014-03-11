(module semantic-object mzscheme
  (require "../contract-utils.ss")
  (require "../inspector/inspector.ss")
  (require (lib "struct.ss" "dherman-struct"))
  (require (all-except (lib "list.ss" "srfi" "1") any))
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "class.ss"))
  (require (all-except (lib "contract.ss") union))
  (require (lib "struct.ss"))

  ;; TODO: (how) can this eventually work with generics?

  ;; TODO: put this utility in a separate planet library
  (define-syntax syntax-for-each
    (syntax-rules ()
      [(_ transformer (arg ...))
       (begin
         (define-syntax anonymous transformer)
         (anonymous arg)
         ...)]))

  (define-syntax send-map
    (syntax-rules ()
      [(_ message exp)
       (map (lambda (elt) (send elt message)) exp)]))

  (define (union . lists)
    (apply lset-union equal? lists))

  (define (union* lists)
    (apply lset-union equal? lists))

  ;; ===========================================================================
  ;; DATA DEFINITIONS
  ;; ===========================================================================

  (with-public-inspector
    (define-struct/opt type-name (package type [dimension 0]))
    ;; TODO: provide with contracts
    (provide (struct type-name (package type dimension))))
;    (provide/contract (struct type-name ([package (listof symbol?)]
;                                         [type symbol?]))))

  ;; build-type-name : (listof symbol) -> type-name
  (define (build-type-name name)
    (let ([rev (reverse name)])
      (make-type-name (reverse (cdr rev))
                      (car rev))))

  ;; dot-notation : (listof symbol) -> string
  (define (dot-notation los)
    (string-join (map symbol->string los) "." 'infix))

  ;; type-name->string : (optional type-name) -> string
  (define (type-name->string name)
    (if (not name)
        "void"
        (dot-notation (append (type-name-package name)
                              (list (type-name-type name))))))

  (provide/contract
   [build-type-name ((listof symbol?) . -> . type-name?)]
   [dot-notation ((listof symbol?) . -> . string?)]
   [type-name->string ((optional/c type-name?) . -> . string?)])

  ;; ===========================================================================
  ;; INTERFACES
  ;; ===========================================================================

  (define semantic-object<%>
    (interface ()
      ;; to-string : -> string
      to-string))

  (define resolvable<%>
    (interface ()
      ;; get-related-types : -> (listof type-name)
      get-related-types))

  (define type<%>
    (interface (semantic-object<%> resolvable<%>)
      ;; get-type-name : -> type-name
      get-type-name))

  ;; ===========================================================================
  ;; CLASSES
  ;; ===========================================================================

  ;; package-name : (listof symbol)
  (define package%
    (class* object% (semantic-object<%>)
      (init-field name)
      (define/public (to-string)
        (dot-notation name))
      (super-new)))

  ;; base-type : type-name
  (define array-type%
    (class* object% (type<%>)
      (init-field base-type)
      (define name
        (copy-struct type-name base-type
                     (type-name-dimension (add1 (type-name-dimension base-type)))))
      (define/public (get-type-name) name)
      (define/public (get-base-type) base-type)
      (define/public (get-dimension)
        (type-name-dimension name))
      (define/public (get-related-types)
        (list base-type))
      (define/public (to-string)
        (format "~a[]" (type-name->string base-type)))
      (super-new)))

  ;; package : (listof symbol)
  ;; name : type-name
  (define ground-type%
    (class* object% (type<%>)
      (init-field package name)
      (define/public (get-package) package)
      (define/public (get-type-name) name)
      (define/pubment (get-related-types) (inner null get-related-types))
      (define/public (to-string) (format "~a" name))
      (super-new)))

  (define primitive-type%
    (class ground-type%
      (init name)
      (super-make-object null name)))

  ;; package : (listof symbol)
  ;; name : type-name
  ;; modifiers : (listof access-flag)
  ;; interfaces : (listof type-name)
  ;; elements : (listof type-element%)
  (define declared-type%
    (class ground-type%
      (init package name)
      (init-field modifiers interfaces elements)
      (define/public (get-modifiers) modifiers)
      (define/public (get-interfaces) interfaces)
      (define/public (get-elements) elements)
      (define/augment (get-related-types)
        (union* (cons interfaces
                      (cons (inner null get-related-types)
                            (send-map get-related-types elements)))))
      (super-make-object package name)))

  ;; superclass : (optional type-name)
  (define class%
    (class declared-type%
      (init package name modifiers interfaces elements)
      (init-field superclass)
      (define/public (get-superclass) superclass)
      (define/augment (get-related-types)
        (union (if superclass (list superclass) null)
               (inner null get-related-types)))
      (super-make-object package name modifiers interfaces elements)))

  (define interface%
    (class declared-type%
      (init package name modifiers interfaces elements)
      (super-make-object package name modifiers interfaces elements)))

  ;; TODO: do I really want strings, and not symbols? this is Scheme, after all

  ;; name : string
  (define type-element%
    (class* object% (semantic-object<%> resolvable<%>)
      (init-field name)
      (define/public (get-name) name)
      (define/pubment (get-related-types) (inner null get-related-types))
      (define/public (to-string)
        (format "~a" name))
      (super-new)))

  ;; name : string
  ;; modifiers : (listof access-flag)
  ;; type : type-name
  (define field%
    (class type-element%
      (init name)
      (init-field modifiers type)
      (inherit get-name)
      (define/public (get-modifiers) modifiers)
      (define/public (get-type) type)
      (define/augment (get-related-types)
        (union (list type) (inner null get-related-types)))
      (define/override (to-string)
        (format "~a ~a" (type-name->string type) (get-name)))
      (super-make-object name)))

  (define initializer%
    (class type-element%
      (super-make-object #f)))

  ;; name : string
  ;; formals : (listof type-name)
  ;; exceptions : (listof type-name)
  ;; modifiers : (listof access-flag)
  (define behavior%
    (class type-element%
      (init name)
      (init-field formals exceptions modifiers)
      (inherit get-name)
      (define/public (get-formals) formals)
      (define/public (get-exceptions) exceptions)
      (define/public (get-modifiers) modifiers)
      (define/augment (get-related-types)
        (union formals exceptions (inner null get-related-types)))
      (define (to-string) (get-name))
      (super-make-object name)))

  ;; name : string
  ;; formals : (listof type-name)
  ;; exceptions : (listof type-name)
  ;; modifiers : (listof access-flag)
  (define constructor%
    (class behavior%
      (override to-string)
      (inherit get-name get-formals get-exceptions)
      (init name formals exceptions modifiers)
      (define (to-string)
        (format "~a(~a) throws ~a"
                (get-name)
                (string-join (map type-name->string (get-formals)) ", ")
                (string-join (map type-name->string (get-exceptions)) ", ")))
      (super-make-object name formals exceptions modifiers)))

  ;; name : string
  ;; formals : (listof type-name)
  ;; exceptions : (listof type-name)
  ;; modifiers : (listof access-flag)
  ;; return-type : (optional type-name)
  (define method%
    (class behavior%
      (init name formals exceptions modifiers)
      (init-field return-type)
      (inherit get-name get-formals get-exceptions)
      (define/public (get-return-type) return-type)
      (define/augment (get-related-types)
        (union (if return-type (list return-type) null)
               (inner null get-related-types)))
      (define/override (to-string)
        (format "~a ~a(~a) throws ~a"
                (type-name->string (get-return-type))
                (get-name)
                (string-join (map type-name->string (get-formals)) ", ")
                (string-join (map type-name->string (get-exceptions)) ", ")))
      (super-make-object name formals exceptions modifiers)))

  ;; TODO: make this a type<%>?

  ;; name : string
  ;; type : type-name
  (define inner-type%
    (class type-element%
      (init name)
      (init-field type)
      (define/public (get-type) type)
      (define/augment (get-related-types)
        (union (list type) (inner null get-related-types)))
      (super-make-object name)))

  (syntax-for-each (syntax-rules ()
                     [(_ prim)
                      (begin
                        (define prim (make-object primitive-type% (build-type-name '(prim))))
                        (provide/contract (prim (is-a?/c primitive-type%))))])
                   (byte char double float int long short boolean))

  (provide semantic-object<%> type<%> resolvable<%>
           package%
           ground-type% primitive-type% declared-type% array-type%
           class% interface%
           type-element% field% initializer% behavior% constructor% method%
           inner-type%))
