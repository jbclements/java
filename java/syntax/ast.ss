(module ast mzscheme
  (require "../inspector/inspector.ss")
  (require (lib "hierarchy.ss" "dherman-struct"))
  (require "../contract-utils.ss")
  (require (lib "contract.ss"))
  (require (lib "match.ss"))
  (require (lib "list.ss"))

  ;; TODO: go through and figure out where I want extra src structs

  ;; ===========================================================================
  ;; BASIC PREDICATES AND CONTRACTS
  ;; ===========================================================================

  ;; primitive-type? : any -> boolean
  ;; true if v is a symbol representing a Java primitive type
  (define (primitive-type? v)
    (case v
      [(void boolean byte short int long char float double) #t]
      [else #f]))
  (provide/contract (primitive-type? (any/c . -> . boolean?)))

  ;; class-element? : any -> boolean
  ;; true if v can be an element in a class/interface declaration
  (define (class-element? v)
    (or (decl? v)
        (and (pair? v) (andmap variable-decl? v))
        (initializer? v)
        (not v)))
  (provide/contract (class-element? (any/c . -> . boolean?)))

  ;; block-element? : any -> boolean
  ;; true if v can be an element in a block-stmt
  (define (block-element? v)
    (or (decl? v)
        (and (pair? v) (andmap variable-decl? v))
        (stmt? v)
        (not v)))
  (provide/contract (block-element? (any/c . -> . boolean?)))

  ;; type-name? : any -> boolean
  ;; true if c is a primitive type name or a class name
  (define (type-name? c)
    (or (primitive-type? c)
        (name? c)))
  (provide/contract (type-name? (any/c . -> . boolean?)))

  ;; ===========================================================================
  ;; SOURCE LOCATION INFORMATION
  ;; ===========================================================================

  (with-public-inspector
   (define-struct src (file line col span pos)))
  (provide/contract (struct src ((file (optional/c path?))
                                 (line (optional/c natural-number/c))
                                 (col (optional/c natural-number/c))
                                 (span (optional/c natural-number/c))
                                 (pos (optional/c natural-number/c)))))

  ;; ===========================================================================
  ;; AST DATA DEFINITION
  ;; ===========================================================================

  (with-public-inspector
   ;(without-hierarchy-contracts
    (define-hierarchy/provide/contract
      (ast ((src (optional/c src?)))
        ;; Common
        (id ((name symbol?)))
        (name ((path (listof id?))
               (id id?)))
        (import ((name name?)
                 (star? boolean?)))
        (type-spec ((base-type type-name?)
                    (dimension natural-number/c)))
        (modifier ((modifier symbol?)))
        (initializer ((static? boolean?)
                      (body block-stmt?)))

        ;; Top-level
        (compilation-unit ((package (optional/c name?))
                           (imports (listof import?))
                           (classes (listof (optional/c type-decl?)))))

        ;; Declarations
        (decl ((modifiers (listof modifier?))
               (name id?))
          (type-decl ((interfaces (listof name?))
                      (body (listof class-element?)))
            (class-decl ((super (optional/c name?))))
            (interface-decl ()))
          (variable-decl ((type type-spec?)
                          (init (optional/c expr?))))
          (behavior-decl ((formals (listof variable-decl?))
                          (throws (listof name?))
                          (body block-stmt?))
            (constructor-decl ())
            (method-decl ((return-type type-spec?)))))

        ;; Statements
        (stmt ()
          (expr-stmt ((expr expr?)))
          (labeled-stmt ((label id?)
                         (stmt (optional/c stmt?))))
          (block-stmt ((body (listof block-element?))))
          (switch-stmt ((expr expr?)
                        (clauses (listof (union case-stmt? block-element?)))))
          (case-stmt ((test (optional/c expr?))))
          (if-stmt ((test expr?)
                    (con (optional/c stmt?))
                    (alt (optional/c stmt?))))
          (for-stmt ((init (union (listof variable-decl?) (listof expr?)))
                     (test (optional/c expr?))
                     (update (listof expr?))
                     (body (optional/c stmt?))))
          (while-stmt ((test expr?)
                       (body (optional/c stmt?))))
          (do-stmt ((body stmt?)
                    (test expr?)))
          (break-stmt ((label (optional/c id?))))
          (continue-stmt ((label (optional/c id?))))
          (return-stmt ((value (optional/c expr?))))
          (throw-stmt ((expr expr?)))
          (synchronized-stmt ((expr expr?)
                              (body stmt?)))
          (try-stmt ((body block-stmt?)
                     (catches (listof catch-stmt?))
                     (finally (optional/c block-stmt?))))
          (catch-stmt ((exception variable-decl?)
                       (body block-stmt?)))
          (assert-stmt ((predicate expr?)
                        (message expr?))))

        ;; Expressions
        (expr ()
          (conditional-expr ((test expr?)
                             (con expr?)
                             (alt expr?)))
          (prefix-expr ((op-src src?)
                        (operator symbol?)
                        (operand expr?)))
          (postfix-expr ((op-src src?)
                         (operator symbol?)
                         (operand expr?)))
          (unary-expr ((op-src src?)
                       (operator symbol?)
                       (operand expr?)))
          (binary-expr ((op-src src?)
                        (operator symbol?)
                        (left expr?)
                        (right expr?)))
          (instanceof-expr ((op-src src?)
                            (expr expr?)
                            (type type-spec?)))
          (literal ((value any/c))
            (boolean-literal ())
            (char-literal ())
            (integer-literal ())
            (long-literal ())
            (float-literal ())
            (double-literal ())
            (string-literal ())
            (null-literal ()))
          (class-expr ((type type-spec?)))
;          (creation ()
;            (object-creation ((container (optional/c expr?))
;                              (name name?)
;                              (args (listof expr?))
;                              (class-body (optional/c (listof class-element?)))))
;            (array-creation ((type type-spec?)
;                             (dim-exprs (listof expr?))
;                             (dim natural-number/c)
;                             (init (optional/c array-initializer?)))))
          (new-object-expr ((container (optional/c expr?))
                            (name name?)
                            (args (listof expr?))
                            (class-body (optional/c (listof class-element?)))))
          (new-array-expr ((type type-spec?)
                           (dim-exprs (listof expr?))
                           (dim natural-number/c)
                           (init (optional/c array-initializer?))))
          (array-initializer ((contents (listof expr?))))
          (call-expr ((object (optional/c expr?))
                      (name name?)
                      (args (listof expr?))))
          (assign-expr ((operator symbol?)
                        (left access?)
                        (right expr?)))
          (cast-expr ((type type-spec?)
                      (expr expr?)))
          (access ()
            (field-access ((object expr?)
                           (name id?)))
            (array-access ((array expr?)
                           (index expr?)))
            (var-access ((var name?)))))
        ))
    ;)
   )

  ;; ===========================================================================
  ;; AST UTILITIES
  ;; ===========================================================================

  ;; name->access : name -> access
  (define (name->access n)
    (make-var-access (ast-src n) n))
  (provide/contract (name->access (name? . -> . access?)))

  ;; access->name : (union field-access var-access) -> name
  (define (access->name a)
    (let ([path (let loop ([a a] [accum null])
                  (match a
                    [($ var-access _ ($ name _ path id))
                     (append path (cons id accum))]
                    [($ field-access _ object name)
                     (loop object (cons name accum))]))])
      (make-name (ast-src a) (reverse (cdr path)) (car path))))
  (provide/contract (access->name ((union field-access? var-access?) . -> . name?)))

  ;; increase-type-dimension : type-spec natural-number -> type-spec
  (define (increase-type-dimension type n)
    (make-type-spec (ast-src type)
                    (type-spec-base-type type)
                    (+ (type-spec-dimension type) n)))
  (provide/contract (increase-type-dimension (type-spec? natural-number/c . -> . type-spec?)))

  )
