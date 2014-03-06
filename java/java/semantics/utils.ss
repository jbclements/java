(module utils mzscheme

  (require (lib "contract.ss")
           (lib "class.ss")
           (prefix srfi-1: (lib "1.ss" "srfi"))
           "semantic-object.ss")

  (define-syntax define-unimplemented
    (syntax-rules ()
      [(_ name ...)
       (begin
         (define name
           (lambda args
             (error (quote name) "unimplemented")))
         ...)]))

  (define semantic-object/c (is-a?/c semantic-object<%>))

  (provide/contract
   [semantic-object=? (semantic-object/c semantic-object/c . -> . boolean?)])

  (define semantic-object=?
    (lambda (s1 s2)
      (cond
       [(is-a? s1 package%) (package=? s1 s2)]
       [(is-a? s1 array-type%) (array-type=? s1 s2)]
       [(is-a? s1 primitive-type%) (primitive-type=? s1 s2)]
       [(is-a? s1 class%) (class=? s1 s2)]
       [(is-a? s1 interface%) (interface=? s1 s2)]
       [else (error 'semantic-object=?
                    "comparison unimplemented for ~a" s1)])))

  ;; package=? :: package% semantic-object<%> -> Boolean
  (define package=?
    (lambda (p1 p2)
      (and (is-a? p2 package%)
           (string=? (send p1 to-string) (send p2 to-string)))))

  ;; array-type=? :: array-type% semantic-object<%> -> Boolean
  (define array-type=?
    (lambda (a1 a2)
      (and (is-a? a2 array-type%)
           (type-name=? (send a1 get-type-name)
                        (send a2 get-type-name)))))

  ;; primitive-type=? :: primitive-type% semantic-object<%> -> Boolean
  (define primitive-type=?
    (lambda (p1 p2)
      (and (is-a? p2 primitive-type%)
           (type-name=? (send p1 get-type-name)
                        (send p2 get-type-name)))))

  ;; class=? :: class% semantic-object<%> -> Boolean
  (define class=?
    (lambda (c1 c2)
      (and (is-a? c2 class%)
           (declared-type=? c1 c2)
           (optional-type-name=? (send c1 get-superclass)
                                 (send c2 get-superclass)))))

  ;; interface=? :: interface% semantic-object<%> -> Boolean
  (define interface=?
    (lambda (i1 i2)
      (and (is-a? i2 interface%)
           (declared-type=? i1 i2))))

  ;; declared-object=? :: declared-type% declared-type% -> Boolean
  (define declared-type=?
    (lambda (d1 d2)
      (and
       ;; package-name subsumed by type-name
       (type-name=? (send d1 get-type-name)
                    (send d2 get-type-name))
       (same-modifiers? (send d1 get-modifiers)
                        (send d2 get-modifiers))
       (srfi-1:lset= type-name=?
                     (send d1 get-interfaces)
                     (send d2 get-interfaces))
       (srfi-1:lset= type-element=?
                     (send d1 get-elements)
                     (send d2 get-elements)))))

  ;; type-element=? :: type-element% type-element% -> Boolean
  (define type-element=?
    (lambda (t1 t2)
      (cond
       [(is-a? t1 field%) (field=? t1 t2)]
       [(is-a? t1 constructor%) (constructor=? t1 t2)]
       [(is-a? t1 method%) (method=? t1 t2)]
       [(is-a? t1 initializer%) (initializer=? t1 t2)]
       [(is-a? t1 inner-type%) (inner-type=? t1 t2)]
       [else (error 'type-element=?
                    "unsupported element: ~a" t1)])))

  ;; field=? :: field% type-element% -> Boolean
  (define field=?
    (lambda (f1 f2)
      (and (is-a? f2 field%)
           (optional-string=? (send f1 get-name) (send f2 get-name))
           (same-modifiers? (send f1 get-modifiers) (send f2 get-modifiers))
           (type-name=? (send f1 get-type) (send f2 get-type)))))

  ;; constructor=? :: constructor% type-element% -> Boolean
  (define constructor=?
    (lambda (c1 c2)
      (and (is-a? c2 constructor%)
           (behavior=? c1 c2)
           (optional-string=? (send c1 get-name) (send c2 get-name))
           (let ([formals1 (send c1 get-formals)]
                 [formals2 (send c2 get-formals)])
             (and (= (length formals1) (length formals2))
                  (andmap type-name=? formals1 formals2)))
           (srfi-1:lset= type-name=?
                         (send c1 get-exceptions)
                         (send c2 get-exceptions))
           (same-modifiers? (send c1 get-modifiers)
                            (send c2 get-modifiers)))))

  ;; method=? :: method% type-element% -> Boolean
  (define method=?
    (lambda (m1 m2)
      (and (is-a? m2 method%)
           (behavior=? m1 m2)
           (optional-type-name=? (send m1 get-return-type)
                                 (send m2 get-return-type)))))

  ;; behavior=? :: behavior% behavior% -> Boolean
  (define behavior=?
    (lambda (b1 b2)
      (and (optional-string=? (send b1 get-name) (send b2 get-name))
           (let ([formals1 (send b1 get-formals)]
                 [formals2 (send b2 get-formals)])
             (and (= (length formals1) (length formals2))
                  (andmap type-name=? formals1 formals2)))
           (srfi-1:lset= type-name=?
                         (send b1 get-exceptions)
                         (send b2 get-exceptions))
           (same-modifiers? (send b1 get-modifiers)
                            (send b2 get-modifiers)))))

  (define initializer=?
    (lambda (i1 i2)
      (error 'initializer=?
             "unimplemented pending clarification of semantics")))

  (define inner-type=?
    (lambda (t1 t2)
      (error 'inner-type=?
             "unimplemented pending clarification of semantics")))

  ;; lift-to-optional :: (@a @a -> Boolean)
  ;;                  -> (Optional @a) (Optional @a) -> Boolean
  ;; converts an equality predicate for Foo into an equality predicate for
  ;; (Optional Foo), such that #f is only equal to #f.
  (define lift-to-optional
    (lambda (obj=?)
      (lambda (obj1 obj2)
        (cond
         [(and obj1 obj2) (obj=? obj1 obj2)]
         [else (and (not obj1) (not obj2))]))))

  ;; same-modifiers? :: (Listof access-flag) (Listof access-flag) -> Boolean
  ;; compares two lists of modifiers for set equality
  (define same-modifiers?
    (lambda (mods1 mods2)
      (srfi-1:lset= eq? mods1 mods2)))

  ;; type-name=? :: type-name type-name -> Boolean
  (define type-name=?
    (lambda (t1 t2)
      (and (equal? (type-name-package t1) (type-name-package t2))
           (eq? (type-name-type t1) (type-name-type t2))
           (= (type-name-dimension t1) (type-name-dimension t2)))))

  ;; optional-type-name=? :: (Optional type-name) (Optional type-name)
  ;;                      -> Boolean
  (define optional-type-name=? (lift-to-optional type-name=?))

  (define optional-string=? (lift-to-optional string=?)))
