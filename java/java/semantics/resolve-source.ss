(module resolve-source mzscheme

  (require (lib "etc.ss")
           (lib "class.ss"))
  (require (prefix ast: "../syntax/ast.ss"))
  (require "../syntax/parser.ss")
  (require "semantic-object.ss")

  ;; resolve-source :: semantic-type-name path -> (Optional declared-type%)
  ;; loads definition of requested type from file at path
  ;; returns #f if not found.
  (define resolve-source
    (lambda (type-name path)
      (find-type type-name (parse-file path))))

  ;; find-type :: semantic-type-name compilation-unit
  ;;           -> (Optional declared-type%)
  (define find-type
    (lambda (type-name compilation-unit)
      ;; should probably check package names here
      ;; slightly tricky, since if class-name is java.lang.Object,
      ;; compilation unit's package will have (name '(java) 'lang).
      (recur loop ([types (ast:compilation-unit-classes compilation-unit)])
        (cond
         [(null? types) #f]
         [(not (car types)) (loop (cdr types))]
         [(eq? (type-name-type type-name)
               (ast:id-name (ast:decl-name (car types))))
          (build-type type-name (car types))]
         [else (loop (cdr types))]))))

  ;; build-type :: semantic-type-name type-decl -> declared-type%
  (define build-type
    (lambda (name decl)
      (cond
       [(ast:class-decl? decl) (build-class name decl)]
       [(ast:interface-decl? decl) (build-interface name decl)]
       [else (error 'build-type "unsupported declaration: ~v" decl)])))

  ;; build-class :: semantic-type-name class-decl -> declared-type%
  (define build-class
    (lambda (class-name class-decl)
      (new class%
           [package null]  ;; FIXME: is this safe?
           [name class-name]
           [modifiers (extract-modifiers class-decl)]
           [interfaces (extract-interfaces class-decl)]
           [elements (extract-elements class-decl)]
           [superclass
            (let ([super-name (ast:class-decl-super class-decl)])
              (if super-name
                  (ast-name->type-name super-name)
                  #f))])))

  ;; build-interface :: semantic-type-name interface-decl -> declared-type%
  (define build-interface
    (lambda (ifc-name ifc-decl)
      (new interface%
           [package null] ;; FIXME: as with build-class, is this safe?
           [name ifc-name]
           [modifiers (extract-modifiers ifc-decl)]
           [interfaces (extract-interfaces ifc-decl)]
           [elements (extract-elements ifc-decl)])))

  ;; extract-modifiers :: ast:type-decl -> (Listof access-flag)
  ;; extracts modifiers and converts them to semantics access flags.  So far as
  ;; I know, all modifier symbols are also access flags, but watch out for
  ;; problems here.
  (define extract-modifiers
    (lambda (type-decl)
      (map ast:modifier-modifier (ast:decl-modifiers type-decl))))

  ;; extract-interfaces :: ast:type-decl -> (Listof type-name) extracts
  ;; interface names from an AST type declaration and converts them into type
  ;; names for the semantic object representation.
  (define extract-interfaces
    (lambda (type-decl)
      (map ast-name->type-name (ast:type-decl-interfaces type-decl))))

  ;; extract-elements :: ast:type-decl -> (Listof type-element%)
  ;; extracts the body from the type declaration and converts it to the
  ;; semantic-object representation.
  (define extract-elements
    (lambda (type-decl)
      (map class-element->type-element
           (flatten-elements (ast:type-decl-body type-decl)))))

  ;; flatten-elements :: (Listof class-element)
  ;;                  -> (Listof (Union decl initializer))
  ;; flattens list structure in list of elements and discards empty elements
  (define flatten-elements
    (lambda (elts)
      (cond
       [(null? elts) null]
       [(not (car elts)) (flatten-elements (cdr elts))]
       [(pair? (car elts)) (append (car elts) (flatten-elements (cdr elts)))]
       [else (cons (car elts) (flatten-elements (cdr elts)))])))

  ;; class-element->type-element :: (Union decl initializer) -> type-element%
  (define class-element->type-element
    (lambda (elt)
      (cond
       [(ast:initializer? elt) (new initializer%)]
       [(ast:type-decl? elt)
        (error 'class-element->type-element
               "inner classes and interfaces not supported")]
       [(ast:variable-decl? elt)
        (new field%
             [name (ast:id->string (ast:decl-name elt))]
             [modifiers (map ast:modifier-modifier (ast:decl-modifiers elt))]
             [type (ast:type-spec->type-name (ast:variable-decl-type elt))])]
       [(ast:constructor-decl? elt)
        (new constructor%
             [name (ast:id->string (ast:decl-name elt))]
             [formals (map ast:variable-decl->type-name
                           (ast:behavior-decl-formals elt))]
             [exceptions (map ast-name->type-name
                              (ast:behavior-decl-throws elt))]
             [modifiers (map ast:modifier-modifier (ast:decl-modifiers elt))])]
       [(ast:method-decl? elt)
        (new method%
             [name (ast:id->string (ast:decl-name elt))]
             [formals (map ast:variable-decl->type-name
                           (ast:behavior-decl-formals elt))]
             [exceptions (map ast-name->type-name
                              (ast:behavior-decl-throws elt))]
             [modifiers (map ast:modifier-modifier (ast:decl-modifiers elt))]
             [return-type (ast:type-spec->type-name
                           (ast:method-decl-return-type elt))])])))

  ;; ast-name->type-name :: ast:type-name -> type-name
  ;; Converts from the AST representation of type names to the semantic-object
  ;; representation of type names.
  (define ast-name->type-name
    (lambda (ast-name)
      (make-type-name (ast:name-path ast-name)
                      (ast:name-id ast-name))))

  (define ast:id->string
    (lambda (id)
      (symbol->string (ast:id-name id))))

  ;; ast:type-spec->type-name :: ast:type-spec -> (Optional type-name)
  (define ast:type-spec->type-name
    (lambda (type-spec)
      (let ([type-name (ast:type-spec-base-type type-spec)]  ;; type-name
            [dimension (ast:type-spec-dimension type-spec)]) ;; N
        (cond
         [(ast:primitive-type? type-name)
          (if (eq? type-name 'void)
              #f
              (make-type-name null type-name dimension))]
         [else
          (make-type-name (map ast:id-name (ast:name-path type-name))
                          (ast:id-name (ast:name-id type-name))
                          dimension)]))))

  ;; ast:variable-decl->type-name :: ast:variable-decl -> type-name
  ;; extracts type from a variable decl and converts it to semantic-object
  ;; representation
  (define ast:variable-decl->type-name
    (lambda (decl)
      (ast:type-spec->type-name (ast:variable-decl-type decl)))))
