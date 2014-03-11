(module class-resolver mzscheme
  (require "../contract-utils.ss")
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "class.ss"))
  (require (lib "contract.ss"))
  (require "semantic-object.ss")

  ;; TODO: raise an exception when resolution fails?

  ;; resolve-all : (union type-name semantic-object<%>) -> void
  (define (resolve-all x)
    (cond
      [(is-a? x resolvable<%>)
       (for-each resolve-all (send x get-related-types))]
      [(and (type-name? x) (lookup-type x))
       => (lambda (type)
            (for-each resolve-all (send type get-related-types)))]
      [else (void)]))

  (define class-resolver<%>
    (interface ()
      ;; resolve-package : (listof symbol) -> (optional package%)
      resolve-package
      ;; resolve-type : type-name -> (optional type<%>)
      resolve-type))

  (define current-class-resolver
    (make-parameter #f (lambda (new-resolver)
                         (unless (is-a? new-resolver class-resolver<%>)
                           (raise-type-error 'current-class-resolver
                                             "class-resolver<%>"
                                             new-resolver))
                         new-resolver)))

  ;; lookup-package : (listof symbol) -> (optional package%)
  (define (lookup-package name)
    (send (current-class-resolver) resolve-package name))

  ;; lookup-type : type-name -> type<%>
  (define (lookup-type name)
    (send (current-class-resolver) resolve-type name))

  (provide/contract
   [class-resolver<%> interface?]
   [lookup-package ((listof symbol?) . -> . (optional/c (is-a?/c package%)))]
   [lookup-type (type-name? . -> . (optional/c (is-a?/c type<%>)))]
   [resolve-all ((union type-name? (is-a?/c semantic-object<%>)) . -> . any)])

  ;; TODO: implement parameterof contract
  (provide current-class-resolver))
