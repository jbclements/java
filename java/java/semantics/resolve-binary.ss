(module resolve-binary mzscheme
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "list.ss" "srfi" "1"))
  (require (all-except (lib "class.ss") class-info))
  (require (lib "match.ss"))
  (require (lib "string.ss"))
  (require (lib "etc.ss"))
  (require (lib "struct.ss"))
  (require "semantic-object.ss")
  (require "class-resolver.ss")
  (require "../syntax/class-file.ss")

  ;; TODO: change all errors to be some specific exn type

  ;; parse-type-path : string -> (listof symbol)
  (define (parse-type-path path)
    (map string->symbol (regexp-split "/" path)))

  (define type-modifiers
    '(public private protected static final abstract strictfp))

;  (define (pretty-type t)
;    (cond
;      [(type-name? t) (type-name->string t)]
;      [(is-a? t semantic-object<%>) (send t to-string)]
;      [(not t) "void"]
;      [else (format "~a" t)]))
;
;  (define (pretty-formals fmls)
;    (string-append
;     "(" (string-join (map pretty-type fmls) ", ") ")"))

  ;; resolve-binary : class-file -> declared-type%
  (define (resolve-binary cf)
    ;; deref : nat -> pool-entry
    (define deref
      (let ([pool (class-file-pool cf)])
        (lambda (i)
          (vector-ref pool (sub1 i)))))

    ;; find-class-name : class-info -> string
    (define (find-class-name cinfo)
      (utf8-info->string (deref (class-info-name-index cinfo))))

    ;; super : -> (optional type-name)
    (define (super)
      (let ([index (class-file-super cf)])
        (if (zero? index)
            #f
            (build-type-name
             (parse-type-path
              (find-class-name (deref index)))))))

    ;; this : -> (listof symbol)
    (define (this)
      (parse-type-path
       (find-class-name
        (deref (class-file-this cf)))))

    ;; analyze-field : field-info -> field%
    (define (analyze-field field)
      (match field
        [($ field-info flag-bits name-index descriptor-index attributes-count attributes)
         (let ([flags (extract-access-flags flag-bits)]
               [name (utf8-info->string (deref name-index))]
               [type-name (parse-field-descriptor
                           (open-input-string
                            (utf8-info->string (deref descriptor-index))))])
           (make-object field% name flags type-name))]))

    ;; analyze-method : method-info -> method%
    (define (analyze-method method)
      (match method
        [($ method-info flag-bits name-index descriptor-index attributes-count attributes)
         (let* ([flags (extract-access-flags flag-bits 'synchronized)]
                [name (utf8-info->string (deref name-index))]
                [method-desc (parse-method-descriptor
                              (open-input-string
                               (utf8-info->string (deref descriptor-index))))]
                [formals (method-descriptor-formals method-desc)]
                [exceptions (cond
                              [(find exceptions-attribute-info? attributes)
                               => (lambda (info)
                                    (map (compose build-type-name parse-type-path find-class-name deref)
                                         (exceptions-attribute-info-exceptions info)))]
                              [else null])]
                [return (method-descriptor-return method-desc)])
           (cond
             [(string=? name "<init>")
              (make-object constructor%
                (symbol->string (last (this)))
                formals
                exceptions
                flags)]
             [(string=? name "<clinit>")
              (make-object initializer%)]
             [else
              (make-object method%
                name
                formals
                exceptions
                flags
                (method-descriptor-return method-desc))]))]))

    (let* ([flags (extract-access-flags (class-file-flags cf) 'super)]
           [modifiers (lset-intersection eq? flags type-modifiers)])
      (match cf
        [($ class-file _ _ _ _ iface-infos fields methods attributes)
         (match (build-type-name (this))
           [(and tname ($ type-name package name 0))
            (let ([interfaces (map (compose build-type-name parse-type-path find-class-name)
                                   iface-infos)]
                  [members (append (map analyze-field fields)
                                   (map analyze-method methods))])
              (if (memq 'interface flags)
                  (make-object interface%
                    ;; TODO: package field is deprecated
                    package
                    tname
                    modifiers interfaces members)
                  (make-object class%
                    ;; TODO: package field is deprecated
                    package
                    tname
                    modifiers interfaces members
                    (super))))])])))

  ;; ===========================================================================
  ;; INTERNAL TYPE DESCRIPTOR PARSER
  ;; ===========================================================================

  ;; method-descriptor : (listof type-name) * (optional type-name)
  (define-struct method-descriptor (formals return))

  ;; parse-method-descriptor : input-port -> (cons (listof lazy-type) lazy-type)
  (define (parse-method-descriptor in)
    (let ([c (read-char in)])
      (if (char=? c #\()
          (let loop ([rev-formals null])
            (if (char=? (peek-char in) #\))
                (begin (read-char in)
                       (make-method-descriptor (reverse rev-formals)
                                               (parse-return-type in)))
                (loop (cons (parse-field-type in) rev-formals))))
          (error 'parse-method-descriptor "bad method descriptor: ~v" c))))

  ;; parse-return-type : input-port -> (optional type-name)
  (define (parse-return-type in)
    (if (char=? (peek-char in) #\V)
        (begin (read-char in) #f)
        (parse-field-type in)))

  ;; parse-field-descriptor : input-port -> type-name
  (define (parse-field-descriptor in)
    (parse-field-type in))

  (define-values (byte-name char-name double-name float-name int-name long-name short-name boolean-name)
    (apply values (map (lambda (t) (send t get-type-name))
                       (list byte char double float int long short boolean))))

  ;; parse-field-type : input-port -> type-name
  (define (parse-field-type in)
    (let ([c (read-char in)])
      (case c
        [(#\B) byte-name]
        [(#\C) char-name]
        [(#\D) double-name]
        [(#\F) float-name]
        [(#\I) int-name]
        [(#\J) long-name]
        [(#\S) short-name]
        [(#\Z) boolean-name]
        [(#\[) (let ([base-type (parse-field-descriptor in)])
                 (copy-struct type-name base-type
                              (type-name-dimension (add1 (type-name-dimension base-type)))))]
        [(#\L) (parse-internal-type-name in)]
        [else  (error 'parse-field-type "bad field descriptor: ~v" c)])))

  ;; parse-internal-type-name : input-port -> type-name
  (define (parse-internal-type-name in)
    (define (parse-rev-elt rev-elt)
      (string->symbol (list->string (reverse rev-elt))))
    (define (return rev-elt rev-path)
      (build-type-name (reverse (cons (parse-rev-elt rev-elt) rev-path))))
    (let loop ([rev-elt null]
               [rev-path null])
      (let ([c (read-char in)])
        (if (eof-object? c)
            (return rev-elt rev-path)
            (case c
              [(#\;) (return rev-elt rev-path)]
              [(#\/) (loop null (cons (parse-rev-elt rev-elt) rev-path))]
              [else  (loop (cons c rev-elt) rev-path)])))))

  (provide resolve-binary))
