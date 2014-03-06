(module class-file mzscheme
  (require (lib "hierarchy.ss" "dherman-struct"))
  (require "../inspector/inspector.ss")
  (require (lib "main.ss" "io"))
  (require (lib "contract.ss"))
  (require (lib "match.ss"))
  (require (all-except (lib "list.ss" "srfi" "1") any))
  (require (lib "etc.ss"))

  (with-public-inspector
   (define-hierarchy/provide/contract
    (info ()
      (class-info ((name-index natural-number/c)))
      (ref-info ((class-index natural-number/c)
                 (name-and-type-index natural-number/c))
        (field-ref-info ())
        (method-ref-info ())
        (interface-method-ref-info ()))
      (string-info ((string-index natural-number/c)))
      (integer-info ((value integer?)))
      (float-info ((bytes bytes?)))
      (long-info ((high-bytes bytes?)
                  (low-bytes bytes?)))
      (double-info ((high-bytes bytes?)
                    (low-bytes bytes?)))
      (name-and-type-info ((name-index natural-number/c)
                           (descriptor-index natural-number/c)))
      (utf8-info ((length natural-number/c)
                  (bytes bytes?)))
      (inner-class-entry ((inner-class-info-index natural-number/c)
                          (outer-class-info-index natural-number/c)
                          (inner-name-index natural-number/c)
                          (inner-class-access-flags integer?))) ; TODO: is that right?
      (element-info ((access-flags integer?) ; TODO: is that right?
                     (name-index natural-number/c)
                     (descriptor-index natural-number/c)
                     (attributes-count natural-number/c)
                     (attributes (listof attribute-info?))) ; TODO: length == count
        (field-info ())
        (method-info ()))
      (attribute-info ()
        (unsupported-attribute-info ((length natural-number/c)
                                     (bytes bytes?)))
        (constant-value-attribute-info ((value-index natural-number/c)))
        (code-attribute-info ()) ; TODO: implement this
        (exceptions-attribute-info ((count natural-number/c)
                                    (exceptions (listof natural-number/c))))
        (inner-classes-attribute-info ())
        (synthetic-attribute-info ())
        (source-file-attribute-info ())
        (line-number-table-attribute-info ())
        (local-variable-table-attribute-info ())
        (deprecated-attribute-info ()))))

   (define-struct class-file (pool flags this super interfaces fields methods attributes))

   (provide (struct class-file (pool flags this super interfaces fields methods attributes))))

  (define (read-constant in)
    (let ([type (read-byte in)])
      (cond
        [(and (<= 1 type *max-constant-type*)
              (vector-ref *read-constant-vector* type))
         => (lambda (reader)
              (reader in))]
        [else
         (error 'read-constant "bad constant type: ~a" type)])))

  (define (read-class-info in)
    (make-class-info (read-integer 2 #f in #t)))

  (define (read-field-ref-info in)
    (let ([class-index         (read-integer 2 #f in #t)]
          [name-and-type-index (read-integer 2 #f in #t)])
      (make-field-ref-info class-index name-and-type-index)))

  (define (read-method-ref-info in)
    (let ([class-index         (read-integer 2 #f in #t)]
          [name-and-type-index (read-integer 2 #f in #t)])
      (make-method-ref-info class-index name-and-type-index)))

  (define (read-interface-method-ref-info in)
    (let ([class-index         (read-integer 2 #f in #t)]
          [name-and-type-index (read-integer 2 #f in #t)])
      (make-interface-method-ref-info class-index name-and-type-index)))

  (define (read-string-info in)
    (make-string-info (read-integer 2 #f in #t)))

  (define (read-integer-info in)
    (make-integer-info (read-integer 4 #f in #t)))

  (define (read-float-info in)
    (make-float-info (read-bytes 4 in)))

  (define (read-long-info in)
    (let ([high-bytes (read-bytes 4 in)]
          [low-bytes  (read-bytes 4 in)])
      (make-long-info high-bytes low-bytes)))

  (define (read-double-info in)
    (let ([high-bytes (read-bytes 4 in)]
          [low-bytes  (read-bytes 4 in)])
      (make-double-info high-bytes low-bytes)))

  (define (read-name-and-type-info in)
    (let ([name-index       (read-integer 2 #f in #t)]
          [descriptor-index (read-integer 2 #f in #t)])
      (make-name-and-type-info name-index descriptor-index)))

  (define (read-utf8-info in)
    (let* ([len   (read-integer 2 #f in #t)]
           [bytes (read-bytes len in)])
      (make-utf8-info len bytes)))

  (define (constant-entry-count constant)
    (if (or (long-info? constant) (double-info? constant)) 2 1))

  (define (read-constant-pool count in)
    (let ([pool (make-vector count #f)])
      (let loop ([i 0])
        (when (< i count)
          (let ([next-constant (read-constant in)])
            (vector-set! pool i next-constant)
            (loop (+ i (constant-entry-count next-constant))))))
      pool))

  (define (read-field-info pool)
    (lambda (in)
      (let* ([access-flags     (read-integer 2 #f in #t)]
             [name-index       (read-integer 2 #f in #t)]
             [descriptor-index (read-integer 2 #f in #t)]
             [attributes-count (read-integer 2 #f in #t)]
             [attributes       (build-list attributes-count
                                           (lambda (i) ((read-attribute-info pool) in)))])
        (make-field-info access-flags name-index descriptor-index attributes-count attributes))))

  (define (read-method-info pool)
    (lambda (in)
      (let* ([access-flags     (read-integer 2 #f in #t)]
             [name-index       (read-integer 2 #f in #t)]
             [descriptor-index (read-integer 2 #f in #t)]
             [attributes-count (read-integer 2 #f in #t)]
             [attributes       (build-list attributes-count
                                           (lambda (i) ((read-attribute-info pool) in)))])
        (make-method-info access-flags name-index descriptor-index attributes-count attributes))))

  ;; TODO: optional extra argument to handle new attribute types
  (define (read-attribute-info pool)
    (lambda (in)
      (let* ([name-index (read-integer 2 #f in #t)]
             [name (utf8-info->string (vector-ref pool (sub1 name-index)))])
        (match name
          ["ConstantValue" (read-constant-value-attribute-info in)]
;          ["Code" (read-code-attribute-info in)]
          ["Exceptions" (read-exceptions-attribute-info in)]
;          ["InnerClasses" (read-inner-classes-attribute-info in)]
;          ["Synthetic" (read-inner-classes-attribute-info in)]
;          ["SourceFile" (read-source-file-attribute-info in)]
;          ["LineNumberTable" (read-line-number-table-attribute-info in)]
;          ["LocalVariableTable" (read-local-variable-table-attribute-info in)]
          ["Deprecated" (read-deprecated-attribute-info in)]
          [_ (read-unsupported-attribute-info in)]))))

  (define (read-inner-classes-attribute-info in)
    (let* ([attribute-length (read-integer 4 #f in #t)]
           [count            (read-integer 2 #f in #t)]
           [classes          (build-list count (lambda (i) (read-inner-class-entry in)))])
      (make-inner-classes-attribute-info count classes)))

  (define (read-inner-class-entry in)
    (let* ([inner-class-info-index   (read-integer 2 #f in #t)]
           [outer-class-info-index   (read-integer 2 #f in #t)]
           [inner-name-index         (read-integer 2 #f in #t)]
           [inner-class-access-flags (read-integer 2 #f in #t)])
      (make-inner-class-entry inner-class-info-index outer-class-info-index inner-name-index inner-class-access-flags)))

  (define (read-exceptions-attribute-info in)
    (let* ([attribute-length (read-integer 4 #f in #t)]
           [count            (read-integer 2 #f in #t)]
           [exceptions       (build-list count (lambda (i) (read-integer 2 #f in #t)))])
      ;(fprintf (current-error-port) "Exceptions: ~v~n" exceptions)
      (make-exceptions-attribute-info count exceptions)))

  (define (read-constant-value-attribute-info in)
    (let ([attribute-length (read-integer 4 #f in #t)])
      (unless (= attribute-length 2)
        (error 'read-attribute-info
               "attribute ConstantValue: expected 2 bytes, found ~a bytes" attribute-length))
      (make-constant-value-attribute-info (read-integer 2 #f in #t))))

  (define (read-synthetic-attribute-info in)
    (let ([attribute-length (read-integer 4 #f in #t)])
      (unless (zero? attribute-length)
        (error 'read-attribute-info
               "attribute Synthetic: expected 0 bytes, found ~a bytes" attribute-length))
      (make-synthetic-attribute-info)))

  (define (read-deprecated-attribute-info in)
    (let ([attribute-length (read-integer 4 #f in #t)])
      (unless (zero? attribute-length)
        (error 'read-attribute-info
               "attribute Deprecated: expected 0 bytes, found ~a bytes" attribute-length))
      (make-deprecated-attribute-info)))

  (define (read-unsupported-attribute-info in)
    (let* ([attribute-length (read-integer 4 #f in #t)]
           [info             (read-bytes attribute-length in)])
      (make-unsupported-attribute-info attribute-length info)))

  (define (read-interfaces count in pool)
    (map (lambda (i) (vector-ref pool (sub1 i)))
         (build-list count (lambda (i) (read-integer 2 #f in #t)))))

  ;; read-array : natural-number input-port (input-port -> a) -> (vectorof a)
  (define (read-array count in reader)
    (build-vector count (lambda (i) (reader in))))

  ;; read-list : natural-number input-port (input-port -> a) -> (listof a)
  (define (read-list count in reader)
    (build-list count (lambda (i) (reader in))))

  ;; read-class-file : [input-port] -> classfile
  (define read-class-file
    (opt-lambda ([in (current-input-port)])
      (let ([magic (read-integer 4 #f in #t)])
        (unless (= magic #xcafebabe)
          (error 'read-class-file "bad class file signature: #x~x" magic))
        (let* ([minor               (read-integer 2 #f in #t)]
               [major               (read-integer 2 #f in #t)]
               [constant-pool-count (read-integer 2 #f in #t)]
               [pool                (read-constant-pool (sub1 constant-pool-count) in)]
               [access-flags        (read-integer 2 #f in #t)]
               [this-index          (read-integer 2 #f in #t)]
               [super-index         (read-integer 2 #f in #t)]
               [interfaces-count    (read-integer 2 #f in #t)]
               [interfaces          (read-interfaces interfaces-count in pool)]
               [fields-count        (read-integer 2 #f in #t)]
               [fields              (read-list fields-count in (read-field-info pool))]
               [methods-count       (read-integer 2 #f in #t)]
               [methods             (read-list methods-count in (read-method-info pool))]
               [attributes-count    (read-integer 2 #f in #t)]
               [attributes          (read-list attributes-count in (read-attribute-info pool))])
          ;                                  (begin
          ;                                    (fprintf (current-error-port) "attributes: ~a~n" attributes-count)
          ;                                    (printf "methods-count: ~a~n" methods-count)
          ;                                    (printf "access-flags: ~x~n" access-flags)
          ;                                    (fprintf (current-error-port) "pool: ~v~n"
          ;                                             (let ([pool* (vector->list pool)])
          ;                                               (map (lambda (entry)
          ;                                                      (if (utf8-info? entry)
          ;                                                          (parse-utf8-info entry)
          ;                                                          entry))
          ;                                                    pool*)))
          (make-class-file pool access-flags this-index super-index interfaces fields methods attributes)))))

;        (let* ([deref (lambda (i) (vector-ref pool (sub1 i)))]
;               [parse-class-info (lambda (ci) (parse-utf8-info (deref (class-info-name-index ci))))])
;          (make-class-file #f;pool
;                           (parse-flags access-flags)
;                           (parse-class-info (deref this-index))
;                           (parse-class-info (deref super-index))
;                           (map parse-class-info interfaces)
;                           fields
;                           methods
;                           attributes)))))

  ;; Parsers:

  (define (utf8-info->string utf8)
    (bytes->string/utf-8 (utf8-info-bytes utf8)))

  (define *access-flags*
    '(public                   ; #x0001
      private                  ; #x0002
      protected                ; #x0004
      static                   ; #x0008
      final                    ; #x0010
      super                    ; #x0020
      synchronized             ; #x0020
      super/synchronized       ; #x0020
      volatile                 ; #x0040
      transient                ; #x0080
      native                   ; #x0100
      interface                ; #x0200
      abstract                 ; #x0400
      strictfp))               ; #x0800

  (define *access-flags-vector*
    '#(public                  ; #x0001
       private                 ; #x0002
       protected               ; #x0004
       static                  ; #x0008
       final                   ; #x0010
       super/synchronized      ; #x0020
       volatile                ; #x0040
       transient               ; #x0080
       native                  ; #x0100
       interface               ; #x0200
       abstract                ; #x0400
       strictfp))              ; #x0800

  (define access-flag/c
    (apply symbols *access-flags*))

  (define *read-constant-vector*
    (vector #f
            read-utf8-info                   ; 1
            #f
            read-integer-info                ; 3
            read-float-info                  ; 4
            read-long-info                   ; 5
            read-double-info                 ; 6
            read-class-info                  ; 7
            read-string-info                 ; 8
            read-field-ref-info              ; 9
            read-method-ref-info             ; 10
            read-interface-method-ref-info   ; 11
            read-name-and-type-info))        ; 12

  (define *max-constant-type* (sub1 (vector-length *read-constant-vector*)))

  ;; extract-access-flags : exact-integer [(union 'super 'synchronized)] -> (listof access-flag)
  (define extract-access-flags
    (opt-lambda (bits [tie-breaker 'super/synchronized])
      (filter-map identity
                  (build-list (vector-length *access-flags-vector*)
                              (lambda (i)
                                (and (bit-set? i bits)
                                     (let ([flag (vector-ref *access-flags-vector* i)])
                                       (if (eq? flag 'super/synchronized)
                                           tie-breaker
                                           flag))))))))

  (provide/contract
   [access-flag/c contract?])

  (provide/contract
   [extract-access-flags ((integer?)
                          ((symbols 'super 'synchronized))
                          . opt-> .
                          (listof access-flag/c))]
   [utf8-info->string (utf8-info? . -> . string?)]
   [read-class-file (() (input-port?) . opt-> . class-file?)]))
