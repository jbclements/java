(module standard-resolver mzscheme
  (require file/unzip)
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "list.ss" "srfi" "1"))
  (require (all-except (lib "class.ss") class-info))
  (require (lib "file.ss"))
  (require (lib "etc.ss"))
  (require "class-resolver.ss")
  (require "semantic-object.ss")
  (require "resolve-binary.ss")
  (require "resolve-source.ss")
  (require "../syntax/class-file.ss")
  (require "../path-params.rkt")

  ;; ===========================================================================
  ;; DELEGATES TO PERFORM RESOLUTION
  ;; ===========================================================================

  ;; java-filename? : path -> boolean
  (define (java-filename? name)
    (bytes=? (filename-extension name) #"java"))

  ;; jar-filename? : path -> boolean
  (define (jar-filename? name)
    (let ([ext (filename-extension name)])
      (or (bytes=? ext #"jar")
          (bytes=? ext #"zip"))))

  ;; class-filename? : path -> boolean
  (define (class-filename? name)
    (bytes=? (filename-extension name) #"class"))

  ;; make-jar-resolver : path -> (path -> (optional declared-type%))
  (define (make-jar-resolver jar)
    (let ([zipdir (read-zip-directory jar)])
      (lambda (entry)
        (let ([entry (path->zip-path entry)])
          (and (zip-directory-contains? zipdir entry)
               (unzip-entry jar zipdir entry (lambda (p dir? in)
                                               (resolve-binary
                                                (read-class-file in)))))))))

  ;; make-directory-resolver : path -> (path -> (optional declared-type%))
  (define (make-directory-resolver basedir)
    (lambda (file)
      (and (class-filename? file)
           (let ([fullpath (build-path basedir file)])
             (and (file-exists? fullpath)
                  (resolve-binary
                   (with-input-from-file fullpath read-class-file)))))))

  ;; make-binary-resolver : path -> (path -> (optional declared-type%))
  (define (make-binary-resolver classpath-entry)
    (cond
      [(directory-exists? classpath-entry)
       (make-directory-resolver classpath-entry)]
      [(and (jar-filename? classpath-entry) (file-exists? classpath-entry))
       (make-jar-resolver classpath-entry)]
      [else
       (error 'make-resolver "bad classpath entry: ~v" classpath-entry)]))

  ;; make-source-directory-resolver :: path type-name
  ;;                                -> path
  ;;                                -> (Optional declared-type%)
  (define make-source-directory-resolver
    (lambda (basedir class-name)
      (lambda (file)
        (and (java-filename? file)
             (let ([full-path (build-path basedir file)])
               (and (file-exists? full-path)
                    (resolve-source class-name full-path)))))))

  ;; FIXME: remove this once resolve-source fully implemented.
  (define resolve-source
    (lambda args
      (error 'resolve-source "unimplemented")))

  ;; make-source-resolver :: path type-name -> path
  ;;                      -> (Optional declared-type%)
  (define make-source-resolver
    (lambda (sourcepath-entry class-name)
      (cond
       [(directory-exists? sourcepath-entry)
        (make-source-directory-resolver sourcepath-entry class-name)]
       [else
        (error 'make-resolver "bad sourcepath entry: ~v" sourcepath-entry)])))

  ;; try : (listof (a -> (optional b))) a -> (optional b)
  (define (try preds x)
    (and (pair? preds)
         (or ((car preds) x)
             (try (cdr preds) x))))

  ;; ===========================================================================
  ;; CLASS RESOLVER OBJECT
  ;; ===========================================================================

  ;; class-filename : type-name string -> path
  ;; FIXME: if we ever support inner classes, we'll have to completely rethink
  ;; this.
  (define (class-filename tn extension)
    (let ([file (string-append (symbol->string (type-name-type tn)) extension)]
          [pkg (type-name-package tn)])
      (if (null? pkg)
          file
          (build-path (let loop ([pkg (cdr pkg)]
                                 [path (string->path (symbol->string (car pkg)))])
                        (if (null? pkg)
                            path
                            (loop (cdr pkg)
                                  (build-path path (symbol->string (car pkg))))))
                      file))))

  (define class-resolver%
    (class* object% (class-resolver<%>)
      (public resolve-package resolve-type)
      (init (classpath (current-classpath)))
      (init (sourcepath (current-sourcepath)))
      (define all-packages (make-hash-table 'equal))
      (define class-resolvers (map make-binary-resolver classpath))

      (define (find-package name)
        (hash-table-get all-packages
                        name
                        (lambda ()
                          (let ([entry (cons (make-object package% name)
                                             (make-hash-table))])
                            (hash-table-put! all-packages name entry)
                            entry))))

      ;; resolve-package : (listof symbol) -> (optional package%)
      (define (resolve-package pkg)
        (and (not (null? pkg))
             (car (find-package pkg))))

      (define (resolve-primitive-type name)
        (and (null? (type-name-package name))
             (case (type-name-type name)
               [(byte)    byte]
               [(char)    char]
               [(int)     int]
               [(long)    long]
               [(short)   short]
               [(float)   float]
               [(double)  double]
               [(boolean) boolean]
               [else      #f])))

      ;; load-type-into-cache : type-name -> (Optional type<%>)
      ;; called when resolve-type detects a cache miss.  This method should not
      ;; change the all-packages hash tables.
      (define (load-type ty)
        ;; unwrapping a thunk here... looks like this code may have been 
        ;; under development...
        
        ;; try resolvers in sequence, returning the first non-#f result.
        (and
         (try class-resolvers (class-filename ty ".class"))
         
         ;; looks like the code from PLaneT doesn't compile here..... 
         ;; where is source-resolvers defined? Commenting this out 
         ;; seems like a big mistake. Ah! code in other parts of this
         ;; file suggests that this was never fully implemented.
         ;; Too bad.
         #;(try source-resolvers (class-filename ty ".java"))))

      ;; resolve-type : type-name -> (optional type<%>)
      (define (resolve-type ty)
        (or (resolve-primitive-type ty)
            (let* ([type-name (type-name-type ty)]
                   [entry (find-package (type-name-package ty))]
                   [package (car entry)]
                   [types (cdr entry)])
              (hash-table-get types
                              type-name
                              (lambda ()
                                (cond [(load-type ty) =>
                                       (lambda (type)
                                         (hash-table-put! types type-name type)
                                         type)]
                                      [else #f]))))))

      (super-new)))

  (provide class-resolver%))
