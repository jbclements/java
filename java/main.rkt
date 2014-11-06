#lang racket/base

;; trying to put together a main.rkt here.  I basically 
;; just slung everything in, and then commented out 
;; semantic-object because it conflicts with ast.ss
;; on the definition for "type-name". It appears
;; to compile...
(require 
  "semantics/class-resolver.ss"
  "semantics/resolve-binary.ss"
  "semantics/resolve-source.ss"
  ;;"semantics/semantic-object.ss"
  "semantics/standard-resolver.ss"
  "semantics/utils.ss"
  "syntax/ast.ss"
  "syntax/class-file.ss"
  "syntax/lexer.ss"
  "syntax/parser.ss"
  "tools/javap.ss"
  )

(provide (all-from-out "semantics/class-resolver.ss"
                       "semantics/resolve-binary.ss"
                       "semantics/resolve-source.ss"
                       ;;"semantics/semantic-object.ss"
                       "semantics/standard-resolver.ss"
                       "semantics/utils.ss"
                       "syntax/ast.ss"
                       "syntax/class-file.ss"
                       "syntax/lexer.ss"
                       "syntax/parser.ss"
                       "tools/javap.ss"))


