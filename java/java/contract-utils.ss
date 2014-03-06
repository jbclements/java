;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contract-Utils: general-purpose PLT contract utilities.
;;  Copyright (C) 2005  Richard Cobbe
;;  Version 1.3
;;
;;  This library is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation; either version 2.1 of the License, or (at
;;  your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful, but WITHOUT
;;  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with this library; if not, write to the Free Software Foundation,
;;  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module contract-utils mzscheme

  (require (lib "contract.ss"))

  ;; produces a contract that recognizes a non-empty list of elements
  ;; which satisfy the contract c.
  (define nelistof/c
    (lambda (c)
      (and/c (listof c) (not/c null?))))

  ;; contract that recognizes arbitrary s-expressions.
  (define sexp/c
    (flat-rec-contract sexp
                       (cons/c sexp sexp)
                       null?
                       number?
                       symbol?
                       string?
                       boolean?
                       char?))

  (define predicate/c (any/c . -> . boolean?))

  (define optional/c (lambda (contract) (union contract false/c)))

  (define positive-int/c
    (and/c natural-number/c (lambda (x) (> x 0))))

  (define-syntax eta
    (syntax-rules ()
      [(_ f) (lambda args (apply f args))]))

  (provide/contract (sexp/c contract?)
                    (predicate/c contract?)
                    (optional/c (-> (union contract? predicate/c) contract?))
                    [positive-int/c contract?]
                    (nelistof/c (-> (union contract? predicate/c) contract?)))

  (provide eta))
