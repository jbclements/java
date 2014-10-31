#lang scribble/manual

@title{Java}

@author[(author+email "David Herman" "dherman@mozilla.com")]

@(require (for-label racket))

The libraries of the "java" collection provide utilities for manipulating Java
programs in both source and binary format.

*** CAVEAT: This library is still in its rather early stages. Expect significant
changes to the API to occur in the future.

*** ADDITIONAL CAVEAT: The previous caveat is now many years old. The remainder
of this file was ported from a doc.txt file.

@defmodule[java]{
                 
@defparam[current-classpath path-list (listof path)]{
A parameter containing a list of paths that point to either directories where
.class files reside or .zip/.jar files containing .class files. This is used by
the standard class resolver in _semantics/standard-resolver.ss_.
}

@verbatim{

> (current-sourcepath [sp]) :: (parameterof (listof path))

A parameter containing a list of paths that point to directories where .java
files reside. This is used by the standard class resolver in
_semantics/standard-resolver.ss_.


}}

@section{syntax/class-file}

@verbatim{
This module provides procedures for reading compiled Java .class files according
to the Java class file format, as specified by Chapter 4 of the Java Virtual
Machine Specification, 2nd Ed.

    http://java.sun.com/docs/books/vmspec/2nd-edition/html/VMSpecTOC.doc.html
    http://java.sun.com/docs/books/vmspec/2nd-edition/html/ClassFile.doc.html

> (struct class-file (pool flags this super interfaces fields methods attributes))

This structure encapsulates all the information read in from a class file.

> (extract-access-flags bits) :: exact-integer [(union 'super 'synchronized)] -> (listof access-flag)

A utility for converting access flags packed in the bits of an exact integer
into a list of their corresponding symbolic names. The possible access flags
occurring in a bit field can be enumerated by taking the union of tables 4.1,
4.4, 4.5, and 4.7 of the JVM specification.

Unfortunately, there is a single overlap amongst these tables: both
ACC_SUPER and ACC_SYNCHRONIZED have the value 0x0020. The second optional
argument allows clients to specify which way to interpret the value; by default
the procedure produces the symbol 'super/synchronized.

    Flag name         Bit      Symbolic value
    ---------         ---      --------------
    ACC_PUBLIC        0x0001   'public
    ACC_PRIVATE       0x0002   'private
    ACC_PROTECTED     0x0004   'protected
    ACC_STATIC        0x0008   'static
    ACC_FINAL         0x0010   'final
    ACC_SUPER         0x0020   'super
    ACC_SYNCHRONIZED  0x0020   'synchronized
    ACC_VOLATILE      0x0040   'volatile
    ACC_TRANSIENT     0x0080   'transient
    ACC_NATIVE        0x0100   'native
    ACC_INTERFACE     0x0200   'interface
    ACC_ABSTRACT      0x0400   'abstract
    ACC_STRICT        0x0800   'strictfp

> (utf8-info->string ui) :: utf8-info -> string

Converts a utf8-info struct into a Unicode string.

> (read-class-file [in]) :: [input-port] -> class-file

Reads a class file from an input port.

> access-flag/c :: contract

Recognizes symbols that name access flags: all of the symbols in the table
above, plus 'super/synchronized.

The _class-file.ss_ library defines a hierarchy of structures representing the
elements of a class file. The structures are organized according to the
following hierarchy:

    info
        |
        +-- class-info
        |       o name-index :: natural-number
        |
        +-- ref-info
        |       o class-index :: natural-number
        |       o name-and-type-index :: natural-number
        |       |
        |       +-- field-ref-info
        |       |
        |       +-- method-ref-info
        |       |
        |       +-- interface-method-ref-info
        |
        +-- string-info
        |       o string-index :: natural-number
        |
        +-- integer-info
        |       o value :: integer
        |
        +-- float-info
        |       o bytes :: bytes
        |
        +-- long-info
        |       o high-bytes :: bytes
        |       o low-bytes :: bytes
        |
        +-- double-info
        |       o high-bytes :: bytes
        |       o low-bytes :: bytes
        |
        +-- name-and-type-info
        |       o name-index :: natural-number
        |       o descriptor-index :: natural-number
        |
        +-- utf8-info
        |       o length :: natural-number
        |       o bytes :: bytes
        |
        +-- inner-class-entry
        |       o inner-class-info-index :: natural-number
        |       o outer-class-info-index :: natural-number
        |       o inner-name-index :: natural-number
        |       o inner-class-access-flags :: integer
        |
        +-- element-info
        |       o access-flags :: integer
        |       o name-index :: natural-number
        |       o descriptor-index :: natural-number
        |       o attributes-count :: natural-number
        |       o attributes :: (listof attribute-info)
        |       |
        |       +-- field-info
        |       |
        |       +-- method-info
        |
        +-- attribute-info
                |
                +-- unsupported-attribute-info
                |       o length :: natural-number
                |       o bytes :: bytes
                |
                +-- constant-value-attribute-info
                |       o value-index :: natural-number
                |
                +-- code-attribute-info
                |
                +-- exceptions-attribute-info
                |       o count :: natural-number
                |       o exceptions :: (listof natural-number)
                |
                +-- inner-classes-attribute-info
                |
                +-- synthetic-attribute-info
                |
                +-- source-file-attribute-info
                |
                +-- line-number-table-attribute-info
                |
                +-- local-variable-table-attribute-info
                |
                +-- deprecated-attribute-info
}}

@section{syntax/ast}
@verbatim{


The _ast.ss_ module defines a hierarchy of structures representing nodes in the
abstract syntax tree of a Java program. The structures are organized according
to the following hierarchy:

    ast
        o src :: (optional src)
        |
        +-- id
        |       o name :: symbol
        |
        +-- name
        |       o path :: (listof id)
        |       o id :: id
        |
        +-- import
        |       o name :: name
        |       o star? :: boolean
        |
        +-- type-spec
        |       o base-type :: type-name
        |       o dimension :: natural-number
        |
        +-- modifier
        |       o modifier :: symbol
        |
        +-- initializer
        |       o static? :: boolean
        |       o body :: block-stmt
        |
        +-- compilation-unit
        |       o package :: (optional name)
        |       o imports :: (listof import)
        |       o classes :: (listof (optional type-decl))
        |
        +-- decl
        |       o modifiers :: (listof modifier)
        |       o name :: id
        |       |
        |       +-- type-decl
        |       |       o interfaces :: (listof name)
        |       |       o body :: (listof class-element)
        |       |       |
        |       |       +-- class-decl
        |       |       |       o super :: (optional name)
        |       |       |
        |       |       +-- interface-decl
        |       |
        |       +-- variable-decl
        |       |       o type :: type-spec
        |       |       o init :: (optional expr)
        |       |
        |       +-- behavior-decl
        |               o formals :: (listof variable-decl)
        |               o throws :: (listof name)
        |               o body :: block-stmt
        |               |
        |               +-- constructor-decl
        |               |
        |               +-- method-decl
        |                       o return-type :: type-spec
        |
        +-- stmt
        |       |
        |       +-- expr-stmt
        |       |       o expr :: expr
        |       |
        |       +-- labeled-stmt
        |       |       o label :: id
        |       |       o stmt :: (optional stmt)
        |       |
        |       +-- block-stmt
        |       |       o body :: (listof block-element)
        |       |
        |       +-- switch-stmt
        |       |       o expr :: expr
        |       |       o clauses :: (listof (union case-stmt block-element))
        |       |
        |       +-- case-stmt
        |       |       o test :: (optional expr)
        |       |
        |       +-- if-stmt
        |       |       o test :: expr
        |       |       o con :: (optional stmt)
        |       |       o alt :: (optional stmt)
        |       |
        |       +-- for-stmt
        |       |       o init :: (union (listof variable-decl) (listof expr))
        |       |       o test :: (optional expr)
        |       |       o update :: (listof expr)
        |       |       o body :: (optional stmt)
        |       |
        |       +-- while-stmt
        |       |       o test :: expr
        |       |       o body :: (optional stmt)
        |       |
        |       +-- do-stmt
        |       |       o body :: stmt
        |       |       o test :: expr
        |       |
        |       +-- break-stmt
        |       |       o label :: (optional id)
        |       |
        |       +-- continue-stmt
        |       |       o label :: (optional id)
        |       |
        |       +-- return-stmt
        |       |       o value :: (optional expr)
        |       |
        |       +-- throw-stmt
        |       |       o expr :: expr
        |       |
        |       +-- synchronized-stmt
        |       |       o expr :: expr
        |       |       o body :: stmt
        |       |
        |       +-- try-stmt
        |       |       o body :: block-stmt
        |       |       o catches :: (listof catch-stmt)
        |       |       o finally :: (optional block-stmt)
        |       |
        |       +-- catch-stmt
        |       |       o exception :: variable-decl
        |       |       o body :: block-stmt
        |       |
        |       +-- assert-stmt
        |               o predicate :: expr
        |               o message :: expr
        |
        +-- expr
                |
                +-- conditional-expr
                |       o test :: expr
                |       o con :: expr
                |       o alt :: expr
                |
                +-- prefix-expr
                |       o op-src :: src
                |       o operator :: symbol
                |       o operand :: expr
                |
                +-- postfix-expr
                |       o op-src :: src
                |       o operator :: symbol
                |       o operand :: expr
                |
                +-- unary-expr
                |       o op-src :: src
                |       o operator :: symbol
                |       o operand :: expr
                |
                +-- binary-expr
                |       o op-src :: src
                |       o operator :: symbol
                |       o left :: expr
                |       o right :: expr
                |
                +-- instanceof-expr
                |       o op-src :: src
                |       o expr :: expr
                |       o type :: type-spec
                |
                +-- literal
                |       o value :: any
                |       |
                |       +-- boolean-literal
                |       |
                |       +-- char-literal
                |       |
                |       +-- integer-literal
                |       |
                |       +-- long-literal
                |       |
                |       +-- float-literal
                |       |
                |       +-- double-literal
                |       |
                |       +-- string-literal
                |       |
                |       +-- null-literal
                |
                +-- class-expr
                |       o type :: type-spec
                |
                +-- new-object-expr
                |       o container :: (optional expr)
                |       o name :: name
                |       o args :: (listof expr)
                |       o class-body :: (optional (listof class-element))
                |
                +-- new-array-expr
                |       o type :: type-spec
                |       o dim-exprs :: (listof expr)
                |       o dim :: natural-number
                |       o init :: (optional array-initializer)
                |
                +-- array-initializer
                |       o contents :: (listof expr)
                |
                +-- call-expr
                |       o object :: (optional expr)
                |       o name :: name
                |       o args :: (listof expr)
                |
                +-- assign-expr
                |       o operator :: symbol
                |       o left :: access
                |       o right :: expr
                |
                +-- cast-expr
                |       o type :: type-spec
                |       o expr :: expr
                |
                +-- access
                        |
                        +-- field-access
                        |       o object :: expr
                        |       o name :: id
                        |
                        +-- array-access
                        |       o array :: expr
                        |       o index :: expr
                        |
                        +-- var-access
                                o var :: name

Every AST node can contain a field of type `src', which represents the source
location information of the item.

> (struct src (file line col span)) :: (optional path) * (optional natural-number) * (optional natural-number) * (optional natural-number)

> (name->access name) :: name -> access

Converts a name to a variable reference.

> (access->name acc) :: (union field-access var-access) -> name

Converts a field or local variable reference to a name.

FIXME: doesn't work with field accesses in general, since those can contain
arbitrary expressions, which can't be represented as names.  (In fact, it
appears that field-accesses always have non-name expressions; all other
field refs get mapped to var-access nodes instead.)

> (increase-type-dimension ts n) :: type-spec natural-number -> type-spec

Constructs a new type-spec which is equivalent to `ts' except with type
dimension increased by `n'.

> primitive-type ::= 'void | 'boolean | 'byte | 'short | 'int | 'long | 'char | 'float | 'double

> (primitive-type? v) :: any -> boolean

Members of class and interface declarations are represented by the type
`class-element', which is define by:

> class-element ::=
    decl
  | (nelistof variable-decl)
  | initializer
  | #f

> (class-element? v) :: any -> boolean

> block-element ::=
    decl
  | (nelistof variable-decl)
  | stmt
  | #f

> (block-element? v) :: any -> boolean

> type-name ::=
    primitive-type
  | name

> (type-name? v) :: any -> boolean
}

@section{syntax/lexer}
                         
@verbatim{


The _lexer.ss_ module provides a lexer for Java.

> Operators :: empty-tokens

> Separators :: empty-tokens

> EmptyLiterals :: empty-tokens

> Keywords :: empty-tokens

> BasicTokens :: tokens

> (struct string-error (string error-token)) :: string position-token

> (java-lexer in) :: input-port -> position-token

}

@section{syntax/parser}
@verbatim{

======================================================================
_syntax/parser.ss_:
======================================================================

The _parser.ss_ module provides a parser for Java.

> (parse in path) :: [input-port (optional (union path string))] -> ast

Parses a Java compilation unit (i.e., the contents of a .java file) from an
input port, with the given file name (used for error reporting).

> (parse-string str) :: string -> ast

Parses a Java compilation unit from a string.

> (parse-file path) :: (union path string) -> ast

Parses a Java compilation unit from a file.
}

@section{semantics/semantic-object}
@verbatim{

The _semantic-object.ss_ library defines a hierarchy of mzscheme classes and
interfaces for the representation of the (static) semantic elements of a Java
program, e.g., classes, interfaces, methods, fields, etc.

> (struct type-name (package type [dimension])) :: (listof symbol) symbol? natural-number

The `type-name' structure contains enough information to identify a type
uniquely and can be used to look up a type with a class resolver (see
_class-resolver.ss). The `package' field contains the name of the type's package
(primitive types and members of the default package use the empty list). The
`type' field contains the symbolic name of the type. The optional `dimension'
field contains the array dimension and defaults to 0, representing a ground
(i.e., non-array) type.

> (build-type-name path) :: (listof symbol) -> type-name

Given a fully qualified type name as a list of symbols, this procedure builds a
`type-name' structure representing that name.

> (dot-notation path) :: (listof symbol) -> string

Given a fully qualified type name as a list of symbols, this procedure
constructs the string representation of the fully qualified name in Java
``dot-notation''.

> (type-name->string tn) :: (optional type-name) -> string

Given a `type-name' struct (or #f, representing the ``void'' type), this
procedure constructs the string representation of the fully qualified name in
Java ``dot-notation''.

> byte, char, double, float, int, long, short, boolean :: primitive-type%

Objects of class primitive-type% representing the Java primitive types of the
same respective name.

Interfaces
----------

The mzscheme interfaces defined in this library are organized according to the
following graph:

    semantic-object<%>   resolvable<%>
          |                   |
          +---------+---------+
                    |
                 type<%>

++ interface semantic-object<%>

> (to-string) :: -> string

Returns a string representation of the semantic object.

++ interface resolvable<%>

> (get-related-types) :: -> (listof type-name)

Returns the list of type-names that are referenced in the definition of the
semantic object.

++ interface type<%> extends semantic-object<%>, resolvable<%>

> (get-type-name) :: -> type-name

Returns the type-name representing the type object.

Classes
-------

The mzscheme classes defined in this library are organized according to the
following hierarchy (classes without parent classes extend the standard mzscheme
object% class):

    package% (semantic-object<%>, resolvable<%>)

    array-type% (type<%>)

    ground-type% (type<%>)
       |
       +-- primitive-type%
       |
       +-- declared-type%
              |
              +-- class%
              |
              +-- interface%

    type-element% (semantic-object<%>, resolvable<%>)
       |
       +-- field%
       |
       +-- behavior%
       |      |
       |      +-- constructor%
       |      |
       |      +-- method%
       |
       +-- initializer%
       |
       +-- inner-type%

++ class package% implements semantic-object<%>

> (new package% (name _)) -> package%
    name :: (listof symbol)

> (to-string) :: -> string

Returns the package name in dot-notation.

++ class array-type% implements type<%>

> (new array-type% (base-type _)) -> array-type%
    base-type :: type-name

> (get-type-name) :: -> type-name

> (get-base-type) :: -> type-name

> (get-dimension) :: -> natural-number

> (get-related-types) :: -> (listof type-name)

> (to-string) :: -> string

++ class ground-type% implements semantic-object<%>, type<%>

> (new ground-type% (package _) (name _)) -> ground-type%
    package :: (listof symbol)
    name :: type-name

> (get-package) :: -> (listof symbol)

THIS METHOD IS DEPRECATED. Use (type-name-package (send obj get-type-name))
instead. In the next major version release, this method will be removed.

> (get-type-name) :: -> type-name

> (get-related-types) :: (listof type-name)

> (to-string) :: -> string

++ class primitive-type% extends ground-type%

> (new primitive-type% (name _)) -> primitive-type%
    name :: type-name

++ class declared-type% extends ground-type%

> (new declared-type% (package-name _) (name _) (modifiers _) (interfaces _) (elements _)) -> declared-type%
    package-name :: (listof symbol)
    name :: type-name
    modifiers :: (listof access-flags)
    interfaces :: (listof type-name)
    elements :: (listof type-element%)

> (get-modifiers) :: -> (listof access-flag)

> (get-interfaces) :: -> (listof type-name)

> (get-elements) :: -> (listof type-element%)

++ class class% extends declared-type%

> (new class% (package _) (name _) (modifiers _) (interfaces _) (elements _) (superclass _)) -> class%
    package :: (listof symbol)
    name :: type-name
    modifiers :: (listof access-flag)
    interfaces :: (listof type-name)
    elements :: (listof type-element%)
    superclass :: (optional type-name)

> (get-superclass) :: -> (optional type-name)

++ class interface% extends declared-type%

> (new interface% (package _) (name _) (modifiers _) (interfaces _) (elements _)) -> interface%
    package :: (listof symbol)
    name :: type-name
    modifiers :: (listof access-flag)
    interfaces :: (listof type-name)
    elements :: (listof type-element%)

++ class type-element% implements semantic-object<%>, resolvable<%>

> (new type-element% (name _)) -> type-element%
    name :: string

> (get-name) :: -> (option string)

> (get-related-types) :: (listof type-name)

> (to-string) :: -> string

++ class field% extends type-element%

> (new field% (name _) (modifiers _) (type _)) -> field%
    name :: string
    modifiers :: (listof access-flag)
    type :: type-name

> (get-modifiers) :: -> (listof access-flag)

> (get-type) :: -> type-name

++ class initializer% extends type-element%

> (new initializer%) -> intializer%

++ class behavior% extends type-element%

> (new behavior% (name _) (formals _) (exceptions _) (modifiers _)) -> behavior%
    name :: string
    formals :: (listof type-name)
    exceptions :: (listof type-name)
    modifiers :: (listof access-flag)

> (get-formals) :: -> (listof type-name)

> (get-exceptions) :: -> (listof type-name)

> (get-modifiers) :: -> (listof access-flag)

++ class constructor% extends behavior%

> (new constructor% (name _) (formals _) (exceptions _) (modifiers _)) -> constructor%
    name :: string
    formals :: (listof type-name)
    exceptions :: (listof type-name)
    modifiers :: (listof access-flag)

++ class method% extends behavior%

> (new method% (name _) (formals _) (exceptions _) (modifiers _) (return-type _)) -> method%
    name :: string
    formals :: (listof type-name)
    exceptions :: (listof type-name)
    modifiers :: (listof access-flag)
    return-type :: (optional type-name)

> (get-return-type) :: -> (optional type-name)

++ class inner-type% extends type-element%

> (new inner-type% (name _) (type _)) -> inner-type%
    name :: string
    type :: type-name

> (get-type) :: -> type-name

}
@section{semantics/class-resolver}
@verbatim{


;; TODO: this should not be starting out #f

> (current-class-resolver [cr]) :: (parameterof class-resolver<%>)

> (lookup-package pkg) :: (listof symbol) -> (optional package%)

> (lookup-type tn) :: type-name -> (optional type<%>)

> (resolve-all t) :: (union type-name semantic-object<%>) -> any

Forces the resolution (via the current class resolver) of the transitive closure
of the `get-related-types' relation on either a semantic object or the type
associated with a type-name.

++ interface class-resolver<%>

> (resolve-package path) :: (listof symbol) -> (optional package%)

> (resolve-type tn) :: type-name -> (optional type<%>)

}

@section{semantics/standard-resolver}
@verbatim{

++ class class-resolver% implements class-resolver<%>

> (new method% (name _) (formals _) (exceptions _) (modifiers _) (return-type _)) -> method%
    name :: string
    formals :: (listof type-name)
    exceptions :: (listof type-name)
    modifiers :: (listof access-flag)
    return-type :: (optional type-name)

> (new class-resolver% (classpath _) (sourcepath _)) -> class-resolver%
    classpath :: (listof path)
    sourcepath :: (listof path)

The initialization arguments specify the paths to search during class
resolution for binary and source resolution, respectively.  Both arguments
are optional.  If they are not specified, this class uses the value of
the current-classpath and current-sourcepath parameters, respectively, at
the time when the class was created.  Therefore, changing the value of
either of these parameters will *not* affect previously-created instances
of class-resolver%.

> (resolve-package path) :: (listof symbol) -> (optional package%)

> (resolve-type tn) :: type-name -> (option type<%>)
}

@section{semantics/resolve-source}
@verbatim{

TODO: (resolve-source ast) :: ast -> (listof declared-type%)

}

@section{semantics/resolve-binary}
@verbatim{

> (resolve-binary cf) :: class-file -> declared-type%

}