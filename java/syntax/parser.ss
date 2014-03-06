(module parser mzscheme
  ;(require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1)))
  (require "../contract-utils.ss")
  (require (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools"))
  (require (lib "contract.ss"))
  (require (lib "etc.ss"))
  (require "lexer.ss")
  (require "ast.ss")

  ;; ===========================================================================
  ;; PARSER
  ;; ===========================================================================

  (define (position->string p)
    (format "(~a,~a)" (position-line p) (position-col p)))

  (define (java-parser src-file)
    (define-syntax (build-src stx)
      (syntax-case stx ()
        ((_ end)
         (syntax (build-src 1 end)))
        ((_ start end)
         (with-syntax ((start-pos (datum->syntax-object
                                   (syntax end)
                                   (string->symbol
                                    (format "$~a-start-pos"
                                            (syntax-object->datum (syntax start))))))
                       (end-pos (datum->syntax-object
                                 (syntax end)
                                 (string->symbol
                                  (format "$~a-end-pos"
                                          (syntax-object->datum (syntax end)))))))
           (syntax
            (make-src src-file
                      (position-line start-pos)
                      (position-col start-pos)
                      (- (position-offset end-pos)
                         (position-offset start-pos))
                      (position-offset start-pos)))))))
    (parser
     (start CompilationUnit)
     (end EOF)
     (src-pos)
     (tokens BasicTokens Keywords Operators Separators EmptyLiterals)
     ;; TODO: define a syntax-error exception and raise it
     (error (lambda (token-ok? token-name token-value start-pos end-pos)
              (if token-ok?
                  (printf "unexpected token: ~a=~a : ~a - ~a~n"
                          token-name token-value (position->string start-pos) (position->string end-pos))
                  (printf "invalid token: ~a, ~a : ~a - ~a~n"
                          token-name token-value (position->string start-pos) (position->string end-pos)))))
     (grammar
      ;; 19.3
      (Literal
       [(INTEGER_LIT) (make-integer-literal (build-src 1) $1)]
       [(HEX_LIT) (make-integer-literal (build-src 1) $1)]
       [(OCT_LIT) (make-integer-literal (build-src 1) $1)]
       [(LONG_LIT) (make-long-literal (build-src 1) $1)]
       [(HEXL_LIT) (make-long-literal (build-src 1) $1)]
       [(OCTL_LIT) (make-long-literal (build-src 1) $1)]
       [(FLOAT_LIT) (make-float-literal (build-src 1) $1)]
       [(DOUBLE_LIT) (make-double-literal (build-src 1)  $1)]
       [(TRUE_LIT) (make-boolean-literal (build-src 1) #t)]
       [(FALSE_LIT) (make-boolean-literal (build-src 1) #f)]
       [(CHAR_LIT) (make-char-literal (build-src 1) $1)]
       [(STRING_LIT) (make-string-literal (build-src 1) $1)]
       [(NULL_LIT) (make-null-literal (build-src 1) null)])

      ;; 19.4
      (Type
       [(PrimitiveType) $1]
       [(ReferenceType) $1])

      (PrimitiveType
       [(NumericType) $1]
       [(boolean) (make-type-spec (build-src 1) 'boolean 0)])

      (NumericType
       [(IntegralType) $1]
       [(FloatingPointType) $1])

      (IntegralType
       [(byte) (make-type-spec (build-src 1) 'byte 0)]
       [(short) (make-type-spec (build-src 1) 'short 0)]
       [(int) (make-type-spec (build-src 1) 'int 0)]
       [(long) (make-type-spec (build-src 1) 'long 0)]
       [(char) (make-type-spec (build-src 1) 'char 0)])

      (FloatingPointType
       [(float) (make-type-spec (build-src 1) 'float 0)]
       [(double) (make-type-spec (build-src 1) 'double 0)])

      (ReferenceType
       [(Name) (make-type-spec (build-src 1) $1 0)]
       [(ArrayType) $1])

      (ClassOrInterfaceType
       [(Name) $1])

      (ClassType
       [(ClassOrInterfaceType) $1])

      (InterfaceType
       [(ClassOrInterfaceType) $1])

      (ArrayType
       [(PrimitiveType Dims) (make-type-spec (build-src 2) (type-spec-base-type $1) $2)]
       [(Name Dims) (make-type-spec (build-src 2) $1 $2)])

      ;;19.5
      (Name
       [(IDENTIFIER) (make-name (build-src 1) null (make-id (build-src 1) $1))]
       [(Name PERIOD IDENTIFIER)
        (make-name (build-src 3) (append (name-path $1) (list (name-id $1))) (make-id (build-src 3 3) $3))])

      ;; 19.6
      (CompilationUnit
       [(PackageDeclaration ImportDeclarations TypeDeclarations)
        (make-compilation-unit (build-src 3) $1 $2 $3)]
       [(ImportDeclarations TypeDeclarations)
        (make-compilation-unit (build-src 2) #f $1 $2)]
       [(PackageDeclaration TypeDeclarations)
        (make-compilation-unit (build-src 2) $1 null $2)]
       [(PackageDeclaration ImportDeclarations)
        (make-compilation-unit (build-src 2) $1 $2 null)]
       [(PackageDeclaration)
        (make-compilation-unit (build-src 1) $1 null null)]
       [(ImportDeclarations)
        (make-compilation-unit (build-src 1) #f $1 null)]
       [(TypeDeclarations)
        (make-compilation-unit (build-src 1) #f null $1)]
       [()
        (make-compilation-unit (make-src src-file 0 0 0) #f null null)])

      (ImportDeclarations
       [(ImportDeclaration) (list $1)]
       [(ImportDeclarations ImportDeclaration) (append $1 (list $2))])

      (TypeDeclarations
       [(TypeDeclaration) (list $1)]
       [(TypeDeclarations TypeDeclaration) (append $1 (list $2))])

      (PackageDeclaration
       [(package Name SEMI_COLON) $2])

      (ImportDeclaration
       [(SingleTypeImportDeclaration) $1]
       [(TypeImportOnDemandDeclaration) $1])

      (SingleTypeImportDeclaration
       [(import Name SEMI_COLON) (make-import (build-src 3) $2 #f)])

      (TypeImportOnDemandDeclaration
       [(import Name PERIOD * SEMI_COLON)
        (make-import (build-src 5) $2 #t)])
      ;(make-name (build-src 2 4) (append (name-path $2) (list (name-id $2))) '*)

      (TypeDeclaration
       [(ClassDeclaration) $1]
       [(InterfaceDeclaration) $1]
       [(SEMI_COLON) #f])

      ;; 19.7
      (Modifiers
       [(Modifier) (list $1)]
       [(Modifiers Modifier) (append $1 (list $2))])

      (Modifier
       [(public)       (make-modifier (build-src 1) 'public)]
       [(protected)    (make-modifier (build-src 1) 'protected)]
       [(private)      (make-modifier (build-src 1) 'private)]
       [(static)       (make-modifier (build-src 1) 'static)]
       [(abstract)     (make-modifier (build-src 1) 'abstract)]
       [(final)        (make-modifier (build-src 1) 'final)]
       [(strictfp)     (make-modifier (build-src 1) 'strictfp)]
       [(native)       (make-modifier (build-src 1) 'native)]
       [(synchronized) (make-modifier (build-src 1) 'synchronized)]
       [(transient)    (make-modifier (build-src 1) 'transient)]
       [(volatile)     (make-modifier (build-src 1) 'volatile)])

      ;; 19.8.1
      (ClassDeclaration
       [(Modifiers class IDENTIFIER Super Interfaces ClassBody)
        (make-class-decl (build-src 6) $1 (make-id (build-src 3 3) $3) $5 $6 $4)]
       [(class IDENTIFIER Super Interfaces ClassBody)
        (make-class-decl (build-src 5) null (make-id (build-src 2 2) $2) $4 $5 $3)])

      (Super
       [() #f]
       [(extends ClassType) $2])

      (Interfaces
       [() null]
       [(implements InterfaceTypeList) $2])

      (InterfaceTypeList
       [(InterfaceType) (list $1)]
       [(InterfaceTypeList COMMA InterfaceType) (append $1 (list $3))])

      (ClassBody
       [(O_BRACE ClassBodyDeclarations C_BRACE) $2])

      (ClassBodyDeclarations
       [() null]
       [(ClassBodyDeclarations ClassBodyDeclaration)
        ;; Flatten multiple field declarations:
        (append $1 (if (list? $2) $2 (list $2)))])

      (ClassBodyDeclaration
       [(ClassMemberDeclaration) $1]
       ;; 1.1
       [(ClassDeclaration) $1]
       ;; 1.1
       [(InterfaceDeclaration) $1]
       [(StaticInitializer) $1]
       [(ConstructorDeclaration) $1]
       [(SEMI_COLON) #f])

      (ClassMemberDeclaration
       [(FieldDeclaration) $1]
       [(MethodDeclaration) $1])

      ;; 19.8.2
      (FieldDeclaration
       [(Modifiers Type VariableDeclarators SEMI_COLON)
        (map (lambda (build-decl)
               (build-decl $1 $2))
             $3)]
       [(Type VariableDeclarators SEMI_COLON)
        (map (lambda (build-decl)
               (build-decl null $1))
             $2)])

      (VariableDeclarators
       [(VariableDeclarator) (list $1)]
       [(VariableDeclarators COMMA VariableDeclarator) (append $1 (list $3))])

      (VariableDeclarator
       [(VariableDeclaratorId)
        (lambda (modifiers type)
          ($1 (build-src 1) modifiers type #f))]
       [(VariableDeclaratorId = VariableInitializer)
        (lambda (modifiers type)
          ($1 (build-src 1) modifiers type $3))])

      (VariableDeclaratorId
       [(IDENTIFIER)
        (lambda (src modifiers type init)
          (make-variable-decl src modifiers (make-id (build-src 1) $1) type init))]
       [(IDENTIFIER Dims)
        (lambda (src modifiers type init)
          (make-variable-decl src modifiers (make-id (build-src 1) $1) (increase-type-dimension type $2) init))])

      (VariableInitializer
       [(Expression) $1]
       [(ArrayInitializer) $1])

      ;; 19.8.3
      (MethodDeclaration
       [(MethodHeader MethodBody) ($1 (build-src 2) $2)])

      (MethodHeader
       [(Modifiers Type MethodDeclarator Throws)
        (lambda (src body)
          ($3 src $1 $2 $4 body))]
       [(Modifiers void MethodDeclarator Throws)
        (lambda (src body)
          ($3 src $1 (make-type-spec (build-src 2 2) 'void 0) $4 body))]
       [(Type MethodDeclarator Throws)
        (lambda (src body)
          ($2 src null $1 $3 body))]
       [(void MethodDeclarator Throws)
        (lambda (src body)
          ($2 src null (make-type-spec (build-src 1 1) 'void 0) $3 body))])

      (MethodDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN)
        (lambda (src modifiers return-type throws body)
          (make-method-decl src modifiers (make-id (build-src 1) $1) $3 throws body return-type))]
       [(IDENTIFIER O_PAREN C_PAREN)
        (lambda (src modifiers return-type throws body)
          (make-method-decl src modifiers (make-id (build-src 1) $1) null throws body return-type))]
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN Dims)
        (lambda (src modifiers return-type throws body)
          (make-method-decl src modifiers (make-id (build-src 1) $1) $3 throws body (increase-type-dimension return-type $5)))]
       [(IDENTIFIER O_PAREN C_PAREN Dims)
        (lambda (src modifiers return-type throws body)
          (make-method-decl src modifiers (make-id (build-src 1) $1) null throws body (increase-type-dimension return-type $4)))])

      (FormalParameterList
       [(FormalParameter) (list $1)]
       [(FormalParameterList COMMA FormalParameter) (append $1 (list $3))])

      (FormalParameter
       [(Type VariableDeclaratorId)
        ($2 (build-src 2) null $1 #f)]
       ;; 1.1
       [(final Type VariableDeclaratorId)
        ($3 (build-src 2) (list (make-modifier (build-src 1) 'final)) $2 #f)])

      (Throws
       [() null]
       [(throws ClassTypeList) $2])

      (ClassTypeList
       [(ClassType) (list $1)]
       [(ClassTypeList COMMA ClassType) (append $1 (list $3))])

      (MethodBody
       [(Block) $1]
       [(SEMI_COLON) #f])

      ;; 19.8.4

      (StaticInitializer
       [(static Block) (make-initializer (build-src 2) #t $2)]
       ;; 1.1
       [(Block) (make-initializer (build-src 1) #f $1)])

      ;; 19.8.5

      (ConstructorDeclaration
       [(Modifiers ConstructorDeclarator Throws ConstructorBody)
        ($2 (build-src 4) $1 $3 $4)]
       [(ConstructorDeclarator Throws ConstructorBody)
        ($1 (build-src 3) null $2 $3)])

      (ConstructorDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN)
        (lambda (src modifiers throws body)
          (make-constructor-decl src modifiers (make-id (build-src 1) $1) $3 throws body))]
       [(IDENTIFIER O_PAREN C_PAREN)
        (lambda (src modifiers throws body)
          (make-constructor-decl src modifiers (make-id (build-src 1) $1) null throws body))])

      (ConstructorBody
       [(O_BRACE ExplicitConstructorInvocation BlockStatements C_BRACE)
        (make-block-stmt (build-src 4) (cons $2 $3))]
       [(O_BRACE ExplicitConstructorInvocation C_BRACE)
        (make-block-stmt (build-src 3) (list $2))]
       [(O_BRACE BlockStatements C_BRACE)
        (make-block-stmt (build-src 3) $2)]
       [(O_BRACE C_BRACE)
        (make-block-stmt (build-src 2) null)])

      (ExplicitConstructorInvocation
       [(this O_PAREN ArgumentList C_PAREN SEMI_COLON)
        (make-expr-stmt (build-src 5)
          (make-call-expr (build-src 5)
            #f
            (make-name (build-src 1) null (make-id (build-src 1) 'this))
            $3))]
       [(this O_PAREN C_PAREN SEMI_COLON)
        (make-expr-stmt (build-src 4)
          (make-call-expr (build-src 4)
            #f
            (make-name (build-src 1) null (make-id (build-src 1) 'this))
            null))]
       [(super O_PAREN ArgumentList C_PAREN SEMI_COLON)
        (make-expr-stmt (build-src 5)
          (make-call-expr (build-src 5)
            #f
            (make-name (build-src 1) null (make-id (build-src 1) 'super))
            $3))]
       [(super O_PAREN C_PAREN SEMI_COLON)
        (make-expr-stmt (build-src 4)
          (make-call-expr (build-src 4)
            #f
            (make-name (build-src 1) null (make-id (build-src 1) 'super))
            null))])

      ;; 19.9.1

      (InterfaceDeclaration
       [(Modifiers interface IDENTIFIER ExtendsInterfaces InterfaceBody)
        (make-interface-decl (build-src 5) $1 (make-id (build-src 3 3) $3) $4 $5)]
       [(Modifiers interface IDENTIFIER InterfaceBody)
        (make-interface-decl (build-src 4) $1 (make-id (build-src 3 3) $3) null $4)]
       [(interface IDENTIFIER ExtendsInterfaces InterfaceBody)
        (make-interface-decl (build-src 4) null (make-id (build-src 2 2) $2) $3 $4)]
       [(interface IDENTIFIER InterfaceBody)
        (make-interface-decl (build-src 3) null (make-id (build-src 2 2) $2) null $3)])

      (ExtendsInterfaces
       [(extends InterfaceType) (list $2)]
       [(ExtendsInterfaces COMMA InterfaceType) (append $1 (list $3))])

      (InterfaceBody
       [(O_BRACE InterfaceMemberDeclarations C_BRACE) $2])

      (InterfaceMemberDeclarations
       [() null]
       [(InterfaceMemberDeclarations InterfaceMemberDeclaration)
        (append $1 (if (list? $2) $2 (list $2)))])

      (InterfaceMemberDeclaration
       [(ConstantDeclaration) $1]
       ;; 1.1
       [(ClassDeclaration) $1]
       ;; 1.1
       [(InterfaceDeclaration) $1]
       [(AbstractMethodDeclaration) $1]
       [(SEMI_COLON) #f])

      (ConstantDeclaration
       [(FieldDeclaration) $1])

      (AbstractMethodDeclaration
       [(MethodHeader SEMI_COLON)
        ($1 (build-src 2) #f)])

      ;; 19.10

      (ArrayInitializer
       [(O_BRACE VariableInitializers COMMA C_BRACE)
        (make-array-initializer (build-src 4) $2)]
       [(O_BRACE VariableInitializers C_BRACE)
        (make-array-initializer (build-src 3) $2)]
       [(O_BRACE COMMA C_BRACE)
        (make-array-initializer (build-src 3) null)]
       [(O_BRACE C_BRACE)
        (make-array-initializer (build-src 2) null)])

      (VariableInitializers
       [(VariableInitializer) (list $1)]
       [(VariableInitializers COMMA VariableInitializer) (append $1 (list $3))])

      ;; 19.11

      (Block
       [(O_BRACE BlockStatements C_BRACE)
        (make-block-stmt (build-src 3) $2)]
       [(O_BRACE C_BRACE)
        (make-block-stmt (build-src 2) null)])

      (BlockStatements
       [(BlockStatement) (if (list? $1) $1 (list $1))]
       [(BlockStatements BlockStatement) (append $1 (if (list? $2) $2 (list $2)))])

      (BlockStatement
       ;; 1.1
       [(ClassDeclaration) $1]
       [(InterfaceDeclaration) $1]
       [(LocalVariableDeclarationStatement) $1]
       [(Statement) $1])

      (LocalVariableDeclarationStatement
       [(LocalVariableDeclaration SEMI_COLON)
        ($1 null)]
       ;; 1.1
       [(final LocalVariableDeclaration SEMI_COLON)
        ($2 (list (make-modifier (build-src 1) 'final)))])

      (LocalVariableDeclaration
       [(Type VariableDeclarators)
        (lambda (modifiers)
          (map (lambda (build-decl)
                 (build-decl modifiers $1))
               $2))])

      (Statement
       [(StatementWithoutTrailingSubstatement) $1]
       [(LabeledStatement) $1]
       [(IfThenStatement) $1]
       [(IfThenElseStatement) $1]
       [(WhileStatement) $1]
       [(ForStatement) $1])

      (StatementNoShortIf
       [(StatementWithoutTrailingSubstatement) $1]
       [(LabeledStatementNoShortIf) $1]
       [(IfThenElseStatementNoShortIf) $1]
       [(WhileStatementNoShortIf) $1]
       [(ForStatementNoShortIf) $1])

      (StatementWithoutTrailingSubstatement
       [(Block) $1]
       [(EmptyStatement) $1]
       [(ExpressionStatement) $1]
       [(SwitchStatement) $1]
       [(DoStatement) $1]
       [(BreakStatement) $1]
       [(ContinueStatement) $1]
       [(ReturnStatement) $1]
       [(SynchronizedStatement) $1]
       [(ThrowStatement) $1]
       [(TryStatement) $1]
       [(AssertStatement) $1])

      (EmptyStatement
       [(SEMI_COLON) #f])

      (LabeledStatement
       [(IDENTIFIER : Statement)
        (make-labeled-stmt (build-src 3) (make-id (build-src 1) $1) $3)])

      (LabeledStatementNoShortIf
       [(IDENTIFIER : StatementNoShortIf)
        (make-labeled-stmt (build-src 3) (make-id (build-src 1) $1) $3)])

      (ExpressionStatement
       [(StatementExpression SEMI_COLON) (make-expr-stmt (build-src 1) $1)])

      (StatementExpression
       [(Assignment) $1]
       [(PreIncrementExpression) $1]
       [(PreDecrementExpression) $1]
       [(PostIncrementExpression) $1]
       [(PostDecrementExpression) $1]
       [(MethodInvocation) $1]
       [(ClassInstanceCreationExpression) $1])

      (IfThenStatement
       [(if O_PAREN Expression C_PAREN Statement)
        (make-if-stmt (build-src 5) $3 $5 #f)])

      (IfThenElseStatement
       [(if O_PAREN Expression C_PAREN StatementNoShortIf else Statement)
        (make-if-stmt (build-src 7) $3 $5 $7)])

      (IfThenElseStatementNoShortIf
       [(if O_PAREN Expression C_PAREN StatementNoShortIf else StatementNoShortIf)
        (make-if-stmt (build-src 7) $3 $5 $7)])

      (SwitchStatement
       [(switch O_PAREN Expression C_PAREN SwitchBlock)
        (make-switch-stmt (build-src 5) $3 $5)])

      (SwitchBlock
       [(O_BRACE SwitchBlockStatementGroups SwitchLabels C_BRACE)
        (append $2 $3)]
       [(O_BRACE SwitchBlockStatementGroups C_BRACE)
        $2]
       [(O_BRACE SwitchLabels C_BRACE)
        $2]
       [(O_BRACE C_BRACE) null])

      (SwitchBlockStatementGroups
       [(SwitchBlockStatementGroup) $1]
       [(SwitchBlockStatementGroups SwitchBlockStatementGroup) (append $1 $2)])

      (SwitchBlockStatementGroup
       [(SwitchLabels BlockStatements)
        (append $1 $2)])

      (SwitchLabels
       [(SwitchLabel) (list $1)]
       [(SwitchLabels SwitchLabel) (append $1 (list $2))])

      (SwitchLabel
       [(case ConstantExpression :)
        (make-case-stmt (build-src 3) $2)]
       [(default :)
        (make-case-stmt (build-src 2) #f)])

      (WhileStatement
       [(while O_PAREN Expression C_PAREN Statement)
        (make-while-stmt (build-src 5) $3 $5)])

      (WhileStatementNoShortIf
       [(while O_PAREN Expression C_PAREN StatementNoShortIf)
        (make-while-stmt (build-src 5) $3 $5)])

      (DoStatement
       [(do Statement while O_PAREN Expression C_PAREN SEMI_COLON)
        (make-do-stmt (build-src 6) $2 $5)])

      (ForStatement
       [(for O_PAREN ForInit SEMI_COLON Expression SEMI_COLON ForUpdate C_PAREN Statement)
        (make-for-stmt (build-src 9) $3 $5 $7 $9)]
       [(for O_PAREN ForInit SEMI_COLON SEMI_COLON ForUpdate C_PAREN Statement)
        (make-for-stmt (build-src 8) $3 #f $6 $8)])

      (ForStatementNoShortIf
       [(for O_PAREN ForInit SEMI_COLON Expression SEMI_COLON ForUpdate C_PAREN StatementNoShortIf)
        (make-for-stmt (build-src 9) $3 $5 $7 $9)]
       [(for O_PAREN ForInit SEMI_COLON SEMI_COLON ForUpdate C_PAREN StatementNoShortIf)
        (make-for-stmt (build-src 8) $3 #f $6 $8)])

      (ForInit
       [() null]
       [(StatementExpressionList) $1]
       [(LocalVariableDeclaration) ($1 null)]
       [(final LocalVariableDeclaration) ($2 (list (make-modifier (build-src 1) 'final)))])

      (ForUpdate
       [() null]
       [(StatementExpressionList) $1])

      (StatementExpressionList
       [(StatementExpression) (list $1)]
       [(StatementExpressionList COMMA StatementExpression) (append $1 (list $3))])

      (BreakStatement
       [(break IDENTIFIER SEMI_COLON)
        (make-break-stmt (build-src 3) (make-id (build-src 2 2) $2))]
       [(break SEMI_COLON)
        (make-break-stmt (build-src 2) #f)])

      (ContinueStatement
       [(continue IDENTIFIER SEMI_COLON)
        (make-continue-stmt (build-src 3) (make-id (build-src 2 2) $2))]
       [(continue SEMI_COLON)
        (make-continue-stmt (build-src 2) #f)])

      (ReturnStatement
       [(return Expression SEMI_COLON)
        (make-return-stmt (build-src 3) $2)]
       [(return SEMI_COLON)
        (make-return-stmt (build-src 2) #f)])

      (ThrowStatement
       [(throw Expression SEMI_COLON)
        (make-throw-stmt (build-src 3) $2)])

      (SynchronizedStatement
       [(synchronized O_PAREN Expression C_PAREN Block)
        (make-synchronized-stmt (build-src 5) $3 $5)])

      (AssertStatement
       [(assert Expression SEMI_COLON)
        (make-assert-stmt #f $2 #f)]
       [(assert Expression : Expression SEMI_COLON)
        (make-assert-stmt #f $2 $4)])

      (TryStatement
       [(try Block Catches)
        (make-try-stmt (build-src 3) $2 $3 #f)]
       [(try Block Catches Finally)
        (make-try-stmt (build-src 4) $2 $3 $4)]
       [(try Block Finally)
        (make-try-stmt (build-src 3) $2 null $3)])

      (Catches
       [(CatchClause) (list $1)]
       [(Catches CatchClause) (append $1 (list $2))])

      (CatchClause
       [(catch O_PAREN FormalParameter C_PAREN Block)
        (make-catch-stmt (build-src 5) $3 $5)])

      (Finally
       [(finally Block) $2])

      ;; 19.12

      (Primary
       [(PrimaryNoNewArray) $1]
       [(ArrayCreationExpression) $1])

      (PrimaryNoNewArray
       [(Literal) $1]
       [(this) (make-var-access (build-src 1) (make-name (build-src 1) null (make-id (build-src 1) 'this)))]
       [(O_PAREN Expression C_PAREN) $2]
       [(ClassInstanceCreationExpression) $1]
       [(FieldAccess) $1]
       [(MethodInvocation) $1]
       [(ArrayAccess) $1]
       ;; 1.1
       [(PrimitiveType PERIOD class) (make-class-expr (build-src 3) $1)]
       ;; 1.1
       [(Name PERIOD class) (make-class-expr (build-src 3) $1)]
       ;; 1.1
       ;;[(ArrayType PERIOD class)
       ;; 1.1
       ;;[(PrimitiveType Dims PERIOD class)
       ;; 1.1
       ;;[(Name Dims PERIOD class)
       ;; 1.1
       [(void PERIOD class) (make-class-expr (build-src 3) 'void)]
       ;; 1.1
       [(Name PERIOD this) (make-var-access (build-src 3)
                             (make-name (build-src 3)
                               (append (name-path $1) (list (name-id $1)))
                               (make-id (build-src 3 3) 'this)))])

      (ClassInstanceCreationExpression
       [(new ClassOrInterfaceType O_PAREN ArgumentList C_PAREN)
        (make-new-object-expr (build-src 2 5) #f $2 $4 #f)]
       [(new ClassOrInterfaceType O_PAREN C_PAREN)
        (make-new-object-expr (build-src 2 4) #f $2 null #f)]
       ;; 1.1
       [(new ClassOrInterfaceType O_PAREN ArgumentList C_PAREN ClassBody)
        (make-new-object-expr (build-src 2 6) #f $2 $4 $6)]
       ;; 1.1
       [(new ClassOrInterfaceType O_PAREN C_PAREN ClassBody)
        (make-new-object-expr (build-src 2 5) #f $2 null $5)]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN ClassBody)
        (make-new-object-expr (build-src 8) $1 (make-id (build-src 4 4) $4) $6 $8)]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN C_PAREN ClassBody)
        (make-new-object-expr (build-src 7) $1 (make-id (build-src 4 4) $4) #f $7)]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-new-object-expr (build-src 7) $1 (make-id (build-src 4 4) $4) $6 #f)]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN C_PAREN)
        (make-new-object-expr (build-src 6) $1 (make-id (build-src 4 4) $4) #f #f)]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN ClassBody)
        (make-new-object-expr (build-src 8) (name->access $1) (make-id (build-src 4 4) $4) $6 $8)]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN C_PAREN ClassBody)
        (make-new-object-expr (build-src 7) (name->access $1) (make-id (build-src 4 4) $4) #f $7)]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-new-object-expr (build-src 7) (name->access $1) (make-id (build-src 4 4) $4) $6 #f)]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN C_PAREN)
        (make-new-object-expr (build-src 6) (name->access $1) (make-id (build-src 4 4) $4) #f #f)])

      (ArgumentList
       [(Expression) (list $1)]
       [(ArgumentList COMMA Expression) (append $1 (list $3))])

      (ArrayCreationExpression
       [(new PrimitiveType DimExprs Dims)
        (make-new-array-expr (build-src 2 4) $2 $3 $4 #f)]
       [(new PrimitiveType DimExprs)
        (make-new-array-expr (build-src 2 3) $2 $3 0 #f)]
       [(new ClassOrInterfaceType DimExprs Dims)
        (make-new-array-expr (build-src 2 4) $2 $3 $4 #f)]
       [(new ClassOrInterfaceType DimExprs)
        (make-new-array-expr (build-src 2 3) $2 $3 0 #f)]
       ;; 1.1
       [(new PrimitiveType Dims ArrayInitializer)
        (make-new-array-expr (build-src 2 4) $2 null $3 $4)]
       ;; 1.1
       [(new ClassOrInterfaceType Dims ArrayInitializer)
        (make-new-array-expr (build-src 2 4) $2 null $3 $4)])

      (DimExprs
       [(DimExpr) (list $1)]
       [(DimExprs DimExpr) (append $1 (list $2))])

      (DimExpr
       [(O_BRACKET Expression C_BRACKET) $2])

      (Dims
       [(O_BRACKET C_BRACKET) 1]
       [(Dims O_BRACKET C_BRACKET) (add1 $1)])

      (FieldAccess
       [(Primary PERIOD IDENTIFIER)
        (make-field-access (build-src 3) $1 (make-id (build-src 3 3) $3))]
       [(super PERIOD IDENTIFIER)
        (make-field-access (build-src 3)
          (name->access (make-name (build-src 1) null (make-id (build-src 1) 'super)))
          (make-id (build-src 3 3) $3))]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER)
        (make-field-access (build-src 5)
          (name->access (make-name (build-src 3)
                          (append (name-path $1) (list (name-id $1)))
                          (make-id (build-src 3 3) 'super)))
          (make-id (build-src 5 5) $5))])

      (MethodInvocation
       [(Name O_PAREN ArgumentList C_PAREN)
        (make-call-expr (build-src 4)
          #f $1 $3)]
       [(Name O_PAREN C_PAREN)
        (make-call-expr (build-src 3)
          #f $1 null)]
       [(Primary PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call-expr (build-src 6)
          $1 (make-name (build-src 3 3) null (make-id (build-src 3 3) $3)) $5)]
       [(Primary PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call-expr (build-src 5)
          $1 (make-name (build-src 3 3) null (make-id (build-src 3 3) $3)) null)]
       [(super PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call-expr (build-src 6)
          (name->access (make-name (build-src 1) null (make-id (build-src 1) 'super)))
          (make-name (build-src 3 3) null (make-id (build-src 3 3) $3))
          $5)]
       [(super PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call-expr (build-src 5)
          (name->access (make-name (build-src 1) null (make-id (build-src 1) 'super)))
          (make-name (build-src 3 3) null (make-id (build-src 3 3) $3))
          null)]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call-expr (build-src 8)
          (name->access (make-name (build-src 3)
                          (append (name-path $1) (list (name-id $1)))
                          (make-id (build-src 3 3) 'super)))
          (make-name (build-src 5 5) null (make-id (build-src 5 5) $5))
          $7)]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call-expr (build-src 7)
          (name->access (make-name (build-src 3)
                          (append (name-path $1) (list (name-id $1)))
                          (make-id (build-src 3 3) 'super)))
          (make-name (build-src 5 5) null (make-id (build-src 5 5) $5))
          null)])

      (ArrayAccess
       [(Name O_BRACKET Expression C_BRACKET)
        (make-array-access (build-src 4) (name->access $1) $3)]
       [(PrimaryNoNewArray O_BRACKET Expression C_BRACKET)
        (make-array-access (build-src 4) $1 $3)])

      (PostfixExpression
       [(Primary) $1]
       [(Name) (name->access $1)]
       [(PostIncrementExpression) $1]
       [(PostDecrementExpression) $1])

      (PostIncrementExpression
       [(PostfixExpression ++)
        (make-postfix-expr (build-src 2) (build-src 2 2) '++ $1)])

      (PostDecrementExpression
       [(PostfixExpression --)
        (make-postfix-expr (build-src 2) (build-src 2 2) '-- $1)])

      (UnaryExpression
       [(PreIncrementExpression) $1]
       [(PreDecrementExpression) $1]
       [(+ UnaryExpression)
        (make-unary-expr (build-src 2) (build-src 1) '+ $2)]
       [(- UnaryExpression)
        (make-unary-expr (build-src 2) (build-src 1) '- $2)]
       [(UnaryExpressionNotPlusMinus) $1])

      (PreIncrementExpression
       [(++ UnaryExpression)
        (make-prefix-expr (build-src 2) (build-src 2 2) '++ $2)])

      (PreDecrementExpression
       [(-- UnaryExpression)
        (make-prefix-expr (build-src 2) (build-src 2 2) '-- $2)])

      (UnaryExpressionNotPlusMinus
       [(PostfixExpression) $1]
       [(~ UnaryExpression)
        (make-unary-expr (build-src 2) (build-src 1) '~ $2)]
       [(! UnaryExpression)
        (make-unary-expr (build-src 2) (build-src 1) '! $2)]
       [(CastExpression) $1])

      (CastExpression
       [(O_PAREN PrimitiveType Dims C_PAREN UnaryExpression)
        (make-cast-expr (build-src 5)
          (make-type-spec (build-src 2 3) (type-spec-base-type $2) $3)
          $5)]
       [(O_PAREN PrimitiveType C_PAREN UnaryExpression)
        (make-cast-expr (build-src 4) $2 $4)]
       [(O_PAREN Expression C_PAREN UnaryExpressionNotPlusMinus)
        (make-cast-expr (build-src 4)
          (make-type-spec (build-src 2 2) (access->name $2) 0)
          $4)]
       [(O_PAREN Name Dims C_PAREN UnaryExpressionNotPlusMinus)
        (make-cast-expr (build-src 5)
          (make-type-spec (build-src 2 3) $2 $3)
          $5)])

      (MultiplicativeExpression
       [(UnaryExpression) $1]
       [(MultiplicativeExpression * UnaryExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '* $1 $3)]
       [(MultiplicativeExpression / UnaryExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '/ $1 $3)]
       [(MultiplicativeExpression % UnaryExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '% $1 $3)])

      (AdditiveExpression
       [(MultiplicativeExpression) $1]
       [(AdditiveExpression + MultiplicativeExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '+ $1 $3)]
       [(AdditiveExpression - MultiplicativeExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '- $1 $3)])

      (ShiftExpression
       [(AdditiveExpression) $1]
       [(ShiftExpression << AdditiveExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '<< $1 $3)]
       [(ShiftExpression >> AdditiveExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '>> $1 $3)]
       [(ShiftExpression >>> AdditiveExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '>>> $1 $3)])

      (RelationalExpression
       [(ShiftExpression) $1]
       ;; GJ - changed to remove shift/reduce conflict
       [(ShiftExpression < ShiftExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '< $1 $3)]
       [(RelationalExpression > ShiftExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '< $1 $3)]
       [(RelationalExpression <= ShiftExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '> $1 $3)]
       [(RelationalExpression >= ShiftExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '>= $1 $3)]
       [(RelationalExpression instanceof ReferenceType)
        (make-instanceof-expr (build-src 3) (build-src 2 2) $1 $3)])

      (EqualityExpression
       [(RelationalExpression) $1]
       [(EqualityExpression == RelationalExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '== $1 $3)]
       [(EqualityExpression != RelationalExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '!= $1 $3)])

      (AndExpression
       [(EqualityExpression) $1]
       [(AndExpression & EqualityExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '& $1 $3)])

      (ExclusiveOrExpression
       [(AndExpression) $1]
       [(ExclusiveOrExpression ^ AndExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '^ $1 $3)])

      (InclusiveOrExpression
       [(ExclusiveOrExpression) $1]
       [(InclusiveOrExpression PIPE ExclusiveOrExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          'or $1 $3)])

      (ConditionalAndExpression
       [(InclusiveOrExpression) $1]
       [(ConditionalAndExpression && InclusiveOrExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          '&& $1 $3)])

      (ConditionalOrExpression
       [(ConditionalAndExpression) $1]
       [(ConditionalOrExpression OR ConditionalAndExpression)
        (make-binary-expr (build-src 3) (build-src 2 2)
          'oror $1 $3)])

      (ConditionalExpression
       [(ConditionalOrExpression) $1]
       [(ConditionalOrExpression ? Expression : ConditionalExpression)
        (make-conditional-expr (build-src 5) $1 $3 $5)])

      (AssignmentExpression
       [(ConditionalExpression) $1]
       [(Assignment) $1])

      (Assignment
       [(LeftHandSide AssignmentOperator AssignmentExpression)
        (make-assign-expr (build-src 3) $2 $1 $3)])

      (LeftHandSide
       [(Name) (name->access $1)]
       [(FieldAccess) $1]
       [(ArrayAccess) $1])

      (AssignmentOperator
       [(=) '=]
       [(*=) '*=]
       [(/=) '/=]
       [(%=) '%=]
       [(+=) '+=]
       [(-=) '-=]
       [(<<=) '<<=]
       [(>>=) '>>=]
       [(>>>=) '>>>=]
       [(&=) '&=]
       [(^=) '^=]
       [(OREQUAL) 'or=])

      (Expression
       [(AssignmentExpression) $1])

      (ConstantExpression
       [(Expression) $1])
      )))

  ;; ===========================================================================
  ;; FRONT END
  ;; ===========================================================================

  ;; parse-file : (union path string) -> ast
  (define (parse-file file)
    (with-input-from-file file
      (lambda ()
        (port-count-lines! (current-input-port))
        (parse (current-input-port) file))))

  ;; parse-string : string -> ast
  (define (parse-string str)
    (let ([in (open-input-string str)])
      (port-count-lines! in)
      (parse in)))

  ;; parse : [input-port (optional (union path string))] -> ast
  (define parse
    (opt-lambda ([in (current-input-port)] [file #f])
      (let ([file-path (if (string? file)
                           (string->path file)
                           file)])
        ((java-parser file-path) (lambda () (java-lexer in))))))

  (provide/contract
   [parse (() (input-port? (optional/c (union path? string?))) . opt-> . ast?)]
   [parse-string (string? . -> . ast?)]
   [parse-file ((union path? string?) . -> . ast?)]))
