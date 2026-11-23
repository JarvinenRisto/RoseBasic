Enumeration
  #PREC_NONE
  #PREC_ASSIGNMENT  ; =
  #PREC_TERNARY
  #PREC_OR          ; Or
  #PREC_AND         ; And
  #PREC_XOR
  #PREC_EQUALITY    ; == !=
  #PREC_COMPARISON  ; < > <= >=
  #PREC_TERM        ; + -
  #PREC_FACTOR      ; * /
  #PREC_POWER
  #PREC_UNARY       ; ! -
  #PREC_CALL        ; . ()
  #PREC_SUBSCRIPT
  #PREC_PRIMARY
EndEnumeration

Global.Token *P
Global.Token *T = 1
Global.LinkedList::Node *N
Global.i control_top = -1

Global.s gThis = "this", gSuper = "super"
Global.s gOverloadName
Global.i gIsNative = -1
Global.i newIsNative = -1
Global expressionResultUsed.i

Global NewList loops() 

Declare block()
Declare multiBlock(endType.i, skipType.i, skipType2.i)
Declare parsePrecedence(precedence.i)

Global noLocal = 0

Macro CIRCULAR_ERROR(s)
  If circularReference > #MAX_CIRCULAR
    Error::Display(*t\fileName, *t\line, "Detected circular reference at the constant "+PeekS(s)+" declaration")
    ProcedureReturn
  EndIf
EndMacro
	
Procedure isNull()
  If *T = #Null Or *N = #Null
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure isPrevNull()
  If *P = #Null
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure isStatement()
  If *T = #Null
    ProcedureReturn 1
  EndIf
  
  Select *T\type
    Case #IDENFIER, #NUMBER, #STRING, #LPAREN, #MINUS, #True, #False, #NOT
      ProcedureReturn 1
  EndSelect
  
  ProcedureReturn 0
EndProcedure

Procedure advance()
  ; move ahead safely
  If *N = #Null
    *P = *T
    *T = #Null
    ProcedureReturn
  EndIf
  
  If *N\value = #Null
    *P = *T
    *T = #Null
    ProcedureReturn
  EndIf
  
  *P = *N\value
  *N = *N\next
  
  If *N = #Null Or *N\value = #Null
    *T = #Null
  Else
    *T = *N\value
  EndIf
EndProcedure

Procedure consume(type.i, message.s)
  If *T = #Null
    ProcedureReturn
  EndIf
    
  If type = *T\type
    advance()
    ProcedureReturn
  EndIf
  
  Error::Display(*t\fileName,*T\line, message)
EndProcedure

Procedure synchronize()
  Error::panicMode = 0
  
  While *T <> #Null
    
    Select *T\type
      Case #FOR, #IF, #WHILE, #DEBUG, #RETURN
        ProcedureReturn
    EndSelect
    
    advance()
  Wend
EndProcedure

Procedure getRule(type.i)
  Select type
    Case #PLUS_EQU, #MINUS_EQU, #MUL_EQU, #DIV_EQU
      ProcedureReturn #PREC_ASSIGNMENT
    Case #QUESTION
      ProcedureReturn #PREC_TERNARY
    Case #NEGATE, #NEW
      ProcedureReturn #PREC_UNARY
    Case #MINUS, #PLUS
      ProcedureReturn #PREC_TERM
    Case #DIV, #MUL, #MOD
      ProcedureReturn #PREC_FACTOR
    Case #POW
      ProcedureReturn #PREC_POWER
    Case #EQU_EQU, #NOT_EQU
      ProcedureReturn #PREC_EQUALITY
    Case #GRT
      ProcedureReturn #PREC_COMPARISON
    Case #LT
      ProcedureReturn #PREC_COMPARISON
    Case #LT_EQU
      ProcedureReturn #PREC_COMPARISON
    Case #GRT_EQU
      ProcedureReturn #PREC_COMPARISON
    Case #AND
      ProcedureReturn #PREC_AND
    Case #OPERATOR_OR
      ProcedureReturn #PREC_OR
    Case #Xor
      ProcedureReturn #PREC_XOR
    Case #LPAREN, #DOT
      ProcedureReturn #PREC_CALL
  EndSelect
  ProcedureReturn #PREC_NONE
EndProcedure
  
Procedure number()
  If *P = #Null
    ProcedureReturn
  EndIf
  
  ByteCode::EmitOP(#OP_CONSTANT)
  
  Select *P\Type
      Case #TDOUBLE
        ByteCode::EmitDouble(*P\double)
      Case #TINTEGER
        ByteCode::EmitInteger(*P\integer)
    EndSelect
    
EndProcedure

Procedure.i NextPowerOfTwo(x.i)
  If x <= 0
    ProcedureReturn 1
  EndIf

  x - 1
  x | x >> 1
  x | x >> 2
  x | x >> 4
  x | x >> 8
  x | x >> 16
  ProcedureReturn x + 1
EndProcedure

Procedure unary()
  If *P = #Null
    ProcedureReturn
  EndIf
    
  Define.i operatorType = *P\type
  Define.Token *after = *T
  
  If operatorType = #NEW
      If *after\type <> #IDENFIER
        Error::Display(*after\fileName, *after\line, "Expect idenfier after 'New'")
      EndIf
      
      ByteCode::EmitOP(#OP_CLASS)
      ByteCode::EmitString(*after\string)
      
      Define.SymbolTable::SClass *C = SymbolTable::lookupsymbol(*after\string)
      ByteCode::EmitByte(LinkedList::count(*C\functions)*2)
              
      Define.s klass = PeekS(*after\string)+":new"
      Define.SymbolTable::SFunction *Fun = SymbolTable::LookupSymbol(@klass)
      Define.s _module = PeekS(SymbolTable::GetModule()+2)
         
  		parsePrecedence(#PREC_ASSIGNMENT)
  		
  	  If *Fun\id <> -1
        ByteCode::EmitOp(#OP_CALL_NATIVE)
        ByteCode::EmitByte(*Fun\ParamCount)
        ByteCode::EmitLong(*Fun\id)
      Else 
        If _module <> ""
          klass = _module+":"+klass
        EndIf
        
        ByteCode::EmitOp(#OP_INVOKE_CONSTR)
        ByteCode::EmitString(@klass) 
    		ByteCode::EmitByte(*Fun\paramCount - 1)
  	  
    		Define.linkedlist::Node *current = *C\functions\head
    		Define.linkedlist::Node *_next
    		Define.i value = 0
    		
      		While *current <> #Null 
      		  *_next = *current\next
      		  
      		  Define.SymbolTable::SFunction *F = *current\value
      		  If *F And *F\isInit = 0
      		    *F\isInit = 1

;       		   ByteCode::EmitOP(#OP_CONSTANT)
;      		     ByteCode::EmitObject(*F\fObj)
;              ByteCode::EmitOP(#OP_SET_PROPERTY)
;              ByteCode::EmitLong((*F\currentProperty) * SizeOf(Quad))
      		  EndIf
   		  
      		  *current = *_next
      		Wend

  		EndIf
    ProcedureReturn
  EndIf
  
  parsePrecedence(#PREC_UNARY)
  
  Select operatorType
    Case #NEGATE, #MINUS
      ByteCode::EmitOp(#OP_NEGATE)
    Case #NOT
      ByteCode::EmitOp(#OP_NOT)
  EndSelect
EndProcedure

Procedure and_()
  Define.i endJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)

  ByteCode::emitOp(#OP_POP)
  parsePrecedence(#PREC_AND)

  ByteCode::patchJump(endJump)
EndProcedure

Procedure or_()
  Define.i elseJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)
  Define.i endJump = ByteCode::emitJump(#OP_JUMP)
  
  ByteCode::patchJump(elseJump)
  ByteCode::emitOp(#OP_POP)
  
  parsePrecedence(#PREC_OR)
  ByteCode::patchJump(endJump)
EndProcedure

Procedure isAssignable(*Expr.Token)
  ProcedureReturn Bool(*Expr\type = #IDENFIER)
EndProcedure

Procedure emitGet(*Expr.Token)
  Select *Expr\type
    Case #IDENFIER
      Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*Expr\string)
      Define.SymbolTable::SVariable *Var = *S
      
      Select *Var\refkind
        Case SymbolTable::#Ref_Class
          ByteCode::EmitOP(#OP_GET_LOCAL)
          ByteCode::EmitLong(0)
          
          ByteCode::EmitOP(#OP_GET_PROPERTY)
          ByteCode::EmitLong(*Var\refOffset * SizeOf(Quad))
          ;ByteCode::EmitString(*Var\name)
        Case SymbolTable::#Ref_Local, SymbolTable::#Ref_Parameter
          
          If *Var\dimensions = 0
            ByteCode::EmitOP(#OP_GET_LOCAL)
            ByteCode::EmitLong(*Var\refOffset * SizeOf(Quad))
          EndIf
      EndSelect
  EndSelect
EndProcedure

Procedure emitSet(*Expr.Token)
  Select *Expr\type
    Case #IDENFIER
      Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*Expr\string)
      Define.SymbolTable::SVariable *Var = *S
      
      Select *Var\refkind
        Case SymbolTable::#Ref_Class
          ByteCode::EmitOP(#OP_SET_PROPERTY)
          ;ByteCode::EmitString(*S\name)
          ByteCode::EmitLong(*Var\refOffset * SizeOf(Quad))
        Case SymbolTable::#Ref_Local, SymbolTable::#Ref_Parameter
          
          If *Var\dimensions > 0
            ByteCode::EmitOP(#OP_SET_ARRAY)
            ByteCode::EmitByte(*Var\refOffset)
          Else
            ByteCode::EmitOP(#OP_SET_LOCAL)
            ByteCode::EmitLong(*Var\refOffset * SizeOf(Quad))
          EndIf
      EndSelect
  EndSelect
EndProcedure

Procedure.s getOperatorOverload(operatorType.i)
    Select operatorType
    Case #PLUS
      ProcedureReturn "operator+"
    Case #MINUS
      ProcedureReturn "operator-"
    Case #MUL
      ProcedureReturn "operator*"
    Case #DIV
      ProcedureReturn "operator/"
    Case #MOD
      ProcedureReturn "operator%"
    Case #POW
      ProcedureReturn "operator^"
    Case #EQU_EQU
      ProcedureReturn "operator=="
    Case #GRT
      ProcedureReturn "operator>"
    Case #LT
      ProcedureReturn "operator<"
    Case #GRT_EQU
      ProcedureReturn "operator>="
    Case #LT_EQU
      ProcedureReturn "operator<="
    Case #PLUS_EQU
      ProcedureReturn "operator+="
    Case #MINUS_EQU
      ProcedureReturn "operator-="
   Case #MUL_EQU
      ProcedureReturn "operator*="
    Case #DIV_EQU
      ProcedureReturn "operator/="
    Case #LPAREN
      ProcedureReturn "operator()"
  EndSelect
  
  ProcedureReturn ""
EndProcedure

Procedure runOperatorOverload2(*Var.SymbolTable::SVariable, operatorType.i)
    If *Var\sym
      Define.s className = PeekS(*Var\Sym\name)
      Define.s operator = getOperatorOverload(operatorType)
      Define.s overload = className + ":" + operator
      Define.SymbolTable::SFunction *overload = SymbolTable::LookupSymbol(@overload)
        
      If *overload
        Define.s _module = PeekS(SymbolTable::getModule()+2)
        If _module <> ""
          overload = _module+":"+overload
        EndIf
        ByteCode::EmitOp(#OP_INVOKE)
        ByteCode::EmitString(@overload)
        ByteCode::EmitByte(1)
        expressionResultUsed = 1
        ProcedureReturn 1
      EndIf
    EndIf
EndProcedure

Procedure runOperatorOverload(*left.Token, operatorType.i)
  If *left\Type = #IDENFIER
    Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*left\string)

    If *S And *S\kind = SymbolTable::#SKind_Variable
      Define.SymbolTable::SVariable *Var = *S
      
      If runOperatorOverload2(*Var, operatorType)
        ProcedureReturn 1
      EndIf
    EndIf
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure binary(*left.Token)
  If *P = #Null
    ProcedureReturn
  EndIf
  
  expressionResultUsed = 1
  
  Define.i operatorType = *P\Type
  Define.i precedence = getRule(operatorType)
  
  ; Parse the right-hand side first (this will handle unary prefix like -3)
  If operatorType = #POW
    parsePrecedence(precedence)
  Else
    parsePrecedence(precedence + 1)
  EndIf
  
  If runOperatorOverload(*left, operatorType)
    ProcedureReturn
  EndIf
  
  ; Now emit the operator opcode (RHS is already on stack)
  Select operatorType
    Case #PLUS
      ByteCode::EmitOP(#OP_ADD)
    Case #MINUS
      ByteCode::EmitOP(#OP_SUBTRACT)
    Case #MUL
      ByteCode::EmitOP(#OP_MULTIPLY)
    Case #DIV
      ByteCode::EmitOP(#OP_DIVIDE)
    Case #MOD
      ByteCode::EmitOP(#OP_MOD)
    Case #POW
      ByteCode::EmitOP(#OP_POW)
    Case #EQU_EQU
      ByteCode::EmitOP(#OP_EQUAL)
    Case #GRT
      ByteCode::EmitOP(#OP_GREATER)
    Case #LT
      ByteCode::EmitOP(#OP_LESS)
    Case #GRT_EQU
      ByteCode::EmitOP(#OP_GRT_EQU)
    Case #LT_EQU
      ByteCode::EmitOP(#OP_LT_EQU)
    Case #NOT_EQU
      ByteCode::EmitOP(#OP_NOT_EQU)
    Case #XOR
      ByteCode::EmitOP(#OP_XOR) 
    Case #PLUS_EQU
      If Not isAssignable(*Left)
        Error::Display(*P\fileName, *P\line, "Invalid target for '+='")
      EndIf

      emitGet(*Left)
      ByteCode::EmitOP(#OP_ADD)
      emitSet(*Left)
    Case #MINUS_EQU
      If Not isAssignable(*Left)
        Error::Display(*P\fileName, *P\line, "Invalid target for '-='")
      EndIf

      emitGet(*Left)
      ByteCode::EmitOP(#OP_SUBTRACT)
      emitSet(*Left)
   Case #MUL_EQU
      If Not isAssignable(*Left)
        Error::Display(*P\fileName, *P\line, "Invalid target for '*='")
      EndIf

      emitGet(*Left)
      ByteCode::EmitOP(#OP_MULTIPLY)
      emitSet(*Left)
    Case #DIV_EQU
      If Not isAssignable(*Left)
        Error::Display(*P\fileName, *P\line, "Invalid target for '/='")
      EndIf

      emitGet(*Left)
      ByteCode::EmitOP(#OP_DIVIDE)
      emitSet(*Left)
    Default
      Error::Display(*P\fileName, *P\line, "Unknown binary operator")
  EndSelect
EndProcedure

Procedure grouping()
  parsePrecedence(#PREC_ASSIGNMENT)
  consume(#RPAREN, "Expect ')' after expression.")
EndProcedure

Procedure string()
  If *P = #Null
    ProcedureReturn
  EndIf
  
  If gIsNative <= 64
    ByteCode::EmitOp(#OP_CONSTANT)
  Else
    ByteCode::EmitOp(#OP_CONST_NATIVE)
  EndIf
  
  ByteCode::EmitString(*P\string)
  
EndProcedure

Procedure argumentList()
  Define.i argCount = 0
  
  If *T And *T\type <> #RPAREN
    Repeat
      parsePrecedence(#PREC_ASSIGNMENT)
      
      If (argCount >= 255)
        error::Display(*T\fileName, *T\line, "Cant have more than 255 arguments.")
      EndIf
      
      argCount + 1
      
      If *T And *T\type = #COMMA
        advance()
      Else
        Break
      EndIf
    ForEver
  EndIf
  
  consume(#RPAREN, "Expect ')' after arguments.")
  ProcedureReturn argCount
EndProcedure

Procedure call(canAssign.i)
  If *P = #Null
    ProcedureReturn
  EndIf
  
  Define *string = *P\string
  Define *prev = *P
  
  Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*string)
    
  If *S And *S\kind = SymbolTable::#SKind_Variable
    Define.SymbolTable::SVariable *V = *S
    
    If *V\dimensions > 0
             					    
      If *V\arrayInit = 0
        ByteCode::EmitOp(#OP_CONSTANT)
        ByteCode::EmitInteger(8+(*V\dimensions * (*V\dimensionSize)) * 8)
        ByteCode::EmitOp(#OP_ALLOC_ARRAY)
        ByteCode::EmitByte(*V\refOffset)
        ByteCode::EmitByte(*V\dimensions)
        *V\arrayInit = 1
      EndIf
      
      Define.i argCount = argumentList()
        
      If *T\type = #EQU
        advance()
        parsePrecedence(#PREC_ASSIGNMENT)
        ByteCode::EmitOP(#OP_SET_ARRAY)
  			ByteCode::EmitByte(*V\refOffset)
      Else
        ByteCode::EmitOP(#OP_GET_ARRAY)
  			ByteCode::EmitByte(*V\refOffset)
  		EndIf
  	Else
  	  argumentList()
  	  runOperatorOverload2(*V, #LPAREN)
    EndIf
  Else
    Define.i argCount = argumentList()
    
    If *S\kind = SymbolTable::#SKind_Function
      Define.SymbolTable::SFunction *Fun = *S
      
      If *Fun\id <> -1
        ByteCode::EmitOp(#OP_CALL_NATIVE)
        ByteCode::EmitByte(*Fun\ParamCount)
        ByteCode::EmitLong(*Fun\id)
        ProcedureReturn
      EndIf
    EndIf
    
    ByteCode::EmitOP(#OP_CALL)
    ByteCode::EmitByte(argCount)
  EndIf
EndProcedure

Procedure compileFunction(*FV.SymbolTable::SFunction) 
  If *FV
    ByteCode::*gFunction = *FV\fObj 
    *FV\addr = ByteCode::ip
    Define *Temp = *N
    *N = *FV\entry
    
    Define currentScope.s = PeekS(SymbolTable::GetCurrentScope())
    SymbolTable::SetScope(@"")
    
		SymbolTable::EnterScope(*FV\class)
		SymbolTable::EnterScope(*FV\name)
		
		Define *PREV = *P
		Define *TOKEN = *T
		
		advance()
		While *T <> #Null
      block()
    Wend
      
    SymbolTable::leaveScope()
    SymbolTable::SetScope(@currentScope)
    
    *N = *Temp
    *T = *TOKEN
    *P = *PREV
  EndIf
EndProcedure

Procedure modifierEqu(*S.SymbolTable::SVariable, Operator.i)
  If SymbolTable::GetCurrentClass() = ""
    Error::Display(*P\fileName, *P\line,"Property assignment outside class")
   ProcedureReturn
  EndIf

  advance()
  
  ByteCode::EmitOP(#OP_GET_LOCAL)
  ByteCode::EmitLong(0)
  ByteCode::EmitOP(#OP_GET_PROPERTY)
  ByteCode::EmitLong(*S\refOffset * SizeOf(Quad))
  ;ByteCode::EmitString(*S\name)
    
  parsePrecedence(#PREC_ASSIGNMENT)
    
  ByteCode::EmitOP(Operator)
  ByteCode::EmitOP(#OP_SET_PROPERTY)
  ByteCode::EmitLong(*S\refOffset * SizeOf(Quad))
  ;ByteCode::EmitString(*S\name)
EndProcedure

Global gThisEnabled = 0

Declare namedVariable(*name.Token, canAssign.i)
Declare super_()

Procedure dot(*left.Token, canAssign.i)
  Select *left\Type
  Case #IDENFIER, #SUPER
   ; namedVariable(*left, 0)
  ;Case #SUPER
  ;  super_()
  Default
    Error::Display(*P\fileName, *P\line,
      "Property access requires an instance")
    ProcedureReturn
EndSelect

  Define.Token *prev = *P
  consume(#IDENFIER, "Expect property name after '.'")
  
  Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*P\string)
  Define.SymbolTable::SSymbol *Override 
  
  If *left And *left\string
    *Override = SymbolTable::LookupSymbol(*left\string)
  EndIf
  
  If *S = #Null
    Define.SymbolTable::SVariable *V = SymbolTable::LookupSymbol(*prev\string)
    
    If *V And *V\kind = SymbolTable::#SKind_Module
      advance()

      ByteCode::emitOp(#OP_GET_GLOBAL)
      ByteCode::emitString(*left\string)
      
      Define.s name = PeekS(*left\string) + ":" + PeekS(*P\string)
      Define.i argCount = argumentList()
      ByteCode::EmitOp(#OP_INVOKE)
      ByteCode::EmitString(@name)
      ByteCode::EmitByte(argCount)
      ;ByteCode::EmitByte(gThisEnabled)
      gThisEnabled = 0
      ProcedureReturn 
    Else
      If *V > 1000 And *V\Sym > 1000
        Define.s name = PeekS(*V\Sym\name) + ":" + PeekS(*P\string)

        *S = SymbolTable::LookupSymbol(@name)
        
        If *S = #Null
          Define.SymbolTable::SClass *C = SymbolTable::LookupSymbol(*V\Sym\name)
          Define.s className = PeekS(*V\Sym\name)
          
          If *C 
            Define.SymbolTable::SClass *finalSuperClass
            *C = *C\superClass
            
            While *C <> 0
              *finalSuperClass = *C
              *C = SymbolTable::LookupSymbol(*C\name)
              
              name = PeekS(*finalSuperClass\name) + ":" + PeekS(*P\string)
              *S = SymbolTable::LookupSymbol(@name)
              
              If *S
                Break
              ElseIf *finalSuperClass\superClass <> 0
                *C = *finalSuperClass\superClass
              Else
                Error::Display(*P\fileName, *P\line, "Cant find a variable: " + PeekS(*P\string) + " from any superClass of: " + className)
              EndIf
            Wend
         
          EndIf
        EndIf
      ElseIf *P And *P\string <> #Null And *prev And *prev\string
        Define.s name = PeekS(*prev\string) + ":" + PeekS(*P\string)
        *S = SymbolTable::LookupSymbol(@name)
      EndIf
    EndIf
  EndIf
  
  If *S = #Null
    Define.SymbolTable::SVariable *V = SymbolTable::LookupSymbol(*left\string)
    If *V And *V\Sym 
      Define name.s = PeekS(*V\Sym\name) + ":" + PeekS(*P\string)
      *S = SymbolTable::LookupSymbol(@name)
      
      If *S = #Null
        name = PeekS(*prev\string) + ":" + PeekS(*P\string)
        *S = SymbolTable::LookupSymbol(@name)
      EndIf
    Else
      Error::Display(*P\filename, *P\line, "Variable: "+PeekS(*P\string)+" is not defined")
    EndIf
   EndIf
   
   If *Override And *Override\kind = SymbolTable::#SKind_Class
     Define.s over = PeekS(*left\string) + ":" + PeekS(*P\string)
     *S = SymbolTable::LookupSymbol(@over)
   EndIf
   
  Define.SymbolTable::SFunction *Fun 

  Select *S\kind
    Case SymbolTable::#SKind_Function
      *Fun = *S
    Case SymbolTable::#SKind_Class
      Define *currentC = *S\name
    Case SymbolTable::#Skind_Variable
      Define.SymbolTable::SVariable *V = *S
  EndSelect
  
  If canAssign And *T And *T\type = #EQU
    advance()

    parsePrecedence(#PREC_ASSIGNMENT)
    
    ByteCode::EmitOP(#OP_SET_PROPERTY)
    ByteCode::EmitLong(*V\refOffset * SizeOf(Quad))
    ByteCode::EmitOP(#OP_POP) 
  ElseIf canAssign And *T And *T\type = #PLUS_EQU
    modifierEqu(*S, #OP_ADD)
  ElseIf canAssign And *T And *T\type = #MINUS_EQU
    modifierEqu(*S, #OP_SUBTRACT)
  ElseIf canAssign And *T And *T\type = #MUL_EQU
    modifierEqu(*S, #OP_MULTIPLY)
  ElseIf canAssign And *T And *T\type = #DIV_EQU
    modifierEqu(*S, #OP_DIVIDE)
  ElseIf *T And *T\type = #LPAREN
    advance()
    
    If *Fun = #Null
      Debug PeekS(*currentC) + ":" + PeekS(*left\string)
      End
    EndIf
    
    gIsNative = *Fun\id
    
    Define.i argCount = argumentList()
    
    If *Fun\inlineyes
	    Define.Interpreter::Chunk *chunk = *Fun\fObj\chunk
							      
	    If Not *chunk\code
			  Define *currentFunction = ByteCode::*gFunction 
        compileFunction(*Fun)
				ByteCode::*gFunction = *currentFunction
			EndIf
	
      runInlining(*Fun\fObj, *chunk)
    Else
      
      If *Fun\id <> -1
        ByteCode::EmitOp(#OP_CALL_NATIVE)
        ByteCode::EmitByte(*Fun\ParamCount)
        ByteCode::EmitLong(*Fun\id)
        ;If *Fun\Type = #SUBR
        ;  ByteCode::EmitOp(#OP_POP)
        ;EndIf
        expressionResultUsed = 1
        gIsNative = -1
      Else
        Define.s klass = PeekS(*Fun\class)+":"+PeekS(*S\name)
        
        If *Fun\isParallel
          Debug "parallel function: " + klass 
          Debug argCount
          ByteCode::EmitOp(#OP_SPAWN)
        Else
          ByteCode::EmitOp(#OP_INVOKE)
        EndIf
        
        ByteCode::EmitString(@klass)
        ByteCode::EmitByte(argCount)
        expressionResultUsed = 1
        ;If gThisEnabled
        ;  gThisEnabled = 0
          ;ByteCode::EmitOp(#OP_POP)
       ; EndIf
		  EndIf
		EndIf
  Else
    ByteCode::EmitOP(#OP_GET_PROPERTY)
    
    If *S\kind = SymbolTable::#SKind_Function
      ByteCode::EmitLong(*Fun\currentProperty * SizeOf(Quad))
    Else
       Define.SymbolTable::SVariable *Var = *S
       ByteCode::EmitLong(*Var\refOffset * SizeOf(Quad))
    EndIf
  EndIf
EndProcedure

Procedure namedVariable(*name.Token, canAssign.i)
  If noLocal = 1
    ProcedureReturn 
  EndIf
  
  Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*name\string)
  
  If *S = #Null
    Debug PeekS(*name\string)
    Debug PeekS(symboltable::getCurrentScope())
    End
  EndIf

  Select *S\kind
    Case SymbolTable::#SKind_CONST
    	circularReference = 0
			handleValue(*S)
		  CIRCULAR_ERROR(*S\name)
    Case SymbolTable::#SKind_Variable
      Define.SymbolTable::SVariable *Var = *S
      
      If *T And *T\type = #EQU And canAssign
        advance()
        parsePrecedence(#PREC_ASSIGNMENT)
        ByteCode::EmitOP(#OP_SET_LOCAL)
        ByteCode::EmitLong(*Var\refOffset * SizeOf(quad))
      ElseIf *Var\dimensions = 0
        ByteCode::EmitOP(#OP_GET_LOCAL)
        ByteCode::EmitLong(*Var\refOffset * SizeOf(quad))
        
        If *Var\refOffset = 0
          gThisEnabled = 1
        EndIf
      EndIf
  EndSelect
EndProcedure

Procedure super_()
  consume(#DOT, "Expect '.' after Super.")
  consume(#IDENFIER, "Expect SuperClass variable/function.")
  
  Define *name = *P\string
  Define.Token _token
  _token\string = @gTHIS
  
  namedVariable(@_token, 0)
  
  Define.s klass = symbolTable::getCurrentClass()
  Define.SymbolTable::SClass *C = SymbolTable::lookupSymbol(@klass)
  Define.SymbolTable::SClass *Super = *C\superclass
  Define.s super = PeekS(*Super\name)+":"+PeekS(*name)
    
  If *T And *T\type = #LPAREN
    advance()
    
    Define.i argCount = argumentList()
    
    ByteCode::EmitOP(#OP_SUPER_INVOKE)
    ByteCode::EmitString(@super)
    ByteCode::emitByte(argCount)
  Else
    Error::Display(*P\fileName, *P\line, "Super variable access is not done by keyword 'super' but 'this'")
  EndIf
EndProcedure

Procedure ternary()
  Define.i then_jump = Bytecode::emitJump(#OP_JUMP_IF_FALSE)
  Bytecode::EmitOp(#OP_POP)
  
  parsePrecedence(#PREC_ASSIGNMENT)
  
  Define.i else_jump = Bytecode::emitJump(#OP_JUMP)
  
  Bytecode::patchJump(then_jump)
  Bytecode::EmitOp(#OP_POP)
  
  consume(#COLON, "Expect ':' for ? ternary operator after first value.")
  
  parsePrecedence(#PREC_ASSIGNMENT)
  
  Bytecode::patchJump(else_jump)
EndProcedure

Procedure parsePrecedence(precedence.i)
  ; consume current token as prefix (moves *P)
  advance()
  If *P = #Null
    ProcedureReturn
  EndIf
  
  Define.i canAssign = Bool(precedence <= #PREC_ASSIGNMENT)
  
  ; Prefix dispatch (literals, unary, grouping)
  Select *P\Type
    Case #IDENFIER
      namedVariable(*P, canAssign)
    Case #TSTRING
      string()
    Case #TINTEGER, #TDOUBLE
      number()
    Case #NEGATE, #MINUS, #NOT, #NEW
      unary()
    Case #LPAREN
      grouping()
    Case #SUPER
      super_()
    Default
      Debug *p\type
      Error::Display(*P\filename, *P\line, "Expect end of expression.")
      ProcedureReturn 
  EndSelect
  
  Define.Token *left = *P
  
  ; Now handle infix operators while their precedence is >= requested
  While *T <> #Null
    Define.i curType = *T\Type
    Define.i rule = getRule(curType)
    
    If rule = #PREC_NONE Or precedence > rule
      Break
    EndIf

    advance()
    If *P = #Null
      ProcedureReturn
    EndIf
    
    Select *P\type
      Case #QUESTION
        ternary()
      Case #DOT
        dot(*left, canAssign)
      Case #LPAREN
        If *left\Type = #IDENFIER
        call(canAssign)
      Else
        Error::Display(*P\fileName, *P\line, "Invalid call target")
      EndIf
      Case #AND
        and_()
      Case #OPERATOR_OR
        or_()
      Default
        binary(*left)
    EndSelect
  Wend
  
  If *T And *T\type = #EQU And canAssign
    advance()
    Error::Display(*T\fileName, *T\line, "Invalid assignment target.")
  EndIf
EndProcedure

Procedure debugStatement()
  advance()
  parsePrecedence(#PREC_ASSIGNMENT)
  ByteCode::EmitOp(#OP_PRINT)
EndProcedure

Procedure ifStatement()
  ; parse initial condition
  advance()
  expressionResultUsed = 1
  parsePrecedence(#PREC_ASSIGNMENT)

  ; emit conditional jump for the IF
  Define.i thenJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)

  ; ALWAYS pop the condition result before compiling the true branch
  ByteCode::EmitOp(#OP_POP)
  
  Define.i multi = 0
  ; compile the IF true branch
  If *T\type = #THEN
    advance()
    block()
  Else
    multiBlock(#ENDIF, #ELSEIF, #ELSE)
    multi = 1
  EndIf

  ; collect end-jumps to patch later
  NewList jumpsList()
  Define.i endJump = ByteCode::emitJump(#OP_JUMP)
  AddElement(jumpsList())
  jumpsList() = endJump

  ; patch the IF conditional jump to point to the current location
  ; (this is the start of ELSEIF / ELSE / ENDIF)
  ByteCode::patchJump(thenJump)
  ByteCode::EmitOp(#OP_POP)
  ; -----------------------
  ; handle any number of ELSEIFs
  ; -----------------------
  While *T And *T\type = #ELSEIF
    advance()                       ; consume ELSEIF
    parsePrecedence(#PREC_ASSIGNMENT)

    ; emit conditional jump for this ELSEIF
    Define.i elseifThenJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)

    ; pop condition value before compiling this elseif's true branch
    ByteCode::EmitOp(#OP_POP)

    ; compile the ELSEIF true branch
    If *T\type = #THEN
      advance()
      block()
    Else
      multiBlock(#ENDIF, #ELSEIF, #ELSE)
    EndIf

    ; emit jump to skip remaining branches and save it
    Define.i thisEndJump = ByteCode::emitJump(#OP_JUMP)
    AddElement(jumpsList())
    jumpsList() = thisEndJump

    ; patch the ELSEIF conditional jump to start of next branch
    ByteCode::patchJump(elseifThenJump)
  Wend

  ; optional final ELSE
  If *T And *T\type = #ELSE
    advance()
    If multi = 0
      block()
    Else
      multiBlock(#ENDIF, #ELSEIF, #ELSE)
    EndIf
  EndIf

  ; patch all end-jumps to the instruction immediately after the whole if/elseif/else chain
  ForEach jumpsList()
    ByteCode::patchJump(jumpsList())
  Next
EndProcedure

Procedure repeatStatement()
  advance()
  
  Define.i loopStart = ByteCode::*gFunction\chunk\count
  control_top + 1
  
  Define.i type = multiBlock(#FOREVER, #UNTIL, #INVALID)
  Define.i exitJump = 0
  
  If type = #UNTIL
    advance()
    parsePrecedence(#PREC_ASSIGNMENT)
    
    exitJump = ByteCode::emitJump(#OP_JUMP_IF_TRUE)
    ByteCode::EmitOp(#OP_POP)
  EndIf
  
  ByteCode::emitLoop(loopStart)
  
  If exitJump <> 0
    ByteCode::patchJump(exitJump)
    ByteCode::EmitOp(#OP_POP)
  EndIf
  
  control_top - 1
  ForEach loops()
    ByteCode::patchJump(loops())
  Next
  ClearList(loops())
EndProcedure

Procedure whileStatement()
  advance()
  
  Define.i loopStart = ByteCode::*gFunction\chunk\count
  control_top + 1
  
  parsePrecedence(#PREC_ASSIGNMENT)
  
  Define.i exitJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)
  ByteCode::EmitOp(#OP_POP)
  
  multiBlock(#ENDWHILE, #INVALID, #INVALID)
  
  ByteCode::EmitOp(#OP_POP)
  ByteCode::emitLoop(loopStart)
  
  ByteCode::patchJump(exitJump)
  ByteCode::EmitOp(#OP_POP)
  
  control_top - 1
  ForEach loops()
    ByteCode::patchJump(loops())
  Next
  ClearList(loops())
EndProcedure

Procedure forStatement()
  advance()
  consume(#IDENFIER, "Expect idenfier name after 'For'")
  Define *Name = *P\string
  consume(#EQU, "Expect = after 'Idenfier'")
  
  parsePrecedence(#PREC_ASSIGNMENT)
  
  Define.SymbolTable::SVariable *Var = SymbolTable::LookupSymbol(*Name)
  
  If *Var = #Null
    
  EndIf
  
  ByteCode::EmitOP(#OP_SET_LOCAL)
	ByteCode::EmitLong(*Var\refOffset*SizeOf(quad))
	ByteCode::emitOp(#OP_POP)
  
  consume(#TO, "Expect To after 'expression'")
  
  Define.i loopStart = ByteCode::*gFunction\chunk\count
  Define.i exitJump = -1
  control_top + 1
  
  ByteCode::EmitOP(#OP_GET_LOCAL)
	ByteCode::EmitLong(*Var\refOffset*SizeOf(quad))

  parsePrecedence(#PREC_ASSIGNMENT)
  
  Define.i stepValue = 1, sign = 1
  Define.i JUMP_OP = #OP_JUMP_IF_TRUE, GREATER_OP = #OP_GREATER
  
  If *T And *T\type = #STEP
    advance()
    
    If *T And *T\type = #NEGATE
      advance()
      sign = -1
    EndIf
    
    consume(#TINTEGER, "Expect integer constant after 'step'")
    stepValue = *P\Integer
    
    If sign = -1
      stepValue = -stepValue
      JUMP_OP = #OP_JUMP_IF_FALSE
      GREATER_OP = #OP_GRT_EQU
    EndIf
  EndIf
  
  ByteCode::EmitOP(GREATER_OP)
  exitJump = ByteCode::emitJump(JUMP_OP)
  ByteCode::emitOp(#OP_POP)
  
  multiBlock(#NEXT, #INVALID, #INVALID)
  
	ByteCode::EmitOP(#OP_GET_LOCAL)
	ByteCode::EmitLong(*Var\refOffset*SizeOf(quad))
	ByteCode::EmitOP(#OP_CONSTANT)
	ByteCode::EmitInteger(stepValue)
	ByteCode::EmitOP(#OP_ADD)
	ByteCode::EmitOP(#OP_SET_LOCAL)
	ByteCode::EmitLong(*Var\refOffset*SizeOf(quad))
	ByteCode::emitOp(#OP_POP)
	
	ByteCode::emitLoop(loopStart)
	
  ByteCode::patchJump(exitJump)
  ByteCode::emitOp(#OP_POP)
  
  control_top - 1
  ForEach loops()
    ByteCode::patchJump(loops())
  Next
  ClearList(loops())
EndProcedure

Procedure breakStatement()
  advance()
  
  If control_top < 0
    Error::Display(*p\fileName, *p\line, "Cant use 'break' outside of a loop statement")
    ProcedureReturn 
  EndIf
  
  AddElement(loops())
  loops() = ByteCode::EmitJump(#OP_JUMP)
EndProcedure

Procedure continueStatement()
  advance()
  
  If control_top < 0
    Error::Display(*p\fileName, *p\line, "Cant use 'continue' outside of a loop statement")
    ProcedureReturn
  EndIf
  
  AddElement(loops())
  loops() = ByteCode::EmitJump(#OP_JUMP)
EndProcedure

Procedure returnStatement()
  advance()
  
  expressionResultUsed = 1
  parsePrecedence(#PREC_ASSIGNMENT)
  ByteCode::EmitOp(#OP_RETURN)
EndProcedure

Procedure multiBlock(endType.i, skipType.i, skipType2.i)
  While *T <> #Null And *T\type <> endType
    block()
    
    If *T And (*T\type = skipType Or *T\type = skipType2)
      ProcedureReturn *T\type
    ElseIf Not *T
      Break
    EndIf
  Wend
  
  consume(endType, "Expect: '"+TokenList::TokenText(endType)+"'after a block")
  ProcedureReturn 0
EndProcedure

Procedure redimStatement()
  advance()
  consume(#IDENFIER, "Expect idenfier after ReDim")
  
  Define *string = *P\string
  Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(*string)
  
  If *S And *S\kind = SymbolTable::#SKind_Variable
    Define.SymbolTable::SVariable *V = *S
    
    If *V\arrayInit = 0
      ByteCode::EmitOp(#OP_CONSTANT)
      ByteCode::EmitInteger(8+(*V\dimensions * (*V\dimensionSize)) * 8)
      ByteCode::EmitOp(#OP_ALLOC_ARRAY)
      ByteCode::EmitByte(*V\refOffset)
      ByteCode::EmitByte(*V\dimensions)
      *V\arrayInit = 1
    EndIf
      
    Bytecode::EmitOp(#OP_REDIM)
    ByteCode::EmitByte(*V\refOffset)
  EndIf
EndProcedure

Procedure yieldStatement()
  advance()
  Bytecode::EmitOp(#OP_YIELD)
EndProcedure

Procedure stopStatement()
  advance()
  Bytecode::EmitOp(#OP_STOP)
EndProcedure

Procedure selectStatement()
  advance()
  parsePrecedence(#PREC_ASSIGNMENT)
  
  Define hasCase = #False, hasDefault = #False
  
  While *T And *T\type <> #ENDSELECT
    
    If *T And *T\type = #CASE
      advance()
      hasCase = #True
      ByteCode::EmitOp(#OP_DUP)
      parsePrecedence(#PREC_ASSIGNMENT)
      ByteCode::EmitOp(#OP_EQUAL)
      Define caseJump = ByteCode::EmitJump(#OP_JUMP_IF_FALSE)
      ByteCode::EmitOp(#OP_POP)
      While *T And *T\type <> #CASE And *T\type <> #DEFAULT And *T\type <> #ENDSELECT
        block()
        
        If *T And (*T\type = #CASE Or *T\type = #DEFAULT)
          AddElement(loops())
          loops() = ByteCode::EmitJump(#OP_JUMP)
        EndIf
      Wend
      ByteCode::patchJump(caseJump)
      ByteCode::EmitOp(#OP_POP)
    ElseIf *T And *T\type = #DEFAULT
      advance()
      If hasDefault
        Error::Display(*T\filename, *T\line, "Multiple 'default' keywords used in one select")
      EndIf
      hasDefault = #True
      While *T And *T\type <> #CASE And *T\type <> #DEFAULT And *T\type <> #ENDSELECT
        block()
      Wend
    Else
      Error::Display(*T\filename, *T\line, "Expect 'case' or 'default'")
      synchronize()
    EndIf
  Wend
  
  ForEach loops()
    ByteCode::patchJump(loops())
  Next
  ClearList(loops())
  
  consume(#ENDSELECT, "Expect 'EndSelect' at end of the select .. case")
  ByteCode::EmitOp(#OP_POP)
EndProcedure

Procedure block()
  If *T = #Null
    ProcedureReturn
  EndIf
  
  Select *T\type
    Case #INIT_VAR
      Define.i value, start = *T\Integer
      advance()
      Define.i _end = *T\Integer
      advance()

       For value = start To _end
         ByteCode::EmitOp(#OP_CONSTANT)
         ByteCode::EmitInteger(0)
         ByteCode::EmitOP(#OP_SET_LOCAL)
         ByteCode::EmitLong(value * SizeOf(quad))
         ByteCode::EmitOP(#OP_POP)
       Next
    Case #SKIP
      advance()
    Case #DEBUG
      debugStatement()
    Case #IF
      ifStatement()
    Case #WHILE
      whileStatement()
    Case #FOR
      forStatement()
    Case #REPEAT
       repeatStatement()
    Case #RETURN
      returnStatement()
    Case #BREAK
      breakStatement()
    Case #CONTINUE
      continueStatement()
    Case #SELECT
      selectStatement()
    Case #YIELD
      yieldStatement()
    Case #STOP
      stopStatement()
    Case #REDIM
      redimStatement()
    Case #END
      advance()
      ByteCode::EmitOp(#OP_HALT)
    Default
      expressionResultUsed = 0
      parsePrecedence(#PREC_ASSIGNMENT)
     
     If Not expressionResultUsed
      ByteCode::EmitOP(#OP_POP)
    EndIf
  EndSelect
  
  If Error::panicMode 
    synchronize()
  EndIf
EndProcedure

Procedure PASS2()
  PrintN("PASS1 done.")
  
  NewList emptyList()
  AddElement(emptyList())
  emptyList() = -1
  
  Define.LinkedList::Node *current = SymbolTable::getResolveConstants(0)
  Define.SymbolTable::SConstant *C
  Define.Token *token
  
  While *current
    *current = handleLinkedList(*current, *current\value, 0)
  Wend
  
  Define.i circular = 0
  Define *Symbol
  
  While Not SymbolTable::isResolveEmpty()
    If circular > #MAX_CIRCULAR
      Error::Display(@"", -1, "Detected circular reference at the " + SymbolTable::getLastResolve() + "declaration")
      Break
    EndIf
    
    *current = SymbolTable::getResolveConstants(1)
    
    While *current
      *current = handleLinkedList(*current, *current\value, 1)
    Wend
    circular = circular + 1
  Wend
  
  *current = *functions\head
  Define.SymbolTable::SFunction *F
  Define.i isMain = #False
  
  SymbolTable::SetScope(@"")
  
  While *current
    *F = *current\value
    
    If *F 
      ByteCode::*gFunction = *F\fObj 
      
      If CompareMemoryString(*F\name, @"main") = 0
        isMain = #True
        ByteCode::*gMainFunction = *F\fObj
        ByteCode::gMainClass = PeekS(*F\class)
        ByteCode::mainEmitted = #True
      EndIf
      
      *F\addr = ByteCode::ip
      *N = *F\entry
      
      If *F\module
        SymbolTable::EnterScope(*F\module)
        SymbolTable::SetModule(*F\module)
      EndIf
      
      If *F\class
        SymbolTable::EnterScope(*F\class)
      EndIf
      
      SymbolTable::EnterScope(*F\name)
      SymbolTable::SetFunction(*F\name)
      
      advance()
      
      While *T <> #Null
        block()
      Wend
      
      If isMain
        ByteCode::EmitOp(#OP_HALT)
        isMain = 0
      Else
        ByteCode::EmitOp(#OP_NIL)
        ByteCode::EmitOp(#OP_RETURN)
      EndIf
      
      SymbolTable::LeaveScope()
      
      If *F\class
        SymbolTable::LeaveScope()
      EndIf
      
      If *F\module
        SymbolTable::LeaveScope()
      EndIf
    EndIf
    
    *current = *current\next
  Wend
  
  If Not ByteCode::mainEmitted
    Error::Display(@"", 0, "main function/sub is missing.")
  EndIf
  
  PrintN("PASS2 done.")
EndProcedure
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 1097
; FirstLine = 1069
; Folding = --------
; EnableXP
; DPIAware