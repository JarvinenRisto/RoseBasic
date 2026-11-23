Procedure ForParser(*next.LinkedList::Node)
  NewList breakList()
  NewList continueList()
  Define.Token *token
  returnIfNull()
  moveNext()
  
  expect(*token, #IDENFIER, #PB_Compiler_Line)
  Define *name = *token\string
  
	moveNext()
	expect(*token, #EQU, #PB_Compiler_Line)
	moveNext()
	
	*next = LogicalExp(*next)
	*token = *next\value
	
	Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*name)
	If *Sym And *Sym\kind <> SymbolTable::#SKind_Variable
	  Error::Display(*token\line, "Expected a Variable, but got "+SymbolTable::Kinds(*Sym\kind)+".", #PB_Compiler_Line)
	ElseIf Not *Sym
	  Error::Display(*token\line, "Variable "+PeekS(*name)+" is not declared.", #PB_Compiler_Line)
	EndIf
	
	Define.SymbolTable::SVariable *VariableV0 = *Sym
	ByteCode::EmitOP(#OP_SET_LOCAL)
	ByteCode::EmitLong(*VariableV0\refOffset*SizeOf(Interpreter::Value))
	ByteCode::emitOp(#OP_POP)
	
	expect(*token, #TO, #PB_Compiler_Line)
	moveNext()
	
  Define.i loopStart = ByteCode::*gFunction\chunk\count
  Define.i exitJump = -1
  
  ByteCode::EmitOP(#OP_GET_LOCAL)
	ByteCode::EmitLong(*VariableV0\refOffset*SizeOf(Interpreter::Value))
	Debug *VariableV0\refOffset*SizeOf(Interpreter::Value)
	
  *next = LogicalExp(*next)
  *token = *next\value
  
  Define.i stepValue = 1, sign = 1
  Define.i JUMP_OP = #OP_JUMP_IF_TRUE, GREATER_OP = #OP_GREATER
  If *token And *token\type = #STEP
    moveNext()
    If *token\type = #NEGATE
      moveNext()
      sign = -1
    EndIf
    expect(*token, #TINTEGER, #PB_Compiler_Line)
    stepValue = *token\integer
    If sign = -1
      stepValue = -stepValue
      JUMP_OP = #OP_JUMP_IF_FALSE
      GREATER_OP = #OP_GRT_EQU
    EndIf
    moveNext()
  EndIf
  
	ByteCode::EmitOP(GREATER_OP)
  exitJump = ByteCode::emitJump(JUMP_OP)
  ByteCode::emitOp(#OP_POP)

	While *token And *token\type <> #NEXT
    Select *token\type
      Case #BREAK
        AddElement(breakList())
        breakList() = ByteCode::EmitJump(#OP_JUMP)
      Case #CONTINUE
        AddElement(ContinueList())
        ContinueList() = ByteCode::EmitJump(#OP_JUMP)
			Case #END
				ByteCode::EmitOp(#OP_HALT)
			Case #RETURN
				*next = ReturnParser(*next\next)
			Case #CDELETE
				*next = DeleteParser(*next\next)
			Case #IF
				*next = IfParser(*next, breakList(), continueList(), loopStart)
			Case #REPEAT
			  *next = RepeatParser(*next)
			Case #WHILE
			  *next = WhileParser(*next)
			Case #FOR
			   *next = ForParser(*next)
			 Default			
			   If *token > 1000 And *token\type = #endif
          Goto skip_repeat
        EndIf

  expect(*token, #IDENFIER, #PB_Compiler_Line)

  Define *string = *token\string
  Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*token\string)

  If *Sym > 1000 And *Sym\name > 1000
    If CompareMemoryString(*Sym\name, @"this", #PB_String_NoCase) = 0
      *next = ThisParser(*next)
      Goto skip_repeat
    EndIf

    moveNext()
    *gCurrentVar = *Sym

    Select *token\type
      Case #DOT
        Define.i found = 0

        If *Sym\kind <> SymbolTable::#SKIND_CLASS
          If *Sym\kind = SymbolTable::#SKIND_VARIABLE
            Define.SymbolTable::SVariable *Var = *Sym
            If *Var > 1000 And *Var\Sym > 1000
              *Sym = *Var\Sym

              Select *Var\RefKind
                Case SymbolTable::#Ref_Class
                  ByteCode::EmitOP(#OP_GET_LOCAL)
                  ByteCode::EmitLong(0)
                  ByteCode::EmitOP(#OP_GET_PROPERTY)
                  ByteCode::EmitInteger(*Var\refOffset)
                Case SymbolTable::#Ref_Local
                  ByteCode::EmitOP(#OP_GET_LOCAL)
                  ByteCode::EmitLong(*Var\refOffset * SizeOf(Interpreter::Value))
                Case SymbolTable::#ref_parameter
                  ; käsittele jos tarvitaan
              EndSelect

              found = 1
            Else
              Error::display(*token\line, "Identifier "+PeekS(*token\string)+" is not a class instance", #PB_Compiler_Line)
              While *token > 1000
                moveNext()
              Wend
            EndIf
          ElseIf *token > 1000
            Error::display(*token\line, "Identifier "+PeekS(*token\string)+" is not a class", #PB_Compiler_Line)
            While *token > 1000
              moveNext()
            Wend
          EndIf
        EndIf

        moveNext()
        expect(*token, #IDENFIER, #PB_Compiler_Line)

        Define.SymbolTable::SClass *gC = *Sym
        If *gC > 1000 And *gC\functions > 1000 And *token\string > 1000
          *newClass = *gC
          Define.LinkedList::Node *cur = *gC\functions\head

          While *cur > 1000
            Define.SymbolTable::SSymbol *gS = *cur\value
            If *gS > 1000 And CompareMemoryString(*gS\name, *token\string, #PB_String_NoCase) = 0
              *Sym = *gS
              moveNext()
              parseClassVar(*token)
              found = 1
              Break
            EndIf
            *cur = *cur\next
          Wend
        EndIf
        
        Define vString.s
        If *token\string > 1000
          vString.s = PeekS(*string) + ":" + PeekS(*token\string)
        EndIf
        
        Define.SymbolTable::SVariable *vSym = SymbolTable::LookupSymbol(@vString)
        
        If *vSym > 1000
          If *vSym\isStatic = 0
            Define.SymbolTable::SSymbol *tempSym = SymbolTable::LookupSymbol(*string)
            If *tempSym > 1000 And *tempSym\kind <> 0
              Error::Display(*token\line, "Class '"+PeekS(*gC\name)+"' variable: "+PeekS(*token\string)+" is not a Static", #PB_Compiler_Line)
            EndIf
          EndIf

          moveNext()
          Select *token\type
            Case #EQU
              *next = Assignment(*vSym, *Next)
            Default
              Debug *token\type
              End
          EndSelect

          found = 1
        EndIf

        If Not found And *token > 1000 And *token\string > 1000
          Error::display(*token\line, "Identifier "+PeekS(*token\string)+" is undefined in a class "+PeekS(*gC\name), #PB_Compiler_Line)
        EndIf

      Case #EQU
   
        If *Sym > 1000
          *next = Assignment(*Sym, *Next)
        EndIf

      Default

        If *rootLine = 0 And *Sym > 1000
          *rootLine = *Sym
        EndIf

        Expect(*token, #LPAREN, #PB_Compiler_Line)

        If *Sym\dimensionSize > 0
          Define.i i
          Define.SymbolTable::SVariable *Var = *Sym

          For i = 1 To *Sym\dimensionSize
            moveNext()
            *next = LogicalExp(*next)
            *next = *next\prev
            gArrayIndex = *Var\RefOffset

            If *Sym\arrayInit = 0
              ByteCode::EmitOp(#OP_CONSTANT)
              ByteCode::EmitInteger(SizeOf(Interpreter::Value) + (*Sym\dimensions * (*Sym\dimensionSize)) * SizeOf(Interpreter::Value))

              If *Var\refKind = SymbolTable::#Ref_Class
                ByteCode::EmitOp(#OP_ALLOC_GLOBAL_ARRAY)
              Else
                ByteCode::EmitOp(#OP_ALLOC_ARRAY)
              EndIf

              ByteCode::EmitByte(*Var\RefOffset)
              ByteCode::EmitByte(*Sym\dimensions)
              *Sym\arrayInit = 1
            EndIf

            If *Sym\dimensionSize > 1 And i <> *Sym\dimensionSize
              If *Sym\dimensions = i
                Break
              EndIf
              moveNext()
              If Expect(*token, #COMMA, #PB_Compiler_Line) = 1
                PrintN("The error was in Array "+PeekS(*Sym\name))
                Break
              EndIf
            EndIf
          Next i

          moveNext()
          Expect(*token, #RPAREN, #PB_Compiler_Line)
          moveNext()
          Expect(*token, #EQU, #PB_Compiler_Line)
          *next = Assignment(*Sym, *Next)
          Goto skip_repeat
        Else
          Define.SymbolTable::SFunction *F = *Sym

          If *F > 1000 And CompareMemoryString(*F\name, @"debug") <> 0
            ByteCode::EmitOp(#OP_GET_GLOBAL)
            ByteCode::EmitString(*F\name)
          EndIf

          *next = FunctionCallDeclared(*next\next, *Sym)
          *token = *next\value
        EndIf

        Expect(*token, #RPAREN, #PB_Compiler_Line)
        *rootLine = 0
    EndSelect

  ElseIf *token > 1000 And *token\string > 1000
    Error::display(*token\line, "Identifier "+PeekS(*token\string)+" is undefined", #PB_Compiler_Line)
  EndIf

  skip_repeat:
  *newClass = 0

		EndSelect
		moveNext()
	Wend
	
	ByteCode::EmitOP(#OP_NOP)
	Debug "size " + ListSize(ContinueList())
	
	If ListSize(ContinueList()) > 0
    ResetList(ContinueList())
    While NextElement(ContinueList())
      ByteCode::patchJump(ContinueList())
    Wend
    ClearList(ContinueList())
  EndIf
  
	ByteCode::EmitOP(#OP_GET_LOCAL)
	ByteCode::EmitLong(*VariableV0\refOffset*SizeOf(Interpreter::Value))
	ByteCode::EmitOP(#OP_CONSTANT)
	ByteCode::EmitInteger(stepValue)
	ByteCode::EmitOP(#OP_ADD)
	ByteCode::EmitOP(#OP_SET_LOCAL)
	ByteCode::EmitLong(*VariableV0\refOffset*SizeOf(Interpreter::Value))
	ByteCode::emitOp(#OP_POP)
	
  ByteCode::emitLoop(loopStart)
  
  If ListSize(breakList()) > 0
    ResetList(breakList())
    While NextElement(breakList())
      ByteCode::patchJump(breakList())
    Wend
    ClearList(breakList())
  EndIf

  ByteCode::patchJump(exitJump)
  ByteCode::emitOp(#OP_POP)
  
  ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 215
; FirstLine = 189
; Folding = -
; EnableXP
; DPIAware