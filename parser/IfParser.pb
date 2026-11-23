Declare  RepeatParser(*next.LinkedList::Node)
Declare  WhileParser(*next.LinkedList::Node)
Declare  ForParser(*next.LinkedList::Node)

Macro statement(then)
  Select *token\type
    Case #BREAK
      If ListSize(breakList()) > 0 And breakList() = -1
        Error::Display(*token\line, "Break keyword used out of Repeat/While/For loop", #PB_Compiler_Line)   
      Else
        AddElement(breakList())
        breakList() = ByteCode::EmitJump(#OP_JUMP)
      EndIf
    Case #CONTINUE
      If cont = 0
        Error::Display(*token\line, "Continue keyword used out of Repeat/While/For loop", #PB_Compiler_Line)  
      Else
         AddElement(ContinueList())
         ContinueList() = ByteCode::EmitJump(#OP_JUMP)
      EndIf
    Case #CDELETE
	    *next = DeleteParser(*next\next)
	  Case #RETURN
		  *next = ReturnParser(*next\next)
		Case #END
		  ByteCode::EmitOp(#OP_HALT)
		Case #IDENFIER
		  Define *string = *token\string
		  Define.Token *tmpTok = *Next\next\value 
		  
		  If CompareMemoryString(*string, @"this", #PB_String_NoCase) = 0
		    *next = ThisParser(*next)
		  ElseIf *tmpTok And *tmpTok\type = #DOT
		    moveNext()
		    moveNext()
		    expect(*token, #IDENFIER, #PB_Compiler_Line)
		    Define *string2 = *token\string
		    moveNext()
		    
		    Define.s str = PeekS(*string)
		    Define.SymbolTable::SVariable *SSVar = SymbolTable::LookupSymbol(@str)
		    
		    Define.SymbolTable::SVariable *Var = *SSVar
		    
		    If *SSVar = 0
		      Error::Display(*token\line, "Idenfier: "+str+" is not defined.", #PB_Compiler_Line)
		      SymbolTable::LeaveScope()
		      SymbolTable::resetScope()
		      ProcedureReturn
		    EndIf
		    
		    If *SSVar\kind = SymbolTable::#SKind_Variable
		      Define.SymbolTable::SClass *class = *SSVar\sym
		    Else
		      Define.SymbolTable::SClass *class = *SSVar
		    EndIf
		    
		    str = PeekS(*class\name)+":"+PeekS(*string2)
		    *SSVar = SymbolTable::LookupSymbol(@str)
		    
		    If *SSVar = 0
		      Error::Display(*token\line, "Idenfier "+PeekS(*string2)+" is not defined in a Class: "+PeekS(*class\name), #PB_Compiler_Line)
		      SymbolTable::LeaveScope()
		      SymbolTable::resetScope()
		      ProcedureReturn
		    EndIf
		    
		    Select *token\type
		      Case #EQU
		        *next = Assignment(*SSVar, *next)
		      Default
		        Define.SymbolTable::SFunction *F = *SSVar
		        If *F\id = -1 
		          If *Var\RefKind = SymbolTable::#REF_local
		            ByteCode::EmitOP(#OP_GET_LOCAL)
      				  ByteCode::EmitLong(*Var\refOffset*SizeOf(Interpreter::Value))
		          Else
		            ByteCode::EmitOp(#OP_GET_GLOBAL)
		            Define.s className = PeekS(*class\name)+":i"
		            ByteCode::EmitString(@className) 
		          EndIf
		          *next = FunctionCallDeclared(*next\next, *F, 1)
		          ByteCode::EmitOp(#OP_INVOKE)
		          ByteCode::EmitString(@str)
		          ByteCode::EmitByte(*F\ParamCount)
		        Else
		          *next = FunctionCallDeclared(*next\next, *F)
		        EndIf
		    EndSelect
		    
		  Else
		    StatementParser(*token, *next, then#V)
		  EndIf
		Default			
			StatementParser(*token, *next, then)
	EndSelect
	moveNext()
EndMacro

Macro multiStatement(endif_)
  While *token And *token\type <> #ENDIF
    Select *token\type
      Case #BREAK
        If ListSize(breakList()) > 0 And breakList() = -1
          Error::Display(*token\line, "Break keyword used out of Repeat/While/For loop", #PB_Compiler_Line)  
        Else
          AddElement(breakList())
          breakList() = ByteCode::EmitJump(#OP_JUMP)
        EndIf
      Case #CONTINUE
        If cont = 0
          Error::Display(*token\line, "Continue keyword used out of Repeat/While/For loop", #PB_Compiler_Line)  
        Else
          AddElement(ContinueList())
          ContinueList() = ByteCode::EmitJump(#OP_JUMP)
        EndIf
      Case #RETURN
        *next = ReturnParser(*next\next)
      Case #CDELETE
        *next = DeleteParser(*next\next)
      Case #END
        ByteCode::EmitOp(#OP_HALT)
      Case #IF
        *next = IfParser(*next, breakList(), continueList())
      Case #WHILE
        *next = WhileParser(*next)
      Case #REPEAT
        *next = RepeatParser(*next)
      Case #FOR
        *next = ForParser(*next)
      Case #ELSE, #endif, #elseif
        Break
      Case #IDENFIER
		    Define *string = *token\string
		    Define.Token *tmpTok = *Next\next\value 
		  
  		  If CompareMemoryString(*string, @"this", #PB_String_NoCase) = 0
  		    *next = ThisParser(*next)
  		  ElseIf *tmpTok And *tmpTok\type = #DOT
  		    moveNext()
  		    moveNext()
  		    expect(*token, #IDENFIER, #PB_Compiler_Line)
  		    Define *string2 = *token\string
  		    moveNext()
  		    
  		    Define.s str = PeekS(*string)
  		    Define.SymbolTable::SVariable *SSVar = SymbolTable::LookupSymbol(@str)
  		    
  		    If *SSVar = 0
  		      Error::Display(*token\line, "Idenfier: "+str+" is not defined.", #PB_Compiler_Line)
  		      SymbolTable::LeaveScope()
  		      SymbolTable::resetScope()
  		      ProcedureReturn
  		    EndIf
  		    
  		    If *SSVar\kind = SymbolTable::#SKind_Variable
  		      Define.SymbolTable::SClass *class = *SSVar\sym
  		    Else
  		      Define.SymbolTable::SClass *class = *SSVar
  		    EndIf
  		    
  		    If *class = #Null
  		      
  		    EndIf
  		    
  		    str = PeekS(*class\name)+":"+PeekS(*string2)
  		    *SSVar = SymbolTable::LookupSymbol(@str)
  		    
  		    If *SSVar = 0
  		      Error::Display(*token\line, "Idenfier "+PeekS(*string2)+" is not defined in a Class: "+PeekS(*class\name), #PB_Compiler_Line)
  		      SymbolTable::LeaveScope()
  		      SymbolTable::resetScope()
  		      ProcedureReturn
  		    EndIf
  		    
  		    Select *token\type
  		      Case #EQU
  		        *next = Assignment(*SSVar, *next)
  		      Default
  		        Define.SymbolTable::SFunction *F = *SSVar
  		        If *F\id = -1 
  		          ByteCode::EmitOp(#OP_GET_GLOBAL)
  		          Define.s className = PeekS(*class\name)+":i"
  		          ByteCode::EmitString(@className) 
  		          *next = FunctionCallDeclared(*next\next, *F, 1)
  		          ByteCode::EmitOp(#OP_INVOKE)
  		          ByteCode::EmitString(@str)
  		          ByteCode::EmitByte(*F\ParamCount)
  		        Else
  		          *next = FunctionCallDeclared(*next\next, *F)
  		        EndIf
  		    EndSelect
  		  Else
  		    StatementParser(*token, *next, endif_#V)
  		  EndIf		  
    EndSelect
    moveNext()
  Wend
EndMacro

Declare IfParser(*next.LinkedList::Node, List breakList(), List continueList(), cont.i = 0)

Global NewList elseIfs()

Procedure ElseIfParser(*next.LinkedList::Node, List breakList(), List continueList(), cont.i)
  returnIfNull()

  ; --- Parse initial IF condition ---
  Define.Token *tempToken = *next\next\value
  
  Select *tempToken\type
    Case #IDENFIER
      Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*tempToken\string)
      If *Sym
        Define.SymbolTable::SVariable *Var = *Sym
        finalEvalType = *Var\type
      EndIf
    Default
      finalEvalType = *tempToken\type
  EndSelect
  
  *next = LogicalExp(*next\next)
  
  ; --- Jump if condition is false ---
  Define.i falseJump = ByteCode::EmitJump(#OP_JUMP_IF_FALSE)
  ByteCode::EmitOP(#OP_POP)

  ; --- Parse THEN block ---
  Define.Token *token = *next\value
  Define.i thenDefined = 0
 
  If *token
    If *token\type = #THEN
      thenDefined = 1
      moveNext()
      statement(then)
    Else
      Define.Token *tmpToken = *next\prev\value
      If *tmpToken\type = #THEN
        thenDefined = 1
        statement(thenTwo)
      Else
        multiStatement(then_endif)
      EndIf
    EndIf
  EndIf
  
  ByteCode::EmitOp(#OP_NOP)
  ; --- After executing THEN, skip rest of IF chain ---
  AddElement(elseIfs())
  elseIfs() = ByteCode::EmitJump(#OP_JUMP)
  ; --- Patch IF false branch to jump here (start of ELSEIF/ELSE) -
  ByteCode::patchJump(falseJump)
  ByteCode::EmitOP(#OP_POP)
  
  ProcedureReturn *next
EndProcedure

Procedure IfParser(*next.LinkedList::Node, List breakList(), List continueList(), cont.i = 0)
  returnIfNull()

  ; --- Parse initial IF condition ---
  Define.Token *tempToken = *next\next\value
  
  Select *tempToken\type
    Case #IDENFIER
      Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*tempToken\string)
      If *Sym
        Define.SymbolTable::SVariable *Var = *Sym
        finalEvalType = *Var\type
      EndIf
    Default
      finalEvalType = *tempToken\type
  EndSelect
  
  *next = LogicalExp(*next\next)
  
  Define.Token *temp = *next\value

  ; --- Jump if condition is false ---
  ByteCode::EmitOP(#OP_NOP)
  Define.i falseJump = ByteCode::EmitJump(#OP_JUMP_IF_FALSE)
  ByteCode::EmitOP(#OP_POP)

  ; --- Parse THEN block ---
  Define.Token *token = *next\value
  Define.i thenDefined = 0
  
  If *token
    If *token\type = #THEN
      thenDefined = 1
      moveNext()
      statement(then)
    Else
      Define.Token *tmpToken = *next\prev\value
      If *tmpToken\type = #THEN
        thenDefined = 1
        statement(thenTwo)
      Else
        multiStatement(then_endif)
      EndIf
    EndIf
  EndIf

  ; --- After executing THEN, skip rest of IF chain ---
  Define.i endJump = ByteCode::EmitJump(#OP_JUMP)

  ; --- Patch IF false branch to jump here (start of ELSEIF/ELSE) ---
  ByteCode::patchJump(falseJump)
  ;ByteCode::EmitOP(#OP_POP)

  ; --- Process next possible branch ---
  *token = *next\value
  
  If *token\type = #ELSEIF
    redo_elseif:
    *next = ElseIfParser(*next, breakList(), continueList(), cont)
    ByteCode::EmitOP(#OP_NOP)
    *token = *next\value
    If *token\type = #ELSEIF
      Goto redo_elseif
    EndIf
  EndIf

  ; --- Handle ELSE block ---
  If *token\type = #ELSE
    ByteCode::patchJump(endJump)
    ByteCode::EmitOP(#OP_POP)
    
    moveNext()
    If thenDefined
      statement(ElseLabel)
      *next = *next\prev
    Else
      multiStatement(else_endif)
    EndIf
    
    ForEach elseIfs()
      ByteCode::patchJump(elseIfs())
    Next
  
    ClearList(elseIfs())
    ;ProcedureReturn *next
  Else
    *next = *next\prev
    *token = *next\value
  EndIf

  ; --- Patch end-of-IF jump (from THEN section) ---
  ByteCode::patchJump(endJump)
  ByteCode::EmitOP(#OP_POP)
  
  ForEach elseIfs()
    ByteCode::patchJump(elseIfs())
  Next
  
  ClearList(elseIfs())
  
  ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 75
; FirstLine = 40
; Folding = -
; EnableXP
; DPIAware