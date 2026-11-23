Procedure RepeatParser(*next.LinkedList::Node)
  NewList breakList()
  NewList continueList()
	Define.Token *token
	returnIfNull()
	moveNext()
	
	ByteCode::EmitOp(#OP_NOP)
	Define.i loopStart = ByteCode::*gFunction\chunk\count

	Define ip = ByteCode::ip
	Define untilIp = 0
	
	While *token And (*token\type <> #FOREVER And *token\type <> #UNTIL)
	  Select *token\type
	    Case #BREAK
	      AddElement(breakList())
	      breakList() = ByteCode::EmitJump(#OP_JUMP)
	    Case #CONTINUE
	      ByteCode::EmitLoop(loopStart)
			Case #END
				ByteCode::EmitOp(#OP_HALT)
			;Case #RETURN
				;*next = ReturnParser(*next\next)
			Case #CDELETE
				*next = DeleteParser(*next\next)
			;Case #IF
			;	*next = IfParser(*next, breakList(), continueList(), loopStart)
			Case #REPEAT
			  *next = RepeatParser(*next)
			;Case #WHILE
			 ; *next = WhileParser(*next)
			;Case #FOR
			;  *next = ForParser(*next)
			Case #IDENFIER
			  Define *string = *token\string
			  Debug PeekS(*string)
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
			    
			    str = PeekS(*class\name)+":"+PeekS(*string2)
			    Define.SymbolTable::SVariable *tmpVar = *SSVar
			    *SSVar = SymbolTable::LookupSymbol(@str)
			    
			    If *SSVar = 0
			      Error::Display(*token\line, "Idenfier "+PeekS(*string2)+" is not defined in a Class: "+PeekS(*class\name), #PB_Compiler_Line)
			      SymbolTable::LeaveScope()
			      SymbolTable::resetScope()
			      ProcedureReturn
			    EndIf
			    
			    Select *token\type
			      Case #EQU
			        ByteCode::EmitOP(#OP_GET_LOCAL)
			        ByteCode::EmitLong(*tmpVar\refOffset*SizeOf(Interpreter::Value))
			        *next = Assignment(*SSVar, *next)
			      Default
			        Define.SymbolTable::SFunction *F = *SSVar
			        If *F\id = -1 
			          
			          If *tmpVar\RefKind = SymbolTable::#REF_local
			            ByteCode::EmitOP(#OP_GET_LOCAL)
			            ByteCode::EmitLong(*tmpVar\refOffset*SizeOf(Interpreter::Value))
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
			          
			          If *tmpVar\RefKind = SymbolTable::#REF_local
			            ByteCode::EmitOP(#OP_GET_LOCAL)
			            ByteCode::EmitLong(*tmpVar\refOffset*SizeOf(Interpreter::Value))
			          EndIf
			          
			          *next = FunctionCallDeclared(*next\next, *F)
			        EndIf
			    EndSelect
			    
			  Else
			    StatementParser(*token, *next, repeat3_)
			  EndIf
			  
			Default
			  StatementParser(*token, *next, repeat2_)
		EndSelect
		moveNext()
	Wend
	
	If Not *token
		ProcedureReturn *next
	EndIf
	
	Define.i exitJump
	
	If *token\type = #UNTIL
		*next = *next\next
		*next = LogicalExp(*next)
		*next = *next\prev
		*token = *Next\value
		untilIp = 1
		exitJump = ByteCode::emitJump(#OP_JUMP_IF_TRUE)
		ByteCode::EmitOp(#OP_POP)
	EndIf
	
	ByteCode::emitLoop(loopStart)
	
	If ListSize(breakList()) > 0
	  ResetList(breakList())
	  While NextElement(breakList())
	    ByteCode::patchJump(breakList())
	  Wend
	  ClearList(breakList())
	EndIf
		
	If untilIp
	  ByteCode::patchJump(exitJump)
	  ByteCode::EmitOp(#OP_POP)
	EndIf
	
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 22
; Folding = -
; EnableXP
; DPIAware