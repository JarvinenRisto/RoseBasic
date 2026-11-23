Procedure GosubParser(*next.LinkedList::Node)
	Define.Token *token
	returnIfNull()
	moveNext()
	expect(*token, #idenfier, #PB_Compiler_Line)
	
	Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*token\string)
	
	If Not *Sym
		Error::Display(*token\line, "Function/Sub "+PeekS(*token\string)+" is not declared", #PB_Compiler_Line)
		ProcedureReturn *next
	ElseIf *Sym\kind <> SymbolTable::#SKIND_FUNCTION
		Error::Display(*token\line, "Idenfier "+PeekS(*token\string)+" is not declared as a Function/Sub", #PB_Compiler_Line)
		ProcedureReturn *next
	EndIf
	
	Define.SymbolTable::SFunction *F = *Sym
	ByteCode::EmitOp(#OP_CONSTANT)
    Define.JumpTarget target
    target\_name = *F\name
    target\_class = *F\class
    target\_jump_ip = ByteCode::ip
    target\_line = *token\line
    LinkedList::addLast(*patchJump, @target, SizeOf(JumpTarget))
    ByteCode::EmitInteger(0)
   	;ByteCode::EmitOp(#OP_THREAD)
    
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 4
; Folding = -
; EnableXP
; DPIAware