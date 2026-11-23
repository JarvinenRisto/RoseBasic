Procedure DeleteParser(*next.LinkedList::Node)
	Define.Token *token
	returnIfNull()
	*token = *next\value
	Define.i line = *token\line
	expect(*token, #IDENFIER, #PB_Compiler_Line)
	Protected *name = *token\string
	
	Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*token\string)
	Define.SymbolTable::SVariable *Var
	
	If Not *Sym 
		Error::Display(*token\fileName, line, PeekS(*name)+" is not declared")
		ProcedureReturn *next
	ElseIf *Sym\kind <> SymbolTable::#SKind_Variable
		Error::Display(*token\fileName, line, PeekS(*name)+" is not a variable")
		ProcedureReturn *next
	EndIf
	
	*Var = *Sym
	
	If Not *var\Sym 
		Error::Display(*token\fileName, line, "Variable: "+PeekS(*Var\name)+" is not a class instance")
		ProcedureReturn *next
	EndIf
	
	Define *delete = @"delete"
	Define.i length = 2 + MemoryStringLength(*Var\Sym\name) + MemoryStringLength(*delete)
	Define.FastString::STRFASTOBJ *obj = FastString::new(length)
	FastString::Add(*obj, *Var\Sym\name)
	FastString::Add(*obj, @":")
	FastString::Add(*obj, *delete)
	Define.SymbolTable::SFunction *F = SymbolTable::LookupSymbol(*obj\data+4)
	FastString::Free(*obj)
	
	*next = value(*next)
	*next = *Next\prev
		
	If *F
		If *F\id <> -1
      		ByteCode::EmitOp(#OP_CALL)
      		ByteCode::EmitByte(*F\id)
      	Else
      	  ByteCode::EmitOp(#OP_INVOKE)
					ByteCode::EmitString(*F\name)
					ByteCode::EmitByte(0)
      		Define.JumpTarget target
      		target\_name = *F\name
      		target\_class = *F\class
      		target\_jump_ip = ByteCode::ip
      		target\_line = *token\line
      		LinkedList::addLast(*patchJump, @target, SizeOf(JumpTarget))
      	EndIf
	Else
		Error::Warning(*token\fileName, line, "Deconstructor is missing from a class: "+PeekS(*Var\Sym\name))
	EndIf

	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 52
; FirstLine = 11
; Folding = -
; EnableXP
; DPIAware