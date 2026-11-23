Procedure ConstructorParser(*next.LinkedList::Node, type.i)
	returnIfNull()
	
	Define *new
	
	If type = #ENDCONSTRUCTOR
		*new = @"new"
	Else
		*new = @"delete"
	EndIf
	
	Define.SymbolTable::SFunction *F = SymbolTable::AddFunction(*new)
	*F\fObj = Interpreter::newFunction()
	*F\fObjValue = AllocateStructure(Interpreter::Value)
	*F\fObjValue\type = Interpreter::#VAL_OBJ
	*F\fObjValue\obj = *F\fObj
	*F\ParamCount = 0
	AddElement(ByteCode::*gFunctionList())
	ByteCode::*gFunctionList() = *F\fObj
	
	Interpreter::*globals(PeekS(SymbolTable::GetCurrentScope()+2)+":"+PeekS(*new)) = *F\fObj
	
	SymbolTable::EnterScope(*F\name)
	
	Define.i localCount = 1
	Define.SymbolTable::SSymbol *Sym = SymbolTable::lookupSymbol(SymbolTable::GetPreviousScope()+2)
	SymbolTable::AddVariableClass(@"this", #IDENFIER, *Sym, SymbolTable::#Ref_Local, 0)

	Define.Token *token = *next\value
	expect(*token, #LPAREN, #PB_Compiler_Line)
	
	Define.i offset = 0
	If type = #ENDCONSTRUCTOR
		Define *string
		
		*F\paramTypes = LinkedList::Init()
		While *token And *token\type <> #RPAREN
			moveNext()
			
			If *token And *token\type <> #RPAREN
				expect(*token, #IDENFIER, #PB_Compiler_Line)
				*string = *token\string
				moveNext()
				
				If *token And *token\type = #LPAREN
					moveNext()
					expect(*token, #RPAREN, #PB_Compiler_Line)
					moveNext()
				EndIf
				
				If *token\type = #IDENFIER
					*Sym = SymbolTable::LookupSymbol(*token\string)
					If Not *Sym
						Error::Display(*token\line, "A", #PB_Compiler_Line)
					EndIf
					SymbolTable::AddVariableClass(*string, *token\type, *Sym, SymbolTable::#Ref_Parameter, offset)
					LinkedList::addFirstInt(*F\paramTypes, *token\type)
				Else
				  SymbolTable::AddVariable(*string, *token\type, SymbolTable::#Ref_Parameter, offset)
				  LinkedList::addFirstInt(*F\paramTypes, *token\type)
				EndIf
				offset = offset + 1
				moveNext()
			EndIf
		Wend
		
		*F\ParamCount = offset 
	Else
		moveNext()
		expect(*token, #RPAREN, #PB_Compiler_Line)
	EndIf
	
	*F\entry = *next
	LinkedList::addFirstInt(*functions, *F)
	
	Repeat
		moveNext()
		
		If *token
			Select *token\type
				Case #DIM
					Define.i start = *next\key
					moveNext()
					*next = DimParser(*next, SymbolTable::#Ref_Local, @localCount, 0)
					LinkedList::DeleteRange(*Tokens, start, *next\key)
			EndSelect
		EndIf
	Until *token And *token\type = type
	
	*F\fObj\localSize = localCount
	Debug *F\fObj\localSize
	SymbolTable::LeaveScope()
	
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 73
; FirstLine = 47
; Folding = -
; EnableXP
; DPIAware