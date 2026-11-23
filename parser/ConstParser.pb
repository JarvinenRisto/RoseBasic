Macro isOperator(type)
	type = #PLUS Or type = #MINUS Or type = #MUL Or type = #DIV Or type = #POW Or type = #EQU Or type = #MOD Or type = #LPAREN Or type = #RPAREN
EndMacro

Macro isValidValueType(type)
	type = #TINTEGER Or type = #TDOUBLE Or type = #TSTRING Or type = #IDENFIER Or type = #number
EndMacro
	
Procedure ConstParser(*next.LinkedList::Node)
	reConst:
	If *next = #Null
		ProcedureReturn *next
	EndIf
	
	Define.Token *token = *next\value
	expect(*token, #IDENFIER, #PB_Compiler_Line)
	Define *name = *token\string
	moveNext()
	expect(*token, #EQU, #PB_Compiler_Line)
	moveNext()
	
	While *token And (*token\type = #DOT Or *token\type = #IDENFIER Or *token\type = #number Or *token\type = #TINTEGER Or *token\type = #TDOUBLE Or *token\type = #TSTRING Or isOperator(*token\type))
		RPN::add(*token)
		moveNext()
	Wend
	*next = *next\prev
	
	Define.SymbolTable::SSymbol *Sym = SymbolTable::addConstant(*name, #LINKEDLIST, RPN::getPostFix())
	*Sym\visible = gVisible
	
	If peek(*next) = #comma
		*next = *next\next
		*next = *next\next
		If *next
			ProcedureReturn ConstParser(*next)
		EndIf
	EndIf	
	
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 5
; Folding = -
; EnableXP
; DPIAware