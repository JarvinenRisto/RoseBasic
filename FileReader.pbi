XIncludeFile "LinkedList.pb"

DeclareModule Lexer
	EnableExplicit
	Declare init()
	Declare destroy()
	Declare run(*file.LinkedList::CustomList, fileNameV.s, length.i)
EndDeclareModule

DeclareModule FileReader
  Declare  Load(fileName.s)
EndDeclareModule

DeclareModule TokenList
	EnableExplicit
	
	IncludeFile "tokens.pb"
	
	Global lastType
	Global *stack, gVisible = SymbolTable::#VPUBLIC
	Global.LinkedList::CustomList *resolveList
	Global.LinkedList::CustomList *functions
	Global.LinkedList::CustomList *patchJump
	Global.LinkedList::CustomList *tokens
	Global Dim TokenText.s(#TMAX)

	Declare DimParser(*next.LinkedList::Node, ref.i, refCounter.i, isStatic.i)
	Declare LogicalExp(*Next.LinkedList::Node)
	Declare FunctionCallDeclared(*next.LinkedList::Node, *F.SymbolTable::SFunction, noEmit = 0)
	Declare FunctionCall(*next.LinkedList::Node, *name)
	Declare PASS1(*head = #Null)
	Declare PASS2()
	
	Macro Init()
		TokenList::*tokens = LinkedList::init()
		TokenList::*functions = LinkedList::init()
		TokenList::*patchJump = LinkedList::init()
		
		TokenList::TokenText(0) = "Short"
		TokenList::TokenText(0) = "Float"
		TokenList::TokenText(0) = "Number (byte)"
		TokenList::TokenText(0) = "Number (short)"
		TokenList::TokenText(0) = "Number (float)"
		TokenList::TokenText(TokenList::#ELSEIF) = "ElseIf"
		TokenList::TokenText(TokenList::#RETURN) = "Return"
		TokenList::TokenText(TokenList::#IDENFIER) = "Idenfier"
		TokenList::TokenText(TokenList::#CLASS) = "Class"
		TokenList::TokenText(TokenList::#END_CLASS) = "EndClass"
		TokenList::TokenText(TokenList::#SUBR) = "Sub"
		TokenList::TokenText(TokenList::#END_SUBR) = "EndSub"
		TokenList::TokenText(TokenList::#FUNCTION) = "Function"
		TokenList::TokenText(TokenList::#END_FUNCTION) = "EndFunction"
		TokenList::TokenText(TokenList::#DIM) = "Dim"
		TokenList::TokenText(TokenList::#CONST) = "Const"
		TokenList::TokenText(TokenList::#AS) = "As"
		TokenList::TokenText(TokenList::#LPAREN) = "("
		TokenList::TokenText(TokenList::#RPAREN) = ")"
		TokenList::TokenText(TokenList::#POW) = "^"
		TokenList::TokenText(TokenList::#DIV) = "/"
		TokenList::TokenText(TokenList::#MUL) = "*"
		TokenList::TokenText(TokenList::#PLUS) = "+"
		TokenList::TokenText(TokenList::#MINUS) = "-"
		TokenList::TokenText(TokenList::#EQU) = "="
		TokenList::TokenText(TokenList::#COMMA) = ","
		TokenList::TokenText(TokenList::#DOT) = "."
		TokenList::TokenText(TokenList::#TINTEGER) = "Integer"
		TokenList::TokenText(TokenList::#NUMBER) = "Number"
		TokenList::TokenText(TokenList::#TDOUBLE) = "Double"
		TokenList::TokenText(TokenList::#TSTRING) = "String"
		TokenList::TokenText(TokenList::#STRING) = "Text (string)"
		TokenList::TokenText(TokenList::#USE) = "Use"
		TokenList::TokenText(TokenList::#LINKEDLIST) = "LINKEDLIST"
		TokenList::TokenText(TokenList::#REPEAT) = "Repeat"
		TokenList::TokenText(TokenList::#FOREVER) = "Forever"
		TokenList::TokenText(TokenList::#UNTIL) = "Until"
		TokenList::TokenText(TokenList::#MOD) = "Modulo"
		TokenList::TokenText(TokenList::#END) = "End"
		TokenList::TokenText(TokenList::#AND) = "And"
		TokenList::TokenText(TokenList::#OPERATOR_OR) = "Or"
		TokenList::TokenText(TokenList::#IF) = "If"
		TokenList::TokenText(TokenList::#ELSE) = "Else"
		TokenList::TokenText(TokenList::#ENDIF) = "EndIf"
		TokenList::TokenText(TokenList::#THEN) = "Then"
		TokenList::TokenText(TokenList::#BREAK) = "Break"
		TokenList::TokenText(TokenList::#CONTINUE) = "Continue"
		TokenList::TokenText(TokenList::#NEW) = "New"
		TokenList::TokenText(TokenList::#MAIN) = "Main"
		TokenList::TokenText(TokenList::#ENDMAIN) = "EndMain"
		TokenList::TokenText(TokenList::#CONSTRUCTOR) = "Constructor"
		TokenList::TokenText(TokenList::#ENDCONSTRUCTOR) = "EndDeConstructor"
		TokenList::TokenText(TokenList::#DECONSTRUCTOR) = "DeConstructor"
		TokenList::TokenText(TokenList::#ENDDECONSTRUCTOR) = "EndDeConstructor"
		
		SymbolTable::init()
		TokenList::*stack = Stack::Alloc(512*8)
	
	EndMacro
	
	Macro AddToken(value)
	  LinkedList::AddLast(TokenList::*tokens, value, SizeOf(Token))
	EndMacro
	
	Macro isOp()
	  Bool(TokenList::lastType = TokenList::#EQU Or TokenList::lastType = TokenList::#PLUS Or TokenList::lastType = TokenList::#MINUS Or TokenList::lastType = TokenList::#MUL Or TokenList::lastType = TokenList::#DIV Or TokenList::lastType = TokenList::#POW Or TokenList::lastType = TokenList::#MOD)
	EndMacro
	
	Macro Destroy()
		Define.LinkedList::Node *Next = TokenList::*tokens\head
		
		While *next <> #Null
			
			If TokenList::*tokens\head\value <> #Null
				Define.TokenList::Token *token = TokenList::*tokens\head\value
				
				If *token\type = TokenList::#TSTRING And *token\string > 1000
					FreeMemory(*token\string)
				EndIf
			EndIf
			
			*next = TokenList::*tokens\head\next
			TokenList::*tokens\head = *next
		Wend
		
		SymbolTable::destroy()
		FreeArray(TokenList::TokenText())
		Stack::Free(TokenList::*stack)
	EndMacro
	
EndDeclareModule

Module FileReader
	Global.LinkedList::CustomList *file = #Null
	
	Procedure Load(fileName.s)
		Define.i key = ReadFile(#PB_Any, fileName)
	
		If key <> 0
			If *file = #Null
				*file = LinkedList::init()
			EndIf
					
			Define.i length = Lof(key)
			Define *mem = AllocateMemory(length)

			If ReadData(key, *mem, length)
				LinkedList::addFirst(*file, *mem, length)
				FreeMemory(*mem)
				CloseFile(key)
				Lexer::run(*file, fileName, length)
			EndIf
		Else
			Error::Display(@"", 0, "File "+fileName+" does Not exists.")
		EndIf
	EndProcedure
		
	Procedure Destroy()
	  Lexer::destroy()
	EndProcedure

EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 115
; FirstLine = 92
; Folding = 0-
; EnableXP
; DPIAware