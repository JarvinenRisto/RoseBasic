Procedure ImportParser(*next.LinkedList::Node)
  returnIfNull()
  Define.Token *token
  moveNext()
  expect(*token, #TSTRING, #PB_Compiler_Line)
  
  Define.s dll = PeekS(*token\string)
  
  CompilerIf #PB_Compiler_OS = #PB_OS_Linux
    dll = dll + ".so"
  CompilerElse
    dll = dll + ".dll"
  CompilerEndIf
  
  Define library = OpenLibrary(#PB_Any, dll)
  If Not library
    Error::Display(*token\fileName, *token\line, "Cant open library: " + dll)
  EndIf
  
  moveNext()
  
  Define.i start = Interpreter::currentBuildIn
  
  NewList aliases.s()
  
  While *Token\type <> #ENDIMPORT
    expect(*token, #IDENFIER, #PB_Compiler_Line)
    
    Protected.SymbolTable::SFunction *F = SymbolTable::addFunction(*token\string)
    moveNext()
    
    expect(*token, #LPAREN, #PB_Compiler_Line)
    
    Define.i offset = 0
    Define *string
    Protected *V.SymbolTable::SVariable 
    
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
				
				expect(*token, #AS, #PB_Compiler_Line)
				moveNext()
				expect(*token, isType(*token\type), #PB_Compiler_Line)
				
				offset = offset + 1
				
				If *F\ParamTypes = 0
					*F\ParamTypes = LinkedList::Init()
					LinkedList::AddLastInt(*F\ParamTypes, *token\type)
				EndIf
				moveNext()
			EndIf
		Wend
    moveNext()
    
    expect(*token, #ALIAS, #PB_Compiler_Line)
    moveNext()
    expect(*token, #TSTRING, #PB_Compiler_Line)
    AddElement(aliases())
    aliases() = PeekS(*token\string)
    moveNext()
    
    If *token And *token\type = #AS
      moveNext()
      If Not isType(*token\type)
        Error::Display(*token\fileName, *token\line, "Expected a valid type, but got " + TokenList::TokenText(*token\type))
      EndIf
      moveNext()
    EndIf
    
		*F\id = Interpreter::currentBuildIn
		*F\paramCount = offset
		*F\type = #NUMBER
		*F\isStatic = 1
		
		Interpreter::currentBuildIn + 1
  Wend
  
  Interpreter::resizeBuildIn(Interpreter::currentBuildIn)
  
  Define.i I = start
  
  ForEach aliases()
    Interpreter::*buildInFunction(I) = GetFunction(library, aliases())
    I + 1
  Next
    
  ClearList(aliases())
  
  ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 76
; FirstLine = 54
; Folding = -
; EnableXP
; DPIAware