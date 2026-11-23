Structure RBVariable
  *name
  number.i
  dimensionSize.i
  dimensions.i
EndStructure

Procedure CountStringMemory(*string, *search)
  Define.i i, length = MemoryStringLength(*string, #PB_ByteLength)
  Define.w word = PeekW(*search)
  Define.i count = 0
  For i = 0 To length Step 2
    If PeekW(*string + i) = word 
      count + 1
    EndIf
  Next
  ProcedureReturn count
EndProcedure

Procedure ReDimParser(*next.LinkedList::Node)
  returnIfNull()
  
  Define.Token *token = *next\value
  expect(*token, #IDENFIER, #PB_Compiler_Line)
  
  Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*token\string)
			
  If Not *Sym
    Error::Display(*token\fileName, *token\line, "Idenfier: "+PeekS(*token\string)+" does not exists")
  ElseIf *Sym\dimensionSize = 0
    Error::Display(*token\fileName, *token\line, "Idenfier: "+PeekS(*token\string)+" is not an array")
  EndIf
  
  moveNext()
  expect(*token, #LPAREN, #PB_Compiler_Line)
  moveNext()
  
  Define dimCount = 1
  
continueRedim:
  *next = LogicalExp(*next)
  *token = *next\value

  If *token\type = #comma
    dimCount + 1
    moveNext()
    Goto continueRedim
  EndIf
  
  expect(*token, #RPAREN, #PB_Compiler_Line)

  If *Sym\dimensions <> dimCount
    Error::Display(*token\fileName, *token\line, "Idenfier: "+PeekS(*Sym\name)+" incorrect dimension count on a redim " + Str(dimCount))
  EndIf
  
  Define.SymbolTable::SVariable *Var = *Sym
  
  ByteCode::EmitOP(#OP_REDIM)
  ByteCode::EmitByte(*Var\RefOffset)
  
  If *Var\RefKind = SymbolTable::#Ref_Class
    ByteCode::EmitByte(1)
  Else
    ByteCode::EmitByte(0)
  EndIf
  
  ProcedureReturn *next
EndProcedure
  
Procedure DimParser(*next.LinkedList::Node, ref.i, *refCounter.Integer, isStatic.i)
	NewList dList.RBVariable()
	
	continueDim:
	returnIfNull()
	
	Define.Token *token = *next\value
	expect(*token, #IDENFIER, #PB_Compiler_Line)
	
	AddElement(dList())
	dList()\name = *token\string
	moveNext()
	
	If *token And *token\type = #LPAREN
	  If CountStringMemory(SymbolTable::GetCurrentScope(), @":") = 2
  	  dList()\number = gLocalArrayCount 
  	  gLocalArrayCount = gLocalArrayCount + 1
  	Else
  	  dList()\number = gGlobalArrayCount 
  	  gGlobalArrayCount = gGlobalArrayCount + 1
  	EndIf
  	
	  moveNext()
	  continueArray:
	  expect(*token, #TINTEGER, #PB_Compiler_Line)
	  dList()\dimensions + 1
	  If *token\integer = 0
	    dList()\dimensionSize + 1
	  Else
	    dList()\dimensionSize + *token\integer
	  EndIf
	  moveNext()
	  If *token And *token\type = #COMMA
	    moveNext()
	    Goto continueArray
	  EndIf
	  expect(*token, #RPAREN, #PB_Compiler_Line)
	  moveNext()
	EndIf
	
	If *token And *token\type = #COMMA
		moveNext()
		Goto continueDim
	EndIf
	
	expect(*token, #AS, #PB_Compiler_Line)
	moveNext()
	
	If Not isDType(*token\type)
		Error::Display(*token\fileName, *token\line, "Expected a valid type (like Byte, etc..) but got "+TokenText(*token\type))
		ProcedureReturn *next
	EndIf
	
	Define *name
	ResetList(dList())
	
	While NextElement(dList())
		*name = dList()\name
		If *token\type = #IDENFIER
			Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(*token\string)
			
			If Not *Sym
				Error::Display(*token\fileName, *token\line, "Idenfier: "+PeekS(*token\string)+" does not exists")
			EndIf
			If dList()\dimensionSize > 0
			  *refCounter\i - 1
			EndIf
			
			Define.SymbolTable::SVariable *Var = SymbolTable::AddVariableClass(*name, *token\type, *Sym, ref, *refCounter\i)
			*Var\visible = gVisible
			*Var\isStatic = isStatic
			*Var\dimensions = dList()\dimensions
			*Var\dimensionSize = dList()\dimensionSize 
			If dList()\dimensions
		    *Var\RefOffset = dList()\number
		  EndIf
		  LinkedList::addLast(*gClassVarList, *Var, SizeOf(SymbolTable::SVariable))
		Else
		   If dList()\dimensionSize > 0
			  *refCounter\i - 1
			EndIf
			Define.SymbolTable::SVariable *Var = SymbolTable::AddVariable(*name, *token\type, ref, *refCounter\i)
			LinkedList::addLast(*gClassVarList, *Var, SizeOf(SymbolTable::SVariable))
		  *Var\dimensionSize = dList()\dimensionSize
		  *Var\dimensions = dList()\dimensions
		  If dList()\dimensions
		    *Var\RefOffset = dList()\number
		  EndIf
		  *Var\visible = gVisible
		  *Var\isStatic = isStatic
		EndIf
		*refCounter\i + 1
	Wend
	
	If *token And *token\type = #COMMA
		ClearList(dList())
		moveNext()
		Goto continueDim
	EndIf
	
	FreeList(dList())
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 105
; FirstLine = 88
; Folding = -
; EnableXP
; DPIAware