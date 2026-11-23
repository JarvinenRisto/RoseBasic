Procedure ThisParser(*next.LinkedList::Node)
  returnIfNull()
  Define.Token *token
  MoveNext()
  
  Define.i redoDot = 0
  Define.i thisLocal = 0
  
  this_redo_dot:
  
  Select *token\type
    Case #DOT
      moveNext()
      
      expect(*token, #IDENFIER, #PB_Compiler_Line)
      Define.SymbolTable::SSymbol *Sym = SymbolTable::LookupSymbol(SymbolTable::GetPreviousScope()+2)
      
      If *Sym = 0
        *Sym = SymbolTable::LookupSymbol(SymbolTable::GetCurrentScope()+2)
      EndIf
      
			Define.FastString::STRFASTOBJ *obj = FastString::New(2+MemoryStringLength(*Sym\name)+MemoryStringLength(*token\string))	
			FastString::Add(*obj, *Sym\name)
			FastString::Add(*obj, @":")
			FastString::Add(*obj, *token\string)
			Define *tmpVar = *token\string
			Define.SymbolTable::SVariable *Var = SymbolTable::LookupSymbol(*obj\data+4)
			FastString::Free(*obj)
			moveNext()
			
			If redoDot > 0 And *Var <> 0
			  thisLocal = *Var\RefOffset*SizeOf(Interpreter::Value)
			EndIf
			
			Select *token\type
			  Case #EQU
			    If redoDot > 0
			      ByteCode::EmitOP(#OP_GET_LOCAL)
			      ByteCode::EmitLong(thisLocal)
			      
			      ByteCode::EmitOP(#OP_GET_PROPERTY)
				  Else
				    ByteCode::EmitOP(#OP_GET_LOCAL)
				  EndIf
					ByteCode::EmitLong(thisLocal)
					*next = Assignment(*Var, *Next)
				Case #DOT
				  redoDot = redoDot + 1
				  Goto this_redo_dot
				Default
				  If redoDot > 0
				    ByteCode::EmitOP(#OP_GET_LOCAL)
						 ByteCode::EmitLong(thisLocal)
				     ByteCode::EmitOP(#OP_GET_PROPERTY)
				  Else
				    ByteCode::EmitOP(#OP_GET_LOCAL)
				  EndIf
					ByteCode::EmitLong(thisLocal)
					Define.SymbolTable::SFunction *F = *Var
					If Not *F
					  Error::Display(*token\fileName, *token\line, "Function "+PeekS(*tmpVar)+" is not defined in a class "+PeekS(SymbolTable::getPreviousScope()+2))
					  While *token And *token\type <> #RPAREN
					    moveNext()
					  Wend
					  ProcedureReturn *next
					EndIf
          *next = FunctionCallDeclared(*next\next, *F, 1)
          ByteCode::EmitOp(#OP_INVOKE)
          Define fName.s = PeekS(SymbolTable::getPreviousScope()+2)+":"+PeekS(*F\name)
          ByteCode::EmitString(@fName)
          ByteCode::EmitByte(*F\ParamCount)
			EndSelect
  EndSelect

  ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 60
; FirstLine = 28
; Folding = -
; EnableXP
; DPIAware