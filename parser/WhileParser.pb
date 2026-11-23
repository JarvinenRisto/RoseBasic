Procedure WhileParser(*next.LinkedList::Node)
  NewList breakList()
  NewList continueList()
  Define.Token *token
  returnIfNull()
  
  Define.i loopStart = ByteCode::*gFunction\chunk\count
	
	*next = LogicalExp(*next\next)
	*token = *next\value
	
	Define.i exitJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)
	ByteCode::emitOp(#OP_POP)

  While *token And *token\type <> #ENDWHILE
    Select *token\type
      Case #BREAK
        AddElement(breakList())
        breakList() = ByteCode::EmitJump(#OP_JUMP)
      Case #CONTINUE
	      ByteCode::EmitLoop(loopStart)
			Case #END
				ByteCode::EmitOp(#OP_HALT)
			Case #RETURN
				*next = ReturnParser(*next\next)
			Case #CDELETE
				*next = DeleteParser(*next\next)
			Case #IF
				*next = IfParser(*next, breakList(), continueList(), loopStart)
			Case #REPEAT
			  *next = RepeatParser(*next)
			Case #WHILE
				*next = WhileParser(*next)
			Default			
				StatementParser(*token, *next, repeat_)
		EndSelect
		moveNext()
	Wend
	
	ByteCode::emitLoop(loopStart)
	
	If ListSize(breakList()) > 0
	  ResetList(breakList())
	  While NextElement(breakList())
	    ByteCode::patchJump(breakList())
	  Wend
	  ClearList(breakList())
	EndIf
	
	ByteCode::patchJump(exitJump)
	ByteCode::emitByte(#OP_POP)   
	
	ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 38
; Folding = -
; EnableXP
; DPIAware