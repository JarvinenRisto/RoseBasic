Procedure SelectParser(*next.LinkedList::Node, List breakList(), List continueList(), cont.i = 0)
  returnIfNull()
  
  *next = LogicalExp(*next\next)  ; parse select expression
  
  NewList caseJumps()
  Define.Token *token = *next\value
  Define defaultUsed = 0 
  Define.i lastFalseJump = -1
  Define.i endSelectJump = -1
  
  Repeat
    Select *token\type
      
      Case #CASE
        ; === Patch previous case’s false jump here ===
        If lastFalseJump <> -1
          ByteCode::patchJump(lastFalseJump)
          lastFalseJump = -1
        EndIf
        
        MoveNext()
        *next = LogicalExp(*next)
        ByteCode::EmitOp(#OP_EQUAL)
        
        ; Jump if false to next CASE or DEFAULT
        lastFalseJump = ByteCode::EmitJump(#OP_JUMP_IF_FALSE)
        
        *token = *next\value
        multiStatement(_case)
        *next = *next\prev
        
        ; pop condition result
        ByteCode::EmitOp(#OP_POP)
        
        ; after executing case body, jump to end
        AddElement(caseJumps())
        caseJumps() = ByteCode::EmitJump(#OP_JUMP)
        
      Case #DEFAULT
        ; Patch last CASE’s false jump (so it falls into default)
        If lastFalseJump <> -1
          ByteCode::patchJump(lastFalseJump)
          lastFalseJump = -1
        EndIf
        
        MoveNext()
        If defaultUsed = 1
          Error::Display(*Token\line, "Multiple DEFAULT in SELECT", #PB_Compiler_Line)
        EndIf
        
        *token = *next\value
        multiStatement(_default)
        *next = *next\prev
        defaultUsed = 1
        
      Case #ENDSELECT
        Break
    EndSelect
    MoveNext()
  Until *token\type = #ENDSELECT
  
  ; Patch last conditional jump (so last case ends correctly)
  If lastFalseJump <> -1
    ByteCode::patchJump(lastFalseJump)
  EndIf
  
  ; Patch all jumps to end
  ForEach caseJumps()
    ByteCode::patchJump(caseJumps())
  Next
  
  ByteCode::EmitOp(#OP_POP)
  
  ProcedureReturn *next
EndProcedure
; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 75
; FirstLine = 17
; Folding = -
; EnableXP
; DPIAware