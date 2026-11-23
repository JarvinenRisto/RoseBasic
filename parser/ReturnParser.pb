Procedure ReturnParser(*next.LinkedList::Node)
  returnIfNull()
  
  *next = LogicalExp(*next)
  
  Define.Token *token = *next\value
  Debug *token\type
  
  ByteCode::EmitOP(#OP_RETURN)
  
  ProcedureReturn *next\prev
EndProcedure
; IDE Options = PureBasic 6.20 Beta 2 (Linux - x64)
; CursorPosition = 7
; Folding = -
; EnableXP
; DPIAware