DeclareModule Error
  
  Declare display(*file, line.i, msg.s)
  Declare Warning(*file, line.i, msg.s)
  
  Global.i panicMode
  Global.i errorCount = 0, previousLine
  Global.s previousMsg 
  
EndDeclareModule

Module Error
  
  Procedure display(*file, line.i, msg.s)
    If previousMsg <> msg And previousLine <> line
      PrintN("File: "+PeekS(*file)+". Error at line: " + Str(line) + ". " + msg) 
      errorCount + 1
    EndIf
    previousLine = line
    previousMsg = msg
    panicMode = 1
  EndProcedure
  
  Procedure Warning(*file, line.i, msg.s)
    PrintN("Warning at line: " + Str(line) + ". " + msg)
  EndProcedure
  
EndModule
; IDE Options = PureBasic 6.30 beta 4 (Linux - x64)
; CursorPosition = 2
; Folding = -
; EnableXP
; DPIAware