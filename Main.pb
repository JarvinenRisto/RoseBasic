XIncludeFile "Lexer.pb"

OpenConsole()
ConsoleTitle("RoseBasic Compiler")
PrintN("RoseBasic Compiler V1 beta - (C) Risto Järvinen 2025.")

Define.i paramCount = CountProgramParameters()

If paramCount = 0
  Print("Usage: ./RoseBasic file")
  End
EndIf

Define.s fileName = ""
Define.s path = ""

fileName = ProgramParameter(0)
path = GetPathPart(fileName)
SetCurrentDirectory(path)

;GlobalMemoryCache::initCache()
Lexer::Init()

FileReader::Load(RemoveString(fileName,path))

TokenList::PASS1(TokenList::*tokens)
TokenList::PASS2()
Lexer::Destroy()

If ProgramParameter(1) = "/syntax"
  PrintN("Syntax check done.")
  End
EndIf

If Error::errorCount = 0
  PrintN("Compiling done.")
  ByteCode::Run()
  Input()
  CloseConsole()
  End
EndIf
; IDE Options = PureBasic
; ExecutableFormat = Console
; CursorPosition = 30
; Optimizer
; EnableThread
; EnableXP
; EnableWayland
; DPIAware
; DllProtection
; SharedUCRT
; Executable = ..\..\game\Compiler\RoseBasic.exe
; DisableDebugger
; CommandLine = Operator.rbc
; CurrentDirectory = ..\test\
; Compiler = PureBasic 6.30 - C Backend (Windows - x64)
; EnablePurifier