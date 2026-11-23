XIncludeFile "Interpreter.pb"
XIncludeFile "Error.pb"

DeclareModule ByteCode
	Global.i mainEmitted, compileToExe
	
	Declare Init()
	Declare Destroy()
	Declare toExe()
	Declare EmitJmpMain(*file, line.i)
	Declare getMain()

	Declare EmitByte(value.b)
	Declare EmitString(*string)
	Declare EmitInteger(value.i)
	Declare EmitObject(value.i)
	Declare EmitLong(value.l)
	Declare EmitDouble(value.d)
	Declare EmitOp(value.b)
	
	Declare EmitRBrace(space.s, isClass.i)
	Declare EmitFunction(*name)
	Declare EmitClass(*name)
	Declare EmitConstant(*name)
	Declare replaceConstant(name.s, value.s)
	Declare EmitJump(instruction.b)
	Declare patchJump(offset.i)
	Declare emitLoop(loopStart.i) 
	Declare run()
	
	Global.Interpreter::ObjFunction *gFunction
	Global.Interpreter::ObjFunction *gMainFunction
	Global.Interpreter::ObjFunction *script
	Global gMainClass.s
	Global gIsInitPadding
	
	Global.Interpreter::ObjFunction NewList *gFunctionList()
	
	Global.i ip = 8
	Global.s webOutput, tmpOutput
	Global instanceEmitted = 0
	
EndDeclareModule

Module ByteCode
	
	IncludeFile "tokens.pb"
	
	#DEFAULT_SIZE = 512
	
	Global.i ip = 8
	
	Procedure run()
	  	  
	  Interpreter::mainContext\gFrameCount = 1
	  Interpreter::mainContext\stackIP = 0
	      
	  Interpreter::RESET_STACK(Interpreter::mainContext)
	  Interpreter::push(Interpreter::mainContext, *script)
	  
	  *gFunction = *script
	  
	  emitOp(#OP_GET_GLOBAL)
	  emitString(@gMainClass)
	  emitOp(#OP_CALL)
	  EmitByte(0)
	  emitOp(#OP_INVOKE_CONSTR)
	  Define main.s = gMainClass+":main"
	  emitString(@main)
	  EmitByte(0)
	  emitOp(#OP_HALT)
	  
	  ForEach *gFunctionList()
	    Debug "-----------------------------"
	    Interpreter::Display(*gFunctionList())
	  Next
	  
	  Interpreter::mainContext\gFrameCount = 1
	  Interpreter::mainContext\stackIP = 0
	      
	  Interpreter::RESET_STACK(Interpreter::mainContext)
    Interpreter::push(Interpreter::mainContext, *script)
   
    Define.d startTime = ElapsedMilliseconds()
    
    Define.Interpreter::CallFrame *frame = Interpreter::mainContext\gFrames[0]
    *frame\function = *script
    *frame\ip = *script\chunk\code
    *frame\slots = #Null
  
    If Interpreter::run(Interpreter::mainContext) = Interpreter::#INTERPRET_RUNTIME_ERROR
      PrintN("Runtime error.")
      Input()
    EndIf
    
    PrintN("Running took "+StrD((ElapsedMilliseconds() - startTime))+"ms.")
	EndProcedure

	Procedure CatchFile(*file_address, file_size, file_name.s)
		Define.i id 
		file_content.s = PeekS(*file_address, file_size, #PB_UTF8)
		id = CreateFile(#PB_Any, file_name)
		If id
			bytes_written = WriteData(id, *file_address, file_size)
			If bytes_written = file_size
				ProcedureReturn id
			EndIf    
			CloseFile(id)      
		EndIf    
		ProcedureReturn 0
	EndProcedure

	Procedure toExe()
		Define.i id = CatchFile(?start_rbRuntime, ?end_rbRuntime - ?start_rbRuntime, GetCurrentDirectory()+"TestRB.exe")
		If id
			FileSeek(id, Lof(id))
			WriteData(id, *code, ip)
			FileSeek(id, Lof(id))
			WriteInteger(id, ip)
			CloseFile(id)
		EndIf
	EndProcedure
	
	Procedure Init()
	   Interpreter::init()
	EndProcedure
	
	Procedure Destroy()
	EndProcedure
	
	Procedure EmitJmpMain(*file, line.i)
		If mainEmitted
			Error::display(*file, line, "Function main can be defined only once.")
		EndIf
		mainEmitted = #True
	EndProcedure
	
	Procedure getMain()
		ProcedureReturn *code + PeekI(*code)
	EndProcedure
	
	Procedure EmitJump(instruction.b)
	  emitOp(instruction)
	  Interpreter::writeChunkLong(*gFunction\chunk, 255)
    ProcedureReturn *gFunction\chunk\count - 4
  EndProcedure
  
  Procedure emitLoop(loopStart.i) 
    emitOP(#OP_LOOP)
    
    Define.i offset = *gFunction\chunk\count - loopStart + 4 
    Interpreter::writeChunkLong(*gFunction\chunk, offset)
  EndProcedure
	
	Procedure patchJump(offset.i)
	  Define.l jump = *gFunction\chunk\count - (offset + 4)
    PokeL(*gFunction\chunk\code+offset, jump)
	EndProcedure
	
Procedure EmitInteger(num.i)
  ; Box the integer as a NaN-boxed number
  Define.q boxed = Interpreter::NUM_VAL(num)

  ; Add to the chunk's constant table (expects q)
  Define.l const = Interpreter::addConstant(*gFunction\chunk, boxed)
  
  ; Emit the constant index into bytecode
  Interpreter::writeChunkLong(*gFunction\chunk, const)
EndProcedure
	
Procedure EmitObject(value.i)
    Define.q boxed = Interpreter::OBJ_VAL(value)
	  Define.i index = Interpreter::addConstant(*gFunction\chunk, boxed)
	  Interpreter::writeChunkLong(*gFunction\chunk, index)
	EndProcedure
	
	Procedure EmitByte(value.b)
	  If *gFunction = #Null Or *gFunction\chunk = #Null
	    ProcedureReturn
	  EndIf
	  Interpreter::writeChunk(*gFunction\chunk, value)
	EndProcedure
	
  Procedure EmitLong(value.l)
	  Interpreter::writeChunkLong(*gFunction\chunk, value)
	EndProcedure
	
Procedure EmitDouble(num.d)
  ; Box the double as a NaN-boxed numeric value
  Define.q boxed = Interpreter::NUM_VAL(num)
  
  ; Add constant to the current function’s chunk
  Define.l const = Interpreter::addConstant(*gFunction\chunk, boxed)
  
  ; Emit the index into bytecode
  Interpreter::writeChunkLong(*gFunction\chunk, const)
EndProcedure
	
Procedure EmitString(*string)
  ; Create a VM ObjString from the given raw string pointer
  Define.ObjString *objString = Interpreter::copyString(*string, MemoryStringLength(*string) * 2)
  
  Debug PeekS(*objString\chars)
  ; Box the object pointer as a NaN-boxed object value
  Define.q boxed = Interpreter::OBJ_VAL(*objString)
  
  ; Add it to the chunk’s constant pool
  If *gFunction = #Null Or *gFunction\chunk = #Null
    ProcedureReturn
  EndIf
  Define.l const = Interpreter::addConstant(*gFunction\chunk, boxed)
  
  ; Write the constant index to the bytecode
  Interpreter::writeChunkLong(*gFunction\chunk, const)
EndProcedure

	
  Procedure EmitOp(value.b)
    If *gFunction = #Null Or *gFunction\chunk = #Null
      ProcedureReturn
    EndIf
	  Interpreter::writeChunk(*gFunction\chunk, value)
	EndProcedure
	
	Procedure EmitClass(*name)
		webOutput = webOutput + "class "+PeekS(*name)+" {"+#LFCR$
	EndProcedure
	
	Procedure EmitConstant(*name)
		Define.s name = PeekS(*name)
		tmpOutput = tmpOutput + PeekS(SymbolTable::getCurrentScope()+2)+"."+name+" = _SET_ROSEBASIC_"+name+";"+#LFCR$
	EndProcedure
	
	Procedure replaceConstant(name.s, value.s)
		webOutput = ReplaceString(webOutput, "_SET_ROSEBASIC_"+name, value)
	EndProcedure
	
	Procedure EmitFunction(*name)
		If CompareMemoryString(*name, @"main") = 0
			webOutput = webOutput + "	static "+PeekS(*name)+"() {"+#LFCR$
		Else
			webOutput = webOutput + "	function "+PeekS(*name)+" () {"+#LFCR$
		EndIf
	EndProcedure
	
	Procedure EmitRBrace(space.s, isClass.i)
		webOutput = webOutput + space + "}"+#LFCR$
		If isClass And tmpOutput <> ""
			webOutput = webOutput + tmpOutput
			tmpOutput = ""
		EndIf
	EndProcedure
	
	DataSection
		start_rbRuntime: 
		;IncludeBinary "rbRuntime.exe"
		end_rbRuntime:
	EndDataSection
EndModule
; IDE Options = PureBasic 6.30 (Windows - x64)
; CursorPosition = 74
; FirstLine = 34
; Folding = -----
; EnableXP
; DPIAware