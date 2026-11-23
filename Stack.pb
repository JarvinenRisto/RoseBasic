XIncludeFile "GlobalMemoryCache.pb"

DeclareModule Stack
	
	Declare Alloc(*Size)
	Declare Free(*Stack)
	Declare PushDouble(*Stack, Value.d)
	Declare.d PopDouble(*Stack)
	Declare PushInteger(*Stack, *Value)
	Declare PopInteger(*Stack)
	
EndDeclareModule

Module Stack
	
	CompilerIf #PB_Compiler_Backend = #PB_Backend_C
		Macro BeginAsm()
			!__asm__ __volatile__ (".intel_syntax noprefix;"
		EndMacro
		
		Macro AsmInput(var0)
			!:[var] "r" (v_#var)
		EndMacro
		
		Macro AsmInput2(input0, input1)
			!".att_syntax"
			!:
			!:[var0] "r" (v_#input0), [var1] "r" (v_#input1) 
		EndMacro  
		
		Macro AsmOutputInput(output, input)
			!".att_syntax"
			!:"=r" (v_#output)
			!:[var] "r" (v_#input)
		EndMacro
		
		Macro AsmOutput(var1)
			!".att_syntax"
			!:"=r" (v_#var1)
			!:
		EndMacro
		
		Macro AsmOutput2(var1, var2)
			!".att_syntax"
			!:"=r" (v_#var1), "=r" (v_#var2)
			!:
		EndMacro
		
		Macro AsmClobbers()
			!:
		EndMacro
		
		Macro EndAsm()
			!);
		EndMacro
	CompilerEndIf
	
	Procedure Alloc(*Size)
		Protected.i newSize = (*Size << 3)
		
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			Define *ptr = GlobalMemoryCache::Alloc(16+newSize)
			*ptr = *ptr + newSize
			EnableASM
			!MOV rcx, [p.v_newSize]
			!MOV rax, [p.p_ptr]
			!MOV [rax], rax
			!MOV [rax+8], rcx
			DisableASM
			ProcedureReturn
		CompilerElse
			Define output.i
			Define *ptr = GlobalMemoryCache::Alloc(16+newSize)
			*ptr = *ptr + newSize
			PokeI(*ptr, *ptr)
			PokeI(*ptr+8, newSize)
			ProcedureReturn *ptr
		CompilerEndIf
	EndProcedure
	
	Procedure PushDouble(*Stack, Value.d)
		Define *ptr
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			EnableASM
			!MOV rcx, [p.p_Stack]
			!MOV rdx, [rcx]
			!SUB rdx, 8
			!MOV [p.p_ptr], rdx
			!MOV [rcx], rdx
			DisableASM
		CompilerElse
			Define *ptr = PeekI(*Stack) - 8
			PokeD(*ptr, value)
			PokeI(*Stack, *ptr)
			ProcedureReturn
		CompilerEndIf
		PokeD(*ptr, value)
	EndProcedure
	
	Procedure.d PopDouble(*Stack)
		Define *ptr
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			EnableASM
			!MOV rcx, [p.p_Stack]
			!MOV rdx, [rcx]
			!MOV [p.p_ptr], rdx
			!ADD rdx, 8
			!MOV [rcx], rdx
			DisableASM
		CompilerElse
			Define *ptr = PeekI(*Stack)
			PokeI(*Stack, *ptr+8)
			ProcedureReturn PeekD(*ptr)
		CompilerEndIf
		ProcedureReturn PeekD(*ptr)
	EndProcedure
	
	Procedure PushInteger(*Stack, *Value)
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			CompilerIf #PB_Compiler_Processor = #PB_Processor_x64	
				EnableASM
				!MOV rcx, [p.p_Stack]
				!MOV rax, [p.p_Value]
				!MOV rdx, [rcx]
				!SUB rdx, 8
				!MOV [rdx], rax
				!MOV [rcx], rdx
				DisableASM
			CompilerElse
				EnableASM
				!MOV ecx, [p.p_Stack]
				!MOV eax, [p.p_Value]
				!MOV edx, [ecx]
				!SUB edx, 4
				!MOV [edx], eax
				!MOV [ecx], edx
				DisableASM
			CompilerEndIf
		CompilerElse
			Define *ptr = PeekI(*Stack) - 8
			PokeI(*ptr, value)
			PokeI(*Stack, *ptr)
		CompilerEndIf
		ProcedureReturn
	EndProcedure
	
	Procedure PopInteger(*Stack)
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			CompilerIf #PB_Compiler_Processor = #PB_Processor_x64
				EnableASM
				!MOV rcx, [p.p_Stack]
				!MOV rdx, [rcx]
				!MOV rax, [rdx]
				!ADD rdx, 8
				!MOV [rcx], rdx
				DisableASM
			CompilerElse
				EnableASM
				!MOV ecx, [p.p_Stack]
				!MOV edx, [ecx]
				!MOV eax, [edx]
				!ADD edx, 4
				!MOV [ecx], edx
				DisableASM
			CompilerEndIf
		CompilerElse
			Define *ptr = PeekI(*Stack)
			PokeI(*Stack, *ptr+8)
			ProcedureReturn PeekI(*ptr)
		CompilerEndIf
		ProcedureReturn
	EndProcedure
	
	Procedure Free(*Stack)
		Define *ptr
		CompilerIf #PB_Compiler_Backend <> #PB_Backend_C
			EnableASM
			!MOV rcx, [p.p_Stack]
			!MOV rax, [rcx+8]
			!SUB rcx, rax
			!MOV [p.p_ptr], rcx
			DisableASM
			GlobalMemoryCache::Free(*ptr)
		CompilerElse
			Define ptr
			Define stack = *Stack
			BeginAsm()
			!"MOV rcx, %0;"
			!"MOV rax, [rcx+8];"
			!"SUB rcx, rax;"
			!"MOV %1, rcx;"
			AsmOutputInput(ptr, stack) 
			AsmClobbers() "rcx", "rax"
			EndAsm()
			GlobalMemoryCache::Free(ptr)
		CompilerEndIf
		ProcedureReturn
	EndProcedure
	
EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; Folding = ------
; EnableXP
; DPIAware