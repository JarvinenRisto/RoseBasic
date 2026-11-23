XIncludeFile "LinkedList.pb"
XIncludeFile "Stack.pb"
XIncludeFile "SymbolTable.pb"
XIncludeFile "ByteCode.pb"
XIncludeFile "FileReader.pbi"
XIncludeFile "RPN.pb"

Module TokenList
	#MAX_CIRCULAR = 20
	
	Structure JumpTarget
		*_name
		*_class
		_line.i
		_jump_ip.i
	EndStructure
	
	Global.i circularReference = 0
	Global.i stringCount
	Global numberCount, lastEvalType, lastEvalParamSize
	Global.SymbolTable::SFunction *rootLine
	Global isParallel.i, hasFunctionId
	
	Declare FunctionParameters(*next.LinkedList::Node, *F.SymbolTable::SFunction)
	
	Global gParallel = 0
	
	Macro moveNext()
		If *next <> #Null And *next > 1000
			If *Next\next < 1000 
				ProcedureReturn 0
			EndIf
			*next = *next\next
			
			If *next < 1000
				ProcedureReturn 0
			EndIf
		EndIf
		If *next <> #Null And *next\value > 1000
			*token = *next\value
		EndIf
	EndMacro
	
	Global *gCurrentClass.SymbolTable::SClass
	
	Macro returnIfNull()
		If *next = #Null
			ProcedureReturn 0
		EndIf
	EndMacro
	
	Global.i lastStackEval = 0
	
	Procedure expect(*token.Token, id, line)
	  If *token And *token\type <> id
	    Debug "type " + *token\type
		  Error::display(*token\fileName, *token\line, "Expected "+TokenText(id)+" but got "+TokenText(*token\type)+".")
		  ProcedureReturn 1
		ElseIf Not *token
			Error::display(@"", -1, "Found extra , (comma)")
		EndIf
	EndProcedure
	
	Procedure peek(*next.LinkedList::Node)
		If *next And *Next\next <> #Null
			Define.Token *token = *next\next\value
			If *token
				ProcedureReturn *token\type
			EndIf
		EndIf
	EndProcedure
	
	Procedure isType(id)
		If id = #ANY Or id = #NUMBER Or id = #STRING
			ProcedureReturn id
		EndIf
		ProcedureReturn 0
	EndProcedure
	
	Macro isDType(type)
		(type = #ANY Or type = #NUMBER Or type = #STRING Or type = #IDENFIER)
	EndMacro
	
	Procedure handleValue(*Sym.SymbolTable::SSymbol)
		Define.SymbolTable::SConstant *C = *Sym
		Define.SymbolTable::SConstant *C2

		Select *C\type
			Case #TINTEGER
				numberCount = numberCount + 1
			
				ByteCode::EmitOp(#OP_CONSTANT)
				ByteCode::EmitInteger(*C\integer)
				lastEvalType = #TINTEGER
			Case #TDOUBLE
				numberCount = numberCount + 1
				ByteCode::EmitOp(#OP_CONSTANT)
				ByteCode::EmitDouble(*C\double)
				lastEvalType = #TDOUBLE
			Case #TSTRING
				stringCount = stringCount + 1
				ByteCode::EmitOp(#OP_CONSTANT)
				ByteCode::EmitString(*C\string)
				lastEvalType = #TSTRING
			Case #IDENFIER
				*Sym = SymbolTable::LookupSymbol(*C\string)
	
				If *Sym And *Sym\Kind = SymbolTable::#SKIND_CONST
					If circularReference > #MAX_CIRCULAR
						ProcedureReturn
					EndIf
					circularReference = circularReference + 1
					handleValue(*Sym)
				EndIf
		EndSelect
	EndProcedure
	
	Global gArrayIndex.i, gArrayDimension.i, isFunctionStatic
	Global *newClass
	Global *currentClass, *currentSym, finalEvalType
	Declare RelExp(*Next.LinkedList::Node)
	Declare AddExp(*next.LinkedList::Node)
	Declare compileFunction(*F.SymbolTable::SFunction) 
	
	Procedure runInlining(*function.ObjFunction, *chunk.Interpreter::Chunk)
  ; create a temporary frame pointing to the chunk we inline
  Define.Interpreter::CallFrame *frame = Interpreter::gFrames(Interpreter::gFrameCount)
  Interpreter::gFrameCount + 1

  *frame\function = *function
  *frame\ip       = *chunk\code

  Define.i max = *frame\ip + *chunk\count
  Define.b op = 0
  Define.l l = 0

  While *frame\ip < max
    ; read the opcode byte and advance ip
    op = *frame\ip\b : *frame\ip + SizeOf(Byte)

    Select op

      Case #OP_CALL_NATIVE
        ByteCode::EmitOP(#OP_CALL_NATIVE)
        ; read next byte (arg count) and long (native index)
        op = *frame\ip\b : *frame\ip + SizeOf(Byte)
        l  = PeekL(*frame\ip) : *frame\ip + SizeOf(Long)
        ByteCode::EmitByte(op)
        ByteCode::EmitLong(l)

      Case #OP_CALL
        ByteCode::EmitOP(#OP_CALL)
        op = *frame\ip\b : *frame\ip + SizeOf(Byte)
        ByteCode::EmitByte(op)

      Case #OP_INVOKE
        ByteCode::EmitOP(#OP_INVOKE)
        ; read constant index (long), fetch boxed constant (q), unbox string
        l = PeekL(*frame\ip) : *frame\ip + SizeOf(Long)
        Define.q constVal = PeekQ(*frame\function\chunk\constants\values + l)
        Define.ObjString *string = Interpreter::AS_STRING(constVal)
        ; emit string literal into new chunk (your EmitString should add constant into current chunk)
        ByteCode::EmitString(*string\chars)
        ; then read and emit arg count byte
        op = *frame\ip\b : *frame\ip + SizeOf(Byte)
        ByteCode::EmitByte(op)

      Case #OP_TRUE
        ByteCode::EmitOP(#OP_TRUE)

      Case #OP_FALSE
        ByteCode::EmitOP(#OP_FALSE)

      Case #OP_GET_PROPERTY
        ByteCode::EmitOP(#OP_GET_PROPERTY)
        l = PeekL(*frame\ip) : *frame\ip + SizeOf(Long)
        Define.q propConst = PeekQ(*frame\function\chunk\constants\values + l)
        Define.ObjString *propName = Interpreter::AS_STRING(propConst)
        ByteCode::EmitString(*propName\chars)

      Case #OP_ADD
        ByteCode::EmitOP(#OP_ADD)

      Case #OP_SUBTRACT
        ByteCode::EmitOP(#OP_SUBTRACT)

      Case #OP_MULTIPLY
        ByteCode::EmitOP(#OP_MULTIPLY)

      Case #OP_DIVIDE
        ByteCode::EmitOP(#OP_DIVIDE)

      Case #OP_MOD
        ByteCode::EmitOP(#OP_MOD)

      Case #OP_POW
        ByteCode::EmitOP(#OP_POW)

      Case #OP_NEGATE
        ByteCode::EmitOP(#OP_NEGATE)

      Case #OP_NOT_EQU
        ByteCode::EmitOP(#OP_NOT_EQU)

      Case #OP_EQUAL
        ByteCode::EmitOP(#OP_EQUAL)

      Case #OP_LESS
        ByteCode::EmitOP(#OP_LESS)

      Case #OP_GREATER
        ByteCode::EmitOP(#OP_GREATER)

      Case #OP_LT_EQU
        ByteCode::EmitOP(#OP_LT_EQU)

      Case #OP_GRT_EQU
        ByteCode::EmitOP(#OP_GRT_EQU)

      Case #OP_NOT
        ByteCode::EmitOP(#OP_NOT)

      Default
        ; Unhandled opcode in inlining: just skip its operands conservatively (or stop)
        ; Here we simply stop inlining to be safe.
        ProcedureReturn
    EndSelect
  Wend
EndProcedure

	
	Procedure value(*next.LinkedList::Node)
	  Define.s klassName
		returnIfNull()
		*currentClass = 0
		*currentSym = 0
		
		Define.Token *token = *next\value
		
		If *token 
			Select *token\type
				Case #TINTEGER
					numberCount = numberCount + 1
					lastEvalType = #TINTEGER
					ByteCode::EmitOp(#OP_CONSTANT)
					ByteCode::EmitInteger(*token\integer)
				Case #TDOUBLE
					numberCount = numberCount + 1
					lastEvalType = #TDOUBLE
					ByteCode::EmitOp(#OP_CONSTANT)
					ByteCode::EmitDouble(*token\double)
				Case #IDENFIER
					lastEvalType = #IDENFIER
					Define.SymbolTable::SSymbol *sym = SymbolTable::LookupSymbol(*token\string)
					
					klassName = PeekS(*token\string)
					
					reSymbol:
					If *Sym  
						If *Sym\Kind = SymbolTable::#SKIND_CONST
							circularReference = 0
							handleValue(*Sym)
						ElseIf *Sym\Kind = SymbolTable::#SKIND_CLASS
							*currentClass = *sym\name
							
							If peek(*next) = #DOT
								moveNext()
								If peek(*next) <> #IDENFIER
									Error::display(*token\fileName, *token\line, "-")
								Else
									moveNext()
								EndIf
								Define.i length = 2 + MemoryStringLength(*sym\name) + MemoryStringLength(*token\string)
								Define.FastString::STRFASTOBJ *obj = FastString::new(length)
								FastString::add(*obj, *sym\name)
								FastString::add(*obj, @":")
								FastString::add(*obj, *token\string)
								*sym = SymbolTable::LookupSymbolNoScopeResolve(*obj\data+4)
								FastString::Free(*obj)
								Goto reSymbol
							Else
								ByteCode::EmitOP(#OP_CLASS)
								ByteCode::EmitString(*sym\name)
								lastEvalType = #CLASS_OBJ
							EndIf
						ElseIf *Sym\kind = SymbolTable::#SKIND_Function
							*rootLine = 0
							moveNext()
							expect(*token, #LPAREN, #PB_Compiler_Line)
							
							Define.SymbolTable::SFunction *F = *Sym
						
							If CompareMemoryString(@"this", @klassName) <> 0
							  
							  If *F\id <> -1
							    *next = FunctionCallDeclared(*next\next, *F)
							  Else
							    Define.SymbolTable::SSymbol *tmpSym = SymbolTable::LookupSymbol(@klassName)
							    If *tmpSym\kind = SymbolTable::#SKind_Class
							      ByteCode::EmitOp(#OP_GET_GLOBAL)
							      ByteCode::EmitString(@klassName) 
							    ElseIf *tmpSym\kind = SymbolTable::#SKind_Variable
							      Define.SymbolTable::SVariable *tmpVar = *tmpSym
							      If *tmpVar\sym <> 0
							        klassName = PeekS(*tmpVar\sym\name)
							      EndIf
							    EndIf 
							  
							    *next = FunctionCallDeclared(*next\next, *F, 1)
							    
							    If *F\inlineyes
							      Define.Interpreter::Chunk *chunk = *F\fObj\chunk
							      
							      If Not *chunk\code
							        Define *currentFunction = ByteCode::*gFunction 
							        compileFunction(*F)
							        ByteCode::*gFunction = *currentFunction
							      EndIf
	
							      runInlining(*F\fObj, *chunk)
							    Else
  							    ByteCode::EmitOp(#OP_INVOKE)
  							    Define.s str = klassName +":"+PeekS(*F\name)
  							    ByteCode::EmitString(@str)
  							    ByteCode::EmitByte(*F\ParamCount)
							    EndIf
							  EndIf
							Else
							  *next = FunctionCallDeclared(*next\next, *F, 1)
							  
                ByteCode::EmitOp(#OP_INVOKE)
                Define fName.s = PeekS(SymbolTable::getPreviousScope()+2)+":"+PeekS(*F\name)
                ByteCode::EmitString(@fName)             
                ByteCode::EmitByte(*F\ParamCount)
              EndIf

							*token = *next\value
							expect(*token, #RPAREN, #PB_Compiler_Line)
							
						ElseIf *Sym\kind = SymbolTable::#SKind_Variable
							Define.SymbolTable::SVariable *Var = *Sym 
							
							If *var\visible <> 0 And SymbolTable::LookupSymbol(*Sym\name) = 0
							  Error::Display(*token\fileName, *token\line, "Idenfier: "+PeekS(*Sym\name)+" is not Public")
							EndIf
							
  							Select *Var\RefKind
  								Case SymbolTable::#Ref_Class
  									lastEvalType = *Var\Type
  									
  									If *var\dimensionSize > 0
  									  moveNext()
  									  
  									  Expect(*token, #LPAREN, #PB_Compiler_Line)
  							      Define.i i, value = 0
  
          			      For i = 1 To *Sym\dimensionSize 
            					    moveNext()
            					    
            					    Define.SymbolTable::SVariable *Var = *Sym
            					    gArrayIndex = *Var\RefOffset
            					          					   
            					    *next = LogicalExp(*next)
  					              *next = *next\prev
  
            					    If *Sym\dimensionSize > 1 And i <> *Sym\dimensionSize
            					  
            					      If *Sym\dimensions = i
            					        Break
            					      EndIf
            					      moveNext()
            					      
            					      If Expect(*token, #COMMA, #PB_Compiler_Line) = 1
            					        PrintN("The error was in Array "+PeekS(*Sym\name)) 
            					        Break
            					      EndIf
            					    EndIf
            					  Next i
            					  
  							
  							      
  							      moveNext()
  							      Expect(*token, #RPAREN, #PB_Compiler_Line)
  							    Else
  							      ;ByteCode::EmitOP(#OP_GET_LOCAL)
  							      ;ByteCode::EmitLong(0)
  								    ByteCode::EmitOP(#OP_GET_PROPERTY)
  									  ByteCode::EmitString(*Var\name)
  									EndIf
  								Case SymbolTable::#Ref_Local
  								  lastEvalType = *Var\Type
  								  
  									If *var\dimensionSize > 0
  							      moveNext()
  							      Expect(*token, #LPAREN, #PB_Compiler_Line)
  							      Define.i i, value = 0
  
          			      For i = 1 To *Sym\dimensionSize 
            					    moveNext()
            					    
            					    Define.SymbolTable::SVariable *Var = *Sym
            					    gArrayIndex = *Var\RefOffset
            					          					    
            					    If *Sym\arrayInit = 0
            					      ByteCode::EmitOp(#OP_CONSTANT)
            					      ByteCode::EmitInteger(10+(*Sym\dimensions * (*Sym\dimensionSize)) * 10)
            					      ByteCode::EmitOp(#OP_ALLOC_ARRAY)
            					      ByteCode::EmitByte(gArrayIndex)
            					      ByteCode::EmitByte(*Sym\dimensionSize)
            					      *Sym\arrayInit = 1
            					    EndIf
            					    
            					    *next = LogicalExp(*next)
  					              *next = *next\prev
  
            					    If *Sym\dimensionSize > 1 And i <> *Sym\dimensionSize
            					  
            					      If *Sym\dimensions = i
            					        Break
            					      EndIf
            					      moveNext()
            					      
            					      If Expect(*token, #COMMA, #PB_Compiler_Line) = 1
            					        PrintN("The error was in Array "+PeekS(*Sym\name)) 
            					        Break
            					      EndIf
            					    EndIf
            					  Next i
            					  
  							      ByteCode::EmitOP(#OP_GET_ARRAY)
  							      ByteCode::EmitByte(gArrayIndex)
  							      
  							      moveNext()
  							      Expect(*token, #RPAREN, #PB_Compiler_Line)
  							    Else
  							      Define.i value = *Var\refOffset*SizeOf(quad)
  	
  									  ByteCode::EmitOP(#OP_GET_LOCAL)
  									  ByteCode::EmitLong(value)
  									EndIf
  								Case SymbolTable::#Ref_Parameter
  									lastEvalType = *Var\Type
  									lastEvalParamSize + 8
  									ByteCode::EmitOP(#OP_GET_LOCAL)
  								  ByteCode::EmitLong((1+*Var\refOffset)*SizeOf(quad))
  							EndSelect
  							
  							
							If *Var\Sym <> 0 And peek(*next) = #DOT
								moveNext()
								moveNext()
								expect(*token, #IDENFIER, #PB_Compiler_Line)
								
								Define.i len = 2 + MemoryStringLength(*Var\Sym\name) + MemoryStringLength(*token\string)
								Define.FastString::STRFASTOBJ *obj = FastString::new(len)
								FastString::add(*obj, *Var\Sym\name)
								FastString::add(*obj, @":")
								FastString::add(*obj, *token\string)
								*sym = SymbolTable::lookupSymbol(*obj\data+4)
								FastString::Free(*obj)
								
								If *sym 
									*currentClass = *Sym\name
									Goto reSymbol
								EndIf
							EndIf
		
						EndIf
					ElseIf Not *Sym
						If *currentClass
							Error::display(*token\fileName, *token\line, "Idenfier: "+PeekS(*currentClass)+"."+PeekS(*token\string)+" is not declared")
						Else
							Error::display(*token\fileName, *token\line, "Idenfier: "+PeekS(*token\string)+" is not declared")
						EndIf
						*currentClass = 0
					EndIf
		
				Case #TSTRING
				  stringCount = stringCount + 1
				  If hasFunctionId >= 64
				    ByteCode::EmitOp(#OP_CONST_NATIVE)
				  Else
				    ByteCode::EmitOp(#OP_CONSTANT)
				  EndIf
					ByteCode::EmitString(*token\string)
					lastEvalType = #TSTRING
				Case #LPAREN
				  moveNext()
				  *next = LogicalExp(*next)
				  *token = *next\value
				  expect(*token, #RPAREN, #PB_Compiler_Line)
				Case #NEW
					moveNext()
					expect(*token, #IDENFIER, #PB_Compiler_Line)
					Define.i line = *token\line
					Define.SymbolTable::SClass *SS = SymbolTable::LookupSymbol(*token\string)
					If *SS = 0 Or *SS\kind <> SymbolTable::#SKIND_CLASS
						Error::Display(*token\fileName, *token\line, "Class "+PeekS(*token\string)+" is not declared")
					EndIf
					*newClass = *SS
					Define *name = *token\string
					moveNext()
					expect(*token, #LPAREN, #PB_Compiler_Line)
					moveNext()
					
					Define *new = @"new"
					Define.i length = 2 + MemoryStringLength(*name) + MemoryStringLength(*new)
					Define.FastString::STRFASTOBJ *obj = FastString::new(length)
					FastString::Add(*obj, *name)
					FastString::Add(*obj, @":")
					FastString::Add(*obj, *new)
					Define.SymbolTable::SFunction *F = SymbolTable::LookupSymbol(*obj\data+4)
					FastString::Free(*obj)
		
					If *F
					  *next = FunctionParameters(*Next, *F)
					Else
					  Error::Display(*token\fileName, line, "Constructor is missing from a class: "+PeekS(*SS\name))
					  ProcedureReturn *next
					EndIf
					
					ByteCode::EmitOP(#OP_CLASS)
					ByteCode::EmitString(*name) 
					ByteCode::EmitOp(#OP_CALL)
					ByteCode::EmitByte(1)
					
					If *F\id <> -1
            ByteCode::EmitOp(#OP_CALL_NATIVE)
            ByteCode::EmitByte(*F\ParamCount)
            ByteCode::EmitLong(*F\id)
          Else
          	ByteCode::EmitOp(#OP_INVOKE_CONSTR)
  					Define.s klass = PeekS(*name)+":"+PeekS(*F\name) 
  					ByteCode::EmitString(@klass) 
  					ByteCode::EmitByte(*F\ParamCount)
          EndIf
					
					*token = *next\value
					expect(*token, #RPAREN, #PB_Compiler_Line)
					
			EndSelect
		EndIf
	
		ProcedureReturn *next\next
	EndProcedure
	
	Procedure notExp(*next.LinkedList::Node)
		returnIfNull()

		Define.Token *token = *next\value
		Define.i type = *token\type
		
		If type = #NOT
			moveNext()
			*next = value(*next)
			ByteCode::EmitOP(#OP_NOT)
		Else
			*next = value(*next)
		EndIf
	
		ProcedureReturn *next
	EndProcedure
	
	Procedure unaryExp(*next.LinkedList::Node)
		returnIfNull()

		Define.Token *token = *next\value
		Define.i type = *token\type
		
		If type = #NEGATE
			moveNext()
			*next = notExp(*next)
			ByteCode::EmitOP(#OP_NEGATE)
		Else
			*next = notExp(*next)
		EndIf
	
		ProcedureReturn *next
	EndProcedure
	
	Procedure ExponentExp(*next.LinkedList::Node)
		returnIfNull()

		*next = unaryExp(*next)
		Define.Token *token = *next\value
		Define.i type = *token\type
		
		While *token And (type = #POW)
			moveNext()
			*next = unaryExp(*next)
			*token = *next\value
			
			Select type
				Case #POW
				  ByteCode::EmitOP(#OP_POW)
			EndSelect
			type = *token\type
		Wend
		
		ProcedureReturn *next
	EndProcedure
	
	Procedure mulExp(*next.LinkedList::Node)
		returnIfNull()

		*next = ExponentExp(*next)
		Define.Token *token = *next\value
		Define.i type = *token\type
		
		While *token And (type = #MUL Or type = #DIV Or type = #MOD)
			moveNext()
			*next = ExponentExp(*next)
			*token = *next\value
			
			Select type
				Case #MUL
				  ByteCode::EmitOP(#OP_MULTIPLY)
				Case #DIV
				  ByteCode::EmitOP(#OP_DIVIDE)
				Case #MOD
					ByteCode::EmitOP(#OP_MOD)
			EndSelect
			type = *token\type
		Wend
		
		ProcedureReturn *next
	EndProcedure
	
	Procedure addExp(*next.LinkedList::Node)
		returnIfNull()

		*next = mulExp(*next)
		Define.Token *token = *next\value
		Define.i type = *token\type
		
		While *token And (type = #PLUS Or type = #MINUS Or type = #PLUS_EQUAL)
			moveNext()
			*next = mulExp(*next)
			*token = *next\value
			
			Select type
			  Case #PLUS_EQUAL
			    End
			  Case #PLUS
					ByteCode::EmitOP(#OP_ADD)
				Case #MINUS
				  ByteCode::EmitOP(#OP_SUBTRACT)
			EndSelect
			type = *token\type
		Wend
		
		ProcedureReturn *next
	EndProcedure
	
	Procedure RelExp(*Next.LinkedList::Node)
		returnIfNull()
		
		*next = addExp(*next)
		Define.Token *token = *next\value
		
		Define.i type = *token\type
		
		While *token And (type = #GRT Or type = #LT Or type = #EQU Or type = #NOT_EQU Or type = #LT_EQU Or type = #GRT_EQU)
			*next = addExp(*next\next)
			*token = *next\value
			
			Select type
			  Case #GRT
				    ByteCode::EmitOp(#OP_GREATER)
				Case #LT
				    ByteCode::EmitOp(#OP_LESS)
				Case #EQU
					ByteCode::EmitOp(#OP_EQUAL)
				Case #NOT_EQU
					ByteCode::EmitOp(#OP_NOT_EQU)
				Case #LT_EQU
					ByteCode::EmitOp(#OP_LT_EQU)
				Case #GRT_EQU
					ByteCode::EmitOp(#OP_GRT_EQU)
			EndSelect
			type = *token\type
		Wend
		
		ProcedureReturn *next
	EndProcedure
	
	Procedure LogicalExp(*Next.LinkedList::Node)
		returnIfNull()
		
		*next = RelExp(*next)
		Define.Token *token = *next\value
		
		Define.i type = *token\type
		
		While *token And (type = #OPERATOR_OR Or type = #AND Or type = #XOr)
			*next = RelExp(*next\next)
			*token = *next\value
			
			Select type
				Case #OPERATOR_OR
					Define.i elseJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)
				  Define.i endJump = ByteCode::emitJump(#OP_JUMP)
				  ByteCode::patchJump(elseJump)
				  ByteCode::EmitOp(#OP_POP)
				  *next = RelExp(*next)
				  *token = *next\value
				  ByteCode::patchJump(endJump)
				Case #AND
          Define.i endJump = ByteCode::emitJump(#OP_JUMP_IF_FALSE)
				  ByteCode::EmitOp(#OP_POP)
				  *next = RelExp(*next)
				  *token = *next\value
				  ByteCode::patchJump(endJump)
			  Case #XOR
          *next = RelExp(*next)
          *token = *next\value
          ByteCode::EmitOp(#OP_XOR)
      EndSelect
      
			type = *token\type
		Wend
		
		ProcedureReturn *Next
	EndProcedure
	
	Procedure isOptional(id)
		If id = #TSTRING_OPTIONAL Or id = #TINTEGER_OPTIONAL Or id = #TDOUBLE_OPTIONAL
			ProcedureReturn #True
		EndIf
		ProcedureReturn #False
	EndProcedure
	
Procedure FunctionParameters(*next.LinkedList::Node, *F.SymbolTable::SFunction)
    returnIfNull()
    
    If *F = #Null
        ProcedureReturn
    EndIf

    Protected.i I, last = *F\ParamCount
    Define.Token *token 
    Define.LinkedList::Node *paramType
    Define.LinkedList::Node *List

    If *F\paramTypes
        *paramType = *F\paramTypes\head
    EndIf

    *token = *next\value
    Debug "Total parameters: " + Str(last)

    For I = 1 To last
        If *paramType
            finalEvalType = *paramType\value
            Debug "Processing parameter " + Str(I) + ", Expected Type: " + Str(finalEvalType)

            *next = LogicalExp(*next)
            
            If *next = #Null
                Debug "Error: *next became NULL after LogicalExp() on parameter " + Str(I)
                ProcedureReturn #Null
            EndIf

            *token = *next\value
            Debug "Token after LogicalExp: " + TokenList::TokenText(*token\type)

            If *paramType\value = #TDOUBLE Or *paramType\value = #TDOUBLE_OPTIONAL
                Select lastEvalType
                    Case #TSTRING, #STRING
                        Error::Display(*token\fileName, *token\line, "Parameter expects a Number value, not a String")
                EndSelect
            EndIf

            If *next And I <> last
                *token = *next\value
                
                If *token\type <> #comma And Not isOptional(*paramType\value)
                    Error::Display(*token\fileName, *token\line, "Function/Sub: "+PeekS(*F\name)+" parameter: "+TokenList::TokenText(*token\type)+" is not optional parameter")
                    ProcedureReturn #Null
                EndIf
                
                expect(*token, #COMMA, #PB_Compiler_Line)
                
                If *next\next = #Null
                    Debug "Error: *next\next is NULL before moving to the next parameter."
                    ProcedureReturn #Null
                EndIf

                *next = *next\next
                If *paramType\next
                    *paramType = *paramType\next
                Else
                    Debug "Warning: paramType ran out of parameters early."
                EndIf
            EndIf
        Else
            Debug "Error: paramType is NULL at parameter " + Str(I)
            ProcedureReturn #Null
        EndIf
    Next I

    finalEvalType = 0
    ProcedureReturn *next
  EndProcedure
  
  Global.SymbolTable::SVariable *gCurrentVar
  
	Procedure FunctionCallDeclared(*next.LinkedList::Node, *F.SymbolTable::SFunction, noEmit = 0)
	  returnIfNull()
		Define.Token *token = *next\value

		Define.i line = *token\line
		
		If *F And *F\kind = SymbolTable::#Skind_function
		  
		  hasFunctionId = *F\id 

		  If *F\ParamCount > 0
		    isFunctionStatic = *F\isStatic
				*next = FunctionParameters(*next, *F)
			EndIf
			
			If noEmit = 0
  			If CompareMemoryString(*F\name, @"debug") = 0
  			  ByteCode::EmitOp(#OP_PRINT)
  			Else
    			If *F\id <> -1
            ByteCode::EmitOp(#OP_CALL_NATIVE)
            ByteCode::EmitByte(*F\ParamCount)
            ByteCode::EmitLong(*F\id)
            If *F\Type = #SUBR
              ByteCode::EmitOp(#OP_POP)
            EndIf
          Else
            ByteCode::EmitOp(#OP_CALL)
          	ByteCode::EmitByte(*F\ParamCount)
          EndIf
        EndIf
      EndIf
    
			lastEvalType = *F\type
		EndIf
		
		lastEvalParamSize = 0
		ProcedureReturn *next
	EndProcedure
	
	Global gVarCount = 0
	
	Procedure FunctionCall(*next.LinkedList::Node, *name)
		returnIfNull()

		Define.Token *token = *next\value
		Define.i line = *token\line
		Define *F.SymbolTable::SFunction = SymbolTable::LookupSymbol(*name)

		If *F And *F\kind = SymbolTable::#Skind_function
		  
		  Debug *F\inlineYes
		  Debug "_------_"
		  
		  If *F\ParamCount > 0
		    isFunctionStatic = *F\isStatic
				*next = FunctionParameters(*next, *F)
      EndIf
      		
      If *F\id <> -1
        ByteCode::EmitOp(#OP_GET_GLOBAL)
			  ByteCode::EmitString(*F\name)
			  ByteCode::EmitOp(#OP_CALL)
      	ByteCode::EmitByte(*F\ParamCount)
      Else
        ByteCode::EmitOp(#OP_GET_GLOBAL)
			  ByteCode::EmitString(*F\name)
			  ByteCode::EmitOp(#OP_CALL)
      	ByteCode::EmitByte(*F\ParamCount)
      EndIf
      		
      lastEvalType = *F\type
    Else
      Error::Display(*token\fileName, line, "Error undeclared function "+PeekS(*name))
      		
      While *token
     	  moveNext()
      Wend
      		
      ProcedureReturn *next
		EndIf
		
		lastEvalParamSize = 0
		ProcedureReturn *next
	EndProcedure
	
	Structure Cases
	  List *token.Token()
	  count.i
	EndStructure
	
	Global gExtendsYes
	Global.SymbolTable::SFunction *gFunc
	Global *currentModule, *currentClass
	
	Procedure FunctionParser(type.i, *next.LinkedList::Node, isStatic.i)
		Define.i localCount = 1
		returnIfNull()
		
		Define.Token *token = *next\value
		expect(*token, #IDENFIER, #PB_Compiler_Line)
		
		Define.i currentLine = *token\line
	
		Protected.SymbolTable::SFunction *F = SymbolTable::addFunction(*token\string, *next)
	
		If *F = #Null
		  While *token And (*token\type <> #END_FUNCTION And *token\type <> #END_SUBR)
		    moveNext()
		  Wend
		  ProcedureReturn *next
		EndIf
		
		*F\fObj = Interpreter::newFunction()
		*F\fObj\name = *F\name
		*F\fObjValue = Interpreter::OBJ_VAL(*F\fObj)
; 		*F\fObjValue = AllocateStructure(Interpreter::Value)
; 	  *F\fObjValue\type = Interpreter::#VAL_OBJ
; 	  *F\fObjValue\obj = *F\fObj
; 	  
		Interpreter::globals(PeekS(SymbolTable::GetCurrentScope()+2)+":"+PeekS(*token\string)) = *F\fObjValue 
		Debug "FunctionParser "+PeekS(SymbolTable::GetCurrentScope()+2)+":"+PeekS(*token\string)
		
		AddElement(ByteCode::*gFunctionList())
		ByteCode::*gFunctionList() = *F\fObj
		SymbolTable::EnterScope(*F\name)
		
		moveNext()
		expect(*token, #LPAREN, #PB_Compiler_Line)
		
		Define.i offset = 1
		Define *string

		;Define.SymbolTable::SSymbol *S = SymbolTable::LookupSymbol(SymbolTable::GetPreviousScope()+2)
		Define.SymbolTable::SSymbol *S = *gCurrentClass
		
		Define.SymbolTable::SVariable *THIS = SymbolTable::AddVariableClass(@"this", #IDENFIER, *S, SymbolTable::#Ref_CLASS, 0)
		*THIS\sym = *S 
		*THIS\isInit = 1
		
		If gExtendsYes
	    Define.SymbolTable::SVariable *SUPER = SymbolTable::AddVariableClass(@"super", #IDENFIER, *S, SymbolTable::#Ref_CLASS, 1)
	    *SUPER\isInit = 1
	    ;offset = 2
	  EndIf
	
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
				
				If *token\type = #IDENFIER
					Define *Sym = SymbolTable::LookupSymbol(*token\string)
					If Not *Sym
						Error::Display(*token\fileName, *token\line, "")
					EndIf
					*V = SymbolTable::AddVariableClass(*string, *token\type, *Sym, SymbolTable::#Ref_Parameter, offset)
					*V\isStatic = isStatic
				Else
				  *V = SymbolTable::AddVariable(*string, *token\type, SymbolTable::#Ref_Parameter, offset)
				  *V\isStatic = isStatic
				EndIf
				offset = offset + 1
				
				If *F\ParamTypes = 0
					*F\ParamTypes = LinkedList::Init()
					LinkedList::AddLastInt(*F\ParamTypes, *token\type)
				Else
					
				EndIf
				moveNext()
			EndIf
		Wend
		
		*F\id = -1
		*F\paramCount = offset
		*F\type = #NUMBER
		*F\visible = gVisible
		*F\isStatic = isStatic
		
		localCount = offset
		
		If peek(*next) = #AS
			moveNext()
			moveNext()
			If Not ISDType(*token\type)
				Error::Display(*token\fileName, *token\line, "Expected a type, but got "+TokenText(*token\type))
			EndIf
			*F\type = *token\type
		EndIf
		
		Define.i id = type + 1
		Define.i line = *token\line
		NewList tempToken.Cases()
				
		*F\entry = LinkedList::init()
		Define currentKey = *next\key + 1
		
		LinkedList::addFirstInt(*functions, *F)
		
		Define returnUsed = #False
		
		While *token And *token\type <> id
			moveNext()
			
			If *token
				Select *token\type
					Case #DIM
						Define.i start = *next\key
						moveNext()
						Define currentLocal = localCount
						*next = DimParser(*next, SymbolTable::#Ref_Local, @localCount, isStatic)
						;LinkedList::DeleteRange(*Tokens, start+2, *next\key)
						Define.Token T, T2
						T\type = #INIT_VAR
						T\Integer = currentLocal
						LinkedList::insert(*Tokens, start, @T, SizeOf(Token))
						T2\type = #VAR_COUNT
						T2\Integer = localCount - 1
						LinkedList::insert(*Tokens, start+1, @T2, SizeOf(Token))
						
						Define.i A, B = start + 2
						T\type = #SKIP
						For A = B To *next\key
						  LinkedList::insert(*Tokens, A, @T, SizeOf(Token))
						Next 
; 					Case #SELECT
; 					  *token\type = #IF
; 					  moveNext()
; 					  AddElement(tempToken())
; 					  InitializeStructure(tempToken(), Cases)
; 					  
; 					  Repeat
; 					    AddElement(tempToken()\token())
; 					    tempToken()\token() = *token
; 					    moveNext()
; 					  Until *token\type = #case
; 					  
; 					  *next = *next\prev
; 					Case #CASE
; 					  If tempToken()\count > 0
; 					    *token\type = #ELSEIF
; 					    Define.i i = 0
; 					    Define.i start = *next\key
; 					    ForEach tempToken()\token()
; 					      LinkedList::insertAfter(*Tokens, *next\key+i, tempToken()\token(), SizeOf(Token))
; 					      i + 1
; 					    Next
; 					    
; 					    Define.Token T
; 					    T\type = #EQU
; 					    LinkedList::insertAfter(*Tokens, *next\key+i, @T, SizeOf(Token))
; 					  Else
; 					    *token\type = #EQU
; 					  EndIf
; 					  tempToken()\count + 1
; 					Case #DEFAULT
; 					  *token\type = #ELSE
; 					Case #ENDSELECT
; 					  *token\type = #ENDIF
; 					  DeleteElement(tempToken(), #PB_List_Last)
					Case #RETURN
					  returnUsed = #True
				EndSelect
			EndIf
		Wend
		
		Define *value
		Define.i finishKey = *next\key + 1
		
		While currentKey < *next\key
		  *value = LinkedList::SelectKeyValue(*Tokens, currentKey)
		  LinkedList::addLastInt(*F\entry, *value)
		  currentKey + 1
		Wend
		
		*F\currentProperty = gVarCount
		*F\fObj\localSize = localCount
		
		If *token = #Null
			PrintN("Line "+Str(line)+": Function "+PeekS(*F\name)+" is missing EndFunction")
		EndIf
		
		If id = #END_SUBR
			*F\type = #SUBR
		Else
			
		EndIf
		
		If *currentModule
		  *F\module = AllocateMemory(MemoryStringLength(*currentModule, #PB_ByteLength)+2)
		  PokeS(*F\module, PeekS(*currentModule))
	  EndIf
	  
	  If *currentClass
		  *F\class = AllocateMemory(MemoryStringLength(*currentClass, #PB_ByteLength)+2)
		  PokeS(*F\class, PeekS(*currentClass))
	  EndIf
	
		*gFunc = *F
		SymbolTable::leaveScope()
		
		If (*token\line-currentLine) = 2 And returnUsed And *F\paramCount = 1
		  ;*F\inlineYes = 1
		EndIf
		
		ProcedureReturn *next
	EndProcedure
	
	Macro parseClassVar(gToken)
	   If *Sym\isStatic = 0 
	     Define.SymbolTable::SSymbol *tempSym = SymbolTable::LookupSymbol(*string)
	     
			 If *tempSym\kind <> 0
			    Error::Display(gToken\line, "Class '"+PeekS(*gC\name)+"' function: "+PeekS(gToken\string)+" is not a Static", #PB_Compiler_Line)
				  Break
				EndIf
			 EndIf
					      
	    Select gToken\type
			  Case #EQU
				  *next = Assignment(*gS, *Next)
				Default
				  If *rootLine = 0
					  *rootLine = *Sym
					EndIf
					Expect(gToken, #LPAREN, #PB_Compiler_Line)
		      Define.SymbolTable::SFunction *F = *Sym
				  ByteCode::EmitOp(#OP_GET_GLOBAL)
				  ByteCode::EmitString(*F\name)
					*next = FunctionCallDeclared(*next\next, *Sym)
					gToken = *next\value
					Expect(gToken, #RPAREN, #PB_Compiler_Line)
					*rootLine = 0
			EndSelect
	EndMacro
	
	Global gGlobalArrayCount = 0
	Global gLocalArrayCount = 0
	Global *gClassVarList 

	XIncludeFile "./parser/ConstParser.pb"
	XIncludeFile "./parser/DimParser.pb"
	XIncludeFile "./parser/ImportParser.pb"
	
	Global noModule = 1
	
	Procedure ClassParser(*next.LinkedList::Node)
	  gExtendsYes = 0
	  gVarCount = 0
	  
		returnIfNull()
		
		Define.Token *token = *next\value
		expect(*token, #IDENFIER, #PB_Compiler_Line)

		Define.SymbolTable::SClass *Sym = SymbolTable::addClass(*token\string)
		*gCurrentClass = *Sym
		*Sym\functions = LinkedList::Init()
		*Sym\variables = LinkedList::Init()
		*gClassVarList = *Sym\variables
		
		Debug ByteCode::*script
		If Not ByteCode::*script
		  ByteCode::*script = Interpreter::newFunction()
		  Debug  "ClassParser " + ByteCode::*script
		EndIf
		ByteCode::*gFunction = ByteCode::*script
		
		SymbolTable::EnterScope(*token\string)
		*currentClass = *token\string
		
		Define.ObjString objStr
		objStr\obj\type = Interpreter::#OBJ_STRING
		objStr\chars = *token\string
		objStr\length = Len(PeekS(*token\string))
		Interpreter::globals(PeekS(*token\string)) = Interpreter::NewClass(@objStr, 30)
		
		Define.ObjClass *currentKlass = Interpreter::globals(PeekS(*token\string))
		Define.Token *tmpToken = *next\next\value
		Define *tmpString = *token\string
		
		If *tmpToken\type = #EXTENDS
		  gExtendsYes = 1
		  moveNext()
		  moveNext()
		  expect(*token, #IDENFIER, #PB_Compiler_Line)
		  
		  If CompareMemoryString(*tmpString, *token\string) = 0
		    Error::Display(*token\fileName, *token\line, "A class "+PeekS(*tmpString)+" cant extend from itself.")
		  EndIf
		  
		  Define.q subClassVal = Interpreter::globals(PeekS(*tmpString))
		  Define.q superClassVal = Interpreter::globals(PeekS(*token\string))
		  
		  If superClassVal <> #Null
		    Define.SymbolTable::SClass *klass = SymbolTable::LookupSymbol(*token\string)
		    *Sym\superClass = *klass
		    Define.LinkedList::CustomList *cList = *klass\functions
		    Define.LinkedList::Node *current = *cList\head
		    
		    While *current <> #Null
		      Define.SymbolTable::SFunction *F = *current\value
		      
		      If *F And *F\visible <> 1
		        Define.SymbolTable::SFunction *F2 = SymbolTable::AddFunction(*F\name)
		        CopyStructure(*F, *F2, SymbolTable::SFunction)
		        LinkedList::AddLast(*Sym\functions, *F, SizeOf(SymbolTable::SFunction))
		        Interpreter::globals(PeekS(SymbolTable::GetCurrentScope()+2)+":"+PeekS(*F\name)) = Interpreter::OBJ_VAL(*F\fObj)
		      EndIf
		      
		      *current = *current\next
		    Wend
		    
		    *cList = *klass\variables
		    Define.LinkedList::Node *current = *cList\head
		    
		    While *current <> #Null
		      Define.SymbolTable::SVariable *Var = *current\value
		      
		      If *Var
  		      Define.s name = PeekS(*klass\name)+":"+PeekS(*Var\name)
  		      Define.SymbolTable::SVariable *V = SymbolTable::lookupSymbol(@name)
  		      If *V And *V\visible <> 1
  		        SymbolTable::AddVariable(*Var\name, *var\type, *var\RefKind, *var\refOffset, *V\visible)
  		      EndIf
		      EndIf
		      *current = *current\next
		    Wend
		  Else
		    Error::Display(*token\fileName, *token\line, "A class "+PeekS(*tmpString)+" does not exists.")
		  EndIf
		  *token\string = *tmpString
		EndIf
		
		;ByteCode::EmitOp(#OP_GET_GLOBAL)
		;ByteCode::EmitString(*token\string)
		
		Define *name = *token\string
		Define.i isStatic = 0
		
		Define varCount = 0
		
		gParallel = 0
		
		While *token And *token\type <> #END_CLASS
			moveNext()
			
			If *next = #Null
				ProcedureReturn
			EndIf
	
			If *token
			  Select *token\type
			    Case #PROTECTED
			      gVisible = SymbolTable::#VProtected  
			    Case #PARALLEL
			      gParallel = 1
			    Case #PUBLIC
			      gVisible = SymbolTable::#VPUBLIC
			    Case #PRIVATE
			      gVisible = SymbolTable::#VPrivate
			    Case #STATIC
			      isStatic = 1
					Case #SUBR
					  *next = FunctionParser(#SUBR, *next\next, isStatic)
					  *gFunc\isStatic = isStatic
					  *gFunc\isParallel = gParallel
						LinkedList::addLastInt(*Sym\functions, *gFunc)
						gVisible = SymbolTable::#VPublic
						gParallel = 0
						isStatic = 0 
						*gFunc\currentProperty = gVarCount
					  gVarCount + 1
					Case #FUNCTION
					  *next = FunctionParser(#FUNCTION, *next\next, isStatic)
					  *gFunc\isStatic = isStatic
					  *gFunc\isParallel = gParallel
					  *gFunc\currentProperty = gVarCount
					  gVarCount + 1
						LinkedList::addLastInt(*Sym\functions, *gFunc)
						gVisible = SymbolTable::#VPublic
						gParallel = 0
						isStatic = 0
					Case #CONST
					  *next = ConstParser(*next\next)
					  gVisible = SymbolTable::#VPublic
					Case #DIM
					  *next = DimParser(*next\next, SymbolTable::#Ref_Class, SymbolTable::@refClass, isStatic)
					  gVisible = SymbolTable::#VPublic
					  isStatic = 0
					  varCount + 1
					  gVarCount + 1
					Case #END_CLASS
					  Break
					Case #CLASS
				    *next = ClassParser(*next\next)
					Case #IMPORT
					  *next = ImportParser(*next)
					Default
						Error::Display(*token\fileName, *token\line, "Got unexpected token "+TokenText(*token\type)+" at class "+PeekS(*name)+" context")
				EndSelect
			EndIf
		Wend
		
		If noModule = 1
		  SymbolTable::resetScope()
		Else
		   SymbolTable::leaveScope()
	  EndIf
	  
	  *currentClass = 0
	  *gCurrentClass = 0
		ProcedureReturn *next
	EndProcedure
	
	Procedure ModuleParser(*next.LinkedList::Node)
		returnIfNull()
		
		Define.Token *token = *next\value
		expect(*token, #IDENFIER, #PB_Compiler_Line)
		
		Define.SymbolTable::SModule *Sym = SymbolTable::addModule(*token\string)
		*Sym\variables = LinkedList::Init()
		*gClassVarList = *Sym\variables
		
		SymbolTable::EnterScope(*token\string)
		Define.ObjString objStr
		objStr\obj\type = Interpreter::#OBJ_STRING
		objStr\chars = *token\string
		objStr\length = Len(PeekS(*token\string))
		Interpreter::globals(PeekS(*token\string)) = Interpreter::NewClass(@objStr, 30)
		
		Define *name = *token\string
		*currentModule = *name
		Define.i isStatic = 1
		Define.i varCount = 0
		
		noModule = 0
		
		While *token And *token\type <> #ENDMODULE
			moveNext()
			
			If *next = #Null
				ProcedureReturn
			EndIf
	
			If *token
			  Select *token\type
			    Case #PROTECTED
			      gVisible = SymbolTable::#VProtected  
			    Case #PUBLIC
			      gVisible = SymbolTable::#VPUBLIC
			    Case #PRIVATE
			      gVisible = SymbolTable::#VPrivate
					Case #SUBR
					  *next = FunctionParser(#SUBR, *next\next, isStatic)
					  *gFunc\isStatic = isStatic
						gVisible = SymbolTable::#VPublic
					Case #FUNCTION
					  *next = FunctionParser(#FUNCTION, *next\next, isStatic)
					  *gFunc\isStatic = isStatic
						gVisible = SymbolTable::#VPublic
					Case #CONST
					  *next = ConstParser(*next\next)
					  gVisible = SymbolTable::#VPublic
					Case #DIM
					  *next = DimParser(*next\next, SymbolTable::#Ref_Class, SymbolTable::@refClass, isStatic)
					  gVisible = SymbolTable::#VPublic
					  varCount + 1
					Case #ENDMODULE
					  Break
					Case #IMPORT
					  *next = ImportParser(*next)
					Case #CLASS
				    *next = ClassParser(*next\next)
					Default
						Error::Display(*token\fileName, *token\line, "Got unexpected token "+TokenText(*token\type)+" at module "+PeekS(*name)+" context")
				EndSelect
			EndIf
		Wend
		
		SymbolTable::leaveScope()
		
		noModule = 1
		
		ProcedureReturn *next
	EndProcedure
	
	Procedure PASS1(*head = #Null)
		Define.LinkedList::Node *Next
		ByteCode::Init()
		
		If *head = #Null
			*Next = *tokens\head
		Else
			*next = *head
		EndIf
		
		Define.Token *token
	  Define.i isStatic = 1
		Define.i varCount = 0
			
		While *next <> #Null
		  
		  If *token
		    Select *token\type
  			    Case #CLASS
  			      *next = ClassParser(*next\next)
            Case #MODULE
             *next = ModuleParser(*next\next)
  			  Default
  			    Debug "PASS1 "+*token\type
  			EndSelect
		  EndIf
		
			moveNext()
		Wend
		
	EndProcedure
	
	Macro checkCType(A)
		If *C\type = #LINKEDLIST
			*C\type = A
		EndIf
	EndMacro
	
	Procedure handleLinkedList(*current.LinkedList::Node, *C.SymbolTable::SConstant, isHead.i)
		Define.LinkedList::Node *current2
	   	Define.LinkedList::CustomList *List
	   	Define.Token *token
	   	Define *top
	   	
	   	If *C And *C\type = #LINKEDLIST
			Define.i pushed = 0
			*List = *C\integer
			*current2 = *list\head
			
			While *current2
				*token = *current2\value

				If *token		
					Select *token\type
						Case #POW
								
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, Pow(value2,value))
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, Pow(value2,value))
									pushed-1
								Case #TSTRING
									Error::Display(*token\fileName, *token\line, "Syntax error (Cant pow String "+PeekS(*token\string)+")")
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
							
						Case #MUL
								
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2*value)
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2*value)
									pushed-1
								Case #TSTRING
									Define *value 
									Define.d value2 
									value2 = Stack::popDouble(*Stack)
									*value = Stack::popInteger(*Stack)
									Define length = (MemoryStringLength(*value) * value2) + 1
									Define.FastString::STRFASTOBJ *str = FastString::new(length)
									Define.i I, L = value2
									For I = 1 To L
										FastString::add(*str, *value)
									Next I
									Stack::PushInteger(*Stack, *str\data+4)
									pushed-1
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
							
						Case #DIV
							
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2/value)
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2/value)
									pushed-1
								Case #TSTRING
									Error::Display(*token\fileName, *token\line, "Syntax error (Cant div String "+PeekS(*token\string)+")")
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
							
						Case #MINUS
								
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2-value)
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2-value)
									pushed-1
								Case #TSTRING
									Error::Display(*token\fileName, *token\line, "Syntax error (Cant minus String "+PeekS(*token\string)+")")
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
							
						Case #PLUS
								
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2+value)
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, value2+value)
									pushed-1
								Case #TSTRING
									Define *value
									Define.d dvalue
									Define.i length
									
									Select lastStackEval
										Case #TINTEGER, #TDOUBLE
											dvalue = Stack::popDouble(*Stack)
											Define *value2 = Stack::popInteger(*Stack)
											Define.s tmpA = StrD(dvalue)
											Define.i len = Len(tmpA)
											Define.FastString::STRFASTOBJ *tmp = FastString::new(1+len)
											FastString::add(*tmp, @tmpA)
											*value = *tmp\data+4
											FastString::Free(*tmp)
										Case #TSTRING
											Define *value = Stack::popInteger(*Stack)
											Define *value2 = Stack::popInteger(*Stack)
									EndSelect
									length = 2 + MemoryStringLength(*value2) + MemoryStringLength(*value)
									Define.FastString::STRFASTOBJ *str = FastString::new(length)
									FastString::add(*str, *value2)
									FastString::add(*str, *value)
									Stack::PushInteger(*Stack, *str\data+4)
									pushed-1
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
						Case #MOD
							Select *C\type
								Case #TINTEGER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, Mod(value2,value))
									pushed-1
								Case #TDOUBLE, #NUMBER
									Define.d value = Stack::popDouble(*Stack)
									Define.d value2 = Stack::popDouble(*Stack)
									Stack::PushDouble(*Stack, Mod(value2,value))
									pushed-1
								Case #TSTRING
									Error::Display(*token\fileName, *token\line, "Syntax error (Cant modulo String "+PeekS(*token\string)+")")
								Case #LINKEDLIST
									If isHead
										*current = *current\next
									Else
										*current = *current\prev
									EndIf
									ProcedureReturn *current
								Case #IDENFIER
									Debug "A"
							EndSelect 
							
						Case #TINTEGER
							checkCType(#TINTEGER)
							Stack::PushDouble(*Stack, *token\integer)
							pushed+1
							lastStackEval = #TINTEGER 
						Case #TDOUBLE, #NUMBER
							checkCType(#TDOUBLE)
							Stack::PushDouble(*Stack, *token\double)
							pushed+1
							lastStackEval = #TDOUBLE 
						Case #TSTRING
						  checkCType(#TSTRING)
						  Stack::PushInteger(*Stack, *token\string)
						  *top = *token\string
							pushed+1
							lastStackEval = #TSTRING 
						Case #IDENFIER
								Define.i length = 2 + MemoryStringLength(*C\Scope) + MemoryStringLength(*token\string)
								Define.FastString::STRFASTOBJ *str = FastString::new(length)
								FastString::add(*str, *C\Scope)
								FastString::add(*str, @":")
								FastString::add(*str, *token\string)
								
								Define.SymbolTable::SConstant *Sym = SymbolTable::LookupSymbol(*str\data+4)
								FastString::Free(*str)
								
								If Not *Sym
									*Sym = SymbolTable::LookupSymbol(*token\string)
									
									If *Sym
										
										Select *Sym\Kind
											Case SymbolTable::#SKIND_CLASS
												Define.LinkedList::Node *next2 = *current2\next
												Define classError = 0
												
												If *next2
													*token = *next2\value
													
													If *token And *token\type = #DOT
														*current2 = *next2
														*next2 = *current2\next
														
														If *next2
															*token = *next2\value
															
															If *token And *token\type = #IDENFIER
																Define.i newLen = 2 + MemoryStringLength(*Sym\name) + MemoryStringLength(*token\string)
																Define.FastString::STRFASTOBJ *newStr = FastString::new(newLen)
																FastString::add(*newStr, *Sym\name)
																FastString::add(*newStr, @":")
																FastString::add(*newStr, *token\string)
																*Sym = SymbolTable::LookupSymbol(*newStr\data+4)
																FastString::Free(*newStr)
																*current2 = *next2
																*next2 = *current2\next
															Else
																Error::Display(*token\fileName, *token\line, "Expected Class "+PeekS(*sym\name)+" idenfier but got ")
															EndIf
														Else
															Error::Display(*token\fileName, *token\line, "Expected Class "+PeekS(*sym\name)+" idenfier but got nothing")
														EndIf
													Else
														classError = 1
													EndIf
												Else
													classError = 1
												EndIf
												
												If classError
													Error::Display(*token\fileName, *token\line, "Class "+PeekS(*sym\name)+" cant be used in a const expression")
												EndIf
											Default
												Error::Display(*token\fileName, *token\line, "Idenfier "+PeekS(*sym\name)+" cant be used in a const expression")
										EndSelect
										
									Else
										Error::Display(*token\fileName, *token\line, "Idenfier "+PeekS(*token\string)+" is not declared")
										
										If isHead
											*current = *current\next
										Else
											*current = *current\prev
										EndIf
										ProcedureReturn *current
									EndIf
								EndIf
								
								continueIdenfier:
								checkCType(*Sym\type)
	
								Define.SymbolTable::SConstant *C2 = *Sym

								Select *C2\type
									Case #TINTEGER
										Stack::PushDouble(*Stack, *C2\integer)
										pushed+1
										lastStackEval = #TINTEGER 
									Case #TDOUBLE, #NUMBER
										Stack::PushDouble(*Stack, *C2\double)
										pushed+1
										lastStackEval = #TDOUBLE 
									Case #TSTRING
										Stack::PushInteger(*Stack, *C2\string)
										pushed+1
										lastStackEval = #TSTRING
									Case #LINKEDLIST
										If isHead
											*current = *current\next
										Else
											*current = *current\prev
										EndIf
										ProcedureReturn *current
								EndSelect
					EndSelect
						
					;lastStackEval = *token\type
				EndIf
					
				*current2 = *current2\next
			Wend
			
			;SymbolTable::EnterScope(*C\Scope)
			Select *C\type
				Case #IDENFIER
					Debug "A"
				Case #TINTEGER
					Define.i I = Stack::popDouble(*Stack)
					SymbolTable::ReAddConstant(*C\scope, *C\name, #TINTEGER, I)
					
				Case #TDOUBLE, #NUMBER
					Define *value = GlobalMemoryCache::Alloc(SizeOf(Double))
					Define.d value = Stack::popDouble(*Stack)
					PokeD(*value, value)
					SymbolTable::ReAddConstant(*C\scope, *C\name, #TDOUBLE, *value)
					
				Case #TSTRING
				  *value = Stack::PopInteger(*Stack)
				  If *value = 0 And pushed = 1
				    *value = *top
				  EndIf
		
				  SymbolTable::ReAddConstant(*C\scope, *C\name, #TSTRING, *value)
				Case #LINKEDLIST
					If pushed > 0
						Stack::popDouble(*Stack)
					EndIf
					If isHead
						*current = *current\next
					Else
						*current = *current\prev
					EndIf
					ProcedureReturn *current
			EndSelect
			SymbolTable::deleteResolve(*current\key)
		EndIf
			
		If isHead
			*current = *current\next
		Else
			*current = *current\prev
		EndIf
		ProcedureReturn *current
	EndProcedure
	
	XIncludeFile "pass2.pbi"
EndModule
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 1430
; FirstLine = 1396
; Folding = -----
; EnableXP
; DPIAware
; DisableDebugger