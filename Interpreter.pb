DeclareModule Interpreter
  IncludeFile "tokens.pb"
  IncludeFile "HashBucket.pb"
  IncludeFile "table.pb"
  
  Enumeration
    #VAL_BOOL = 0
    #VAL_NIL = 1
    #VAL_NUMBER = 2
    #VAL_OBJ = 3
  EndEnumeration
  
  Enumeration
    #OBJ_STRING
    #OBJ_NATIVE
    #OBJ_CLOSURE
    #OBJ_FUNCTION
    #OBJ_CLASS
    #OBJ_INSTANCE
    #OBJ_ARRAY
  EndEnumeration
  
  Enumeration
    #INTERPRET_OK
    #INTERPRET_RUNTIME_ERROR
  EndEnumeration
  
  #FRAMES_MAX = 64
  #STACK_MAX = #FRAMES_MAX * 512
  #LOCAL_START = 16380
  #GC_HEAP_GROW_FACTOR = 2
  
  #SIGN_BIT = $8000000000000000
  #QNAN = $7FFC000000000000
  
  #TAG_NIL  = 1 
  #TAG_FALSE = 2 
  #TAG_TRUE = 3 
  #NIL_VAL  = #QNAN | #TAG_NIL
  
  #FALSE_VAL = #QNAN | #TAG_FALSE
  #TRUE_VAL  = #QNAN | #TAG_TRUE
  
  Structure Base
    capacity.i
    count.i
  EndStructure
  
  Structure ValueArray Extends Base
    *values
  EndStructure
  
  Structure Chunk Extends Base
    *code
    *constants.ValueArray
  EndStructure
  
  Structure CallFrame
    *function.ObjFunction
    *ip.Byte
    *slots
    *this.Value
    slotCount.l
    isConstructor.b
  EndStructure
  
  Structure ExecutionContext
    gStack.q[#STACK_MAX]
    stackIP.i
    gFrames.CallFrame[#FRAMES_MAX]
    gFrameCount.i
    id.i
  EndStructure
  
  Global.ExecutionContext mainContext
  mainContext\gFrameCount = 1
  
  Global.q Dim gStack(#STACK_MAX)
  Global.Obj *gObjects = #Null
  Global.CallFrame Dim gFrames.CallFrame(#FRAMES_MAX)
  Global gFrameCount
  Global gBytesAllocated
  Global gNextGC = 1024 * 1024 
  Global gcRequested.i = 0
  Global gcRunning.i   = 0
  Global.HashTable::hash_table *strings
  
  Global Dim *buildInFunction(64)
  Global currentBuildIn = 65
  
  Global Dim *Arrays(0)
  Global Dim *globalArrays(0)
  
  Global NewMap globals.q()
  
  Global NewMap fieldCache()
  Global NewList threads()
  
  Declare.q DoubleToValue(number.d)
  Declare.i AS_OBJ(value.q)
  Declare RoseBasic_DefaultImpl()
  Declare run(*ctx.ExecutionContext)
  Declare newFunction()
  Declare.q newClass(*name.ObjString, functionCount.i)
  Declare.q newInstance(*klass.ObjClass)
  Declare newNative(*function) 
  Declare newClosure(*function)
  Declare writeChunk(*chunk.Chunk, op.b)
  Declare writeChunkLong(*chunk.Chunk, op.l)
  Declare addConstant(*chunk.Chunk, value.q)
  Declare Display(*function.ObjFunction)
  Declare writeChunkShort(*chunk.Chunk, op.u)
  Declare copyString(*chars, length.i) 
  Declare printValue(value.q)
  
  Macro NUM_VAL(n)
    Interpreter::doubleToValue(n)
  EndMacro
  
  Structure ArrayOfPointer
    *Index[0]
  EndStructure
  
  Macro JUMP()
    If gcRequested And gcRunning = 0
      gcRunning = 1
      Debug "GC triggered"
      collectGarbage(*ctx)
      gcRunning = 0
      gcRequested = 0
    EndIf
  
    READ_BYTE()
    CompilerIf #PB_Compiler_Backend = #PB_Backend_C  
      !goto *(void*)p_addr->f_index[v_op];
    CompilerElse 
      EnableASM  
      JMP *addr\Index[op]
      DisableASM 
    CompilerEndIf 
  EndMacro
  
  #TAG_PTR = 4
  
  Macro PTR_VAL(p)
    (#QNAN | #TAG_PTR | (p & $0000FFFFFFFFFFFF))
  EndMacro
  
  Macro IS_PTR(v)
    ((v & #QNAN) = #QNAN And (v & $7) = #TAG_PTR)
  EndMacro
  
  Macro AS_PTR(v)
    (v & $0000FFFFFFFFFFFF)
  EndMacro

  Macro RESET_STACK(context)
   ; Interpreter::gFrameCount = 0
    context\stackIP = 0
  EndMacro
  
  Macro OBJ_VAL(Num)
    Interpreter::#SIGN_BIT | Interpreter::#QNAN | PeekQ(@num)
  EndMacro
  
  Macro BOOL_VAL(b)
    ((#QNAN) | (Bool(b) + #TAG_FALSE))
  EndMacro
  
  Macro AS_STRING(objV)
    (Interpreter::AS_OBJ(objV))
  EndMacro
  
  Macro init()
    Interpreter::RoseBasic_DefaultImpl()
  EndMacro
  
  Macro Push(ctx, value)
    ctx\gStack[ctx\stackIP] = value
    If ctx\stackIP < 32767
      ctx\stackIP + 1
    EndIf
  EndMacro
  
  Declare resizeBuildIn(size.i)
  
EndDeclareModule

Module Interpreter
  
  Global NewList *grayStack.Obj()
  
  CompilerIf #PB_Compiler_Debugger
  Procedure ASSERT_STRING(*s.ObjString)
    If *s = 0 Or *s\chars = 0
      Debug "NULL string"
      End
    EndIf
    If MemorySize(*s\chars) < (*s\length+1)*2
      Debug "STRING BUFFER OVERFLOW"
      End
    EndIf
  EndProcedure
  CompilerEndIf

  Declare freeObject(*object.Obj)
  Declare reallocate(*pointer, oldSize.i, newSize.i)
  
  Macro FREE_ARRAY(type, pointer, oldCount) 
    reallocate(pointer, SizeOf(type) * (oldCount), 0)
  EndMacro
  
  Macro IS_NIL(obj)  
    ((obj) = #NIL_VAL)
  EndMacro
  
  Macro ALLOCATE(type, count) 
    reallocate(#Null, 0, type*count)
  EndMacro
  
  Macro GROW_CAPACITY(capacity)
    If capacity < 16
      capacity = 16
    Else
      capacity = capacity * 2
    EndIf
  EndMacro
  
  Procedure.l HashString(*chars, lengthChars.i)
  Protected.l hash = $811C9DC5
  For i = 0 To lengthChars - 1
    hash ! PeekW(*chars + i * 2)
    hash * 16777619
  Next
  ProcedureReturn hash
EndProcedure
  
  Procedure.d ValueToDouble(value.q)
    Protected result.d
    ;PokeQ(@result, value)
    CopyMemory(@value, @result, 8)
    ProcedureReturn result
  EndProcedure
  
  CompilerIf #PB_Compiler_Debugger

  Macro ASSERT_NOT_PTR_D(v)
    If (v <> 0) And (v = Int(v))
      Debug "ASSERT FAIL: DoubleToValue called with non-floating value (looks like ptr/int): " + Hex(v) + " at " + #PB_Compiler_File + ":" + Str(#PB_Compiler_Line)
      End
    EndIf
  EndMacro
  
  CompilerElse
  Macro ASSERT_NOT_PTR_D(v)
  EndMacro
  CompilerEndIf

  Procedure.q DoubleToValue(number.d)
    Protected value.q
    ;PokeD(@value, number)
    CopyMemory(@number, @value, 8)
    ProcedureReturn value
  EndProcedure
    
  Procedure.i AS_OBJ(value.q)
    ProcedureReturn value & ~(#SIGN_BIT | #QNAN)
  EndProcedure
  
  Macro ALLOCATE_OBJ(type, objectType, no) 
    allocateObject(SizeOf(type), objectType, no)
  EndMacro
  
  Macro GROW_ARRAY(type, pointer, oldCount, newCount) 
    reallocate(pointer, SizeOf(type) * (oldCount), SizeOf(type) * (newCount))
  EndMacro
  
  Macro FREE(type, pointer) 
    reallocate(pointer, SizeOf(type), 0)
  EndMacro
  
  Macro READ_BYTE()
    op = *frame\ip\b : *frame\ip + SizeOf(Byte)
  EndMacro
  
  Macro READ_LONG()
    l = PeekL(*frame\ip) : *frame\ip + SizeOf(Long)
  EndMacro
  
  Macro READ_STRING()
    PeekQ(*frame\function\chunk\constants\values+PeekL(*frame\ip)) : *frame\ip + SizeOf(Long)
  EndMacro
  
  Procedure.b IS_OBJ(v.q)
    ProcedureReturn Bool((v & (#QNAN | #SIGN_BIT)) = (#QNAN | #SIGN_BIT))
  EndProcedure
  
  Macro BINARY_OP_BOOL(op)
    ; Ensure both operands are numbers
    Define.Q b
    Define.Q a
    
    If Not IS_NUM(Peek(*ctx, -1)) Or Not IS_NUM(Peek(*ctx, -2))
      b = Pop(*ctx)
      a = Pop(*ctx)
      Define.Q dq = Pop(*ctx)
      push(*ctx, b)
      push(*ctx, dq)
      ;PrintN("Runtime error: Expected numbers in binary op")
      ;ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    ; Pop two boxed values
    Define.Q b = Pop(*ctx)
    Define.Q a = Pop(*ctx)
    
    ; Extract their numeric payloads
    Define.d left  = AS_NUMBER(a)
    Define.d right = AS_NUMBER(b)
    
    ; Perform the operation and rebox as boolean
    Define.q result = BOOL_VAL(left op right)
    
    ; Push the result back
    Push(*ctx, result)
  EndMacro
  
  Macro IS_NUM(v)
    (((v) & #QNAN) <> #QNAN)
  EndMacro
  
  Macro IS_STRING(obj)  
    isObjType(obj, #OBJ_STRING)
  EndMacro
  
  Macro IS_FUNCTION(obj)     
    isObjType(obj, #OBJ_FUNCTION)
  EndMacro
  
  Macro IS_NATIVE(obj)      
    isObjType(obj, #OBJ_NATIVE)
  EndMacro
  
  Macro IS_CLASS(obj)       
    isObjType(obj, #OBJ_CLASS)
  EndMacro
  
  Macro IS_NUMBER(value)  
    (((value) & #QNAN) <> #QNAN)
  EndMacro
  
  Macro IS_INSTANCE(obj)    
    isObjType(obj, #OBJ_INSTANCE)
  EndMacro
  
  Macro IS_BOOL(v)
    (((v) = (#QNAN | #TAG_TRUE)) Or ((v) = (#QNAN | #TAG_FALSE)))
  EndMacro
  
  Macro Peek(ctx, distance)
    ctx\gStack[ctx\stackIP + distance]
  EndMacro
  
  Macro isFalsey(obj)
    Bool((IS_NIL(obj) Or IS_BOOL(obj) And Not AS_BOOL(obj)))
  EndMacro
  
  Macro takeString(chars, length)
    allocateString(chars, length)
  EndMacro
  
  CompilerIf #PB_Compiler_OS = #PB_OS_Windows
    Import ""
      PB_StringHeap
      PB_Memory_Heap
      PB_MemoryBase
    EndImport
    
    Procedure _TestHeaps(File$, Line)
      Protected StringHeap, MemoryBase, MemoryHeap
      
      If HeapValidate_(PB_StringHeap, 0, 0) = 0
        MessageRequester("StringHeap corrupted !", File$+" : "+Str(Line))
      EndIf
      
      If HeapValidate_(PB_MemoryBase, 0, 0) = 0
        MessageRequester("MemoryBase heap corrupted !", File$+" : "+Str(Line))
      EndIf
      
      If HeapValidate_(PB_Memory_Heap, 0, 0) = 0
        MessageRequester("AllocateMemory heap corrupted !", File$+" : "+Str(Line))
      EndIf
    EndProcedure
    
    Macro TestHeaps
      _TestHeaps(#PB_Compiler_File, #PB_Compiler_Line)
    EndMacro 
  CompilerEndIf
  
  Procedure resizeBuildIn(size.i)
    ReDim *buildInFunction(Interpreter::currentBuildIn)
  EndProcedure
  
  Structure AsciiString
    Obj.Obj
    length.i
    *chars
    hash.l
  EndStructure
  
   Procedure allocateObject(size.i, type.i, no.i = 0) 
    Define.Obj *object = reallocate(#Null, 0, size)
    *object\type = type
    *object\isMarked = #False
    *object\next = #Null
    If no = 0
      *object\next = *gObjects
      *gObjects = *object
    EndIf
    ProcedureReturn *object
  EndProcedure
  
  Procedure Push_Native(*ctx.ExecutionContext, value.q)
    Define.objString *string = AS_OBJ(value)
    Define.s str = PeekS(*string\chars, *string\length)
    Define *ptr = ALLOCATE(SizeOf(Ascii), *string\length+1)
    PokeS(*ptr, str, *string\length, #PB_Ascii)
    *ctx\gStack[*ctx\stackIP] = DoubleToValue(*ptr)
    *ctx\stackIP + 1
  EndProcedure
  
  Procedure.q Pop(*ctx.ExecutionContext)
    If *ctx\stackIP <= 0
      ProcedureReturn 0
    EndIf
    *ctx\stackIP - 1
    ProcedureReturn *ctx\gStack[*ctx\stackIP]
  EndProcedure
  
  Macro markObject(obj)
    If obj <> #Null And obj\isMarked = #False
      obj\isMarked = #True
      AddElement(*grayStack())
      *grayStack() = obj
    EndIf
  EndMacro
  
  Macro markValue(v)
    If IS_OBJ(v)
      Define.Obj *obj = AS_OBJ(v)
      markObject(*obj)
    EndIf
  EndMacro
  
   Procedure markRoots(*ctx.ExecutionContext)
    
    For i = 0 To *ctx\stackIP - 1
      markValue(*ctx\gStack[i])
    Next
  
    For i = 0 To *ctx\gFrameCount - 1
      Define.CallFrame *frame = @*ctx\gFrames[i]
  
      If *frame\slots <> 0
        For j = 0 To *frame\function\localSize - 1
          markValue(PeekQ(*frame\slots + j*8))
        Next
      EndIf
  
      markObject(*frame\function\obj)
    Next
    
    ForEach globals()
      markValue(globals())
    Next
  
  EndProcedure
  
  Macro markTable(hashMap)
    ResetMap(hashMap())
    While NextMapElement(hashMap())
      markObject(hashMap()\obj)
    Wend
  EndMacro
  
  Macro blackenObject(cObj)
    Select cObj\type
      Case #OBJ_CLASS
        Define.ObjClass *klass = cObj
        markObject(*klass\name\obj)
        ;markTable(*klass\functions)
      Case #OBJ_FUNCTION
        Define.ObjFunction *func = cObj
        markObject(*func\name\obj)
      Case #OBJ_INSTANCE
        Define.ObjInstance *instance = cObj
        markObject(*instance\klass\obj)
        ;markTable(*instance\fields)
    EndSelect
  EndMacro
  
  Macro traceReferences()
    ResetList(*grayStack())
    While NextElement(*grayStack())
      blackenObject(*grayStack())
    Wend
    ClearList(*grayStack())
  EndMacro
  
  Procedure sweep()
    Define.Obj *prev = #Null
    Define.Obj *object = *gObjects
    
    While *object <> #Null
      If *object\isMarked
        *object\isMarked = #False
        *prev = *object
        *object = *object\next
      Else
        Define.Obj *unreached = *object
        *object = *object\next
        
        If *prev = #Null
          *gObjects = *object
        Else
          *prev\next = *object
        EndIf
  
        freeObject(*unreached)
      EndIf
    Wend
  EndProcedure
  
  Procedure collectGarbage(*ctx.ExecutionContext)
    markRoots(*ctx)
    traceReferences()
    sweep()
    gNextGC = gBytesAllocated * #GC_HEAP_GROW_FACTOR
  EndProcedure
  
  Procedure reallocate(*pointer, oldSize.i, newSize.i)
    
    If newSize = 0
      If *pointer <> 0
        If MemorySize(*pointer) = -1
          Debug "FREE INVALID PTR: " + Hex(*pointer)
          End
        EndIf
        FreeMemory(*pointer)
      EndIf
      ProcedureReturn 0
    EndIf
    
    If *pointer
      oldSize = MemorySize(*pointer)
    EndIf
  
    gBytesAllocated + (newSize - oldSize)
    
    If gBytesAllocated > gNextGC
      gcRequested = 1
    EndIf
  
    If *pointer > 0
      Define *result = ReAllocateMemory(*pointer, newSize)
      
      If *result = #Null
        End 1
      EndIf
    Else 
      ProcedureReturn AllocateMemory(newSize)
    EndIf
    ProcedureReturn *result
  EndProcedure
  
  Procedure.i RB_StringAlloc(lengthChars.i)
    Protected *mem = reallocate(#Null, 0, (lengthChars + 1) * 2)
    PokeW(*mem + lengthChars * 2, 0)
    ProcedureReturn *mem
  EndProcedure
  
  Procedure.s RB_StringPeek(*chars, lengthChars)
    ProcedureReturn PeekS(*chars, lengthChars, #PB_Unicode)
  EndProcedure
  
  Procedure allocateString(*chars, lengthChars.i)
    Define.ObjString *string = ALLOCATE_OBJ(ObjString, #OBJ_STRING, 0)
    *string\length = lengthChars
    *string\chars  = *chars
    *string\hash   = HashString(*chars, lengthChars)
    If *strings = 0
      *strings = hashtable::new(256)
    EndIf
    hashtable::insert(*strings, *string, #NIL_VAL)
    ProcedureReturn *string
  EndProcedure
  
  Procedure copyPBString(s.s)
    Define.i len = Len(s)
    Define *chars = RB_StringAlloc(len)
    PokeS(*chars, s, len, #PB_Unicode)
    ProcedureReturn allocateString(*chars, len)
  EndProcedure
  
  Procedure copyString(*chars, lengthChars.i)
    Define *heapChars = RB_StringAlloc(lengthChars)
    If *chars = #Null
      ProcedureReturn allocateString(*heapChars, lengthChars)
    EndIf
    CopyMemory(*chars, *heapChars, lengthChars * 2)
    ProcedureReturn allocateString(*heapChars, lengthChars)
  EndProcedure
  
  Macro AS_FUNCTION(objV)
    (AS_OBJ(v))
  EndMacro
  
  Macro AS_CLASS(objV)
    (AS_OBJ(v))
  EndMacro
  
  Macro AS_INSTANCE(v)
    (AS_OBJ(v))
  EndMacro
  
  Procedure AS_STRING_N(obj.q)
    Define.ObjString *Str = AS_OBJ(obj)
    ProcedureReturn *Str\chars
  EndProcedure
  
  Procedure AS_NATIVE(obj.q)
    Define.ObjNative *native = AS_OBJ(obj)
    ProcedureReturn *native\function
  EndProcedure
  
  Procedure.d valueToNum(value.q) 
    Define.d num
    CopyMemory(@value, @num, SizeOf(quad))
    ProcedureReturn num;
  EndProcedure
    
  Macro AS_NUMBER(value)
    valueToNum(value)
  EndMacro
  
  Macro AS_BOOL(obj)
    (obj) = #TRUE_VAL
  EndMacro
  
  Procedure.i isObjType(value.q, type.i)
    If value <> 0 And IS_OBJ(value)
      Protected.Obj *obj = AS_OBJ(value)
      If *obj <> #Null
        ProcedureReturn Bool(*obj\type = type)
      EndIf
    EndIf
    ProcedureReturn #False
  EndProcedure
  
  Procedure valuesEqual(a.q, b.q)
    
    If (IS_NUMBER(a) And IS_NUMBER(b)) 
      ProcedureReturn Bool(AS_NUMBER(a) = AS_NUMBER(b))
    EndIf
    
    ProcedureReturn Bool(a = b)
    ProcedureReturn #False
  EndProcedure
  
  Procedure initValueArray(*array.ValueArray)
    *array\capacity = 0
    *array\count = 0
    *array\values = #Null
  EndProcedure
  
  Procedure freeValueArray(*array.ValueArray)
    FREE_ARRAY(Double, *array\values, *array\capacity)
    initValueArray(*array)
  EndProcedure
  
  Procedure writeValueArray(*array.ValueArray, value.q)
    
    If *array\capacity < *array\count + SizeOf(Quad)
      Define.i oldCapacity = *array\capacity
      GROW_CAPACITY(*array\capacity)
      *array\values = GROW_ARRAY(Value, *array\values, oldCapacity, *array\capacity)
    EndIf
    
    PokeQ(*array\values+*array\count, value)
    *array\count + SizeOf(Quad)
  EndProcedure
  
  Procedure initChunk(*chunk.Chunk)
    *chunk\capacity = 0
    *chunk\count = 0
    *chunk\code = 0
    *chunk\constants = AllocateMemory(SizeOf(ValueArray))
    initValueArray(*chunk\constants)
  EndProcedure
  
  Procedure freeChunk(*chunk.Chunk)
    FreeMemory(*chunk\code)
    FreeMemory(*chunk\constants)
    ;FREE_ARRAY(Byte, *chunk\code, *chunk\capacity)
    ;freeValueArray(*chunk\constants)
    *chunk\capacity = 0
    *chunk\count = 0
    *chunk\code = 0
    *chunk\constants = 0
    ;initChunk(*chunk)
  EndProcedure
  
  Procedure writeChunk(*chunk.Chunk, op.b)
    
    If *chunk\capacity < *chunk\count + 1
      Define.i oldCapacity = *chunk\capacity
      GROW_CAPACITY(*chunk\capacity)
      *chunk\code = GROW_ARRAY(Byte, *chunk\code, oldCapacity, *chunk\capacity)
    EndIf

    PokeB(*chunk\code+*chunk\count, op)
    *chunk\count + 1
  EndProcedure
  
  Procedure writeChunkShort(*chunk.Chunk, op.u)
    
    If *chunk\capacity < *chunk\count + 1
      Define.i oldCapacity = *chunk\capacity
      GROW_CAPACITY(*chunk\capacity)
      *chunk\code = GROW_ARRAY(Byte, *chunk\code, oldCapacity, *chunk\capacity)
    EndIf
    
    PokeU(*chunk\code+*chunk\count, op)
    *chunk\count + 2
  EndProcedure
  
  Procedure writeChunkLong(*chunk.Chunk, op.l)
    
    If *chunk\capacity < *chunk\count + SizeOf(Long)
      Define.i oldCapacity = *chunk\capacity
      GROW_CAPACITY(*chunk\capacity)
      *chunk\code = GROW_ARRAY(Long, *chunk\code, oldCapacity, *chunk\capacity)
    EndIf
    
    PokeL(*chunk\code+*chunk\count, op)
    *chunk\count + SizeOf(Long)
  EndProcedure
  
  Procedure addConstant(*chunk.Chunk, value.q)
    PUSH(Interpreter::mainContext, value)
    writeValueArray(*chunk\constants, value)
    Pop(Interpreter::mainContext)
    ProcedureReturn *chunk\constants\count - SizeOf(Quad)
  EndProcedure
  
  Procedure concatenate(*ctx.ExecutionContext)
    Define.q vb = peek(*ctx, -2)
    Define.q va = peek(*ctx, -1)
    Define.ObjString *a = AS_OBJ(vb)
    Define.ObjString *b = AS_OBJ(va)
    ;     Define.Value *va = peek(-1)
    ;     Define.Value *vb = peek(-2)
    ;     Define.ObjString *b = *va\obj
    ;     Define.ObjString *a = *vb\obj
    Define.i length = *a\length + *b\length
    Define *chars = RB_StringAlloc(length)
    CopyMemory(*a\chars, *chars, *a\length * 2)
    CopyMemory(*b\chars, *chars + *a\length * 2, *b\length * 2)
    Define.ObjString *result = takeString(*chars, length)
    pop(*ctx)
    pop(*ctx)
    ;     Define.Value value
    ;     value\type = #VAL_OBJ
    ;     value\obj = *result
    PUSH(*ctx, OBJ_VAL(*result))
  EndProcedure
  
  Procedure newFunction()
    Define.ObjFunction *function = ALLOCATE_OBJ(ObjFunction, #OBJ_FUNCTION, 0)
    *function\arity = 0
    *function\name = #Null
    *function\chunk = AllocateMemory(SizeOf(Chunk))
    initChunk(*function\chunk)
    ProcedureReturn *function
  EndProcedure
  
  Procedure newClosure(*function)
    Define.ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, #OBJ_CLOSURE, 0)
    *closure\function = *function
    ProcedureReturn *closure
  EndProcedure
  
  Procedure.q newClass(*name.ObjString, functionCount.i)
    Define.ObjClass *klass = ALLOCATE_OBJ(ObjClass, #OBJ_CLASS, 0)
    *klass\functions = HashTable::new(functionCount + 8)
    *klass\obj\type = #OBJ_CLASS
    *klass\name = copyString(*name\chars, *name\length)
    ProcedureReturn OBJ_VAL(*klass)
  EndProcedure
  
  Procedure.q newClassSuper(*name.ObjString, functionCount.i, *superClass)
    Define.ObjClass *klass = ALLOCATE_OBJ(ObjClass, #OBJ_CLASS, 0)
    *klass\functions = HashTable::new(functionCount)
    *klass\obj\type = #OBJ_CLASS
    *klass\name = copyString(*name\chars, *name\length)
    ProcedureReturn OBJ_VAL(*klass)
  EndProcedure
  
  Procedure.q newInstance(*klass.ObjClass)
    Define.ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, #OBJ_INSTANCE, 1)
  
    Define.i bytes =  5012
    *instance\fields = AllocateMemory(bytes)
    FillMemory(*instance\fields, bytes, 0)
  
    *instance\klass = *klass
    ProcedureReturn OBJ_VAL(*instance)
  EndProcedure
    
  Procedure newNative(*function) 
    Define.ObjNative *native = ALLOCATE_OBJ(ObjNative, #OBJ_NATIVE, 0)
    *native\function = *function
    ProcedureReturn *native              
  EndProcedure
  
  Procedure freeObject(*object.Obj)
    
    Select *object\type
      Case #OBJ_CLOSURE
        gBytesAllocated - MemorySize(*object)
        FreeMemory(*object)
      Case #OBJ_STRING
        Define.ObjString *string = *object
        FreeMemory(*string\chars)
        gBytesAllocated - MemorySize(*object)
        FreeMemory(*object)
      Case #OBJ_CLASS
        Define.ObjClass *klass = *object
        HashTable::destroy(*klass\functions)
        freeObject(*klass\name)
        gBytesAllocated - MemorySize(*object)
        FreeMemory(*object)
      Case #OBJ_INSTANCE
        Define.ObjInstance *instance = *object
        FreeMemory(*instance\fields)
        gBytesAllocated - MemorySize(*object)
        FreeMemory(*object)
    EndSelect
    
  EndProcedure
  
  Procedure freeObjects()
    Define.Obj *object = *gObjects
    While (*object <> #Null) 
      Define.Obj *lNext = *object\Next
      freeObject(*object)      
      *object = *lNext
    Wend
  EndProcedure
  
  Procedure.b call(*ctx.ExecutionContext, *function.ObjFunction, argCount.i)
  
    If *ctx\gFrameCount >= #FRAMES_MAX
      PrintN("Runtime Error: Stack overflow.")
      ProcedureReturn #False
    EndIf
  
    Define.CallFrame *frame = @*ctx\gFrames[*ctx\gFrameCount]
    *ctx\gFrameCount + 1
  
    *frame\function = *function
    *frame\ip       = *function\chunk\code
    *frame\isConstructor = #False
  
    ; slot0 reserved + args + locals
    Define.i totalSlots = 1 + argCount + *function\localSize
  
    If totalSlots > 0
      If *frame\slots = #Null
        *frame\slots = AllocateMemory(totalSlots * SizeOf(Quad))
      ElseIf MemorySize(*frame\slots) < totalSlots * SizeOf(Quad)
        *frame\slots = ReAllocateMemory(*frame\slots, totalSlots * SizeOf(Quad))
      EndIf
    Else
      *frame\slots = #Null
    EndIf
  
    ; slot0 = NIL for plain function calls
    PokeQ(*frame\slots + 0, #NIL_VAL)
  
    ; args -> slot1..slotN
    For i = argCount To 1 Step -1
      PokeQ(*frame\slots + (i * SizeOf(Quad)), Pop(*ctx))
    Next
  
    ; pop callee from value stack (CRITICAL)
    Pop(*ctx)
  
    *frame\slotCount = totalSlots
    ProcedureReturn #True
  EndProcedure
    
  Procedure.b invokeFromConstructor(*ctx.ExecutionContext, *klass.ObjClass, *name, argCount.i)
    
    Define.ObjFunction *method = GetMethod(*name)
    
    If Not *method
      ProcedureReturn #False
    EndIf
    
    If *ctx\gFrameCount = #FRAMES_MAX ;gFrameCount = #FRAMES_MAX
      PrintN("Stack overflow.")
      ProcedureReturn #False
    EndIf
    
    ; === Prepare new CallFrame ===
    Define.CallFrame *frame = *ctx\gFrames[*ctx\gFrameCount] ;gFrames(gFrameCount)
    ;gFrameCount + 1
    *ctx\gFrameCount + 1
    
    *frame\function = *method
    *frame\ip = *method\chunk\code
    *frame\isConstructor = #True
    
    ; === If function has locals/args ===
    If *method\localSize > 0
  
      ; Allocate slot array for args + locals (each 8 bytes)
      If *frame\slots = #Null
        Define value = (argCount + *method\localSize) * SizeOf(quad)
        *frame\slots = AllocateMemory((1 + argCount + *method\localSize) * SizeOf(quad))
      EndIf
      
      ; === Copy arguments from stack ===
      If argCount > 0
        For i = argCount To 1 Step -1
          Define.q arg = Pop(*ctx)               ; Pop returns a NaN-boxed quad
          PokeQ(*frame\slots + i * SizeOf(quad), arg)
        Next
      EndIf
      
      ; === Copy instance ("this") into slot 0 ===
      Define.q instance = peek(*ctx, -1)           ; Top of stack
      PokeQ(*frame\slots, instance)
      
    Else
      ; No locals, still copy instance
      Define.q instance = peek(*ctx, -1)
      If *frame\slots = #Null
        *frame\slots = AllocateMemory(SizeOf(quad))
      EndIf
      PokeQ(*frame\slots, instance)
    EndIf
    
    ProcedureReturn #True
  EndProcedure
  
  Procedure.b invokeFromSuperClass(*ctx.ExecutionContext, *name, argCount.i)
    ; === Look up method from superclass (globals table) ===
    Define.ObjFunction *method = GetMethod(*name)
    If Not *method
      ProcedureReturn #False
    EndIf
    
    ; === Check stack frame limit ===
    If *ctx\gFrameCount = #FRAMES_MAX ;gFrameCount = #FRAMES_MAX
      PrintN("Stack overflow.")
      ProcedureReturn #False
    EndIf
    
    ; === Prepare new call frame ===
;     Define.CallFrame *frame = gFrames(gFrameCount)
;     gFrameCount + 1
    Define.CallFrame *frame = *ctx\gFrames[*ctx\gFrameCount] 
    *ctx\gFrameCount + 1
    
    *frame\function = *method
    *frame\ip = *method\chunk\code
    
    ; === Allocate slot array if needed ===
    If *method\localSize > 0
 
      If *frame\slots = #Null
        *frame\slots = AllocateMemory((1 + argCount + *method\localSize) * SizeOf(quad))
      EndIf
      
      ; === Copy arguments from stack ===
      If argCount > 0
        For i = argCount To 1 Step -1
          Define.q arg = Pop(*ctx) ; NaN-boxed value
          PokeQ(*frame\slots + (i * SizeOf(quad)), arg)
        Next
      EndIf
      
      ; === Copy instance (this) to slot 0 ===
      Define.q instance = peek(*ctx, -1)
      PokeQ(*frame\slots, instance)
    Else
      ; === Handle methods without locals ===
      Define.q instance = peek(*ctx, -1)
      If *frame\slots = #Null
        *frame\slots = AllocateMemory(SizeOf(quad))
      EndIf
      PokeQ(*frame\slots, instance)
    EndIf
    
    ProcedureReturn #True
  EndProcedure
  
  
  Procedure.b invokeFromClass(*ctx.ExecutionContext, *klass.ObjClass, *name, argCount.i)

  Define.ObjFunction *method = GetMethod(*name)
  If Not *method
    ProcedureReturn #False
  EndIf

  If *ctx\gFrameCount = #FRAMES_MAX
    PrintN("Stack overflow.")
    ProcedureReturn #False
  EndIf

  Define.CallFrame *frame = @*ctx\gFrames[*ctx\gFrameCount]
  *ctx\gFrameCount + 1

  *frame\function = *method
  *frame\ip = *method\chunk\code
  *frame\isConstructor = #False

  ; slot0 (this) + args + locals
  Define.i totalSlots = 1 + argCount + *method\localSize

  If *frame\slots = #Null
    *frame\slots = AllocateMemory(totalSlots * SizeOf(Quad))
  ElseIf MemorySize(*frame\slots) < totalSlots * SizeOf(Quad)
    *frame\slots = ReAllocateMemory(*frame\slots, totalSlots * SizeOf(Quad))
  EndIf

  ; args -> slot1..slotN
  For i = argCount To 1 Step -1
    PokeQ(*frame\slots + (i * SizeOf(Quad)), Pop(*ctx))
  Next

  ; receiver -> slot0
  Define.q instance = peek(*ctx, -1)
  PokeQ(*frame\slots + 0, instance)

  ; pop receiver from value stack (CRITICAL)
  Pop(*ctx)

  *frame\slotCount = totalSlots
  ProcedureReturn #True
EndProcedure
  
  
  Declare run(*ctx.ExecutionContext)
  Procedure ErrorHandler(*ctx.ExecutionContext)
    MessageRequester("OnError test", "The following error happened: " + ErrorMessage())
    run(*ctx)
  EndProcedure
  
  Procedure.b invokeConstructor(*ctx.ExecutionContext, *name, argCount.i)
    ; Get the receiver value (the instance)
    Define.q receiver = peek(*ctx, -(argCount + 1))
    
    ; Extract object pointer from NaN-boxed value
    Define.ObjInstance *instance = AS_OBJ(receiver)

    ; Get its class
    Define.ObjClass *klass = *instance\klass
    
    ; Call the constructor from class
    ProcedureReturn invokeFromConstructor(*ctx, *klass, *name, argCount)
  EndProcedure
  
  Procedure.b invoke(*ctx.ExecutionContext, *name, argCount.i)
    Define.q receiver = peek(*ctx, -(argCount + 1))
    If Not IS_INSTANCE(receiver)
      ProcedureReturn #False
    EndIf
  
    Define.ObjInstance *instance = AS_INSTANCE(receiver)
    If *instance = #Null
      ProcedureReturn #False
    EndIf
  
    ProcedureReturn invokeFromClass(*ctx, *instance\klass, *name, argCount)
  EndProcedure
  
  
  Procedure.b callValue(*ctx.ExecutionContext, callee.q, argCount.i)
    
    ; === Must be an object ===
    If Not IS_OBJ(callee)
        ProcedureReturn #False
    EndIf
    
    Define.Obj *obj = AS_OBJ(callee)

    Select *obj\type
        
        ;----------------------------------------------------
        Case #OBJ_INSTANCE
            ; Calling an instance directly usually means invoking "()"
            ; (optional: method resolution logic here)
            ProcedureReturn #True
        
        ;----------------------------------------------------
        Case #OBJ_CLASS
            ; Create instance from class
            Define.ObjClass *klass = *obj
            
            ; Create new instance
            Define.q boxedInstance = newInstance(*klass)
            
            ; Replace callee location on stack with instance
            PokeQ(@*ctx\gStack[*ctx\stackIP - argCount - 1], boxedInstance)
            
            
            ProcedureReturn #True
        
        ;----------------------------------------------------
        Case #OBJ_FUNCTION
            Define.ObjFunction *function = *obj
            
            ; Call using context, NOT global stack/frame.
            ProcedureReturn call(*ctx, function, argCount)
        
    EndSelect
    
    ProcedureReturn #False
  EndProcedure
  
  Procedure printValue(value.q)
    OpenConsole()
    
    If value = 0
      PrintN("<Null>")
      ProcedureReturn value
    EndIf
    
    ; === Check for Number ===
    If (value & #QNAN) <> #QNAN
      PrintN(StrD(ValueToDouble(value))) ; helper to extract double
      ProcedureReturn value
    EndIf
    
    ; === Type tags ===
    Select value
      Case #NIL_VAL
        PrintN("Nil")
        ProcedureReturn value
        
      Case #TRUE_VAL
        PrintN("True")
        ProcedureReturn value
        
      Case #FALSE_VAL
        PrintN("False")
        ProcedureReturn value
    EndSelect
    
    ; === Object values ===
    If IS_OBJ(value)
      Define.Obj *obj = AS_OBJ(value)
      If *obj = #Null
        PrintN("<ObjNull>")
        ProcedureReturn value
      EndIf
      
      Select *obj\type
        Case #OBJ_STRING
          Define.ObjString *objS = *obj
          PrintN("String <" + RB_StringPeek(*objS\chars, *objS\length) + ">")
          
        Case #OBJ_NATIVE
          Define.ObjNative *objN = *obj
          PrintN("ObjNative <" + Str(*objN\function) + ">")
          
        Case #OBJ_FUNCTION
          Define.ObjFunction *objF = *obj
          PrintN("ObjFunction <" + PeekS(*objF\name) + ">")
          
        Case #OBJ_CLASS
          Define.ObjClass *objC = *obj
          PrintN("ObjClass <" + PeekS(*objC\name\chars) + "> "+Str(*obj))
          
        Case #OBJ_INSTANCE
          Define.ObjInstance *objI = *obj
          PrintN("ObjInstance <" + PeekS(*objI\klass\name\chars) + ">")
          
        Case #OBJ_ARRAY
          Define.ObjArray *arr = *obj
          PrintN("<Array> count: " + Str(*arr\items\count))
          For i = 0 To *arr\items\count - 1
            Define.q element = PeekQ(*arr\items\values + i * SizeOf(quad))
            printValue(element)
          Next
          
        Default
          PrintN("<Unknown object>")
      EndSelect
      
    Else
      PrintN("<Unknown boxed value> " + Str(value))
    EndIf
    
    ProcedureReturn value
  EndProcedure
  
  ProcedureC.q RoseBasic_Split(argCount.i, *Stack)
    ; Stack layout (top-down): [string, separator, number]
    Define.q numberVal     = PeekQ(*Stack - 8)
    Define.q separatorVal  = PeekQ(*Stack - 16)
    Define.q stringVal     = PeekQ(*Stack - 24)
    
    ; Unbox the string and separator
    Define.ObjString *strObj       = AS_OBJ(stringVal)
    Define.ObjString *separatorObj = AS_OBJ(separatorVal)
    
    Define number = Int(ValueToDouble(numberVal))
 
    ; Perform the split
    Define.s result = StringField(RB_StringPeek(*strObj\chars, *strObj\length),
                              number,
                              RB_StringPeek(*separatorObj\chars, *separatorObj\length))

    Define.ObjString *objResult = copyPBString(result)
    
    ; Box as a value
    ProcedureReturn OBJ_VAL(*objResult)
  EndProcedure
  
  
  ProcedureC.q RoseBasic_FileOpen(argCount.i, *Stack)
    Define.q stringVal = PeekQ(*Stack - 8)
    Define.ObjString *obj = AS_OBJ(stringVal)
    
    Define.s path = RB_StringPeek(*obj\chars, *obj\length)
    Define id = ReadFile(#PB_Any, path)
    
    ProcedureReturn DoubleToValue(id)
  EndProcedure
  
  ProcedureC RoseBasic_FileClose(argCount.i, *Stack)
    Define.q idVal = PeekQ(*Stack - 8)
    Define id = Int(ValueToDouble(idVal))
    
    If IsFile(id)
      CloseFile(id)
    EndIf
  EndProcedure
  
  ProcedureC.q RoseBasic_FileReadLine(argCount.i, *Stack)
    Define.q idVal = PeekQ(*Stack - 8)
    Define id = Int(ValueToDouble(idVal))
    
    If Not IsFile(id)
      ProcedureReturn #NIL_VAL
    EndIf
    
    Define format = ReadStringFormat(id)
    Define.s line = ReadString(id, format)
    
    Define.ObjString *obj = copyPBString(line)
    ProcedureReturn OBJ_VAL(*obj)
  EndProcedure
  
  #HIGH_RANDOM = 2147483646 
  #LOW_RANDOM = 715827882
  
  ProcedureC.q RoseBasic_Random(argCount.i, *Stack)
    ; Stack layout (top-down): [min, max]
    Define.q maxVal = PeekQ(*Stack - 0)
    Define.q minVal = PeekQ(*Stack - 8)
    
    Define.d max = ValueToDouble(maxVal)
    Define.d min = ValueToDouble(minVal)
    
    ; Generate random number between min and max
    Define.d r = ((max - min) * (Random(#HIGH_RANDOM) / #HIGH_RANDOM)) + min
    
    ProcedureReturn DoubleToValue(r)
  EndProcedure
  
  Procedure.q GetMethod(*name)
    ProcedureReturn AS_OBJ(globals(PeekS(*name)))
    
    Protected index = (*name >> 3) & (#HASH_SIZE - 1)
    Protected *entry.MethodCacheEntry = @methodCache(index)
    
    If *entry\ptr = *name
      ProcedureReturn AS_OBJ(*entry\method)
      
      ; empty slot: populate cache
    ElseIf *entry\ptr = 0
      *entry\ptr = *name
      *entry\method = globals(PeekS(*name))  ; fallback to old lookup once
      ProcedureReturn AS_OBJ(*entry\method)
      
      ; fallback on collision
    Else
      ProcedureReturn AS_OBJ(globals(PeekS(*name)))
    EndIf
    
    If Not *method
      ProcedureReturn 0
    EndIf
  EndProcedure
  
  Procedure CreateExecutionContext(argCount.i)
      Protected *ctx.ExecutionContext = AllocateStructure(ExecutionContext)
      
      *ctx\stackIP = 0
      *ctx\gFrameCount = 0
      
      ProcedureReturn *ctx
  EndProcedure
  
  Procedure RoseBasic_DefaultImpl()
    CompilerIf #PB_Compiler_OS = #PB_OS_Linux
      Define.i lib = OpenLibrary(#PB_Any, "./rbglfw.so")
    CompilerElse
      Define.i lib = OpenLibrary(#PB_Any, "./rbglfw.dll")
    CompilerEndIf
    
    If lib
      *buildInFunction(0) = GetFunction(lib, "RoseBasic_Print")
      *buildInFunction(1) = GetFunction(lib, "RoseBasic_Waitkey")
      *buildInFunction(2) = GetFunction(lib, "RoseBasic_Input")
      *buildInFunction(3) = GetFunction(lib, "RoseBasic_DrawScreen")
      *buildInFunction(4) = GetFunction(lib, "RoseBasic_CloseScreen")
      *buildInFunction(5) = GetFunction(lib, "RoseBasic_OpenScreen")
      *buildInFunction(6) = GetFunction(lib, "RoseBasic_DrawImagePart")
      *buildInFunction(7) = GetFunction(lib, "RoseBasic_Box")
      *buildInFunction(8) = GetFunction(lib, "RoseBasic_Text")
      *buildInFunction(9) =  GetFunction(lib, "RoseBasic_ShouldClose")
      *buildInFunction(10) = GetFunction(lib, "RoseBasic_InitSocket")
      *buildInFunction(11) = GetFunction(lib, "RoseBasic_CloseSocket")
      *buildInFunction(12) = GetFunction(lib, "RoseBasic_DestroySocket")
      *buildInFunction(13) = GetFunction(lib, "RoseBasic_AcceptSocket")
      *buildInFunction(14) = GetFunction(lib, "RoseBasic_SetSocketOpt")
      *buildInFunction(15) = GetFunction(lib, "RoseBasic_ListenSocket")
      *buildInFunction(16) = GetFunction(lib, "RoseBasic_BindSocket")
      *buildInFunction(17) = GetFunction(lib, "RoseBasic_RecvSocket")
      *buildInFunction(18) = GetFunction(lib, "RoseBasic_SendSocket")
      *buildInFunction(19) = GetFunction(lib, "RoseBasic_MemoryAlloc")
      *buildInFunction(20) = GetFunction(lib, "RoseBasic_MemoryFree")
      *buildInFunction(21) = GetFunction(lib, "RoseBasic_ConsoleOpen")
      *buildInFunction(22) = GetFunction(lib, "RoseBasic_ConsolePrint")
      *buildInFunction(23) = GetFunction(lib, "RoseBasic_ConsolePrintN")
      *buildInFunction(24) = GetFunction(lib, "RoseBasic_PeekS")
      *buildInFunction(25) = GetFunction(lib, "RoseBasic_SetMaxFPS")
      *buildInFunction(26) = GetFunction(lib, "RoseBasic_LoadImage")
      *buildInFunction(27) = GetFunction(lib, "RoseBasic_FreeImage")
      *buildInFunction(28) = GetFunction(lib, "RoseBasic_DrawImage")
      *buildInFunction(29) = GetFunction(lib, "RoseBasic_SetCameraScale")
      *buildInFunction(30) = GetFunction(lib, "RoseBasic_RotateImage")
      *buildInFunction(31) = GetFunction(lib, "RoseBasic_LoadMap")
      *buildInFunction(32) = GetFunction(lib, "RoseBasic_FreeMap")
      *buildInFunction(33) = GetFunction(lib, "RoseBasic_DrawMap")
      *buildInFunction(34) = GetFunction(lib, "RoseBasic_MouseWheelUp")
      *buildInFunction(35) = GetFunction(lib, "RoseBasic_MouseWheelDown")
      *buildInFunction(36) = GetFunction(lib, "RoseBasic_Sin")
      *buildInFunction(37) = GetFunction(lib, "RoseBasic_Cos")
      *buildInFunction(38) = GetFunction(lib, "RoseBasic_MapGetHit")
      *buildInFunction(39) = GetFunction(lib, "RoseBasic_SetCameraPosition")
      *buildInFunction(40) = GetFunction(lib, "RoseBasic_GetCameraX")
      *buildInFunction(41) = GetFunction(lib, "RoseBasic_GetCameraY")
      *buildInFunction(42) = GetFunction(lib, "RoseBasic_AddLight")
      *buildInFunction(43) = GetFunction(lib, "RoseBasic_GetTime")
      *buildInFunction(44) = @RoseBasic_Split()
      *buildInFunction(45) = GetFunction(lib, "RoseBasic_Int")
      *buildInFunction(46) = GetFunction(lib, "RoseBasic_SetCycle")
      *buildInFunction(47) = @RoseBasic_FileOpen()
      *buildInFunction(48) = @RoseBasic_FileClose()
      *buildInFunction(49) = @RoseBasic_FileReadLine()
      *buildInFunction(50) = GetFunction(lib, "RoseBasic_LoadSound")
      *buildInFunction(51) = GetFunction(lib, "RoseBasic_FreeSound")
      *buildInFunction(52) = GetFunction(lib, "RoseBasic_KeyboardPress")
      *buildInFunction(53) = GetFunction(lib, "RoseBasic_ScreenWidth")
      *buildInFunction(54) = GetFunction(lib, "RoseBasic_ScreenHeight")
      *buildInFunction(55) = GetFunction(lib, "RoseBasic_GetCameraScale")
      *buildInFunction(56) = GetFunction(lib, "RoseBasic_GenerateMap")
      *buildInFunction(57) = GetFunction(lib, "RoseBasic_MapGetData")
      *buildInFunction(58) = GetFunction(lib, "RoseBasic_Sqr")
      *buildInFunction(59) = @RoseBasic_Random()
      *buildInFunction(60) = GetFunction(lib, "RoseBasic_Line")
      *buildInFunction(61) = GetFunction(lib, "RoseBasic_Color")
      *buildInFunction(62) = GetFunction(lib, "RoseBasic_Dot")
      *buildInFunction(63) = GetFunction(lib, "RoseBasic_Circle")
      *buildInFunction(64) = GetFunction(lib, "RoseBasic_KeyboardDown")
    Else
      PrintN("rbglfw.so not found in current directory: " + GetCurrentDirectory())
    EndIf
  EndProcedure

  Procedure run(*ctx.ExecutionContext)
    Define.b op = 0
    Define.l l = 0
    
    ;Define.CallFrame *frame = gFrames(gFrameCount - 1)
    Define.CallFrame *frame = *ctx\gFrames[*ctx\gFrameCount - 1]
    
    Protected *addr.ArrayOfPointer = ?jt 
    DataSection : jt:
      Data.i ?OP_CONSTANT, ?OP_NIL, ?OP_TRUE, ?OP_FALSE, ?OP_POP, ?OP_GET_LOCAL, ?OP_SET_LOCAL, ?OP_GET_GLOBAL, ?OP_DEFINE_GLOBAL,
             ?OP_GET_PROPERTY, ?OP_SET_PROPERTY, ?OP_EQUAL, ?OP_GREATER, ?OP_LESS, ?OP_ADD, ?OP_SUBTRACT, ?OP_MULTIPLY, ?OP_DIVIDE, ?OP_NOT,  ?OP_NEGATE, 
             ?OP_PRINT, ?OP_JUMP, ?OP_JUMP_IF_FALSE, ?OP_LOOP, ?OP_CALL, ?OP_INVOKE, ?OP_CLOSURE, ?OP_RETURN, ?OP_CLASS,
             ?OP_MOD, ?OP_POW, ?OP_NOT_EQU, ?OP_SET_ARRAY, ?OP_GET_ARRAY, ?OP_ALLOC_ARRAY, ?OP_FREE_ARRAY, ?OP_JUMP_IF_TRUE, ?OP_LT_EQU, ?OP_GRT_EQU, ?OP_HALT, ?OP_NOP,
             ?OP_CALL_NATIVE, ?OP_INVOKE_CONSTR, ?OP_TO_BOOL, ?OP_CONST_NATIVE, ?OP_REDIM, ?OP_XOR,
             ?OP_SUPER_INVOKE, ?OP_DUP, ?OP_SPAWN, ?OP_YIELD, ?OP_STOP
    EndDataSection
    
    Define.q value  = DoubleToValue(1024.0)
    Define.q value2 = DoubleToValue(768.0)
    
    
    Push(*ctx, value)
    Push(*ctx, value2)
    CallCFunctionFast(*buildInFunction(5), 2, @*ctx\gStack[*ctx\stackIP])
    Pop(*ctx)
    
    JUMP()
    
OP_STOP:
    KillThread(*ctx\id)
    ProcedureReturn #INTERPRET_OK 
    JUMP()
OP_YIELD:
    Delay(0)
    JUMP()
OP_SPAWN:
    Define.q nameVal = READ_STRING()
    READ_BYTE()
    argCount = op

    Define.ObjString *methodName = AS_OBJ(nameVal)

    ; Lookup callee (same logic as invoke)
    Define.q callee = peek(*ctx, -(argCount + 1))

    ; Create new execution context
    Define.ExecutionContext *newCtx = CreateExecutionContext(argCount)

    ; Copy arguments to new context stack
    For i = argCount To 1 Step -1
        PUSH(*newCtx, peek(*ctx, -i))
    Next

    ; Push callee into new context
    PUSH(*newCtx, callee)
    
    If Not invoke(*newCtx, *methodName\chars, argCount)
        PrintN("Runtime error: spawn failed.")
        ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    *newCtx\id = CreateThread(@run(), *newCtx)
    
    PUSH(*ctx, BOOL_VAL(#True))

    JUMP()
OP_DUP:
    push(*ctx, peek(*ctx, -1))
    JUMP()
OP_SUPER_INVOKE:
    ; 1. Read the method name constant (a NaN-boxed ObjString)
    Define val = READ_STRING()
    Define.ObjString *methodName = AS_OBJ(val)
    
    ; 2. Read argument count (next byte)
    READ_BYTE()
    Define argCount.i = op
    
    ; 3. Peek the superclass from the stack (the receiver is before args)
    Define.q superVal = Peek(*ctx, -argCount - 1)
    
    If Not IS_OBJ(superVal)
      PrintN("Runtime error: OP_SUPER_INVOKE expected object (superclass)")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    Define.ObjClass *superClass = AS_OBJ(superVal)
    
    ; 4. Invoke method from superclass using nan-boxed API
    If Not invokeFromSuperClass(*ctx, *methodName\chars, argCount)
      PrintN("OP_SUPER_INVOKE failed on " + PeekS(*methodName\chars))
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    ; 5. Refresh frame pointer after call
    *frame = *ctx\gFrames[*ctx\gFrameCount - 1] ;gFrames(gFrameCount - 1)
    
    JUMP()
    
    OP_XOR:
    BINARY_OP_BOOL(XOr)
    JUMP()
    OP_REDIM:
    READ_BYTE()
    l = op
    Define dimensionSize
    Define.i dimensions = PeekI(*Arrays(l) + MemorySize(*Arrays(l)) - SizeOf(Value))
    
    dimensionSize = 1
    For i = 0 To dimensions - 1
      dimensionSize * AS_NUMBER(pop(*ctx))
    Next
    
    Define newSize = 8+dimensionSize*8
    
    *Arrays(l) = ReAllocateMemory(*Arrays(l), newSize)
    PokeI(*Arrays(l)+(newSize-8), dimensions)
    
    JUMP()
    OP_TO_BOOL:
    Define.q value = POP(*ctx) ; 64-bit NaN-boxed value
    
    If IS_NIL(value) Or (IS_BOOL(value) And Not AS_BOOL(value)) Or (IS_NUM(value) And AS_NUMBER(value) = 0)
      value = BOOL_VAL(#False)
    Else
      value = BOOL_VAL(#True)
    EndIf
    
    PUSH(*ctx, value)
    JUMP()
    OP_NOP:
    JUMP()
    OP_PRINT:
    printValue(pop(*ctx))
    JUMP()
    OP_POP:
    POP(*ctx)
    JUMP()
    OP_CONST_NATIVE:
    PUSH_NATIVE(*ctx, PeekI(*frame\function\chunk\constants\values+PeekL(*frame\ip)))
    *frame\ip + SizeOf(Long)
    JUMP()
    OP_CONSTANT:
    PUSH(*ctx, PeekQ(*frame\function\chunk\constants\values+PeekL(*frame\ip)))
    *frame\ip + SizeOf(Long)
    JUMP()
    OP_NEGATE:
    Define.q av = peek(*ctx, -1)
    If Not IS_NUM(av)
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    Define.d valw = ValueToDouble(POP(*ctx))
    PUSH(*ctx, DoubleToValue(-valw))
    JUMP()
    OP_RETURN:
    Define.q result = POP(*ctx)
    ;gFrameCount - 1
    *ctx\gFrameCount - 1
    
    
    ;POP(*ctx)
    
    If *ctx\gFrameCount = 0 ;gFrameCount = 0
      Pop(*ctx)
      CallCFunctionFast(*buildInFunction(4), 0, 0) ; return value handling: should be boxed
      ProcedureReturn #INTERPRET_OK
    EndIf
    PUSH(*ctx, result)
    *frame = *ctx\gFrames[*ctx\gFrameCount - 1] ;gFrames(gFrameCount - 1)
    JUMP()
OP_ADD:


  Define.q rightV = peek(*ctx, -1)
  Define.q leftV  = peek(*ctx, -2)
  
  ; number + number
  If IS_NUMBER(leftV) And IS_NUMBER(rightV)
    Define.d resQ = ValueToDouble(leftV) + ValueToDouble(rightV)
    *ctx\gStack[*ctx\stackIP - 2] = DoubleToValue(resQ)
    *ctx\stackIP - 1
    JUMP()
  EndIf

  ; string + string
  If IS_OBJ(leftV) And IS_OBJ(rightV)
    Define.Obj *lo = AS_OBJ(leftV)
    Define.Obj *ro = AS_OBJ(rightV)

    If *lo\type = #OBJ_STRING And *ro\type = #OBJ_STRING
      Define.ObjString *a = *lo
      Define.ObjString *b = *ro

      Define.i newLength = *a\length + *b\length
      Define *chars = RB_StringAlloc(newLength)

      CopyMemory(*a\chars, *chars, *a\length)
      CopyMemory(*b\chars, *chars + *a\length, *b\length)

      Define.ObjString *result = takeString(*chars, newLength)
      
      POP(*ctx)
      POP(*ctx)
      PUSH(*ctx, OBJ_VAL(*result))
      JUMP()
    EndIf
  EndIf

  ; number + string
  If IS_NUMBER(leftV) And IS_OBJ(rightV)
    Define.Obj *ro = AS_OBJ(rightV)
    If *ro\type = #OBJ_STRING
      Define.s tmp = StrD(ValueToDouble(leftV))
      Define.ObjString *numStr = copyPBString(tmp)
      Define.ObjString *b = *ro

      Define.i newLength = *numStr\length + *b\length
      Define *chars = RB_StringAlloc(newLength)

      CopyMemory(*numStr\chars, *chars, *numStr\length)
      CopyMemory(*b\chars, *chars + *numStr\length, *b\length)

      Define.ObjString *result = takeString(*chars, newLength)

      POP(*ctx)
      POP(*ctx)
      PUSH(*ctx, OBJ_VAL(*result))
      JUMP()
    EndIf
  EndIf

  ; string + number
  If IS_OBJ(leftV) And IS_NUMBER(rightV)
    Define.Obj *lo = AS_OBJ(leftV)
    If *lo\type = #OBJ_STRING
      Define.s tmp = StrD(ValueToDouble(rightV))
      Define.ObjString *numStr = copyPBString(tmp)
      Define.ObjString *a = *lo

      Define.i newLength = *a\length + *numStr\length
      Define *chars = RB_StringAlloc(newLength)

      CopyMemory(*a\chars, *chars, *a\length)
      CopyMemory(*numStr\chars, *chars + *a\length, *numStr\length)

      Define.ObjString *result = takeString(*chars, newLength)

      POP(*ctx)
      POP(*ctx)
      PUSH(*ctx, OBJ_VAL(*result))
      JUMP()
    EndIf
  EndIf
  
    PrintN("OP_ADD: invalid operand types")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
    
    OP_SUBTRACT:
    Define.q rightV = peek(*ctx, -1)
    Define.q leftV  = peek(*ctx, -2)
    If Not IS_NUM(leftV) Or Not IS_NUM(rightV)
      PrintN("OP_SUBTRACT: invalid operand types")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    Define.d r = ValueToDouble(rightV)
    Define.d lk = ValueToDouble(leftV)
    Define.q res = DoubleToValue(lk - r)
    POP(*ctx) ; right
    POP(*ctx) ; left
    PUSH(*ctx, res)
    JUMP()
    
    OP_MULTIPLY:
    Define.q rightV = peek(*ctx, -1)
    Define.q leftV  = peek(*ctx, -2)
    
    If Not IS_NUM(leftV) Or Not IS_NUM(rightV)
      PrintN("OP_MULTIPLY: invalid operand types")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    Define.d r = ValueToDouble(rightV)
    Define.d lk = ValueToDouble(leftV)
    Define.q res = DoubleToValue(lk * r)
    POP(*ctx) ; right
    POP(*ctx) ; left
    PUSH(*ctx, res)
    JUMP()
    OP_DIVIDE: ; Optimized NaN-boxed
    Define.q rightV = peek(*ctx, -1)
    Define.q leftV  = peek(*ctx, -2)
    
    If Not IS_NUM(leftV) Or Not IS_NUM(rightV)
      PrintN("OP_DIVIDE: invalid operand types")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    Define.d r = ValueToDouble(rightV)
    Define.d lk = ValueToDouble(leftV)
    Define.q res = DoubleToValue(lk / r)
    POP(*ctx) ; right
    POP(*ctx) ; left
    PUSH(*ctx, res)
    JUMP()
    
    OP_MOD: ;Optimized
    Define.q rightV = peek(*ctx, -1)
    Define.q leftV  = peek(*ctx, -2)
    
    If Not IS_NUM(leftV) Or Not IS_NUM(rightV)
      PrintN("OP_DIVIDE: invalid operand types")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    Define.d r = ValueToDouble(rightV)
    Define.d lk = ValueToDouble(leftV)
    Define.q res = DoubleToValue(Mod(lk, r))
    POP(*ctx) ; right
    POP(*ctx) ; left
    PUSH(*ctx, res)
    JUMP()
    OP_POW:
    Define.q rightV = peek(*ctx, -1)
    Define.q leftV  = peek(*ctx, -2)
    
    If Not IS_NUM(leftV) Or Not IS_NUM(rightV)
      PrintN("OP_DIVIDE: invalid operand types")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    
    Define.d r = ValueToDouble(rightV)
    Define.d lk = ValueToDouble(leftV)
    Define.q res = DoubleToValue(Pow(lk, r))
    POP(*ctx) ; right
    POP(*ctx) ; left
    PUSH(*ctx, res)
    JUMP()
    OP_NIL:
    PUSH(*ctx, #NIL_VAL)
    JUMP()
    
    OP_TRUE:
    PUSH(*ctx, #TRUE_VAL)
    JUMP()
    
    OP_FALSE:
    PUSH(*ctx, #FALSE_VAL)
    JUMP()
    OP_NOT:
    Define.q value = POP(*ctx)
    Define.q result
    
    If IS_BOOL(value)
      result = BOOL_VAL(Not AS_BOOL(value))
    ElseIf IS_NUM(value)
      result = BOOL_VAL(Not AS_NUMBER(value))
    ElseIf IS_NIL(value)
      result = BOOL_VAL(#True)
    Else
      ; Non-nil objects are truthy
      result = BOOL_VAL(#False)
    EndIf
    
    PUSH(*ctx, result)
    JUMP()
    OP_EQUAL:
    Define.q value2 = POP(*ctx)
    Define.q value1 = POP(*ctx)
    value2 = BOOL_VAL(valuesEqual(value1, value2))
    PUSH(*ctx, value2)
    JUMP()
    OP_GREATER:
    BINARY_OP_BOOL(>)
    JUMP()
    OP_LESS:
    BINARY_OP_BOOL(<)
    JUMP()
    OP_LT_EQU:
    BINARY_OP_BOOL(<=)
    JUMP()
    OP_GRT_EQU:
    BINARY_OP_BOOL(>=)
    JUMP()
    OP_NOT_EQU:
    Define.q value2 = POP(*ctx)
    Define.q value1 = POP(*ctx)
    value2 = BOOL_VAL(Bool(valuesEqual(value1, value2) <> #True))
    PUSH(*ctx, value2)
    JUMP()
    OP_JUMP_IF_FALSE:
    READ_LONG()
    Define.q av = peek(*ctx, -1)
    If isFalsey(av)
      *frame\ip + l
    EndIf
    JUMP()
    OP_JUMP_IF_TRUE:
    READ_LONG()
    Define.q av = peek(*ctx, -1)
    If Not isFalsey(av)
      *frame\ip + l
    EndIf
    JUMP()
    OP_JUMP:
    READ_LONG()
    *frame\ip + l
    JUMP()
    OP_LOOP:
    READ_LONG()
    *frame\ip - l
    JUMP()
    OP_CLOSURE:
    Define.ObjClosure *closure = newClosure(PeekI(*frame\function\chunk\constants\values+PeekL(*frame\ip)))
    *frame\ip + SizeOf(Long)
    Define.q value = OBJ_VAL(*closure)
    PUSH(*ctx, value)
    JUMP()
OP_SET_PROPERTY:
  READ_LONG()

  Define.q value = POP(*ctx)
  Define.q recv  = POP(*ctx)

  If Not IS_INSTANCE(recv)
    PrintN("OP_SET_PROPERTY: receiver not instance")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
  EndIf

  Define.ObjInstance *instance = AS_OBJ(recv)
  If *instance = #Null Or *instance\fields = #Null
    PrintN("OP_SET_PROPERTY: null instance/fields")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
  EndIf

  PokeQ(*instance\fields + l, value)

  ; assignment evaluates to stored value
  PUSH(*ctx, value)
  JUMP()
OP_GET_PROPERTY:
  READ_LONG()

  ; consume receiver so it doesn't remain on the stack
  Define.q receiver = POP(*ctx)

  If Not IS_INSTANCE(receiver)
    PrintN("OP_GET_PROPERTY: receiver not instance")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
  EndIf

  Define.ObjInstance *instance = AS_OBJ(receiver)
  If *instance = #Null Or *instance\fields = #Null
    PrintN("OP_GET_PROPERTY: null instance/fields")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
  EndIf

  ; push only the property value
  PUSH(*ctx, PeekQ(*instance\fields + l))
  JUMP()
    OP_DEFINE_GLOBAL:
    l = PeekL(*frame\ip)
    *frame\ip + SizeOf(Long)
    
    ; Read string constant (variable name)
    Define.q value = PeekQ(*frame\function\chunk\constants\values + l)
    Define.ObjString *string = AS_OBJ(value)
    
    ; Define global variable
    globals(PeekS(*string\chars, *string\length)) = peek(*ctx, -1)
    
    JUMP()
    OP_GET_GLOBAL:
    l = PeekL(*frame\ip)
    *frame\ip + SizeOf(Long)
    
    ; Read the variable name from constants
    Define.q value = PeekQ(*frame\function\chunk\constants\values + l)
    Define.ObjString *string = AS_OBJ(value)

    ; Push the global’s value (already NaN-boxed)
    PUSH(*ctx, globals(PeekS(*string\chars, *string\length)))
    JUMP()
OP_CALL:
  READ_BYTE()
  Define.i argCount = op

  Define.q callee = peek(*ctx, -(argCount + 1))
  If Not callValue(*ctx, callee, argCount)
    PrintN("OP_CALL")
    ProcedureReturn #INTERPRET_RUNTIME_ERROR
  EndIf

  *frame = *ctx\gFrames[*ctx\gFrameCount - 1]
  JUMP()
    OP_CALL_NATIVE:
    READ_BYTE()
    Define.i argCount = op
    READ_LONG()
    Define.i cindex = l
    
    ; Call native C function (returns NaN-boxed value as .q)
    Define.q result = CallCFunctionFast(*buildInFunction(cindex), argCount, @*ctx\gStack[*ctx\stackIP])
    ;Define.q result = CallCFunctionFast(*buildInFunction(cindex), argCount, @gStack(stackIP))
    Debug result
    
    ; Clean up stack (remove arguments)
    If *ctx\stackIP > 1
      *ctx\stackIP - argCount
    EndIf

    ; Push returned value (already NaN-boxed)
    PUSH(*ctx, result)
    
    JUMP()
    OP_INVOKE_CONSTR:
    Define.q value = READ_STRING()
    Define.ObjString *string = AS_OBJ(value)
    READ_BYTE()
    result = invokeConstructor(*ctx, *string\chars, op)
    If Not result
      PrintN("OP_INVOKE_R")
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    *frame = *ctx\gFrames[*ctx\gFrameCount - 1] ;gFrames(gFrameCount - 1)
    JUMP()
    OP_INVOKE:
    Define.q value = READ_STRING()
    Define.ObjString *string = AS_OBJ(value)
    READ_BYTE()
    Define.b _op = op
    ;READ_BYTE()
    If Not invoke(*ctx, *string\chars, _op)
      PrintN("OP_INVOKE " + PeekS(*String\chars))
      ProcedureReturn #INTERPRET_RUNTIME_ERROR
    EndIf
    *frame = *ctx\gFrames[*ctx\gFrameCount - 1]
    If op
      ;pop(*ctx)
    EndIf
    JUMP()
    OP_CLASS:
    ; Read the class name constant
    l = PeekL(*frame\ip)
    *frame\ip + SizeOf(Long)
    
    ; Read the extra byte (e.g., superclass index, or flag)
    READ_BYTE()
    Define.i flag = op
    
    ; Get the string constant (boxed value)
    Define.q constVal = PeekQ(*frame\function\chunk\constants\values + l)
    Define.ObjString *name = AS_STRING(constVal)
    
    ; Create new class object
    Define.ObjClass *klass = newClass(*name, flag)
    
    ; Push the boxed class object
    PUSH(*ctx, OBJ_VAL(*klass))
    
    JUMP()
    OP_SET_LOCAL:
    READ_LONG()
    PokeQ(*frame\slots+l, peek(*ctx, -1))
    JUMP()
    OP_GET_LOCAL:
    READ_LONG()
    PUSH(*ctx, PeekQ(*frame\slots+l))
    JUMP()     
OP_ALLOC_ARRAY:
    READ_BYTE() 
    l = op  
    READ_BYTE()
    If ArraySize(*Arrays()) < l
      ReDim *Arrays(l)
    EndIf
    If *Arrays(l) = #Null
      Define.i size = AS_NUMBER(pop(*ctx)) + 8
      *Arrays(l) = AllocateMemory(size)
      PokeI(*Arrays(l)+(size-8), op)
    Else
      *ctx\stackIP - 1
    EndIf
 
    JUMP()			  
OP_GET_ARRAY:
    READ_BYTE()
    Define.i dimensions = PeekI(*Arrays(op) + (MemorySize(*Arrays(op)) - 8))

    If dimensions > 1
      Dim coords.i(dimensions - 1)
      
      For i = 0 To dimensions - 1 
        coords(i) = AS_NUMBER(pop(*ctx))
      Next
      
      Define.i offset = 0
      Define.i stride = 1
      
      For i = dimensions - 1 To 0 Step -1
        offset + coords(i) * stride
        stride * PeekI(*Arrays(op) + MemorySize(*Arrays(op)) - 8 - (i * 4) - 4)
      Next
      
      Debug "OP_GET_ARRAY " + offset + " "+ AS_NUMBER(PeekQ(*Arrays(op) + (offset * SizeOf(Quad))))
      ; Push the computed address
      PUSH(*ctx, PeekQ(*Arrays(op) + (offset * SizeOf(Quad))))
    Else
      Define.i cindex = AS_NUMBER(pop(*ctx))
      PUSH(*ctx, PeekQ(*Arrays(op) + (cindex * SizeOf(Quad))))
    EndIf
    
    JUMP()
OP_FREE_ARRAY:
    READ_BYTE()
    If ArraySize(*Arrays()) >= op And *Arrays(op)
      FreeMemory(*Arrays(op))
      *Arrays(op) = #Null
    EndIf
    JUMP()		  
OP_SET_ARRAY:
    READ_BYTE()
    Define.i arrayIndex = op
    
    ; Retrieve number of dimensions stored at end of array block
    Define.i dimensions = PeekI(*Arrays(arrayIndex) + MemorySize(*Arrays(arrayIndex)) - 8)
    ; Pop the boxed value to store
    Define.q value = POP(*ctx)
    
    If dimensions > 1
      ; Multi-dimensional array
      Dim coords.i(dimensions - 1)
      
      ; Pop coordinates (reverse order)
      For i = 0 To dimensions - 1
        coords(i) = AS_NUMBER(POP(*ctx))
      Next
      
      ; Compute flat memory offset
      Define.i offset = 0
      Define.i stride = 1
      
      For i = dimensions - 1 To 0 Step -1
        offset + coords(i) * stride
        stride * PeekI(*Arrays(arrayIndex) + MemorySize(*Arrays(arrayIndex)) - 8 - (i * 4) - 4)
      Next
      
      Debug "OP_SET_ARRAY " + offset + " "+ AS_NUMBER(value)
      ; Store boxed value at computed slot
      PokeQ(*Arrays(arrayIndex) + (offset * SizeOf(Quad)), value)
    Else
      ; Single-dimension array
      Define.i cindex = AS_NUMBER(POP(*ctx))
      PokeQ(*Arrays(arrayIndex) + (cindex * SizeOf(Quad)), value)
    EndIf
    
    JUMP()
    
    OP_HALT:
    CallCFunctionFast(*buildInFunction(4), 0, 0)
    ProcedureReturn #INTERPRET_OK
    JUMP()   
  EndProcedure
  
  Procedure Display(*function.ObjFunction)
    RESET_STACK(mainContext)
    push(Interpreter::mainContext, *function)
    
    Define.Interpreter::CallFrame *frame = Interpreter::gFrames(gFrameCount) 
    gFrameCount + 1
    
    *frame\function = *function
    *frame\ip = *function\chunk\code
    *frame\slots = Interpreter::@gStack()   
    
    Define.i max = *frame\ip + *function\chunk\count
    
    Define.b op = 0
    Define.l l = 0
    Define.CallFrame *frame = gFrames(gFrameCount - 1)
    
    If *function\name <> 0
      Debug PeekS(*function\name)
    EndIf
    
    While *frame\ip < max
      READ_BYTE()
      
      Select op
        Case #OP_DUP
        Case #OP_YIELD
        Case #OP_STOP
        Case #OP_SPAWN
          READ_STRING()
          READ_BYTE()
        Case #OP_REDIM
          READ_BYTE()
        Case 48
            READ_BYTE()
        Case #OP_CALL_NATIVE
          Debug "["+*frame\ip+"] OP_CALL_NATIVE "
             READ_BYTE()
          Read_LONG()
        Case #OP_CONST_NATIVE
          READ_STRING()
          Debug "["+*frame\ip+"] OP_CONST_NATIVE "
        Case 55
          Debug "["+*frame\ip+"] OP_DUP"
        Case 53
          Debug "["+*frame\ip+"] OP_ARRAY_STORE" 
        Case 54
          Debug "["+*frame\ip+"] OP_ARRAY_INDEX"  
        Case #OP_SUPER_INVOKE
          READ_STRING()
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_SUPER_INVOKE"  
        Case 49
          READ_BYTE()
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_REDIM"
        Case #OP_NOP
          Debug "["+*frame\ip+"] OP_NOP"
        Case #OP_CONSTANT
          Debug "["+*frame\ip+"] OP_CONSTANT " +PeekL(*frame\ip)
          *frame\ip + SizeOf(Long)
        Case #OP_POP
          Debug "["+*frame\ip+"] OP_POP"
          sameCounter = 0
        Case #OP_GET_LOCAL
          Debug "["+*frame\ip+"] OP_GET_LOCAL "+PeekL(*frame\ip)
          *frame\ip + SizeOf(Long)
        Case #OP_SET_LOCAL
          Debug "["+*frame\ip+"] OP_SET_LOCAL "+PeekL(*frame\ip)
          *frame\ip + SizeOf(Long)
        Case #OP_NEGATE
          Debug "["+*frame\ip+"] OP_NEGATE" 
        Case #OP_ADD
          Debug "["+*frame\ip+"] OP_ADD" 
        Case #OP_SUBTRACT
          Debug "["+*frame\ip+"] OP_SUB" 
        Case #OP_MULTIPLY
          Debug "["+*frame\ip+"] OP_MUL" 
        Case #OP_DIVIDE
          Debug "["+*frame\ip+"] OP_DIV" 
        Case #OP_MOD
          Debug "["+*frame\ip+"] OP_MOD" 
        Case #OP_POW
          Debug "["+*frame\ip+"] OP_POW" 
        Case #OP_NIL
          Debug "["+*frame\ip+"] OP_NIL" 
        Case #OP_TRUE
          Debug "["+*frame\ip+"] OP_TRUE" 
        Case #OP_FALSE
          Debug "["+*frame\ip+"] OP_FALSE" 
        Case #OP_NOT
          Debug "["+*frame\ip+"] OP_NOT" 
        Case #OP_EQUAL
          Debug "["+*frame\ip+"] OP_EQUAL" 
        Case #OP_NOT_EQU
          Debug "["+*frame\ip+"] OP_NOT_EQUAL" 
        Case #OP_GREATER
          Debug "["+*frame\ip+"] OP_GREATER" 
        Case #OP_LESS
          Debug "["+*frame\ip+"] OP_LESS" 
        Case #OP_CALL
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_CALL " + op
        Case #OP_RETURN
          Debug "["+*frame\ip+"] OP_RETURN "
        Case #OP_CLASS
          READ_STRING()
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_CLASS "
        Case #OP_GET_PROPERTY
          READ_LONG()
          
         ; Define.q value = READ_STRING()  ; NaN-boxed value
          ;If IS_OBJ(value)
          ;  Define *objString.Interpreter::ObjString = (value & $0000FFFFFFFFFFFF)
         ;   Debug "[" + *frame\ip + "] OP_GET_PROPERTY " + PeekS(*objString\chars)
         ; Else
          ;  Debug "[" + *frame\ip + "] OP_GET_PROPERTY <non-object>"
          ;EndIf
          Debug "[" + *frame\ip + "] OP_GET_PROPERTY " + l
        Case #OP_SET_PROPERTY
          READ_LONG()
          
;           Define.q value = READ_STRING()
;           If IS_OBJ(value)
;             Define *objString.Interpreter::ObjString = (value & $0000FFFFFFFFFFFF)
;             Debug "[" + *frame\ip + "] OP_SET_PROPERTY " + PeekS(*objString\chars)
;           Else
;             Debug "[" + *frame\ip + "] OP_SET_PROPERTY <non-object>"
;           EndIf
          Debug "[" + *frame\ip + "] OP_SET_PROPERTY " + l
        Case #OP_SET_ARRAY
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_SET_ARRAY " + op
        Case #OP_GET_ARRAY
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_GET_ARRAY " + op 
        Case #OP_ALLOC_ARRAY
          READ_BYTE()
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_ALLOC_ARRAY " + op
        Case #OP_FREE_ARRAY
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_FREE_ARRAY"
        Case #OP_JUMP_IF_FALSE
          READ_LONG()
          Debug "["+*frame\ip+"] OP_JUMP_IF_FALSE " + l
        Case #OP_JUMP
          READ_LONG()
          Debug "["+*frame\ip+"] OP_JUMP"
        Case #OP_LOOP
          READ_LONG()
          Debug "["+*frame\ip+"] OP_LOOP"
        Case #OP_JUMP_IF_TRUE
          READ_LONG()
          Debug "["+*frame\ip+"] OP_JUMP_IF_TRUE " + l
        Case #OP_LT_EQU
          Debug "["+*frame\ip+"] OP_LT_EQU"
        Case #OP_GRT_EQU
          Debug "["+*frame\ip+"] OP_GRT_EQU"
        Case #OP_GET_GLOBAL
          Debug "["+*frame\ip+"] OP_GET_GLOBAL "+READ_STRING()
        Case #OP_PRINT
          Debug "["+*frame\ip+"] OP_PRINT"
        Case #OP_CLOSURE
          *frame\ip + SizeOf(Long)
          Debug "["+*frame\ip+"] OP_CLOSURE"
        Case #OP_HALT
          Debug "["+*frame\ip+"] OP_HALT"
        Case #OP_DEFINE_GLOBAL
          Debug "["+*frame\ip+"] OP_DEFINE_GLOBAL "+ READ_STRING()
        Case #OP_INVOKE
          Debug "["+*frame\ip+"] OP_INVOKE "+READ_STRING()
          READ_BYTE()
          READ_BYTE()
        Case #OP_CALL_NATIVE
          READ_BYTE()
          READ_LONG()
          Debug "["+*frame\ip+"] OP_CALL_NATIVE " + op + " " + l
        Case #OP_NIL
          Debug "["+*frame\ip+"] OP_NIL"
        Case #OP_INVOKE_CONSTR
          READ_STRING()
          READ_BYTE()
          Debug "["+*frame\ip+"] OP_INVOKE_CONSTR"
        Case #OP_TO_BOOL
          Debug "["+*frame\ip+"] #OP_TO_BOOL"
        Default
          Debug "["+*frame\ip+"] UNKOWN "+Str(op)
          ProcedureReturn
      EndSelect
    Wend
    
  EndProcedure
  
EndModule
; IDE Options = PureBasic 6.30 (Windows - x64)
; CursorPosition = 239
; FirstLine = 212
; Folding = --------------------
; EnableThread
; EnableXP
; DPIAware
; Compiler = PureBasic 6.21 - C Backend (Windows - x64)
; EnablePurifier