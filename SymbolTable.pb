XIncludeFile "LinkedList.pb"
XIncludeFile "FastString.pb"
XIncludeFile "GlobalMemoryCache.pb"


DeclareModule SymbolTable
  EnableExplicit
  
  IncludeFile "tokens.pb"
  
  Enumeration ; Symbol kind
    #SKind_Variable
    #SKind_Function
    #SKind_Class
    #SKind_Const
    #SKind_Module
  EndEnumeration
  
  Global Dim Kinds.s(#SKind_Const)
  
  Enumeration ; Ref kind
    #Ref_Class
    #Ref_Local
    #Ref_Parameter
  EndEnumeration
  
  Enumeration ;Visibility
    #VPublic
    #VPrivate
    #VProtected
  EndEnumeration

  Structure SSymbol
    *name
    kind.i
    buildIn.b
    visible.i
    isStatic.i
    arrayInit.i
    dimensions.i
    dimensionSize.i
  EndStructure
  
  Structure SClass Extends SSymbol
    id.i
    localCount.l
    *module
    *cObj.ObjClass
    *superClass
    *variables.LinkedList::CustomList
    *constants.LinkedList::CustomList
    *functions.LinkedList::CustomList
  EndStructure
  
  Structure SModule Extends SSymbol
    *variables.LinkedList::CustomList
  EndStructure
    
  Global refClass = 1, classCount = 0
  
  Structure SConstant Extends SSymbol
    Type.i
    *scope
    op.i
    StructureUnion
      integer.i
      double.d
      *string
    EndStructureUnion
  EndStructure
  
  Structure SVariable Extends SSymbol
    Type.i
    RefKind.i   ; Class, local, parameter
    RefOffset.i 
    *Sym.SSymbol
    isInit.a
  EndStructure
  
  Structure SFunction Extends SSymbol
    currentProperty.i
    Type.i    ; Return type
    id.i
    *addr
    *class
    *module
    isParallel.a
    *fObj.ObjFunction
    fObjValue.q
    ParamCount.i
    *entry.LinkedList::Node
    *ParamTypes.LinkedList::CustomList
    inlineYes.b
    isInit.b
  EndStructure
  
  Global *module, *function
  Global NewMap *Symbols.SSymbol()
  
  Declare.s getLastResolve()
  Declare isResolveEmpty()
  Declare deleteResolve(key.i)
  Declare getResolveConstants(isHead.i)
  
  Declare resetScope()
  Declare LookupSymbol(*string)
  Declare LookupSymbolNoScopeResolve(*string)
  Declare LookupSymbolCurrentScope(*string)
  Declare addModule(*string)
  Declare addClass(*string, buildIn = 0)
  Declare addFunction(*string, *next.LinkedList::Node = 0)
  Declare addVariable(*string, Type.i, RefKind.i, RefOffset.i, visible.i = 0)
  Declare AddVariableClass(*string, Type.i, *sym, RefKind.i, RefOffset.i)
  Declare addConstant(*string, Type.i, *value)
  Declare ReAddConstant(*scope, *string, Type.i, *value)
  Declare enterScope(*string)
  Declare leaveScope()
  Declare getCurrentScope()
  Declare getPreviousScope()
  Declare init()
  Declare destroy()
  Declare setScope(*String)
  Declare.s getCurrentClass()
  Declare SetFunction(*string)
  Declare SetModule(*string)
  Declare getModule()
  Declare LookupSymbolScoped(*name)
EndDeclareModule

Module SymbolTable
  
  Global.LinkedList::CustomList *Scopes 
  Global Dim *buildInVars.SVariable(3)
  Global Dim *buildInFunctions.SFunction(100)
  Global Dim *buildInClasses.SClass(14)
  Global Dim *buildInConstants.SConstant(26)
  Global.LinkedList::CustomList *resolveConstants
  
  Procedure enumerateSymbols()
    ForEach *Symbols() 
      Debug *Symbols()\name
    Next
  EndProcedure
  
  Procedure.s getLastResolve()
    Define.SConstant *C
    Define.s constName = ""
    Define.s scope = ""
    Define.LinkedList::Node *Next = *resolveConstants\head
    
    While *next
      *C = *Next\value
      
      If *C
        scope = PeekS(*C\scope)
        constName + PeekS(*C\name) + " "
      EndIf
      
      *next = *next\next
    Wend
    
    ProcedureReturn "Class:"+scope+" constant's:"+constName
  EndProcedure
  
  Procedure isResolveEmpty()
    ProcedureReturn LinkedList::isEmpty(*resolveConstants)
  EndProcedure
  
  Procedure getResolveConstants(isHead.i)
    If isHead = 0
      ProcedureReturn *resolveConstants\current
    EndIf
    ProcedureReturn *resolveConstants\head
  EndProcedure
  
  Procedure deleteResolve(key.i)
    LinkedList::delete(*resolveConstants, key)
  EndProcedure
  
  Procedure init()
    Kinds(#SKind_Variable) = "Variable"
    Kinds(#SKind_Function) = "Function"
    Kinds(#SKind_Class) = "Class"
    Kinds(#SKind_Const) = "Const"
    
    *Scopes = LinkedList::init()
    LinkedList::addLast(*Scopes, @"", 2)
    
    *buildInFunctions(31) = addFunction(@"debug")
     With *buildInFunctions(31)
      \id = 31
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    *buildInClasses(0) = addClass(@"screen", 1)
    *buildInClasses(0)\functions = LinkedList::Init()
    
    enterScope(@"screen")
    
    *buildInFunctions(0) = addFunction(@"print")
    
    With *buildInFunctions(0)
      \id = 0
      \ParamCount = 1
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    *buildInFunctions(2) = addFunction(@"destroy")
    
    With *buildInFunctions(2)
      \id = 4
      \isStatic = 1
      \ParamCount = 0
      \Type = #SUBR
      \ParamTypes = #Null
    EndWith
    
    *buildInFunctions(3) = addFunction(@"draw")
    
    With *buildInFunctions(3)
      \id = 3
      \isStatic = 1
      \ParamCount = 0
      \Type = #SUBR
      \ParamTypes = #Null
    EndWith
    
    *buildInFunctions(4) = addFunction(@"open")
    
    With *buildInFunctions(4)
      \id = 5
      \ParamCount = 2
      \isStatic = 1
      \Type = #NUMBER
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TSTRING_OPTIONAL)
    EndWith
    
    *buildInFunctions(6) = addFunction(@"box")
    
    With *buildInFunctions(6)
      \id = 7
      \ParamCount = 4
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(60) = addFunction(@"line")
    
    With *buildInFunctions(60)
      \id = 60
      \ParamCount = 4
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(61) = addFunction(@"color")
    
    With *buildInFunctions(61)
      \id = 61
      \ParamCount = 3
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(62) = addFunction(@"dot")
    
    With *buildInFunctions(62)
      \id = 62
      \ParamCount = 2
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(63) = addFunction(@"circle")
    
    With *buildInFunctions(63)
      \id = 63
      \ParamCount = 3
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(7) = addFunction(@"text")
    
    With *buildInFunctions(7)
      \id = 8
      \ParamCount = 3
      \isStatic = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    *buildInFunctions(8) = addFunction(@"shouldclose")
    
    With *buildInFunctions(8)
      \id = 9
      \ParamCount = 0
      \Type = #NUMBER
    EndWith
    
    Define.i I 
    For I = 0 To 8
      If *buildInFunctions(I)
        LinkedList::addLast(*buildInClasses(0)\functions, *buildInFunctions(I), SizeOf(SFunction))
      EndIf
    Next I
    
    *buildInFunctions(24) = addFunction(@"setmaxfps")
    
    With *buildInFunctions(24)
      \id = 25
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes = LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    LinkedList::addLast(*buildInClasses(0)\functions, *buildInFunctions(24), SizeOf(SFunction))
    
    *buildInFunctions(53) = addFunction(@"getwidth")
    
    With *buildInFunctions(53)
      \id = 53
      \ParamCount = 0
      \Type = #TINTEGER
    EndWith
    
    LinkedList::addLast(*buildInClasses(0)\functions, *buildInFunctions(53), SizeOf(SFunction))
    
    *buildInFunctions(54) = addFunction(@"getheight")
    
    With *buildInFunctions(54)
      \id = 54
      \ParamCount = 0
      \Type = #TINTEGER
    EndWith
    
    LinkedList::addLast(*buildInClasses(0)\functions, *buildInFunctions(54), SizeOf(SFunction))
    
    leaveScope()
    
    *buildInClasses(1) = addClass(@"system", 1)
    *buildInClasses(1)\constants = LinkedList::Init()
    
    enterScope(@"system")
    
    *buildInConstants(0) = addConstant(@"os", #TSTRING, @"windows")
    LinkedList::addLast(*buildInClasses(1)\constants, *buildInConstants(0), SizeOf(SConstant))
    
    LeaveScope()
    
    *buildInConstants(1) = addConstant(@"true", #TINTEGER, 1)
    *buildInConstants(2) = addConstant(@"false", #TINTEGER, 0)
    *buildInConstants(24) = addConstant(@"null", #TINTEGER, 0)
    
    *buildInClasses(2) = addClass(@"tilemap", 1)
   
    enterScope(@"tilemap")
    
    *buildInFunctions(31) = addFunction(@"load")
    With *buildInFunctions(31)
      \id = 31
      \ParamCount = 2
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      \Type = #TINTEGER
    EndWith
    
    *buildInFunctions(32) = addFunction(@"free")
    With *buildInFunctions(32)
      \id = 32
      \ParamCount = 0
      \Type = #SUBR
    EndWith
     *buildInFunctions(33) = addFunction(@"draw")
    With *buildInFunctions(33)
      \id = 33
      \ParamCount = 0
      \Type = #SUBR
    EndWith
    *buildInFunctions(38) = addFunction(@"gethit")
    With *buildInFunctions(38)
      \id = 38
      \ParamCount = 2
      \Type = #TINTEGER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
     *buildInFunctions(57) = addFunction(@"getdata")
    With *buildInFunctions(57)
      \id = 57
      \ParamCount = 2
      \Type = #TINTEGER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    *buildInFunctions(56) = addFunction(@"createdungeon")
    With *buildInFunctions(56)
      \id = 56
      \ParamCount = 3
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    LeaveScope()
    
    *buildInClasses(3) = addClass(@"Socket", 1)
    *buildInClasses(3)\functions = LinkedList::Init()
    
    enterScope(@"Socket")
    
    *buildInConstants(5) = addConstant(@"AF_UNSPEC", #TINTEGER, 0)
    *buildInConstants(6) = addConstant(@"AF_INET", #TINTEGER, 2)
    *buildInConstants(7) = addConstant(@"AF_INET6", #TINTEGER, 23)
    *buildInConstants(8) = addConstant(@"AF_IRDA", #TINTEGER, 26)
    *buildInConstants(9) = addConstant(@"AF_BTH", #TINTEGER, 32)
    *buildInConstants(10) = addConstant(@"IPPROTO_ICMP", #TINTEGER, 1)
    *buildInConstants(11) = addConstant(@"IPPROTO_IGMP", #TINTEGER, 2)
    *buildInConstants(12) = addConstant(@"BTHPROTO_RFCOMM", #TINTEGER, 3)
    *buildInConstants(13) = addConstant(@"IPPROTO_TCP", #TINTEGER, 6)
    *buildInConstants(14) = addConstant(@"IPPROTO_UDP", #TINTEGER, 17)
    *buildInConstants(15) = addConstant(@"IPPROTO_ICMPV6", #TINTEGER, 58)
    *buildInConstants(16) = addConstant(@"SOCK_STREAM", #TINTEGER, 1)
    *buildInConstants(17) = addConstant(@"SOCK_DGRAM", #TINTEGER, 2)
    *buildInConstants(18) = addConstant(@"SOCK_RAW", #TINTEGER, 3)
    *buildInConstants(19) = addConstant(@"SOCK_RDM", #TINTEGER, 4)
    *buildInConstants(20) = addConstant(@"SOCK_SEQPACKET", #TINTEGER, 5)
    *buildInConstants(21) = addConstant(@"SO_REUSEADDR", #TINTEGER, 4)
    *buildInConstants(22) = addConstant(@"SOL_SOCKET",#TINTEGER, 65535)
    
    *buildInFunctions(9) = addFunction(@"new")
    
    With *buildInFunctions(9)
      \id = 10
      \ParamCount = 3
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(10) = addFunction(@"Close")
    
    With *buildInFunctions(10)
      \id = 11
      \ParamCount = 0
      \Type = #SUBR
    EndWith
    
    *buildInFunctions(11) = addFunction(@"delete")
    
    With *buildInFunctions(11)
      \id = 12
      \ParamCount = 0
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(12) = addFunction(@"Accept")
    
    With *buildInFunctions(12)
      \id = 13
      \ParamCount = 0
      \Type = #NUMBER
    EndWith
    
    *buildInFunctions(13) = addFunction(@"SetOpt")
    
    With *buildInFunctions(13)
      \id = 14
      \ParamCount = 3
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(14) = addFunction(@"Listen")
    
    With *buildInFunctions(14)
      \id = 15
      \ParamCount = 1
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(15) = addFunction(@"Bind")
    
    With *buildInFunctions(15)
      \id = 16
      \ParamCount = 3
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(16) = addFunction(@"Recv")
    
    With *buildInFunctions(16)
      \id = 17
      \ParamCount = 3
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(17) = addFunction(@"Send")
    
    With *buildInFunctions(17)
      \id = 18
      \ParamCount = 3
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    For I = 9 To 17
      If *buildInFunctions(I)
        LinkedList::addLast(*buildInClasses(3)\functions, *buildInFunctions(I), SizeOf(SFunction))
      EndIf
    Next I
    
    leaveScope()
    
    *buildInClasses(4) = addClass(@"memory", 1)
    *buildInClasses(4)\functions = LinkedList::Init()
    
    enterScope(@"memory")
    
    *buildInFunctions(18) = addFunction(@"alloc")
    
    With *buildInFunctions(18)
      \id = 19
      \ParamCount = 1
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(19) = addFunction(@"free")
    
    With *buildInFunctions(19)
      \id = 20
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    For I = 18 To 19
      If *buildInFunctions(I)
        LinkedList::addLast(*buildInClasses(4)\functions, *buildInFunctions(I), SizeOf(SFunction))
      EndIf
    Next I
    
    *buildInFunctions(23) = addFunction(@"peeks")
    
    With *buildInFunctions(23)
      \id = 24
      \ParamCount = 3
      \Type = #STRING
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    LinkedList::addLast(*buildInClasses(4)\functions, *buildInFunctions(23), SizeOf(SFunction))
    *buildInConstants(23) = addConstant(@"ASCII",#TINTEGER, 1)
    
    leaveScope()
    
    *buildInClasses(5) = addClass(@"Console", 1)
    *buildInClasses(5)\functions = LinkedList::Init()
    
    enterScope(@"Console")
    
    *buildInFunctions(20) = addFunction(@"Open")
    
    With *buildInFunctions(20)
      \id = 21
      \ParamCount = 0
      \Type = #SUBR
    EndWith
    
    *buildInFunctions(21) = addFunction(@"Print")
    
    With *buildInFunctions(21)
      \id = 22
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    *buildInFunctions(22) = addFunction(@"PrintN")
    
    With *buildInFunctions(22)
      \id = 23
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    For I = 20 To 22
      If *buildInFunctions(I)
        LinkedList::addLast(*buildInClasses(5)\functions, *buildInFunctions(I), SizeOf(SFunction))
      EndIf
    Next I
    
    leaveScope()
    
    *buildInClasses(6) = addClass(@"string", 1)
    *buildInClasses(6)\functions = LinkedList::Init()
    *buildInClasses(6)\variables = LinkedList::Init()
    
    enterScope(@"string")
    *buildInVars(0) = addVariable(@"this", #NUMBER, #Ref_Class, SymbolTable::RefClass)
    refClass + 1
    *buildInVars(1) = addVariable(@"length", #NUMBER, #Ref_Class, SymbolTable::RefClass)
    refClass + 1
    *buildInVars(1) = addVariable(@"byteLength", #NUMBER, #Ref_Class, SymbolTable::RefClass)
    refClass + 1
    LinkedList::addLast(*buildInClasses(6)\variables, *buildInVars(1), SizeOf(SVariable))
    leaveScope()
    
    *buildInClasses(7) = addClass(@"image", 1)
    *buildInClasses(7)\functions = LinkedList::Init()
    enterScope(@"image")
    
    *buildInFunctions(25) = addFunction(@"load")
    
    With *buildInFunctions(25)
      \id = 26
      \ParamCount = 1
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith
    
    *buildInFunctions(26) = addFunction(@"free")
    
    With *buildInFunctions(26)
      \id = 27
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(6) = addFunction(@"drawpart")
    
    With *buildInFunctions(6)
      \id = 6
      \ParamCount = 6
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(27) = addFunction(@"draw")
    
    With *buildInFunctions(27)
      \id = 28
      \ParamCount = 3
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(29) = addFunction(@"rotate")
    
    With *buildInFunctions(29)
      \id = 30
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    For I = 25 To 27
      LinkedList::addLast(*buildInClasses(7)\functions, *buildInFunctions(I), SizeOf(SFunction))
    Next I
    
    LinkedList::addLast(*buildInClasses(7)\functions, *buildInFunctions(6), SizeOf(SFunction))
    LinkedList::addLast(*buildInClasses(7)\functions, *buildInFunctions(29), SizeOf(SFunction))
    leaveScope()
    
    *buildInClasses(8) = addClass(@"camera", 1)
    *buildInClasses(8)\functions = LinkedList::Init()
    enterScope(@"camera")
    
    *buildInFunctions(28) = addFunction(@"setscale")
    With *buildInFunctions(28)
      \id = 29
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    *buildInFunctions(39) = addFunction(@"setposition")
    With *buildInFunctions(39)
      \id = 39
      \ParamCount = 2
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    *buildInFunctions(40) = addFunction(@"getx")
    With *buildInFunctions(40)
      \id = 40
      \ParamCount = 0
      \Type = #TDOUBLE
    EndWith
    
    *buildInFunctions(41) = addFunction(@"gety")
    With *buildInFunctions(41)
      \id = 41
      \ParamCount = 0
      \Type = #TDOUBLE
    EndWith
    
    *buildInFunctions(55) = addFunction(@"getscale")
    With *buildInFunctions(55)
      \id = 55
      \ParamCount = 0
      \Type = #TINTEGER
    EndWith
    
    For I = 28 To 28
      LinkedList::addLast(*buildInClasses(8)\functions, *buildInFunctions(I), SizeOf(SFunction))
    Next I
    
    LinkedList::addLast(*buildInClasses(8)\functions, *buildInFunctions(55), SizeOf(SFunction))
    leaveScope()
    
    *buildInClasses(9) = addClass(@"keyboard", 1)
    *buildInClasses(9)\functions = LinkedList::Init()
    enterScope(@"keyboard")
    
    *buildInFunctions(1) = addFunction(@"wait")
    
    With *buildInFunctions(1)
      \id = 1
      \isStatic = 1
      \ParamCount = 0
      \Type = #STRING
      \ParamTypes = #Null
    EndWith
    
    *buildInFunctions(52) = addFunction(@"press")
    
    With *buildInFunctions(52)
      \id = 52
      \isStatic = 1
      \ParamCount = 1
      \Type = #TINTEGER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    LinkedList::addLast(*buildInClasses(9)\functions, *buildInFunctions(1), SizeOf(SFunction))
    
    *buildInFunctions(64) = addFunction(@"down")
    
    With *buildInFunctions(64)
      \id = 64
      \isStatic = 1
      \ParamCount = 1
      \Type = #TINTEGER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    LinkedList::addLast(*buildInClasses(9)\functions, *buildInFunctions(1), SizeOf(SFunction))
    
    leaveScope()
    
    *buildInClasses(10) = addClass(@"mouse", 1)
    *buildInClasses(10)\functions = LinkedList::Init()
    enterScope(@"mouse")
    
    *buildInFunctions(34) = addFunction(@"wheelup")
    
    With *buildInFunctions(34)
      \id = 34
      \isStatic = 1
      \ParamCount = 0
      \Type = #TINTEGER
      \ParamTypes = #Null
    EndWith
     
    *buildInFunctions(35) = addFunction(@"wheeldown")
    
    With *buildInFunctions(35)
      \id = 35
      \isStatic = 1
      \ParamCount = 0
      \Type = #TINTEGER
      \ParamTypes = #Null
    EndWith
    
    LinkedList::addLast(*buildInClasses(10)\functions, *buildInFunctions(34), SizeOf(SFunction))
    LinkedList::addLast(*buildInClasses(10)\functions, *buildInFunctions(35), SizeOf(SFunction))
    
    leaveScope()
    
    *buildInClasses(11) = addClass(@"math", 1)
    *buildInClasses(11)\functions = LinkedList::Init()
    enterScope(@"math")
    
    Define.d value = #PI
    *buildInConstants(26) = addConstant(@"pi", #TDOUBLE, @value)
    *buildInClasses(11)\constants = LinkedList::Init()
    LinkedList::addLast(*buildInClasses(11)\constants, *buildInConstants(26), SizeOf(SConstant))
    
    *buildInFunctions(36) = addFunction(@"sin")
    With *buildInFunctions(36)
      \id = 36
      \isStatic = 1
      \ParamCount = 1
      \Type = #TDOUBLE
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    LinkedList::addLast(*buildInClasses(11)\functions, *buildInFunctions(36), SizeOf(SFunction))
    
   *buildInFunctions(37) = addFunction(@"cos")
    With *buildInFunctions(37)
      \id = 37
      \isStatic = 1
      \ParamCount = 1
      \Type = #TDOUBLE
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    LinkedList::addLast(*buildInClasses(11)\functions, *buildInFunctions(37), SizeOf(SFunction))
    
    *buildInFunctions(58) = addFunction(@"sqr")
    With *buildInFunctions(58)
      \id = 58
      \isStatic = 1
      \ParamCount = 1
      \Type = #TDOUBLE
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    LinkedList::addLast(*buildInClasses(11)\functions, *buildInFunctions(58), SizeOf(SFunction))
    
    leaveScope()
    
    *buildInClasses(12) = addClass(@"light", 1)
    *buildInClasses(12)\functions = LinkedList::Init()
    enterScope(@"light")
    
   *buildInFunctions(42) = addFunction(@"add")
    With *buildInFunctions(42)
      \id = 42
      \isStatic = 1
      \ParamCount = 5
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    LinkedList::addLast(*buildInClasses(12)\functions, *buildInFunctions(42), SizeOf(SFunction))
    leaveScope()
    
    *buildInFunctions(59) = addFunction(@"random")
    
    With *buildInFunctions(59)
      \id = 59
      \isStatic = 1
      \ParamCount = 2
      \Type = #TDOUBLE
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
      LinkedList::addLastInt(\ParamTypes, #TDOUBLE)
    EndWith
    
    *buildInFunctions(43) = addFunction(@"getclock")
    
    With *buildInFunctions(43)
      \id = 43
      \ParamCount = 0
      \Type = #STRING
    EndWith
    
    *buildInFunctions(46) = addFunction(@"setclockcycle")
    
    With *buildInFunctions(46)
      \id = 46
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
       LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(45) = addFunction(@"int")
    
    With *buildInFunctions(45)
      \id = 45
      \ParamCount = 1
      \Type = #NUMBER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
    EndWith

    *buildInFunctions(44) = addFunction(@"split")
    
    With *buildInFunctions(44)
      \id = 44
      \isStatic = 1
      \ParamCount = 3
      \Type = #TSTRING
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInClasses(12) = addClass(@"file", 1)
   
    enterScope(@"file")
    
    *buildInFunctions(47) = addFunction(@"open")
    With *buildInFunctions(47)
      \id = 47
      \isStatic = 1
      \ParamCount = 1
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      \Type = #TINTEGER
    EndWith
    
    *buildInFunctions(48) = addFunction(@"close")
    With *buildInFunctions(48)
      \id = 48
      \isStatic = 1
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    *buildInFunctions(49) = addFunction(@"readline")
    With *buildInFunctions(49)
      \id = 49
      \isStatic = 1
      \ParamCount = 1
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
      \Type = #TSTRING
    EndWith
    LeaveScope()
    
    *buildInClasses(13) = addClass(@"sound", 1)
    *buildInClasses(13)\functions = LinkedList::Init()
    enterScope(@"sound")
    
    *buildInFunctions(50) = addFunction(@"load")
    
    With *buildInFunctions(50)
      \id = 50
      \isStatic = 1
      \ParamCount = 2
      \Type = #TINTEGER
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TSTRING)
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
     
    *buildInFunctions(51) = addFunction(@"free")
    
    With *buildInFunctions(51)
      \id = 51
      \isStatic = 1
      \ParamCount = 1
      \Type = #SUBR
      \ParamTypes =  LinkedList::init()
      LinkedList::addLastInt(\ParamTypes, #TINTEGER)
    EndWith
    
    LinkedList::addLast(*buildInClasses(13)\functions, *buildInFunctions(50), SizeOf(SFunction))
    LinkedList::addLast(*buildInClasses(13)\functions, *buildInFunctions(51), SizeOf(SFunction))
    
    leaveScope()
    
    *resolveConstants = LinkedList::Init()
  EndProcedure
  
  Procedure destroy()
    Define.i I
    FreeMap(*Symbols())
    LinkedList::destroy(*resolveConstants)
    *resolveConstants = #Null
    LinkedList::destroy(*Scopes)
    *Scopes = #Null
    LinkedList::destroy(*buildInClasses(0)\functions)
    *buildInClasses(0)\functions = #Null
    FreeArray(*buildInFunctions())
    FreeArray(*buildInClasses())
    FreeArray(*buildInConstants())
    FreeArray(*buildInVars())
  EndProcedure
    
  Procedure setScope(*string)
    Define.i length = 2 + MemoryStringLength(*string) + MemoryStringLength(*Scopes\head\value) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    FastString::add(*obj, *string)
    LinkedList::addFirst(*Scopes, *obj\data+4, 4+PeekL(*obj\data)*SizeOf(Character))
    FastString::Free(*obj)
  EndProcedure
  
  Procedure enterScope(*string)
    Define.i length = 2 + MemoryStringLength(*string) + MemoryStringLength(*Scopes\head\value) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    FastString::add(*obj, *Scopes\head\value)
    FastString::add(*obj, @":")
    FastString::add(*obj, *string)
    LinkedList::addFirst(*Scopes, *obj\data+4, 4+PeekL(*obj\data)*SizeOf(Character))
    FastString::Free(*obj)
    If FindString(PeekS(*Scopes\head\value), "::")
      leaveScope()
    EndIf
  EndProcedure
  
  Procedure getPreviousScope()
    ProcedureReturn *Scopes\head\next\value
  EndProcedure
  
  Procedure getCurrentScope()
    ProcedureReturn *Scopes\head\value
  EndProcedure
  
  Procedure.s getCurrentClass()
    ProcedureReturn StringField(PeekS(*Scopes\head\value),2,":")
  EndProcedure
  
  Procedure leaveScope()
    LinkedList::delete(*Scopes, *Scopes\head\key)
  EndProcedure
  
  Procedure resetScope()
    *Scopes\head\value = *Scopes\head\next\value 
  EndProcedure
  
  Procedure LookupSymbol(*string)
    If *string < 1000
      ProcedureReturn
    EndIf
    
    Define.FastString::STRFASTOBJ *obj 
    Define.LinkedList::Node *current = *Scopes\head
    Define *ptr
    
    While *current
      Define.i length = 2 + MemoryStringLength(*current\value) + MemoryStringLength(*string)
      *obj = FastString::new(length)
      FastString::add(*obj, *current\value)
      FastString::add(*obj, @":")
      FastString::add(*obj, *string)
      *ptr = *Symbols(PeekS(*obj\data+4))
      FastString::Free(*obj)
      
      If *ptr
        *module = *current\value
        ProcedureReturn *ptr
      EndIf
      
      *current = *current\next
    Wend
    
    ProcedureReturn #Null
  EndProcedure
  
  Procedure LookupSymbolScoped(*name)
    Define.SymbolTable::SSymbol *S
    
    ; 1) nykyinen scope (Constructor / Function)
    *S = LookupSymbolCurrentScope(*name)
    If *S : ProcedureReturn *S : EndIf
    
    If GetModule()
      Define tmp.s = PeekS(*function)+":"+PeekS(*name)
      Debug "LookupSymbolScoped " + tmp
      
      *S = LookupSymbol(@tmp)
      If *S : ProcedureReturn *S : EndIf
    EndIf
  
    ; 4) globaali (viimeinen keino)
    *S = LookupSymbol(*name)
    ProcedureReturn *S
  EndProcedure

  Procedure LookupSymbolCurrentScope(*string)
    Define.i length = 2 + MemoryStringLength(*string) + MemoryStringLength(*Scopes\head\value) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    FastString::add(*obj, *Scopes\head\value)
    FastString::add(*obj, @":")
    FastString::add(*obj, *string)
    Define *ptr = *Symbols(PeekS(*obj\data+4))
    FastString::Free(*obj)
    ProcedureReturn *ptr
  EndProcedure
  
  Procedure LookupSymbolNoScopeResolve(*string)
    Define.i length = 2 + MemoryStringLength(*string)
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    FastString::add(*obj, @":")
    FastString::add(*obj, *string)
    Define *ptr = *Symbols(PeekS(*obj\data+4))
    FastString::Free(*obj)
    ProcedureReturn *ptr
  EndProcedure
  
  Procedure removeSymbol(*Sym.SSymbol)
    Define.i length = 2 + MemoryStringLength(*Sym\name) + MemoryStringLength(*Scopes\head\value) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    
    FastString::add(*obj, *Scopes\head\value)
    FastString::add(*obj, @":")
    FastString::add(*obj, *Sym\name)
    
    DeleteMapElement(*Symbols(), PeekS(*obj\data+4))
    
    FastString::Free(*obj)
  EndProcedure
  
  Procedure removeSymbolNoScope(*name)
    Define.i length = 2 + MemoryStringLength(*name) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    
    FastString::add(*obj, @":")
    FastString::add(*obj, *name)
    
    DeleteMapElement(*Symbols(), PeekS(*obj\data+4))
    
    FastString::Free(*obj)
  EndProcedure
  
  Procedure addSymbol(*Sym.SSymbol, isGlobal.i = 0)
    Define.s key = PeekS(*Sym\name)
    
    Define.i length = 2 + MemoryStringLength(*Scopes\head\value) + MemoryStringLength(*Sym\name)
    Define.FastString::STRFASTOBJ *obj = FastString::new(length)
    
    If Not isGlobal
      FastString::add(*obj, *Scopes\head\value)
      FastString::add(*obj, @":")
      FastString::add(*obj, @key)
      *Symbols(PeekS(*obj\data+4)) = *Sym
    Else
      FastString::add(*obj, @":")
      FastString::add(*obj, @key)
      *Symbols(PeekS(*obj\data+4)) = *Sym
    EndIf
    
    FastString::Free(*obj)
  EndProcedure
  
  Procedure addSymbolNoScope(*Sym.SSymbol, *name) 
    Define.FastString::STRFASTOBJ *obj = FastString::new(2 + MemoryStringLength(*name))
    FastString::add(*obj, @":")
    FastString::add(*obj, *name)
    *Symbols(PeekS(*obj\data+4)) = *Sym
    FastString::Free(*obj)
  EndProcedure
  
  Procedure addModule(*string)
    Protected *M.SModule = LookupSymbolNoScopeResolve(*string)
    
    If *M
      PrintN("Error: Module declared twice "+PeekS(*string))
      ProcedureReturn #Null
    EndIf
    
    *M = GlobalMemoryCache::Alloc(SizeOf(SModule))
    *M\name = *string
    *M\kind = #SKind_Module

    addSymbol(*M, 1)
    ProcedureReturn *M
  EndProcedure
  
  Procedure addClass(*string, buildIn = 0)
    refClass = 1
    Protected *C.SClass = LookupSymbolCurrentScope(*string)

    If *C
      PrintN("Error: Class declared twice "+PeekS(*string))
      ProcedureReturn #Null
    EndIf
    
    *C = GlobalMemoryCache::Alloc(SizeOf(SClass))
    *C\name = *string
    *C\kind = #SKind_Class 
    *C\id = classCount
    *C\buildIn = buildIn
  
    Define *prevScope = getPreviousScope()
    If *prevScope
        *C\module = *prevScope
    EndIf
    
    addSymbol(*C, 1)
    ProcedureReturn *C
  EndProcedure
  
  Procedure GetModule()
    ProcedureReturn *module
  EndProcedure
  
  Procedure SetModule(*string)
    *module = *string
  EndProcedure
  
  Procedure SetFunction(*string)
    *function = *string
  EndProcedure
  
  Procedure buildSignature(*string, List temp()) 
    Define.Token *Token 
    Define.i length = 2 + MemoryStringLength(*string) * 2
    Define.FastString::STRFASTOBJ *obj = FastString::new(length) 
    FastString::add(*obj, *String)
    
    ForEach temp() 
      *token = temp() 
      If *token And *token\type = #AS
        NextElement(temp())
        *token = temp()
        Define.s type = Str(*token\type)
        
        If *token\type = #IDENFIER
          FastString::add(*obj, *token\string) 
        Else
          FastString::add(*obj, @type) 
        EndIf
      EndIf 
    Next 
    ClearList(temp()) 
    
    ProcedureReturn *obj\data+4
  EndProcedure
    
  Procedure addFunction(*string, *next.LinkedList::Node = 0)
    Protected *F.SFunction = LookupSymbolCurrentScope(*string)

    If *F
;       NewList temp()
;       Define.Token *Token
;       Define.i paramCount = 0
;       
;       While *next
;         *token = *next\value
;         AddElement(temp())
;         temp() = *token
;         If *token\type = #RPAREN
;           Break
;         ElseIf *token\type = #comma
;           paramCount + 1
;         EndIf
;         *Next = *next\next
;       Wend
; 
;       If *F\ParamCount <> paramCount
;         Protected *FN.SFunction  = GlobalMemoryCache::Alloc(SizeOf(SFunction))
;         *FN\id = -1
;         *FN\name = buildSignature(*string, temp())
;         *FN\class = getCurrentScope()+2
;         *FN\kind = #SKind_Function
;         *FN\ParamCount = 0
;         *FN\isStatic = 0
;         addSymbol(*FN)
;         ProcedureReturn *FN
;       EndIf
    EndIf

    *F = GlobalMemoryCache::Alloc(SizeOf(SFunction))
    *F\id = -1
    *F\name = *string
    
    Define *prevScope = getPreviousScope()
    If *prevScope
      If PeekS(*prevScope) <> ""
        *F\module = *prevScope
      EndIf
    EndIf
    
    *F\class = getCurrentScope()+2
    *F\kind = #SKind_Function
    *F\ParamCount = 0
    *F\isStatic = 0
    addSymbol(*F)
    ProcedureReturn *F
  EndProcedure
    
  Procedure addVariable(*string, Type.i, RefKind.i, RefOffset.i, visible.i = 0)
    Protected *V.SVariable = LookupSymbolCurrentScope(*string)
    
    If *V
      PrintN("Error: Variable declared twice "+PeekS(*V\name))
      ProcedureReturn #Null
    EndIf
    
    *V = GlobalMemoryCache::Alloc(SizeOf(SVariable))
    *V\name = *string
    *V\kind = #SKind_Variable
    *V\Type = Type
    *V\RefKind = RefKind
    *V\RefOffset = RefOffset
    *V\visible = visible
    
    addSymbol(*V)
    ProcedureReturn *V
  EndProcedure
  
  Procedure AddVariableClass(*string, Type.i, *sym, RefKind.i, RefOffset.i)
    Protected *V.SVariable = LookupSymbolCurrentScope(*string)
    
    If *V
      PrintN("Error: Variable declared twice "+PeekS(*V\name))
      ProcedureReturn #Null
    EndIf
    
    *V = GlobalMemoryCache::Alloc(SizeOf(SVariable))
    *V\name = *string
    *V\kind = #SKind_Variable
    *V\Type = Type
    *V\RefKind = RefKind
    *V\Sym = *Sym
    *V\RefOffset = RefOffset
    
    addSymbol(*V)
    ProcedureReturn *V
  EndProcedure
  
  Procedure addConstant(*string, Type.i, *value)
    Protected *C.SConstant = LookupSymbolCurrentScope(*string)
    
    If *C
      removeSymbol(*C)
    EndIf
    
    Define.i length
    
; --- safer allocation & initialization for SConstant ---
*C = GlobalMemoryCache::Alloc(SizeOf(SConstant))
If *C = #Null
  Debug "addConstant: FAILED to allocate SConstant"
  ProcedureReturn #Null
EndIf

; zero the whole structure to avoid reading uninitialized pointers later
FillMemory(*C, SizeOf(SConstant))

; basic initialisation
*C\kind = #SKind_Const
*C\Type = Type
*C\scope = getCurrentScope() + 2

; copy name (always allocate & null-terminate)
If *string
  Define.i nameChars = MemoryStringLength(*string)         ; characters
  Define.i nameBytes = nameChars * SizeOf(Character)       ; bytes (use SizeOf(Character) for portability)
  *C\name = GlobalMemoryCache::Alloc(nameBytes + SizeOf(Character)) ; space for terminator
  If *C\name = #Null
    Debug "addConstant: FAILED to allocate name"
    GlobalMemoryCache::Free(*C)
    ProcedureReturn #Null
  EndIf
  CopyMemory(*string, *C\name, nameBytes)
  ; null-terminate (set last character)
  PokeB(*C\name + nameBytes, 0)
  ; For 2-byte char encodings, also set next byte to 0 (above writes at byte-level)
  If SizeOf(Character) = 2
    PokeB(*C\name + nameBytes + 1, 0)
  EndIf
Else
  *C\name = #Null
EndIf

; handle value-by-type safely (example: integer/double/string)
Select Type
  Case #TINTEGER
    *C\integer = *value
  Case #TDOUBLE
    *C\double = PeekD(*value)
    ; DO NOT free *value here unless you KNOW ownership
  Case #TSTRING, #IDENFIER
    If *value
      Define.i vChars = MemoryStringLength(*value)
      Define.i vBytes = vChars * SizeOf(Character)
      *C\string = GlobalMemoryCache::Alloc(vBytes + SizeOf(Character))
      If *C\string = #Null
        Debug "addConstant: FAILED to allocate string value"
        ; free name and *C to avoid leaks
        If *C\name
          GlobalMemoryCache::Free(*C\name)
        EndIf
        GlobalMemoryCache::Free(*C)
        ProcedureReturn #Null
      EndIf
      CopyMemory(*value, *C\string, vBytes)
      PokeB(*C\string + vBytes, 0)
      If SizeOf(Character) = 2
        PokeB(*C\string + vBytes + 1, 0)
      EndIf
    Else
      *C\string = #Null
    EndIf
  Case #LINKEDLIST
    *C\integer = *value
EndSelect


; finally call addSymbol
addSymbol(*C)
ProcedureReturn *C

  EndProcedure
  
  Procedure ReAddConstant(*scope, *string, Type.i, *value)
    Define.FastString::STRFASTOBJ *obj = FastString::new(2 + MemoryStringLength(*scope) + MemoryStringLength(*string))
    FastString::add(*obj, *scope)
    FastString::add(*obj, @":")
    FastString::add(*obj, *string)
    
    Protected *C.SConstant = LookupSymbol(*obj\data+4)
    
    If *C
      removeSymbolNoScope(*obj\data+4)
    EndIf
    
    Define.i length 
    
    *C = GlobalMemoryCache::Alloc(SizeOf(SConstant))
    *C\name = *string
    *C\kind = #SKind_Const
    *C\Type = Type
    *C\scope = *scope
    
    Select type 
      Case #TINTEGER
        *C\integer = *value
      Case #TDOUBLE
        *C\double = PeekD(*value)
        GlobalMemoryCache::Free(*value)
      Case #TSTRING, #IDENFIER
        If *value
          length = MemoryStringLength(*value) * 2
          *C\String = GlobalMemoryCache::Alloc(length+2)
          CopyMemory(*value, *C\String, length)
        EndIf
      Case #LINKEDLIST
        *C\integer = *value
    EndSelect
    
    If Type = #LINKEDLIST
      LinkedList::addLast(*resolveConstants, *C, SizeOf(SConstant))
    EndIf
    
    addSymbolNoScope(*C, *obj\data+4)
    FastString::Free(*obj)
    ProcedureReturn *C
  EndProcedure
  
EndModule
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 9
; Folding = ------
; EnableThread
; EnableXP
; DPIAware