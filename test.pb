EnableExplicit

XIncludeFile "GlobalMemoryCache.pb"    ; your module
XIncludeFile "SymbolTable.pb"    ; contains addConstant/addSymbol/etc.

; --- Fake stubs for missing pieces (replace with your real ones) ---
Global Dim *Symbols(10000)
Structure ScopeNode
  *value
  *next.ScopeNode
EndStructure

; --- Test helpers ---
Procedure.s MemStr(ptr)
  If ptr
    ProcedureReturn PeekS(ptr)
  Else
    ProcedureReturn "<NULL>"
  EndIf
EndProcedure

; --- 1. Init cache ---
GlobalMemoryCache::InitCache()
Debug "Cache initialized. TotalSize=" + Str(GlobalMemoryCache::totalSize)

; --- 2. Add a single constant ---
Define.s name = "PI"
Define.d val = 3.14159
Define *valPtr = AllocateMemory(SizeOf(Double))
PokeD(*valPtr, val)
Define *namePtr = AllocateMemory(StringByteLength(name, #PB_Unicode) + 2)
PokeS(*namePtr, name)

Define.SymbolTable::SConstant *C = SymbolTable::addConstant(*namePtr, #TDOUBLE, *valPtr)
If *C
  Debug "Added constant OK: name=" + MemStr(*C\name) + " value=" + StrD(*C\double)
Else
  Debug "addConstant FAILED!"
EndIf

; --- 3. Stress test many allocations ---
Define i
For i = 1 To 10000
  Define.s n = "CONST_" + Str(i)
  Define *np = AllocateMemory(StringByteLength(n, #PB_Unicode) + 2)
  PokeS(*np, n)
  Define valI = i
  Define *vp = AllocateMemory(SizeOf(Integer))
  PokeI(*vp, valI)
  Define.SymbolTable::SConstant *const = SymbolTable::addConstant(*np, #TINTEGER, *vp)
  If *const = #Null
    Debug "addConstant failed at i=" + Str(i)
    Break
  EndIf
Next

Debug "After stress: totalSize=" + Str(GlobalMemoryCache::totalSize)

; --- 4. Verify a few random constants exist ---
Debug "Check CONST_1 name=" + MemStr(SymbolTable::*Symbols("GLOBAL:CONST_1"))
Debug "Check CONST_5000 name=" + MemStr(SymbolTable::*Symbols("GLOBAL:CONST_5000"))
Debug "Check CONST_10000 name=" + MemStr(SymbolTable::*Symbols("GLOBAL:CONST_10000"))

; --- 5. Cleanup ---
GlobalMemoryCache::FreeCache()
Debug "Cache freed. totalSize=" + Str(GlobalMemoryCache::totalSize)
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 2
; Folding = -
; EnableXP
; DPIAware