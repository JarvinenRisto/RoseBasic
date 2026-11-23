XIncludeFile "GlobalMemoryCache.pb"

DeclareModule RBArray
  
  Structure CustomArray
    dimensions.i[5]
    dimensionSizes.i[5]
    *data
    size.i
  EndStructure
  
  Declare init(Array dims(1), dimCount.i)

EndDeclareModule

Module RBArray
  
  Procedure init(Array dims(1), dimCount.i)
    Define.CustomArray custom
    Define s = 1, i
    
    For i = dimCount - 1 To 0 Step -1
      custom\dimensions[i] = s
      s = s * dims(i)
      custom\dimensionSizes[i] = dims(i)
    Next
    
    custom\size = s
    custom\data = GlobalMemoryCache::Alloc(size)
    ProcedureReturn custom
  EndProcedure
  
EndModule

; IDE Options = PureBasic 6.20 (Linux - x64)
; CursorPosition = 27
; Folding = -
; EnableXP
; DPIAware