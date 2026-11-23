#TABLE_MAX_LOAD = 0.75

Structure Entry
  *key.ObjString
  value.q
  probeDistance.i
EndStructure

Structure Table
  count.i
  capacity.i
  *entries.Entry
EndStructure


; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 14
; EnableXP
; DPIAware