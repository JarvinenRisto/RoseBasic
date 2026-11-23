#HASH_SIZE = 4096

Structure CacheEntry
  *instance.ObjInstance
  *string
  value.q
EndStructure

Structure MethodCacheEntry
    *ptr 
    method.q
EndStructure
  
Global Dim methodCache.MethodCacheEntry(#HASH_SIZE - 1) 
Global Dim cache.CacheEntry(#HASH_SIZE - 1)

Declare.q GetMethod(*string)
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 16
; EnableXP
; DPIAware