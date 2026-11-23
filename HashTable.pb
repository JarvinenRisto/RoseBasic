DeclareModule HashTable
  EnableExplicit
  
  #INITIAL_SIZE = 85
  
  Structure item
    *key
    StructureUnion
    *value
    valueQ.q
    EndStructureUnion
  EndStructure
  
  Structure hash_table
    base_size.i
    size.i
    count.i
    *items.item  ; pointer to allocated array
  EndStructure
  
  Declare new(size.i)
  Declare destroy(*ht.hash_table)
  Declare resize(*ht.hash_table, baseSize.i)
  Declare insert(*ht.hash_table, *key, *value)
  Declare insertQ(*ht.hash_table, *key, value.q)
  Declare search(*ht.hash_table, *key)
  Declare.q searchQ(*ht.hash_table, *key)
  Declare delete(*ht.hash_table, *key)
  Declare setAll(*ht.hash_table, *value)
EndDeclareModule

; =============================
; Implementation
; =============================
Module HashTable
  EnableExplicit
  
  #PRIME1 = 151
  #PRIME2 = 163
  
  Global.item *DELETED_ITEM = 0
  
  Macro resizeUp(ht)
    Define.i newSize = ht\base_size * 2
    resize(ht, newSize)
  EndMacro
  
  Macro resizeDown(ht)
    Define.i newSize = ht\base_size / 2
    resize(ht, newSize)
  EndMacro
  
  Procedure isPrime(x.i)
    If x < 2 : ProcedureReturn 0 : EndIf
    If x = 2 Or x = 3 : ProcedureReturn 1 : EndIf
    If (x % 2) = 0 : ProcedureReturn 0 : EndIf
    Define.i i, limit = Round(Sqr(x), #PB_Round_Down)
    For i = 3 To limit Step 2
      If (x % i) = 0 : ProcedureReturn 0 : EndIf
    Next
    ProcedureReturn 1
  EndProcedure
  
  Procedure nextPrime(x.i)
    While isPrime(x) = 0
      x + 1
    Wend
    ProcedureReturn x
  EndProcedure
  
  Procedure floor_mod(mod_num.i, check_num.i)
    If check_num % mod_num = 0
      ProcedureReturn check_num - 1
    EndIf
    ProcedureReturn check_num
  EndProcedure
  
  Procedure newItem(*key, *value)
    Define.item *it = AllocateStructure(item)
    Define.i len = MemoryStringLength(*key, #PB_ByteLength)
    *it\key = AllocateMemory(len + 3)
    CopyMemory(*key, *it\key, len)
    *it\value = *value
    ProcedureReturn *it
  EndProcedure
  
  Procedure newItemQ(*key, value.q)
    Define.item *it = AllocateStructure(item)
    Define.i len = MemoryStringLength(*key, #PB_ByteLength)
    *it\key = AllocateMemory(len + 3)
    CopyMemory(*key, *it\key, len)
    *it\valueQ = value
    ProcedureReturn *it
  EndProcedure
  
  Procedure deleteItem(*it.item)
    If *it
      If *it\key : FreeMemory(*it\key) : EndIf
      FreeStructure(*it)
    EndIf
  EndProcedure
  
  Procedure hash(*string, a.i, m.i)
    Protected.l hashValue = 0
    Protected.i i, len = MemoryStringLength(*string, #PB_ByteLength)
    For i = 0 To len Step 2
      hashValue + Pow(a, len - (i+1)) * PeekU(*string + i)
      hashValue % m
    Next
    ProcedureReturn hashValue
  EndProcedure
  
Procedure step_from_hash(num_buckets.i, hash_b.i)
  ProcedureReturn (hash_b % (num_buckets - 1)) + 1
EndProcedure

Procedure getHash(*string, num_buckets.i, attempt.i)
  Define.i hash_a = hash(*string, #PRIME1, num_buckets)
  Define.i hash_b = hash(*string, #PRIME2, num_buckets)
  Define.i Stepa = (hash_b % (num_buckets - 1)) + 1
  ProcedureReturn (hash_a + attempt * Stepa) % num_buckets
EndProcedure
  
  Procedure insert(*ht.hash_table, *key, *value)
    Define.i load = *ht\count * 100 / *ht\size
    If load > 70 : resizeUp(*ht) : EndIf
    
    Define.item *it = newItem(*key, *value)
    Define.i index = getHash(*it\key, *ht\size, 0)
    Define.item *current = PeekI(*ht\items + SizeOf(Integer) * index)
    Define.i i = 1
    
    While *current <> #Null And *current <> *DELETED_ITEM
      index = getHash(*it\key, *ht\size, i)
      *current = PeekI(*ht\items + SizeOf(Integer) * index)
      i + 1
    Wend
    
    PokeI(*ht\items + SizeOf(Integer) * index, *it)
    *ht\count + 1
  EndProcedure
  
  Procedure insertQ(*ht.hash_table, *key, value.q)
    Define.i load = *ht\count * 100 / *ht\size
    If load > 70 : resizeUp(*ht) : EndIf
    
    Define.item *it = newItemQ(*key, value)
    Define.i index = getHash(*it\key, *ht\size, 0)
    Define.item *current = PeekI(*ht\items + SizeOf(Integer) * index)
    Define.i i = 1
    
    While *current <> #Null And *current <> *DELETED_ITEM
      index = getHash(*it\key, *ht\size, i)
      *current = PeekI(*ht\items + SizeOf(Integer) * index)
      Debug i
      i + 1
    Wend
    
    PokeI(*ht\items + SizeOf(Integer) * index, *it)
    *ht\count + 1
  EndProcedure
  
  Procedure search(*ht.hash_table, *key)
    Define.i index = getHash(*key, *ht\size, 0)
    Define.item *current = PeekI(*ht\items + SizeOf(Integer) * index)
    Define.i i = 1
    
    While *current <> #Null
      If *current <> *DELETED_ITEM
        If CompareMemoryString(*current\key, *key, #PB_String_NoCase) = #PB_String_Equal
          ProcedureReturn *current\value
        EndIf
      EndIf
      index = getHash(*key, *ht\size, i)
      *current = PeekI(*ht\items + SizeOf(Integer) * index)
      i + 1
    Wend
    ProcedureReturn #Null
  EndProcedure
  
  Procedure.q searchQ(*ht.hash_table, *key)
    Define.i index = getHash(*key, *ht\size, 0)
    Define.item *current = PeekI(*ht\items + SizeOf(Integer) * index)
    Define.i i = 1
    
    While *current <> #Null
      If *current <> *DELETED_ITEM
        If CompareMemoryString(*current\key, *key, #PB_String_NoCase) = #PB_String_Equal
          ProcedureReturn *current\valueQ
        EndIf
      EndIf
      index = getHash(*key, *ht\size, i)
      *current = PeekI(*ht\items + SizeOf(Integer) * index)
      i + 1
    Wend
    ProcedureReturn #Null
  EndProcedure
  
  Procedure setAll(*ht.hash_table, *value)
    Protected.i i
    Protected.item *it
    For i = 0 To *ht\size - 1
      *it = PeekI(*ht\items + SizeOf(Integer) * i)
      If *it <> #Null And *it <> *DELETED_ITEM
        *it\value = *value
      EndIf
    Next
  EndProcedure
  
  Procedure delete(*ht.hash_table, *key)
    Define.i load = *ht\count * 100 / *ht\size
    If load < 10 : resizeDown(*ht) : EndIf
    
    Define.i index = getHash(*key, *ht\size, 0)
    Define.item *current = PeekI(*ht\items + SizeOf(Integer) * index)
    Define.i i = 1
    
    While *current <> #Null
      If *current <> *DELETED_ITEM
        If CompareMemoryString(*current\key, *key, #PB_String_NoCase) = #PB_String_Equal
          deleteItem(*current)
          PokeI(*ht\items + SizeOf(Integer) * index, *DELETED_ITEM)
          *ht\count - 1
          ProcedureReturn
        EndIf
      EndIf
      index = getHash(*key, *ht\size, i)
      *current = PeekI(*ht\items + SizeOf(Integer) * index)
      i + 1
    Wend
  EndProcedure
  
  Procedure new(size.i)
    Define.hash_table *ht = AllocateStructure(hash_table)
    *ht\count = 0
    *ht\base_size = size
    *ht\size = nextPrime(*ht\base_size)
    *ht\items = AllocateMemory(*ht\size * SizeOf(Integer))
    If *DELETED_ITEM = 0
      *DELETED_ITEM = AllocateStructure(hash_table)
    EndIf
    ProcedureReturn *ht
  EndProcedure
  
  Procedure destroy(*ht.hash_table)
    If *ht = 0 : ProcedureReturn : EndIf
    Protected.i i
    Protected.item *it
    For i = 0 To *ht\size - 1
      *it = PeekI(*ht\items + SizeOf(Integer) * i)
      If *it <> #Null And *it <> *DELETED_ITEM
        deleteItem(*it)
      EndIf
    Next
    FreeMemory(*ht\items)
    FreeStructure(*ht)
  EndProcedure
  
  Procedure resize(*ht.hash_table, baseSize.i)
    If baseSize < #INITIAL_SIZE
      ProcedureReturn
    EndIf
    
    Define.hash_table *new_ht = new(baseSize)
    Define.i i
    Protected.item *it
    
    For i = 0 To *ht\size - 1
      *it = PeekI(*ht\items + SizeOf(Integer) * i)
      If *it <> #Null And *it <> *DELETED_ITEM
        insert(*new_ht, *it\key, *it\value)
      EndIf
    Next
    
    ; Swap contents
    FreeMemory(*ht\items)
    *ht\items = *new_ht\items
    *ht\size = *new_ht\size
    *ht\count = *new_ht\count
    *ht\base_size = *new_ht\base_size
    
    *new_ht\items = 0
    destroy(*new_ht)
  EndProcedure
EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 120
; FirstLine = 96
; Folding = ----
; EnableXP
; DPIAware