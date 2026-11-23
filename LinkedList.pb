

DeclareModule LinkedList
  EnableExplicit
  
  Structure CustomList
    *head.Node
    *current.Node
    key.i
  EndStructure

  Structure Node
    *next.Node
    *prev.Node
    *value
    key.i
    ownsValue.a   ; 1 = FreeMemory(value) on delete/destroy
  EndStructure
  

  Declare init()
  Declare isEmpty(*list.CustomList)
  Declare setKey(*list.CustomList, key.i)
  Declare destroy(*list.CustomList)

  Declare addFirst(*list.CustomList, *value, size.i)
  Declare addLast(*list.CustomList, *value, size.i)

  ; Stores external pointer, DOES NOT copy, DOES NOT free
  Declare addFirstPtr(*list.CustomList, *ptr)
  Declare addFirstInt(*list.CustomList, *ptr)
  Declare addLastInt(*list.CustomList, value.i)

  Declare insert(*list.CustomList, key.i, *value, size.i)
  Declare insertAfter(*list.CustomList, key.i, *value, size.i)

  Declare deleteRange(*list.CustomList, min.i, max.i)
  Declare delete(*list.CustomList, key.i)

  Declare lookup(*list.CustomList, key.i)
  Declare selectKey(*list.CustomList, key.i)
  Declare selectKeyValue(*list.CustomList, key.i)

  Declare count(*list.CustomList)
  Declare printList(*list.CustomList)
EndDeclareModule

Module LinkedList

  Procedure NewNode()
    Define.Node *n = AllocateMemory(SizeOf(Node))
    FillMemory(*n, SizeOf(Node), 0)
    ProcedureReturn *n
  EndProcedure

  Procedure init()
    Define.CustomList *list = AllocateMemory(SizeOf(CustomList))
    FillMemory(*list, SizeOf(CustomList), 0)

    *list\head    = NewNode()
    *list\current = *list\head
    ProcedureReturn *list
  EndProcedure

  Procedure isEmpty(*list.CustomList)
    If *list = #Null Or *list\head = #Null Or *list\head\next = #Null
      ProcedureReturn 1
    EndIf
    ProcedureReturn 0
  EndProcedure

  Procedure setKey(*list.CustomList, key.i)
    *list\key = key
    If *list\current
      *list\current\key = key
    EndIf
  EndProcedure

  Procedure destroy(*list.CustomList)
    If *list = #Null : ProcedureReturn : EndIf
  
    Define.Node *current = *list\head
    Define.Node *next
  
    Static NewMap freedValue.i()
    Static NewMap freedNode.i()
    ClearMap(freedValue())
    ClearMap(freedNode())
  
    While *current
      *next = *current\next
  
      ; node-ketjun korruptio / sama node kahdesti
      If FindMapElement(freedNode(), Str(*current))
        Debug "ERROR: node visited twice: node=" + Hex(*current)
        ProcedureReturn
      EndIf
      AddMapElement(freedNode(), Str(*current))
  
      If *current\ownsValue And *current\value
        If FindMapElement(freedValue(), Str(*current\value))
          Debug "ERROR: value freed twice: value=" + Hex(*current\value) + " node=" + Hex(*current)
        Else
          AddMapElement(freedValue(), Str(*current\value))
          FreeMemory(*current\value)
        EndIf
      EndIf
  
      FreeMemory(*current)
      *current = *next
    Wend
  
    FreeMemory(*list)
  EndProcedure

  ; Copies data, node OWNS the allocated buffer
  Procedure addFirst(*list.CustomList, *value, size.i)
    Define.Node *first = NewNode()
  
    *first\value = AllocateMemory(size)
    CopyMemory(*value, *first\value, size)
    *first\ownsValue = 1
  
    *first\prev = #Null
    *first\next = *list\head
    If *list\head
      *list\head\prev = *first
    EndIf
    *list\head = *first
  
    *list\key + 1
    *first\key = *list\key
  EndProcedure

  ; Stores external pointer, DOES NOT free it
  Procedure addFirstPtr(*list.CustomList, *ptr)
    Define.Node *first = NewNode()
  
    *first\value = *ptr
    *first\ownsValue = 0   ; EI KOSKAAN vapauteta
  
    *first\prev = #Null
    *first\next = *list\head
    If *list\head
      *list\head\prev = *first
    EndIf
    *list\head = *first
  
    *list\key + 1
    *first\key = *list\key
  EndProcedure

  ; Copies data, node OWNS the allocated buffer
  Procedure addLast(*list.CustomList, *value, size.i)
    *list\key + 1
    *list\current\key = *list\key

    *list\current\value = AllocateMemory(size)
    CopyMemory(*value, *list\current\value, size)
    *list\current\ownsValue = 1

    *list\current\next = NewNode()
    *list\current\next\prev = *list\current
    *list\current = *list\current\next
  EndProcedure

  ; Stores integer directly in pointer-sized field, DOES NOT free
  Procedure addLastInt(*list.CustomList, value.i)
    *list\key + 1
    *list\current\key = *list\key

    *list\current\value = value
    *list\current\ownsValue = 0

    *list\current\next = NewNode()
    *list\current\next\prev = *list\current
    *list\current = *list\current\next
  EndProcedure

  ; Replaces node's value with a fresh owned copy
  Procedure insert(*list.CustomList, key.i, *value, size.i)
    Define.Node *current = *list\head

    While *current
      If *current\key = key
        If *current\ownsValue And *current\value
          FreeMemory(*current\value)
        EndIf

        *current\value = AllocateMemory(size)
        CopyMemory(*value, *current\value, size)   ; <-- fixed
        *current\ownsValue = 1
        ProcedureReturn
      EndIf
      *current = *current\next
    Wend
  EndProcedure

  Procedure insertAfter(*list.CustomList, key.i, *value, size.i)
    Define.Node *current = *list\head

    While *current And *current\key <> key
      *current = *current\next
    Wend
    If *current = #Null : ProcedureReturn : EndIf

    Define.Node *newNode = NewNode()
    *newNode\value = AllocateMemory(size)
    CopyMemory(*value, *newNode\value, size)
    *newNode\ownsValue = 1

    *newNode\next = *current\next
    *newNode\prev = *current
    If *current\next
      *current\next\prev = *newNode
    EndIf
    *current\next = *newNode
  EndProcedure
  
 Procedure addFirstInt(*list.CustomList, value.i)
  addFirstPtr(*list, value)
EndProcedure

  Procedure delete(*list.CustomList, key.i)
    Define.Node *current = *list\head
    Define.Node *previous = #Null

    While *current
      If *current\key = key

        If *previous
          *previous\next = *current\next
        Else
          *list\head = *current\next
        EndIf

        If *current\next
          *current\next\prev = *previous
        EndIf

        If *current\ownsValue And *current\value
          FreeMemory(*current\value)
        EndIf

        FreeMemory(*current)
        ProcedureReturn 1
      EndIf

      *previous = *current
      *current  = *current\next
    Wend

    ProcedureReturn 0
  EndProcedure

  Procedure deleteRange(*list.CustomList, min.i, max.i)
    Define.i count = max - min + 1
    While count > 0
      delete(*list, min)
      count - 1
    Wend
  EndProcedure

  Procedure lookup(*list.CustomList, key.i)
    Define.Node *current = *list\head
    While *current
      If *current\key = key
        ProcedureReturn *current
      EndIf
      *current = *current\next
    Wend
    ProcedureReturn #Null
  EndProcedure

  Procedure selectKey(*list.CustomList, key.i)
    ProcedureReturn lookup(*list, key)
  EndProcedure

  Procedure selectKeyValue(*list.CustomList, key.i)
    Define.Node *n = lookup(*list, key)
    If *n : ProcedureReturn *n\value : EndIf
    ProcedureReturn #Null
  EndProcedure

  Procedure count(*list.CustomList)
    Define.Node *current = *list\head
    Define.i amount = 0
    While *current
      amount + 1
      *current = *current\next
    Wend
    ProcedureReturn amount
  EndProcedure
  
  Structure VToken
      type.a
      line.i
      *fileName
      StructureUnion
        integer.i
        double.d
        *string
      EndStructureUnion
  EndStructure

  Procedure printList(*list.CustomList)
    Define.Node *current = *list\head
    While *current
      Define.VToken *token = *current\value
      If *token 
        Debug *token\type
      EndIf
      *current = *current\next
    Wend
  EndProcedure

EndModule
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 167
; FirstLine = 147
; Folding = ----
; EnableXP
; DPIAware
; DisableDebugger