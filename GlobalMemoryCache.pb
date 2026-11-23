; GlobalMemoryCache.pb - corrected version

DeclareModule GlobalMemoryCache
  EnableExplicit

  Global.i isInitialized = 0

  Declare InitCache()
  Declare FreeCache()
  Declare Alloc(size.i)
  Declare ReAlloc(*ptrV, size.i)
  Declare Size(*ptrV)
  Declare Free(*ptrV)
  Declare Copy(*ptrV, *ptr2)

  Global.i totalSize
EndDeclareModule

Module GlobalMemoryCache

  Structure blockMeta
    size.i
    free.i
    *next.blockMeta
  EndStructure

  Structure MemoryPage
    *memory
    *firstBlock.blockMeta
    *endPos
    *prev.MemoryPage
    *next.MemoryPage
  EndStructure
  
  Global _end
  
  Structure block_t
    inuse.a
    size.i
    *prev.block_t
    *next.block_t
    padding.i
  EndStructure
  
  #FIRST_ALLOC_SIZE = 132000
  #FREE_SIZE_LIMIT = 128000
  
  #MINREQ = 500000
  #PAGE_SIZE = 1000000
  #META_SIZE = SizeOf(blockMeta)
  #ALIGNMENT = 16

  Global.MemoryPage *currentPage = #Null
  Global.MemoryPage *head = #Null

; -------------------------
; findFreeBlock(lastPtr, size)
; returns pointer to a free block or #Null
; lastPtr is an output pointer to the previous block (or #Null)
; -------------------------
Procedure findFreeBlock(*last.blockMeta, size.i)
  Protected *current.blockMeta

  If *currentPage = #Null
    ProcedureReturn #Null
  EndIf

  *current = *currentPage\firstBlock
  If *last <> #Null
    *last = #Null
  EndIf

  While *current
    If (*current\free = 1) And (*current\size >= size)
      ProcedureReturn *current
    EndIf
    *last = *current
    *current = *current\next
  Wend

  ; search previous pages
  Protected *tmp.MemoryPage = *currentPage\prev
  While *tmp
    *current = *tmp\firstBlock
    While *current > 1000
      If (*current\free = 1) And (*current\size >= size)
        ProcedureReturn *current
      EndIf
      *last = *current
      *current = *current\next
    Wend
    *tmp = *tmp\prev
  Wend

  ProcedureReturn #Null
EndProcedure


; -------------------------
; requestBlock(size)
; -------------------------
Procedure requestBlock(size.i)
  Protected *block.blockMeta
  Protected *last.blockMeta = #Null

  *block = findFreeBlock(@*last, size)

  If *block
    *block\free = 0
    ProcedureReturn *block + #META_SIZE
  EndIf

  ; --- Allocate from current page ---
  If *currentPage = #Null
    ProcedureReturn #Null
  EndIf

  Protected memSize = MemorySize(*currentPage\memory)
  Protected nextPos = *currentPage\endPos

  ; Need new page?
  If (nextPos + #META_SIZE + size) > memSize
    Protected *newPage.MemoryPage = AllocateMemory(SizeOf(MemoryPage))
    If *newPage = #Null : ProcedureReturn #Null : EndIf

    *newPage\memory = AllocateMemory(#PAGE_SIZE)
    If *newPage\memory = #Null
      FreeMemory(*newPage)
      ProcedureReturn #Null
    EndIf

    *newPage\prev = *currentPage
    *newPage\next = #Null
    *newPage\firstBlock = #Null
    *newPage\endPos = 0

    *currentPage\next = *newPage
    *currentPage = *newPage
    memSize = MemorySize(*currentPage\memory)
    nextPos = 0
  EndIf

  ; Allocate block
  Protected *blockPtr.blockMeta = *currentPage\memory + nextPos
  *currentPage\endPos = nextPos + #META_SIZE + size

  If *last
    *last\next = *blockPtr
  Else
    *currentPage\firstBlock = *blockPtr
  EndIf

  *blockPtr\free = 0
  *blockPtr\size = size
  *blockPtr\next = #Null

  ProcedureReturn *blockPtr + #META_SIZE
EndProcedure


; -------------------------
; InitCache / FreeCache
; -------------------------
  Procedure InitCache()
    Protected required = #PAGE_SIZE

    *currentPage = AllocateMemory(SizeOf(MemoryPage))
    If *currentPage = #Null
      ProcedureReturn
    EndIf

    *currentPage\memory = #Null
    *currentPage\firstBlock = #Null
    *currentPage\prev = #Null
    *currentPage\next = #Null
    *currentPage\endPos = 0
    *head = *currentPage

    Repeat
      *currentPage\memory = AllocateMemory(required)
      If *currentPage\memory
        Break
      EndIf
      required = required >> 1
    Until required < #MINREQ

    If *currentPage\memory = #Null
      FreeMemory(*currentPage)
      *currentPage = #Null
      *head = #Null
      ProcedureReturn
    EndIf

    isInitialized = 1
  EndProcedure

  Procedure FreeCache()
    Debug "TotalSize before cleanup: " + Str(totalSize)
    Protected.MemoryPage *page = *head
    Protected.MemoryPage *nextPage = #Null

    While *page <> #Null
      If *page\memory <> #Null
        FreeMemory(*page\memory)
      EndIf
      *nextPage = *page\next
      FreeMemory(*page)
      *page = *nextPage
    Wend

    isInitialized = 0
    *head = #Null
    *currentPage = #Null
    totalSize = 0
  EndProcedure

; -------------------------
; Alloc / AllocThread / FreeThread
; -------------------------
  Procedure Alloc(size.i)
    If size <= 0
      ProcedureReturn #Null
    EndIf

    If Not isInitialized
      InitCache()
      If Not isInitialized
        ProcedureReturn #Null
      EndIf
    EndIf

    totalSize = totalSize + size
    ProcedureReturn requestBlock(size)
  EndProcedure

; -------------------------
; ReAlloc (copy old -> new)
; -------------------------
; ReAlloc: expand an allocator block
Procedure ReAlloc(*ptrV, size.i)
  ; If caller passed Null, behave like Alloc
  If *ptrV = #Null
    ProcedureReturn Alloc(size)
  EndIf

  Protected.blockMeta *block = *ptrV - #META_SIZE

  ; If the current block is already big enough, reuse it
  If *block\size >= size
    ProcedureReturn *ptrV
  EndIf

  ; Allocate a new block (this returns pointer to user-data)
  Protected *newPtr = Alloc(size)
  If *newPtr = #Null
    ProcedureReturn #Null
  EndIf

  ; Defensive copy: use YOUR CopyMemory signature (src, dest, size)
  If *block\size > 0
    CopyMemory(*ptrV, *newPtr, *block\size) ; src = old, dest = new, size = oldSize
  Else
    Debug "WARNING: ReAlloc skipped copy because old block size <= 0"
  EndIf

  ; Free the old block (marks as free in your allocator)
  Free(*ptrV)
  ProcedureReturn *newPtr
EndProcedure



; -------------------------
; Copy memory between two blocks (old -> dest)
; -------------------------
; Copy: copy between two allocated blocks (src -> dest)
Procedure Copy(*ptrV, *ptr2)
  ; Here ptrV = destination in your earlier code, but we'll follow semantics:
  ; We'll assume the intention is Copy(dest, src) as before — however you said
  ; CopyMemory is (src, dest, size). To avoid confusion, this routine will
  ; copy from ptr2 (source) to ptrV (destination).
  If *ptrV = #Null Or *ptr2 = #Null
    ProcedureReturn
  EndIf

  Protected.blockMeta *blockDest = *ptrV - #META_SIZE
  Protected.blockMeta *blockSrc  = *ptr2 - #META_SIZE

  Define.i copySize = *blockSrc\size
  If *blockDest\size < copySize
    copySize = *blockDest\size
  EndIf

  If copySize > 0
    CopyMemory(*ptr2, *ptrV, copySize) ; src = ptr2, dest = ptrV, size = copySize
  Else
    Debug "WARNING: Copy skipped, computed copySize = " + Str(copySize)
  EndIf
EndProcedure

; -------------------------
; Size/Free
; -------------------------
  Procedure Size(*ptrV)
    If *ptrV = #Null
      ProcedureReturn 0
    EndIf
    Protected.blockMeta *block = *ptrV - #META_SIZE
    ProcedureReturn *block\size
  EndProcedure

  Procedure Free(*ptrV)
    If *ptrV = #Null
      ProcedureReturn
    EndIf
    Protected.blockMeta *block = *ptrV - #META_SIZE
    If *block\free = 1
      ProcedureReturn
    EndIf
    *block\free = 1
    totalSize = totalSize - *block\size
  EndProcedure

EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 1
; Folding = --
; EnableXP
; DPIAware