;XIncludeFile "GlobalMemoryCache.pb"
XIncludeFile "LinkedList.pb"
XIncludeFile "HashTable.pb"
XIncludeFile "TokenList.pb"
XIncludeFile "FileReader.pbi"

Module Lexer
  
  CompilerIf #PB_Compiler_OS = #PB_OS_Windows
    ImportC "msvcrt.lib"
      wcstod.d(*txt, *endoftxt=#Null) As "wcstod"
    EndImport
  CompilerEndIf
  
  Structure ArrayPtr
    c.c[0]
  EndStructure
  
  Global.HashTable::hash_table *ht
  
  Macro addToBuffer()
    PokeU(*stringBuffer+pos, c)
    pos = pos + 2
  EndMacro
  
  Macro isNumber(c)
    c >= '0' And c <= '9'
  EndMacro
  
  Macro isCharacter(c)
    ((c >= 'a' And c <= 'z') Or (c >= 'A' And c <= 'Z') Or c = '_' Or c = 228 Or c = 46)
  EndMacro
  
  Macro isSpace(c)
    (c = ' ' Or c = #TAB Or c = #LF Or c = 13)
  EndMacro 
  
  Macro isDeclare(id)
    (id = TokenList::#CLASS Or id = TokenList::#SUBR Or id = TokenList::#FUNCTION Or id = TokenList::#DIM Or id = TokenList::#CONST Or id = TokenList::#COMMA Or id = TokenList::#DOT)
  EndMacro
  
  Procedure init()
    *ht = HashTable::new(100)
    
    HashTable::insert(*ht, @"delete", TokenList::#CDELETE)
    HashTable::insert(*ht, @"deconstructor", TokenList::#DECONSTRUCTOR)
    HashTable::insert(*ht, @"enddeconstructor", TokenList::#ENDDECONSTRUCTOR)
    
    HashTable::insert(*ht, @"const", TokenList::#CONST)
    HashTable::insert(*ht, @"static", TokenList::#STATIC)
    HashTable::insert(*ht, @"super", TokenList::#SUPER)
    HashTable::insert(*ht, @"parallel", TokenList::#PARALLEL)
    HashTable::insert(*ht, @"yield", TokenList::#Yield)
    HashTable::insert(*ht, @"stop", TokenList::#STOP)
    
    HashTable::insert(*ht, @"module", TokenList::#MODULE)
    HashTable::insert(*ht, @"endmodule", TokenList::#ENDMODULE)
    HashTable::insert(*ht, @"operator", TokenList::#OPERATOR)
    HashTable::insert(*ht, @"endoperator", TokenList::#ENDOPERATOR)
    HashTable::insert(*ht, @"debug", TokenList::#DEBUG)
    HashTable::insert(*ht, @"xor", TokenList::#XOR)
    HashTable::insert(*ht, @"select", TokenList::#SELECT)
    HashTable::insert(*ht, @"case", TokenList::#CASE)
    HashTable::insert(*ht, @"default", TokenList::#DEFAULT)
    HashTable::insert(*ht, @"endselect", TokenList::#ENDSELECT)
    HashTable::insert(*ht, @"elseif", TokenList::#ELSEIF)
    HashTable::insert(*ht, @"redim", TokenList::#REDIM)
    HashTable::insert(*ht, @"protected", TokenList::#PROTECTED)
    HashTable::insert(*ht, @"private", TokenList::#PRIVATE)
    HashTable::insert(*ht, @"public", TokenList::#PUBLIC)
    HashTable::insert(*ht, @"extends", TokenList::#EXTENDS)
    HashTable::insert(*ht, @"use", TokenList::#USE)
    HashTable::insert(*ht, @"function", TokenList::#FUNCTION)
    HashTable::insert(*ht, @"endfunction", TokenList::#END_FUNCTION)
    HashTable::insert(*ht, @"return", TokenList::#RETURN)
    HashTable::insert(*ht, @"class", TokenList::#CLASS)
    HashTable::insert(*ht, @"endclass", TokenList::#END_CLASS)
    HashTable::insert(*ht, @"new", TokenList::#NEW)
    HashTable::insert(*ht, @"constructor", TokenList::#CONSTRUCTOR)
    HashTable::insert(*ht, @"endconstructor", TokenList::#ENDCONSTRUCTOR)
    HashTable::insert(*ht, @"sub", TokenList::#SUBR)
    HashTable::insert(*ht, @"endsub", TokenList::#END_SUBR)
    HashTable::insert(*ht, @"end", TokenList::#END)
    HashTable::insert(*ht, @"as", TokenList::#AS)
    HashTable::insert(*ht, @"number", TokenList::#NUMBER)
    HashTable::insert(*ht, @"string", TokenList::#STRING)
    HashTable::insert(*ht, @"dim", TokenList::#DIM)
    HashTable::insert(*ht, @"continue", TokenList::#CONTINUE)
    HashTable::insert(*ht, @"break", TokenList::#BREAK)
    HashTable::insert(*ht, @"if", TokenList::#IF)
    HashTable::insert(*ht, @"else", TokenList::#ELSE)
    HashTable::insert(*ht, @"endif", TokenList::#ENDIF)
    HashTable::insert(*ht, @"while", TokenList::#WHILE)
    HashTable::insert(*ht, @"endwhile", TokenList::#ENDWHILE)
    HashTable::insert(*ht, @"then", TokenList::#THEN)
    HashTable::insert(*ht, @"or", TokenList::#OPERATOR_OR)
    HashTable::insert(*ht, @"and", TokenList::#And)
    HashTable::insert(*ht, @"not", TokenList::#Not)
    HashTable::insert(*ht, @"main", TokenList::#MAIN)
    HashTable::insert(*ht, @"endmain", TokenList::#END_FUNCTION)
    HashTable::insert(*ht, @"for", TokenList::#FOR)
    HashTable::insert(*ht, @"to", TokenList::#TO)
    HashTable::insert(*ht, @"next", TokenList::#NEXT)
    HashTable::insert(*ht, @"step", TokenList::#STEP)
    HashTable::insert(*ht, @"repeat", TokenList::#REPEAT)
    HashTable::insert(*ht, @"forever", TokenList::#FOREVER)
    HashTable::insert(*ht, @"until", TokenList::#UNTIL)
    HashTable::insert(*ht, @"import", TokenList::#IMPORT)
    HashTable::insert(*ht, @"endimport", TokenList::#ENDIMPORT)
    HashTable::insert(*ht, @"alias", TokenList::#ALIAS)
    
    TokenList::Init()
  EndProcedure
  
  Procedure destroy()
    HashTable::destroy(*ht)
    TokenList::Destroy()
  EndProcedure
  
  Procedure toLower(*S.String)
    *S\s = LCase(*S\s)
  EndProcedure
  
  Procedure run(*file.LinkedList::CustomList, fileNameV.s, length.i)		
    Define.s string = PeekS(*file\head\value, length, #PB_UTF8)
    Define.ArrayPtr *pText = @string
    Define *stringBuffer = AllocateMemory(1024)
    Define.i I, pos, line = 1, lastType
    Define.i dotFound, lineContinued, prevType
    Define.c c
    Define.TokenList::Token token
    Define.i isSelect = 0
    Define *selectList.LinkedList::CustomList
    
    Define fileName = AllocateMemory(MemoryStringLength(@fileNameV)*2 + 2)
    PokeS(fileName, fileNameV)

    Define useEnabled = 0
    
    For I = 0 To length
      c = *pText\c[I]
      
      Select c
        Case '0' To '9'
          dotFound = #False
          
          While isNumber(c)
            addToBuffer()
            
            I = I + 1
            c = *pText\c[I]
            
            If c = '.'
              If dotFound = #True
                PrintN("Line "+Str(line)+": Extra dot found at the number. (Unexpected character .)")
                Break
              EndIf
              
              dotFound = #True
              
              addToBuffer()
              I = I + 1
              c = *pText\c[I]
            EndIf
          Wend
          
          I = I - 1
          
          If prevType
            prevType = 0
            PrintN("Line "+Str(line)+": Unexpected out of a place number.")
            FillMemory(*stringBuffer, pos)
            pos = 0
            Continue
          EndIf
          
          If dotFound 
            token\type = TokenList::#TDOUBLE
            CompilerIf #PB_Compiler_OS = #PB_OS_Linux
              token\double = ValD(PeekS(*stringBuffer))
            CompilerElse
              token\double = wcstod(*stringBuffer)
            CompilerEndIf
          Else
            token\type = TokenList::#TINTEGER
            token\integer = Val(PeekS(*stringBuffer, pos, #PB_Unicode))
          EndIf
          
          token\fileName = fileName
          TokenList::AddToken(@token)
          
          FillMemory(*stringBuffer, pos)
          pos = 0
        Case 'a' To 'z', 'A' To 'Z', '_', 228, 246
          
          While (c >= 'a' And c <= 'z') Or (c >= 'A' And c <= 'Z') Or (c = '_') Or (c = 228) Or (c = 246) Or isNumber(c)
            addToBuffer()
            
            I = I + 1
            c = *pText\c[I]
          Wend
          
          If *pText\c[I-1] = '_' And isSpace(*pText\c[I-2]) And isSpace(*pText\c[I])
            lineContinued = #True
            pos = 0
            Continue
          EndIf
          
          I = I - 1
          
          PokeS(*stringBuffer, LCase(PeekS(*stringBuffer)))
          
          token\type = HashTable::search(*ht, *stringBuffer)
          
          If token\type = TokenList::#IDENFIER
            token\string = AllocateMemory(pos + 2)
            CopyMemory(*stringBuffer, token\string, pos)
          ElseIf token\type = TokenList::#CONSTRUCTOR
            token\type = TokenList::#FUNCTION
            token\fileName = fileName
            TokenList::AddToken(@token)
            token\type = TokenList::#IDENFIER
            token\string = AllocateMemory(pos + 2)
            CopyMemory(@"new", token\string, pos)
          ElseIf token\type = TokenList::#DECONSTRUCTOR
            token\type = TokenList::#FUNCTION
            token\fileName = fileName
            TokenList::AddToken(@token)
            token\type = TokenList::#IDENFIER
            token\string = AllocateMemory(pos + 2)
            CopyMemory(@"delete", token\string, pos)
          ElseIf token\type = TokenList::#OPERATOR
            token\type = TokenList::#FUNCTION
            token\fileName = fileName
            TokenList::AddToken(@token)
            token\type = TokenList::#IDENFIER
            token\string = AllocateMemory(22)
            Define.i currPos = pos + 2
            Define.s operator = "operator"
            I + 1
            While isSpace(*pText\c[I])
              I + 1
            Wend
            operator + Chr(*pText\c[I]) 
            If *pText\c[1+I] = '=' Or *pText\c[1+I] = '>' Or *pText\c[1+I] = '<' Or *pText\c[1+I] = ')'
              I + 1
              operator + Chr(*pText\c[I])
              currPos + 2
            EndIf 
            CopyMemory(@operator, token\string, currPos)
          ElseIf token\type = TokenList::#ENDCONSTRUCTOR Or token\type = TokenList::#ENDOPERATOR
            token\type = TokenList::#END_FUNCTION
          ElseIf token\type = TokenList::#MAIN
            token\type = TokenList::#FUNCTION
            token\fileName = fileName
            TokenList::AddToken(@token)
            token\type = TokenList::#IDENFIER
            token\string = AllocateMemory(pos + 2)
            CopyMemory(@"main", token\string, pos)
          ElseIf token\type = TokenList::#USE
            useEnabled = 1
            FillMemory(*stringBuffer, pos)
            pos = 0
            Continue
          EndIf
          
          token\fileName = fileName
          TokenList::AddToken(@token)
          FillMemory(*stringBuffer, pos)
          pos = 0
        Case '"'
          c = '0'
          
          While c <> '"'
            I = I + 1
            
            If I > length
              PrintN("Line "+Str(line)+": Literal string not terminated ("+Chr('"')+" missing).")
              Break
            EndIf
            
            c = *pText\c[I]
            
            If c = '\' And *pText\c[1+I] = 'n'
              I+1
              PokeU(*stringBuffer+pos, #LF)
              pos = pos + 2
              PokeU(*stringBuffer+pos, #CR)
              pos = pos + 2
              Continue
            ElseIf c = '\' And *pText\c[1+I] = '\'
              I+1
              PokeU(*stringBuffer+pos, '\')
              pos = pos + 2
              Continue
            ElseIf c = '\' And *pText\c[1+I] = '"'
              I+1
              PokeU(*stringBuffer+pos, '"')
              pos = pos + 2
              Continue
            ElseIf c = '"'
              Break
            EndIf
            
            addToBuffer()
          Wend
          
          If useEnabled = 1
            useEnabled = 0
            FileReader::Load(PeekS(*stringBuffer))
            FillMemory(*stringBuffer, pos)
            pos = 0
            Continue
          EndIf
          
          token\type = TokenList::#TSTRING
          token\string = AllocateMemory(pos + 2)
          CopyMemory(*stringBuffer, token\string, pos)
          token\fileName = fileName
          TokenList::AddToken(@token)
          
          FillMemory(*stringBuffer, pos)
          pos = 0
        Case ','
          token\type = TokenList::#COMMA
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '.'
          token\type = TokenList::#DOT
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '('
          token\type = TokenList::#LPAREN
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case ')'
          token\type = TokenList::#RPAREN
          token\fileName = fileName
          TokenList::AddToken(@token)
          ;Case '['
          ;  token\type = TokenList::#LBRACKET
          ;  token\fileName = fileName
          ;		TokenList::AddToken(@token)
          ;		prevType = 0
          ;	Case ']'
          ;	  token\type = TokenList::#RBRACKET
          ;	  token\fileName = fileName
          ;	  TokenList::AddToken(@token)
          ;	  prevType = 0
        Case '>'
          If *pText\c[1+I] = '='
            token\type = TokenList::#GRT_EQU
            I + 1
          Else
            token\type = TokenList::#GRT
          EndIf
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '<'
          If *pText\c[1+I] = '>'
            token\type = TokenList::#NOT_EQU
            I + 1
          ElseIf *pText\c[1+I] = '='
            token\type = TokenList::#LT_EQU
            I + 1
          Else
            token\type = TokenList::#LT
          EndIf
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '?'
          token\type = TokenList::#QUESTION
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case ':'
          token\type = TokenList::#COLON
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '='
          If *pText\c[1+I] = '='
            token\type = TokenList::#EQU_EQU
            I + 1
          Else
            token\type = TokenList::#EQU
          EndIf
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '+'
          If *pText\c[1+I] = '=' 
            token\fileName = fileName
            token\type = TokenList::#PLUS_EQU
            TokenList::AddToken(@token)
            i + 1
          Else
            token\type = TokenList::#PLUS
            token\fileName = fileName
            TokenList::AddToken(@token)
          EndIf
          prevType = 0
        Case '-'
          
          If (lastType <> TokenList::#TINTEGER And lastType <> TokenList::#TDOUBLE And lastType <> TokenList::#IDENFIER And lastType <> TokenList::#RPAREN)
            token\type = TokenList::#NEGATE
            token\fileName = fileName
            TokenList::AddToken(@token)
          ElseIf *pText\c[1+I] = '='
            token\type = TokenList::#MINUS_EQU
            TokenList::AddToken(@token)
            i + 1
          Else
            token\type = TokenList::#MINUS
            token\fileName = fileName
            TokenList::AddToken(@token)
          EndIf
          prevType = 0
        Case '*'
          If *pText\c[1+I] = '=' 
            token\type = TokenList::#MUL_EQU
            TokenList::AddToken(@token)
            i + 1
          Else
            token\type = TokenList::#MUL
            token\fileName = fileName
            TokenList::AddToken(@token)
          EndIf
          prevType = 0
        Case '%'
          token\type = TokenList::#MOD
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '^'
          token\type = TokenList::#POW
          token\fileName = fileName
          TokenList::AddToken(@token)
          prevType = 0
        Case '/'
          If *pText\c[1+I] = '*'
            I = I + 2
            
            Repeat
              If *pText\c[I] = #LF
                line = line + 1
              EndIf
              
              I = I + 1
              
              If I > length
                PrintN("Line "+Str(line)+", File "+fileName+": Multiline comment Not terminated (*/ missing).")
                Break
              EndIf
            Until *pText\c[I] = '*' And *pText\c[1+I] = '/'
            
            I = I + 2
            
            Continue
          ElseIf *pText\c[1+I] = '/'
            I = I + 2
            
            While *pText\c[I] <> #LF
              
              If *pText\c[I] = #LF
                line = line + 1
              EndIf
              
              I = I + 1
              
              If I > length
                Break
              EndIf
            Wend
            
            Continue
          ElseIf *pText\c[1+I] = '='
            token\type = TokenList::#DIV_EQU
            token\fileName = fileName
            TokenList::AddToken(@token)
            i + 1
          Else
            token\type = TokenList::#DIV
            token\fileName = fileName
            TokenList::AddToken(@token)
          EndIf
          prevType = 0
        Case #LF
          If lineContinued = #True
            lineContinued = #False
            Continue
          EndIf
          line = line + 1
          token\line = line
      EndSelect
      
      lastType = token\type
    Next I
    
    FreeMemory(*stringBuffer)
  EndProcedure
  
EndModule 
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 213
; FirstLine = 185
; Folding = ---
; EnableXP
; DPIAware