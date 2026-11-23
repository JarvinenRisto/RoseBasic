XIncludeFile "HashTable.pb"

Enumeration 
  #IDENFIER
  #TSTRING
  #TSTRING_OPTIONAL
  #TINTEGER_OPTIONAL
  #TDOUBLE_OPTIONAL
  #TINTEGER
  #TDOUBLE
  #STRING
  #NUMBER
  #LINKEDLIST
  #LPAREN
  #RPAREN
  #POW
  #MUL
  #DIV
  #MOD
  #PLUS
  #MINUS
  #DOT
  #EQU
  #COMMA
  #NEW
  #NOT
  #NEGATE
  #GRT
  #LT
  #NOT_EQU
  #LT_EQU
  #GRT_EQU
  #OPERATOR_OR
  #AND
  #DIM
  #AS
  #SUBR
  #END_SUBR
  #FUNCTION
  #END_FUNCTION
  #THEN
  #BREAK
  #CONTINUE
  #ELSE
  #CDELETE
  #RETURN
  #END
  #ENDIF
  #IF
  #WHILE
  #REPEAT
  #FOR
  #FOREVER
  #UNTIL
  #ENDWHILE
  #TO
  #STEP
  #NEXT
  #EXTENDS
  #END_CLASS
  #PUBLIC
  #PRIVATE
  #PROTECTED
  #STATIC
  #DECONSTRUCTOR
  #ENDDECONSTRUCTOR
  #CONSTRUCTOR
  #ENDCONSTRUCTOR
  #CONST
  #CLASS
  #USE
  #MAIN
  #ENDMAIN
  #SELECT
  #CASE
  #DEFAULT
  #ENDSELECT
  #BOOLEAN
  #IMPORT
  #ENDIMPORT
  #ALIAS
  #REDIM
  #INIT_VAR
  #VAR_COUNT
  #ELSEIF
  #XOR
  #PLUS_EQUAL
  #DEBUG
  #SKIP
  #EQU_EQU
  #SUPER
  #ANY
  #OPERATOR
  #ENDOPERATOR
  #MODULE
  #ENDMODULE
  #INVALID
  #PLUS_EQU
  #MINUS_EQU
  #MUL_EQU
  #DIV_EQU
  #QUESTION
  #COLON
  #PARALLEL
  #YIELD
  #STOP
  #TMAX
EndEnumeration

Enumeration
  #CLASS_OBJ = 255
EndEnumeration

Enumeration 
  #OP_CONSTANT
  #OP_NIL
  #OP_TRUE 
  #OP_FALSE 
  #OP_POP 
  #OP_GET_LOCAL 
  #OP_SET_LOCAL 
  #OP_GET_GLOBAL 
  #OP_DEFINE_GLOBAL 
  #OP_GET_PROPERTY 
  #OP_SET_PROPERTY 
  #OP_EQUAL 
  #OP_GREATER 
  #OP_LESS 
  #OP_ADD 
  #OP_SUBTRACT 
  #OP_MULTIPLY 
  #OP_DIVIDE 
  #OP_NOT  
  #OP_NEGATE 
  #OP_PRINT 
  #OP_JUMP 
  #OP_JUMP_IF_FALSE 
  #OP_LOOP 
  #OP_CALL 
  #OP_INVOKE 
  #OP_CLOSURE 
  #OP_RETURN 
  #OP_CLASS 
  #OP_MOD
  #OP_POW 
  #OP_NOT_EQU 
  #OP_SET_ARRAY 
  #OP_GET_ARRAY
  #OP_ALLOC_ARRAY 
  #OP_FREE_ARRAY 
  #OP_JUMP_IF_TRUE 
  #OP_LT_EQU 
  #OP_GRT_EQU
  #OP_HALT 
  #OP_NOP
  #OP_CALL_NATIVE 
  #OP_INVOKE_CONSTR
  #OP_TO_BOOL
  #OP_CONST_NATIVE
  #OP_REDIM
  #OP_XOR
  #OP_SUPER_INVOKE
  #OP_DUP
  #OP_SPAWN
  #OP_YIELD
  #OP_STOP
EndEnumeration

Structure Obj
  type.b
  *next.Obj
  isMarked.b
EndStructure

Structure ObjString
  Obj.Obj
  length.i
  *chars
  hash.l
EndStructure

Structure ObjClass
  Obj.Obj
  *name.ObjString
  *functions.HashTable::hash_table 
  fieldCount.i
EndStructure

Structure ObjFunction
  Obj.Obj
  arity.i
  *chunk.Chunk
  *name.ObjString
  localSize.l
EndStructure

Structure ObjInstance
  Obj.Obj
  *klass.ObjClass
  *fields 
EndStructure  

Structure ObjClosure
  Obj.Obj
  *function.ObjFunction
EndStructure     

Structure ObjNative
  Obj.Obj
  *function
EndStructure

Structure ObjArray
  Obj.Obj
  capacity.i
  count.i
  *items.ValueArray
EndStructure

Structure Token
  type.a
  line.i
  *fileName
  StructureUnion
    integer.i
    double.d
    *string
  EndStructureUnion
EndStructure
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 108
; FirstLine = 84
; EnableXP
; DPIAware