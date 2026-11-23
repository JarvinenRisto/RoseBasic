XIncludeFile "LinkedList.pb"

DeclareModule RPN	
	IncludeFile "tokens.pb"
	
	Declare getPostFix()
	Declare add(*token.Token)
	
	Global.LinkedList::CustomList *infix = #Null
	Global.LinkedList::CustomList *postfix = #Null
	
EndDeclareModule

Module RPN
	
	Procedure add(*token.Token)
		If *infix = #Null
			*infix = LinkedList::init()
		EndIf

		LinkedList::addLast(*infix, *token, SizeOf(Token))
	EndProcedure
	
	Procedure toPostFix()
		Define.LinkedList::CustomList *stack = LinkedList::Init()
		Define.LinkedList::Node *Next = *infix\head
		Define.LinkedList::Node *Next2
		Define.Token *token, *token2

		While *next
			
			If *next
				*token = *next\value
				
				If *token
					Select *token\type
						Case #LPAREN
							LinkedList::addFirst(*stack, *token, SizeOf(Token))
						Case #POW
							*Next2 = linkedList::lookup(*stack, *stack\key)
							
							If *next2 > 0
								*token2 = *next2\value
								
								While *token2 And *token2\type = #POW
									*token2 = *next2\value
									If *token2
										LinkedList::addLast(*postfix, *token2, SizeOf(Token))
										LinkedList::delete(*stack, *next2\key)
									EndIf
									*next2 = *next2\next
								Wend
							EndIf
							
							LinkedList::addFirst(*stack, *token, SizeOf(Token))
						Case #MUL, #DIV, #MOD
							*Next2 = linkedList::lookup(*stack, *stack\key)
							
							If *next2 > 0
								*token2 = *next2\value
								
								While *token2 And (*token2\type = #POW Or *token2\type = #MUL Or *token2\type = #DIV Or *token2\type = #MOD)
									*token2 = *next2\value
									If *token2
										LinkedList::addLast(*postfix, *token2, SizeOf(Token))
										LinkedList::delete(*stack, *next2\key)
									EndIf
									*next2 = *next2\next
								Wend
								
								LinkedList::addFirst(*stack, *token, SizeOf(Token))
							EndIf
						Case #PLUS, #MINUS
							*Next2 = linkedList::lookup(*stack, *stack\key)
							
							If *next2 > 0
								*token2 = *next2\value
								
								While *token2 And (*token2\type = #MUL Or *token2\type = #DIV Or *token2\type = #POW Or *token2\type = #PLUS Or *token2\type = #MINUS)
									*token2 = *next2\value
									If *token2
										LinkedList::addLast(*postfix, *token2, SizeOf(Token))
										LinkedList::delete(*stack, *next2\key)
									EndIf
									*next2 = *next2\next
								Wend
							EndIf
							
							LinkedList::addFirst(*stack, *token, SizeOf(Token))
						Case #RPAREN
							*Next2 = linkedList::lookup(*stack, *stack\key)
							
							If *next2 > 0
								*token2 = *next2\value
								
								While *token2 And *token2\type <> #LPAREN
									*token2 = *next2\value
									If *token2
										LinkedList::addLast(*postfix, *token2, SizeOf(Token))
										LinkedList::delete(*stack, *next2\key)
									EndIf
									*next2 = *next2\next
								Wend
								
								LinkedList::delete(*postfix, *postfix\key)	
							EndIf
								
						Default
							LinkedList::addLast(*postfix, *token, SizeOf(Token))
					EndSelect
					
				EndIf
			EndIf
			
			*next = *next\next
		Wend

		*next2 = *stack\head
		While *next2
			*token = *next2\value
			If *token
				LinkedList::addLast(*postfix, *token, SizeOf(Token))
			EndIf
			*next2 = *next2\next
		Wend
		
		LinkedList::Destroy(*stack)
		*stack = #Null
	EndProcedure
	
	Procedure getPostFix()
		*postfix = LinkedList::init()
		toPostFix()
		LinkedList::Destroy(*infix)
		*infix = #Null
		ProcedureReturn *postfix
	EndProcedure

EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; CursorPosition = 126
; FirstLine = 90
; Folding = -
; EnableXP
; DPIAware