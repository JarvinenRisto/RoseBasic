XIncludeFile "GlobalMemoryCache.pb"

DeclareModule FastString
	
	Structure Object
		type.i
	EndStructure
	
	Structure STRFASTOBJ
		Obj.Object
		*Data
		refs.i
	EndStructure
	
	Declare new(size.i)
	Declare free(*StrFast.STRFASTOBJ)
	Declare add(*StrFast.STRFASTOBJ, *String, Len = 0)
	Declare link(*StrFast.STRFASTOBJ)
	Declare incPrefCount(*StrFast.STRFASTOBJ)
	
EndDeclareModule

Module FastString
	
	Procedure new(size.i)
		Protected *StrFast.STRFASTOBJ = AllocateStructure(STRFASTOBJ)
		If *StrFast
			*StrFast\Obj\type = 27
			*StrFast\refs = 1
			*StrFast\Data = GlobalMemoryCache::Alloc(4 + size * SizeOf(Character))
			If *StrFast\Data
				PokeL(*StrFast\Data, 0)  ; bytes 1-4 storing current len (chars)
				ProcedureReturn *StrFast
			Else
				FreeStructure(*StrFast)
			EndIf
		EndIf
		ProcedureReturn #Null
	EndProcedure
	
	Procedure free(*StrFast.STRFASTOBJ)
		If *StrFast
			If *StrFast\refs <= 0  ; Muisti vapautetaan vain, jos viittauksia ei ole
				If *StrFast\Data
					GlobalMemoryCache::Free(*StrFast\Data)
				EndIf
				FreeStructure(*StrFast)
			EndIf
		EndIf
	EndProcedure
	
	Procedure link(*StrFast.STRFASTOBJ)
		If *StrFast
			If *StrFast\refs > 0
				*StrFast\refs - 1
			EndIf
			If *StrFast\refs = 0  ; Vapautetaan muisti, jos viittauksia ei ole
				If *StrFast\Data
					GlobalMemoryCache::Free(*StrFast\Data)
				EndIf
				FreeStructure(*StrFast)
			EndIf
		EndIf
	EndProcedure
	
	Procedure incPrefCount(*StrFast.STRFASTOBJ)
		If *StrFast
			*StrFast\refs + 1
		EndIf
	EndProcedure
	
	Procedure add(*StrFast.STRFASTOBJ, *String, Len = 0)
		Protected NewLenB, *FastLen.Long
		If *StrFast
			If Len <= 0 And *String > 100
				Len = MemoryStringLength(*String)
			EndIf
			If Len
				*FastLen = *StrFast\Data
				NewLenB = (*FastLen\l + Len) * SizeOf(Character)
				If NewLenB > GlobalMemoryCache::Size(*StrFast\Data)
					Protected *NewData = GlobalMemoryCache::ReAlloc(*StrFast\Data, 4 + NewLenB * 1.28)
					If *NewData
						*StrFast\Data = *NewData
					Else
						ProcedureReturn
					EndIf
					*FastLen = *StrFast\Data
				EndIf
				CopyMemory(*String, *StrFast\Data + 4 + *FastLen\l * SizeOf(Character), Len * SizeOf(Character))
				*FastLen\l + Len
			EndIf
		EndIf
	EndProcedure
	
EndModule
; IDE Options = PureBasic 6.21 (Linux - x64)
; Folding = --
; EnableXP
; DPIAware