"Filed out from Dolphin Smalltalk"!

RefactoryCategoryChange subclass: #RemoveCategoryChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RemoveCategoryChange guid: (GUID fromString: '{ae6cec4f-39ab-4bad-a9c8-ebb8593dd62d}')!

RemoveCategoryChange comment: ''!

!RemoveCategoryChange categoriesForClass!Refactory-Change Objects! !

!RemoveCategoryChange methodsFor!

asUndoOperation
	^AddCategoryChange new
		changeClass: self changeClass;
		category: category;
		yourself!

primitiveExecute
	| class catalogue existing |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	existing := catalogue keys detect: [:k | k name = category] ifNone: [].
	existing ifNotNil: [catalogue removeKey: existing]! !

!RemoveCategoryChange categoriesForMethods!
asUndoOperation!public! !
primitiveExecute!public! !
!

