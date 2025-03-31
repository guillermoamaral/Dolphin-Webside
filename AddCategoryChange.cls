"Filed out from Dolphin Smalltalk"!

RefactoryCategoryChange subclass: #AddCategoryChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AddCategoryChange guid: (GUID fromString: '{8eb4977e-9a49-4989-bafc-f2b365181fa2}')!

AddCategoryChange comment: ''!

!AddCategoryChange categoriesForClass!Refactory-Change Objects! !

!AddCategoryChange methodsFor!

asUndoOperation
	^RemoveCategoryChange new
		changeClass: self changeClass;
		category: category;
		yourself!

primitiveExecute
	| class catalogue existing |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	existing := catalogue keys detect: [:k | k name = category] ifNone: [].
	existing ifNil: [catalogue at: category asMethodCategory put: {}]! !

!AddCategoryChange categoriesForMethods!
asUndoOperation!public! !
primitiveExecute!public! !
!

