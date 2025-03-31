"Filed out from Dolphin Smalltalk"!

RefactoryClassChange subclass: #RefactoryCategoryChange
	instanceVariableNames: 'category'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RefactoryCategoryChange guid: (GUID fromString: '{a56b47bc-9b75-4e97-b085-6127c9cb60eb}')!

RefactoryCategoryChange comment: ''!

!RefactoryCategoryChange categoriesForClass!Refactory-Change Objects! !

!RefactoryCategoryChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'category' put: category;
		yourself!

category: aString
	category := aString!

fromWebsideJson: json
	super fromWebsideJson: json.
	category := json at: 'category' ifAbsent: []! !

!RefactoryCategoryChange categoriesForMethods!
asWebsideJson!public! !
category:!public! !
fromWebsideJson:!public! !
!

