"Filed out from Dolphin Smalltalk 7"!

RefactoryCategoryChange subclass: #RenameCategoryChange
	instanceVariableNames: 'newName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RenameCategoryChange guid: (GUID fromString: '{56140d1e-1ec2-4cd9-aa5a-abc85a9200d5}')!
RenameCategoryChange comment: ''!
!RenameCategoryChange categoriesForClass!Refactory-Change Objects! !
!RenameCategoryChange methodsFor!

asUndoOperation
	^RenameCategoryChange new
		changeClass: self changeClass;
		category: newName;
		newName: category;
		yourself!

asWebsideJson
	^super asWebsideJson
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	newName := json at: 'newName' ifAbsent: []!

newName: aString
	newName := aString!

primitiveExecute
	| class catalogue source target methods |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	source := catalogue keys detect: [:k | k name = category] ifNone: [^self].
	target := catalogue keys detect: [:k | k name = newName] ifNone: [].
	methods := catalogue at: source.
	target notNil
		ifTrue: [methods := (catalogue at: target) copyWith: methods]
		ifFalse: [target := newName asMethodCategory].
	catalogue
		at: target put: methods;
		removeKey: source! !
!RenameCategoryChange categoriesFor: #asUndoOperation!public! !
!RenameCategoryChange categoriesFor: #asWebsideJson!public! !
!RenameCategoryChange categoriesFor: #fromWebsideJson:!public! !
!RenameCategoryChange categoriesFor: #newName:!public! !
!RenameCategoryChange categoriesFor: #primitiveExecute!public! !

