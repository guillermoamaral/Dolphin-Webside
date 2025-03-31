"Filed out from Dolphin Smalltalk"!

RefactoryChange subclass: #RefactoryPackageChange
	instanceVariableNames: 'packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RefactoryPackageChange guid: (GUID fromString: '{a233a4fd-81a3-45c7-9a63-338591e63744}')!

RefactoryPackageChange comment: ''!

!RefactoryPackageChange categoriesForClass!Refactory-Change Objects! !

!RefactoryPackageChange methodsFor!

asUndoOperation!

asWebsideJson
	^super asWebsideJson
		at: 'name' put: packageName;
		yourself!

executeNotifying: aBlock 
	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self primitiveExecute.
	aBlock value.
	^undo!

fromWebsideJson: json
	super fromWebsideJson: json.
	packageName := json at: 'name' ifAbsent: []!

primitiveExecute
	^self subclassResponsibility! !

!RefactoryPackageChange categoriesForMethods!
asUndoOperation!public! !
asWebsideJson!public! !
executeNotifying:!public! !
fromWebsideJson:!public! !
primitiveExecute!public! !
!

