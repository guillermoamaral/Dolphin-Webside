"Filed out from Dolphin Smalltalk"!

RefactoryPackageChange subclass: #RenamePackageChange
	instanceVariableNames: 'newName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RenamePackageChange guid: (GUID fromString: '{64e42418-9e10-4e76-81bf-a783daad96b7}')!

RenamePackageChange comment: ''!

!RenamePackageChange categoriesForClass!Refactory-Change Objects! !

!RenamePackageChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'name' put: packageName;
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	newName := json at: 'newName' ifAbsent: []!

primitiveExecute
	| pm package |
	pm := Package manager.
	package := pm packageNamed: packageName.
	package ifNotNil: [pm renamePackage: package to: newName]! !

!RenamePackageChange categoriesForMethods!
asWebsideJson!public! !
fromWebsideJson:!public! !
primitiveExecute!public! !
!

