"Filed out from Dolphin Smalltalk"!

RefactoryPackageChange subclass: #RemovePackageChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RemovePackageChange guid: (GUID fromString: '{411f07c2-aecb-44a9-9d66-f292cff602fb}')!

RemovePackageChange comment: ''!

!RemovePackageChange categoriesForClass!Refactory-Change Objects! !

!RemovePackageChange methodsFor!

primitiveExecute
	| pm package |
	pm := Package manager.
	package := pm packageNamed: packageName.
	package ifNotNil: [pm removePackage: package]! !

!RemovePackageChange categoriesForMethods!
primitiveExecute!public! !
!

