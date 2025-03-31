"Filed out from Dolphin Smalltalk"!

RefactoryPackageChange subclass: #AddPackageChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AddPackageChange guid: (GUID fromString: '{a2e72d80-c0cb-4bf5-8398-127e871225f6}')!

AddPackageChange comment: ''!

!AddPackageChange categoriesForClass!Refactory-Change Objects! !

!AddPackageChange methodsFor!

primitiveExecute
	| pm filename |
	pm := Package manager.
	^pm packageNamed: packageName
		ifNone: 
			[filename := File composePath: PackageFolder dolphinRootPathname subPath: packageName.
			pm newPackage: filename]! !

!AddPackageChange categoriesForMethods!
primitiveExecute!public! !
!

