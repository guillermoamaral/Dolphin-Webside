"Filed out from Dolphin Smalltalk 7"!

WebsideResource subclass: #WebsideWorkspace
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WebsideWorkspace guid: (GUID fromString: '{dcba7ff2-0ed4-4a98-8467-b131777c73b8}')!
WebsideWorkspace comment: ''!
!WebsideWorkspace categoriesForClass!Unclassified! !
!WebsideWorkspace methodsFor!

declareVariable: aString	bindings at: aString reduced put: nil!

initialize	super initialize.	bindings := Dictionary new!

receiver	^nil! !
!WebsideWorkspace categoriesFor: #declareVariable:!public! !
!WebsideWorkspace categoriesFor: #initialize!public! !
!WebsideWorkspace categoriesFor: #receiver!public! !

