"Filed out from Dolphin Smalltalk 7"!

WebsideResource subclass: #WebsideWorkspace
	instanceVariableNames: 'contents bindings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WebsideWorkspace guid: (GUID fromString: '{dcba7ff2-0ed4-4a98-8467-b131777c73b8}')!
WebsideWorkspace comment: ''!
!WebsideWorkspace categoriesForClass!Unclassified! !
!WebsideWorkspace methodsFor!

asWebsideJson
	| json |
	json := super asWebsideJson.
	json
		at: 'id' put: id asString;
		at: 'source' put: contents.
	^json!

bindings
	^bindings!

contents
	^contents!

contents: aString
	contents := aString!

declareVariable: aString	bindings at: aString reduced put: nil!

initialize	super initialize.	bindings := Dictionary new!

receiver	^nil! !
!WebsideWorkspace categoriesFor: #asWebsideJson!public! !
!WebsideWorkspace categoriesFor: #bindings!public! !
!WebsideWorkspace categoriesFor: #contents!public! !
!WebsideWorkspace categoriesFor: #contents:!public! !
!WebsideWorkspace categoriesFor: #declareVariable:!public! !
!WebsideWorkspace categoriesFor: #initialize!public! !
!WebsideWorkspace categoriesFor: #receiver!public! !

