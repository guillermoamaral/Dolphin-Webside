﻿"Filed out from Dolphin Smalltalk"!

Object subclass: #WebsideResource
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

WebsideResource guid: (GUID fromString: '{fb90d8b3-e1bf-4830-b05d-2d19107070b8}')!

WebsideResource comment: ''!

!WebsideResource categoriesForClass!Unclassified! !

!WebsideResource methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'id' put: id asString;
		yourself!

id	^id!

id: anIID	id := anIID!

initialize	super initialize.	id := self class newId! !

!WebsideResource categoriesForMethods!
asWebsideJson!public! !
id!public! !
id:!public! !
initialize!public! !
!

!WebsideResource class methodsFor!

new	^super new initialize!

newId	^IID newUnique! !

!WebsideResource class categoriesForMethods!
new!public! !
newId!public! !
!

