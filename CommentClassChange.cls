﻿"Filed out from Dolphin Smalltalk"!

RefactoryClassChange subclass: #CommentClassChange
	instanceVariableNames: 'comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

CommentClassChange guid: (GUID fromString: '{86fa936b-3c4d-4835-88a3-9df9f185b725}')!

CommentClassChange comment: ''!

!CommentClassChange categoriesForClass!Refactory-Change Objects! !

!CommentClassChange methodsFor!

asUndoOperation
	self class new comment: ''!

asWebsideJson
	^super asWebsideJson
		at: 'comment' put: comment;
		yourself!

comment: aString
	comment := aString!

fromWebsideJson: json
	super fromWebsideJson: json.
	comment := json at: 'comment' ifAbsent: ['']!

primitiveExecute
	self changeClass comment: comment! !

!CommentClassChange categoriesForMethods!
asUndoOperation!public! !
asWebsideJson!public! !
comment:!public! !
fromWebsideJson:!public! !
primitiveExecute!public! !
!

