"Filed out from Dolphin Smalltalk 7"!

Object subclass: #StarToken
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
StarToken guid: (GUID fromString: '{101115ed-ae95-41e5-8df0-7c87efce62a9}')!
StarToken comment: ''!
!StarToken categoriesForClass!Unclassified! !
!StarToken methodsFor!

matchFirstAfterStartOn: aMatchAlgorithm
	aMatchAlgorithm matchFirstStar!

matchFirstOn: aMatchAlgorithm
	aMatchAlgorithm matchFirstStar!

matchLastOn: aMatchAlgorithm
	"
	do nothing
	"
	!

printOn: aStream
	aStream nextPut: $*! !
!StarToken categoriesFor: #matchFirstAfterStartOn:!double dispatching!public! !
!StarToken categoriesFor: #matchFirstOn:!double dispatching!public! !
!StarToken categoriesFor: #matchLastOn:!double dispatching!public! !
!StarToken categoriesFor: #printOn:!printing!public! !

!StarToken class methodsFor!

current
	current isNil ifTrue: [self initializeCurrent].
	^current!

initialize
	"
	StarToken initialize
	"
	super initialize.
	self initializeCurrent!

initializeCurrent
	current := self new! !
!StarToken class categoriesFor: #current!public! !
!StarToken class categoriesFor: #initialize!initializing!public! !
!StarToken class categoriesFor: #initializeCurrent!initializing!public! !

