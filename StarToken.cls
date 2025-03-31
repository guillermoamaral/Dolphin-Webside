"Filed out from Dolphin Smalltalk"!

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

!StarToken categoriesForMethods!
matchFirstAfterStartOn:!double dispatching!public! !
matchFirstOn:!double dispatching!public! !
matchLastOn:!double dispatching!public! !
printOn:!printing!public! !
!

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

!StarToken class categoriesForMethods!
current!public! !
initialize!initializing!public! !
initializeCurrent!initializing!public! !
!

