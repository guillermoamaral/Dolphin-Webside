"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MatchToken
	instanceVariableNames: 'string start stop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MatchToken guid: (GUID fromString: '{ac4831ee-87bf-4c98-a3e7-3d962ed2e1a4}')!
MatchToken comment: ''!
!MatchToken categoriesForClass!Unclassified! !
!MatchToken methodsFor!

asString
	^string copyFrom: start to: stop!

at: i
	^string at: i!

beginsWith: aMatchToken
	self length >= aMatchToken length ifFalse: [^false].
	aMatchToken start to: aMatchToken stop do: [:i | | char |
		char := aMatchToken at: i.
		((self at: start + i - 1) = char or: [char isQuestionMark]) ifFalse: [^false]].
	^true
!

endsWith: aMatchToken
	| end |
	self length >= aMatchToken length ifFalse: [^false].
	end := aMatchToken stop.
	end to: aMatchToken start by: -1 do: [:i | | char |
		char := aMatchToken at: i.
		((self at: stop - end + i) = char or: [char isQuestionMark]) ifFalse: [^false]].
	^true!

indexOf: aMatchToken
	^string indexOfString: aMatchToken asString from: start to: stop!

length
	^stop - start + 1!

matchFirstAfterStartOn: aMatchAlgorithm
	aMatchAlgorithm matchFirstTokenAfterStar!

matchFirstOn: aMatchAlgorithm
	aMatchAlgorithm matchFirstToken!

matchLastOn: aMatchAlgorithm
	aMatchAlgorithm matchLastToken!

printOn: aStream
	start to: stop do: [:i | aStream nextPut: (string at: i)]!

start
	^start!

start: anInteger
	start := anInteger!

stop
	^stop!

stop: anInteger
	stop := anInteger!

string
	^string!

string: aString
	string := aString! !
!MatchToken categoriesFor: #asString!converting!public! !
!MatchToken categoriesFor: #at:!accessing!public! !
!MatchToken categoriesFor: #beginsWith:!inquiries!public! !
!MatchToken categoriesFor: #endsWith:!inquiries!public! !
!MatchToken categoriesFor: #indexOf:!inquiries!public! !
!MatchToken categoriesFor: #length!inquiries!public! !
!MatchToken categoriesFor: #matchFirstAfterStartOn:!double dispatching!public! !
!MatchToken categoriesFor: #matchFirstOn:!double dispatching!public! !
!MatchToken categoriesFor: #matchLastOn:!double dispatching!public! !
!MatchToken categoriesFor: #printOn:!printing!public! !
!MatchToken categoriesFor: #start!accessing!public! !
!MatchToken categoriesFor: #start:!accessing!public! !
!MatchToken categoriesFor: #stop!accessing!public! !
!MatchToken categoriesFor: #stop:!accessing!public! !
!MatchToken categoriesFor: #string!accessing!public! !
!MatchToken categoriesFor: #string:!accessing!public! !

!MatchToken class methodsFor!

on: aString from: start to: stop
	^self new
		string: aString;
		start: start;
		stop: stop! !
!MatchToken class categoriesFor: #on:from:to:!public! !

