"Filed out from Dolphin Smalltalk"!

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

!MatchToken categoriesForMethods!
asString!converting!public! !
at:!accessing!public! !
beginsWith:!inquiries!public! !
endsWith:!inquiries!public! !
indexOf:!inquiries!public! !
length!inquiries!public! !
matchFirstAfterStartOn:!double dispatching!public! !
matchFirstOn:!double dispatching!public! !
matchLastOn:!double dispatching!public! !
printOn:!printing!public! !
start!accessing!public! !
start:!accessing!public! !
stop!accessing!public! !
stop:!accessing!public! !
string!accessing!public! !
string:!accessing!public! !
!

!MatchToken class methodsFor!

on: aString from: start to: stop
	^self new
		string: aString;
		start: start;
		stop: stop! !

!MatchToken class categoriesForMethods!
on:from:to:!public! !
!

