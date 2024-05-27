"Filed out from Dolphin Smalltalk 7"!

Object subclass: #StringPattern
	instanceVariableNames: 'stream tokens'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StringPattern guid: (GUID fromString: '{2e9b341a-0233-41fa-a861-14edc351a4fb}')!
StringPattern comment: ''!
!StringPattern categoriesForClass!Unclassified! !
!StringPattern methodsFor!

addStar
	stream atEnd ifTrue: [^self].
	tokens add: StarToken current.
	stream next.
	[stream peek ifNil: [^self] ifNotNil: [:c | c isAsterisk]]
		whileTrue: [stream next]!

addToken
	| star pos end token |
	star := false.
	pos := stream position + 1.
	[stream atEnd or: [star := stream next isAsterisk]] whileFalse.
	star ifTrue: [stream skip: -1].
	end := stream position.
	pos <= end ifTrue: [
		token := MatchToken on: stream collection from: pos to: end.
		tokens add: token]!

input
	^stream contents!

match: aString
	^self match: aString index: 1!

match: aString index: anInteger
	^self match: aString index: anInteger ifAbsent: nil!

match: aString index: anInteger ifAbsent: aBlock
	| algorithm |
	algorithm := MatchAlgorithm new
		pattern: self;
		string: aString;
		index: anInteger.
	algorithm run.
	algorithm hasMatched ifTrue: [^algorithm matchRange].
	^aBlock notNil ifTrue: [
		aBlock arity = 0
			ifTrue: [aBlock value]
			ifFalse: [aBlock evaluateWith: algorithm failure]]!

matches: aString
	| range |
	range := self match: aString.
	^range notNil and: [range x = 1 and: [range y = aString size]]!

on: aString
	stream := aString readStream.
	tokens := OrderedCollection new.
	self scan!

printOn: aStream
	tokens do: [:t | t printOn: aStream]!

scan
	[stream atEnd] whileFalse: [self addToken; addStar]!

tokens
	^tokens copy! !
!StringPattern categoriesFor: #addStar!public!scanning! !
!StringPattern categoriesFor: #addToken!public!scanning! !
!StringPattern categoriesFor: #input!accessing!public! !
!StringPattern categoriesFor: #match:!inquiries!public! !
!StringPattern categoriesFor: #match:index:!inquiries!public! !
!StringPattern categoriesFor: #match:index:ifAbsent:!inquiries!public! !
!StringPattern categoriesFor: #matches:!inquiries!public! !
!StringPattern categoriesFor: #on:!accessing!public! !
!StringPattern categoriesFor: #printOn:!printing!public! !
!StringPattern categoriesFor: #scan!public!scanning! !
!StringPattern categoriesFor: #tokens!accessing!public! !

!StringPattern class methodsFor!

on: aString
	^self new on: aString! !
!StringPattern class categoriesFor: #on:!instance creation!public! !

