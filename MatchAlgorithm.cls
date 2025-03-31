"Filed out from Dolphin Smalltalk"!

Object subclass: #MatchAlgorithm
	instanceVariableNames: 'string pattern start stop tokens failure ranges'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MatchAlgorithm guid: (GUID fromString: '{fb86432e-7484-40e1-9ffd-1f4642dcd99b}')!

MatchAlgorithm comment: ''!

!MatchAlgorithm categoriesForClass!Unclassified! !

!MatchAlgorithm methodsFor!

failBecause: aString
	failure := aString!

failure
	^failure!

hasMatched
	^failure isNil and: [tokens isEmpty]!

hasTokens
	^failure isNil and: [tokens notEmpty]!

index: anInteger
	start := anInteger!

initialize
	super initialize.
	start := 1!

matchFirstStar
	| range |
	range := string start @ nil.
	ranges add: tokens first -> range.
	tokens removeFirst.
	self hasTokens
		ifTrue: [tokens first matchFirstAfterStartOn: self]
		ifFalse: [
			range y: string stop.
			string stop: string start - 1]!

matchFirstToken
	| token min match save |
	token := tokens first.
	min := token length.
	match := false.
	save := string start.
	[
		string length >= min and: [
			match := string beginsWith: token.
			match not]]
		whileTrue: [string start: string start + 1].
	match
		ifTrue: [
			ranges add: token -> (string start @ (string start + min - 1)).
			string start: string start + token length.
			tokens removeFirst]
		ifFalse: [
			tokens size = 1 ifTrue: [
				string start: save.
				^self privateMatchLastToken].
			self
				failBecause: token asString , ' not present at position '
					, string start printString]!

matchFirstTokenAfterStar
	| token i |
	token := tokens first.
	i := string indexOf: token.
	i = 0
		ifTrue: [
			self
				failBecause: token asString , ' not present in inteval ['
					, string start printString
					, ', '
					, string stop printString]
		ifFalse: [
			ranges last value y: i - 1.
			ranges add: token -> (i @ (i + token length - 1)).
			string start: i + token length.
			tokens removeFirst]!

matchLastToken
	tokens size = 1
		ifTrue: [self matchFirstToken]
		ifFalse: [self privateMatchLastToken]!

matchNextToken
	tokens first matchFirstOn: self!

matchRange
	| sorted |
	sorted := (ranges collect: [:r | r value]) select: [:p | p x <= p y].
	sorted := sorted asSortedCollection: [:p :q | p x < q x].
	^sorted notEmpty ifTrue: [sorted first x @ sorted last y]!

pattern: aStringPattern
	pattern := aStringPattern!

privateMatchLastToken
	| token min match |
	token := tokens last.
	min := token length.
	match := false.
	[
		string length >= min and: [
			match := string endsWith: token.
			match not]]
		whileTrue: [string stop: string stop - 1].
	match
		ifTrue: [
			ranges add: token -> (string stop - min + 1 @ string stop).
			string stop: string stop - token length.
			tokens removeLast]
		ifFalse: [
			self
				failBecause: token asString , ' not present at position '
					, (string stop - token length + 1) printString]!

reset
	| end |
	failure := nil.
	tokens := pattern tokens.
	string start: start.
	end := stop notNil ifTrue: [stop] ifFalse: [string string size].
	string stop: end.
	ranges := OrderedCollection new: tokens size!

run
	self reset.
	tokens isEmpty ifTrue: [^self].
	tokens last matchLastOn: self.
	[self hasTokens] whileTrue: [self matchNextToken]!

string: aString
	string := MatchToken on: aString from: 1 to: aString size! !

!MatchAlgorithm categoriesForMethods!
failBecause:!private! !
failure!outputs!public! !
hasMatched!public!testing! !
hasTokens!public!testing! !
index:!inputs!public! !
initialize!initializing!public! !
matchFirstStar!computing!public! !
matchFirstToken!computing!public! !
matchFirstTokenAfterStar!computing!public! !
matchLastToken!computing!public! !
matchNextToken!computing!public! !
matchRange!outputs!public! !
pattern:!inputs!public! !
privateMatchLastToken!computing!public! !
reset!private! !
run!computing!public! !
string:!inputs!public! !
!

!MatchAlgorithm class methodsFor!

new
	^super new initialize! !

!MatchAlgorithm class categoriesForMethods!
new!public! !
!

