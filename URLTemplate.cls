﻿"Filed out from Dolphin Smalltalk"!

Object subclass: #URLTemplate
	instanceVariableNames: 'raw pattern parameters sample description'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

URLTemplate guid: (GUID fromString: '{7dd6fd49-79ba-4946-9983-eddb03e0d83f}')!

URLTemplate comment: ''!

!URLTemplate categoriesForClass!Unclassified! !

!URLTemplate methodsFor!

= anObject
	^anObject class == self class and: [pattern input = anObject pattern input]!

argumentsFrom: anURL
	| arguments parts uri |
	arguments := Dictionary new.
	uri := self trimSlashesFrom: anURL relativeUri.
	parts := $/ split: uri.
	parameters keysAndValuesDo: [:i :param | | value |
		value := parts at: i ifAbsent: [].
		arguments at: param put: value].
	^arguments!

description
	^description!

description: aString
	description := aString!

hasDescription
	^description size > 0!

hash
	^pattern input hash
!

initializeParameters
	| spec segments |
	parameters := Dictionary new.
	spec := self trimSlashesFrom: raw.
	segments := $/ split: spec.
	segments keysAndValuesDo: [:i :segment | 
		(segment beginsWith: '{')
			ifTrue: [parameters at: i put: segment allButFirst allButLast]]!

initializePattern
	| uri |
	uri := sample copy.
	parameters keysDo: [:i | uri segmentAt: i put: '*'].
	pattern := StringPattern on: uri path!

initializeSample
	| uri |
	uri := raw reject: [:c | c = ${ or: [c = $}]].
	sample := uri asURL.
	parameters keysAndValuesDo: [:i :s | sample segmentAt: (sample segments indexOf: s) put: i printString]!

matches: anURL
	(pattern matches: anURL relativeUri) ifFalse: [^false].
	^(raw endsWith: '*') or: [sample segments size + 1 = anURL segments size]!

on: aString
	raw := aString.
	self initializeParameters; initializeSample; initializePattern!

parameters
	^parameters asArray!

pattern
	^pattern!

printOn: aStream
	aStream nextPutAll: raw!

trimSlashesFrom: aString
	| string |
	string := (aString beginsWith: '/')
		ifTrue: [aString allButFirst]
		ifFalse: [aString].
	(string endsWith: '/') ifTrue: [string := string allButLast].
	^string! !

!URLTemplate categoriesForMethods!
=!comparing!public! !
argumentsFrom:!private! !
description!accessing!public! !
description:!accessing!public! !
hasDescription!private! !
hash!comparing!public! !
initializeParameters!private! !
initializePattern!private! !
initializeSample!private! !
matches:!private! !
on:!private! !
parameters!accessing!public! !
pattern!accessing!public! !
printOn:!printing!public! !
trimSlashesFrom:!private! !
!

!URLTemplate class methodsFor!

on: aString
	^self new on: aString! !

!URLTemplate class categoriesForMethods!
on:!instance creation!public! !
!

