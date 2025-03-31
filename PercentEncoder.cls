"Filed out from Dolphin Smalltalk"!

Object subclass: #PercentEncoder
	instanceVariableNames: 'reserved'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PercentEncoder guid: (GUID fromString: '{183707c5-6b54-4780-b5d8-71fa30c46297}')!

PercentEncoder comment: ''!

!PercentEncoder categoriesForClass!Unclassified! !

!PercentEncoder methodsFor!

decode: aString
	"
	PercentEncoder decode: 'this%20is%20AT+%40'
	"
	| reader raw |
	reader := aString readStream.
	raw := String streamContents: [:strm | 
		[reader atEnd] whileFalse: [| char |
			char := reader next.
			(reader position > 1 and: [char = $+]) ifTrue: [strm space] ifFalse: [| code |
				char = $%
					ifTrue: [
						code := reader next digitValue * 16 + reader next digitValue.
						char := Character utf8Value: code].
				strm nextPut: char]]].
	^String fromUTF8: raw!

encode: aString
	"
	PercentEncoder encode: 'this is AT @'
	"
	^String streamContents: [:strm | 
		aString do: [:char | 
			((reserved includes: char) or: [char codePoint > 127])
				ifTrue: [
					char asUtf8String asByteArray do: [:byte | | hex |
						hex := byte printStringBase: 16.
						strm nextPut: $%; nextPutAll: hex]]
				ifFalse: [strm nextPut: char]]]!

initialize
	super initialize.
	self initializeReserved!

initializeReserved
	reserved := ' ?:@&=+$#;%/\!!'! !

!PercentEncoder categoriesForMethods!
decode:!public!services! !
encode:!public!services! !
initialize!initializing!public! !
initializeReserved!initializing!public! !
!

!PercentEncoder class methodsFor!

current
	Current isNil ifTrue: [Current := self new].
	^Current!

decode: aString
	^self current decode: aString!

encode: aString
	^self current encode: aString!

new
	^super new initialize! !

!PercentEncoder class categoriesForMethods!
current!accessing!public! !
decode:!public!services! !
encode:!public!services! !
new!instance creation!public! !
!

