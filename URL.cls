"Filed out from Dolphin Smalltalk"!

Object subclass: #URL
	instanceVariableNames: 'scheme user password host port segments query fragment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

URL guid: (GUID fromString: '{4516f8c3-aa63-452c-8c04-353175f5a128}')!

URL comment: 'URL fromString: ''http://server:123/this/is/the/paht?one=1'''!

!URL categoriesForClass!Unclassified! !

!URL methodsFor!

, aString
	| url segment |
	url := self copy.
	self hasPath
		ifTrue: [
			segment := self isSlash
				ifTrue: [aString]
				ifFalse: [segments last , aString].
			url segmentAt: segments size put: segment]
		ifFalse: [url addSegment: aString].
	^url!

/ anURL
	anURL isString ifTrue: [^self / anURL asURL].
	self assert: [anURL isRelative].
	self assert: [self hasQuery not].
	^self copy removeTrailingSlash
		addSegments: anURL segments;
		query: anURL query!

= anURL
	self class == anURL class ifFalse: [^false].
	scheme = anURL scheme ifFalse: [^false].
	host = anURL host ifFalse: [^false].
	self port = anURL port ifFalse: [^false].
	fragment = anURL fragment ifFalse: [^false].
	(query noDifference: anURL query) ifFalse: [^false].
	segments = anURL segments ifFalse: [^false].
	^true!

addPath: aString
	| parts |
	parts := $/ split: aString.
	parts := parts reject: [:s | s isEmpty].
	self addSegments: parts!

addSegment: aString
	segments add: nil.
	self segmentAt: segments size put: aString!

addSegments: aCollection
	aCollection do: [:segment | self addSegment: segment]!

addTrailingSlash
	self hasTrailingSlash ifFalse: [self addSegment: '/']!

asHttp
	^self isHttp ifTrue: [self] ifFalse: [self copy beHttp]!

asJson
	^self asString!

asURL
	^self!

asWebSocketURL
	^self isSecure ifTrue: [self asWss] ifFalse: [self asWs]!

asWs
	^self isWs ifTrue: [self] ifFalse: [self copy beWs]!

asWss
	^self isWss ifTrue: [self] ifFalse: [self copy beWss]!

baseUri
	| base |
	self isSlash ifTrue: [^'/'].
	base := self hasPath ifTrue: [self segmentAt: 1] ifFalse: [''].
	^'/' , base!

baseUri: aString
	self hasPath
		ifTrue: [self segmentAt: 1 put: aString]
		ifFalse: [self addSegment: aString]!

baseUrl
	^self root / self baseUri!

beHttp
	self scheme: 'http'!

beHttps
	self scheme: 'https'!

beWs
	self scheme: 'ws'!

beWss
	self scheme: 'wss'!

decodeString: aString
	^aString notNil ifTrue: [PercentEncoder decode: aString]!

defaultPort
	(self isHttps or: [self isWss]) ifTrue: [^443].
	(self isHttp or: [self isWs]) ifTrue: [^80].
	^nil!

encodedFragment: aString
	| encoded |
	encoded := self decodeString: aString.
	self fragment: encoded!

encodedPassword: aString
	| decoded |
	decoded := self decodeString: aString.
	self password: decoded!

encodedPath: aString
	| decoded |
	decoded := self decodeString: aString.
	self path: decoded!

encodedQueryAt: aString put: anotherString
	| name value |
	name := self decodeString: aString.
	value := self decodeString: anotherString.
	self queryAt: name put: value!

encodedUser: aString
	| decoded |
	decoded := self decodeString: aString.
	self user: decoded!

encodeString: aString
	^aString notNil ifTrue: [PercentEncoder encode: aString]!

firstSegment
	self isSlash ifTrue: [^nil].
	^self hasPath ifTrue: [self segmentAt: 1]!

fragment
	^fragment!

fragment: aString
	fragment := aString!

hasExplicitPort
	^port notNil!

hash
	^scheme hash * host hash * port hash * fragment hash * query hash * segments hash!

hashFragment
	^self fragment ifNotNil: [:f | '#' , f]!

hasPath
	^segments notEmpty!

hasQuery
	^query notEmpty!

hasScheme
	^scheme notNil!

hasTrailingSlash
	^segments notEmpty and: [segments last = '/']!

host
	^host!

host: aString
	host := (aString notNil and: [aString beginsWith: '['])
		ifTrue: [aString allButFirst allButLast]
		ifFalse: [aString].
	host notNil ifTrue: [host := host asLowercase]!

initialize
	super initialize.
	segments := OrderedCollection new.
	query := OrderedCollection new!

isAbsolute
	^scheme notNil and: [host notNil]!

isEmpty
	^host isNil and: [segments isEmpty]!

isHttp
	^scheme = 'http'!

isHttps
	^scheme = 'https'!

isRelative
	^self isAbsolute not!

isSecure
	^self isHttps or: [self isWss]!

isSlash
	^segments size = 1 and: [segments first = '/']!

isWebSocketURL
	^self isWss or: [self isWs]!

isWs
	^scheme = 'ws'
!

isWss
	^scheme = 'wss'!

parseAuthorityFrom: aString
	| index |
	index := aString indexOf: $@.
	index > 0
		ifTrue: 
			[self parseUserInfoFrom: (aString copyFrom: 1 to: index - 1).
			self parseHostPortFrom: (aString copyFrom: index + 1)]
		ifFalse: [self parseHostPortFrom: aString]!

parseFrom: aString
	| start end index |
	start := 1.
	end := aString size.
	index := aString indexOf: $#.
	index > 0
		ifTrue: 
			[fragment := self decodeString: (aString copyFrom: index + 1 to: end).
			end := index - 1].
	index := aString indexOf: $?.
	index > 0
		ifTrue: 
			[self parseQueryFrom: (aString copyFrom: index + 1 to: end).
			end := index - 1].
	index := aString indexOfSubCollection: '://'.
	(index > 0 and: [index <= end])
		ifTrue: 
			[scheme := aString copyFrom: 1 to: index - 1.
			start := index + 3]
		ifFalse: 
			[index := aString indexOf: $:.
			(index > 0
				and: [index <= end and: [#(#mailto #telnet) includes: (aString copyFrom: 1 to: index - 1)]])
					ifTrue: 
						[scheme := aString copyFrom: 1 to: index - 1.
						start := index + 1]].
	self hasScheme
		ifTrue: 
			[index := aString indexOf: $/ startingAt: start.
			index > 0
				ifTrue: 
					[self parseAuthorityFrom: (aString copyFrom: start to: index - 1).
					start := index]
				ifFalse: [^self parseAuthorityFrom: (aString copyFrom: start to: end)]].
	self parsePathFrom: (aString copyFrom: start to: end)!

parseHostPortFrom: aString
	| stream hostString portNumber |
	stream := aString readStream.
	(hostString := stream upTo: $:) isEmpty ifFalse: [self host: (self decodeString: hostString)].
	stream atEnd
		ifFalse: 
			[portNumber := Integer readFrom: stream.
			(portNumber between: 1 and: 65535) ifFalse: [self error: 'Domain must be between 1 and 65535'].
			port := portNumber]!

parsePathFrom: aString
	| stream |
	stream := aString readStream.
	stream peekFor: $/.
	[stream atEnd] whileFalse: 
			[| segment |
			segment := String streamContents: 
							[:stringStream |
							[stream atEnd not and: [stream peek ~= $/]] whileTrue: [stringStream nextPut: stream next]].
			segment = '.'
				ifFalse: 
					[segment = '..'
						ifTrue: [self removeLastSegment]
						ifFalse: [self addSegment: (self decodeString: segment)]].
			((stream peekFor: $/) and: [stream atEnd]) ifTrue: [self addTrailingSlash]]!

parseQueryFrom: aString
	| stream string index |
	stream := aString readStream.
	[stream atEnd] whileFalse: 
			[string := stream upTo: $&.
			index := string indexOf: $=.
			index > 0
				ifFalse: [query add: (self decodeString: string) -> nil]
				ifTrue: 
					[query add: (self decodeString: (string copyFrom: 1 to: index - 1))
								-> (self decodeString: (string copyFrom: index + 1 to: string size))]]!

parseUserInfoFrom: aString
	| stream userString |
	stream := aString readStream.
	(userString := stream upTo: $:) isEmpty ifFalse: [user := self decodeString: userString].
	stream atEnd ifFalse: [password := self decodeString: stream upToEnd]!

password
	^password!

password: aString
	password := aString!

path
	^String streamContents: [:strm | self printPathOn: strm]!

path: aString
	| path |
	path := $/ split: aString.
	path := path reject: [:s | s isEmpty].
	(aString endsWith: '/') ifTrue: [path := path , #('/')].
	self segments: path!

pathAndQuery
	^String
		streamContents: [:strm | self printPathOn: strm; printQueryOn: strm]!

port
	^port ifNil: [self defaultPort]!

port: anInteger
	port := anInteger!

postCopy
	super postCopy.
	segments := segments copy.
	query := query copy!

printAuthorityOn: aStream
	(user notNil and: [password notNil]) ifTrue: [
		aStream
			nextPutAll: user;
			nextPut: $:;
			nextPutAll: password;
			nextPut: $@]!

printFragmentOn: aStream
	fragment notNil ifTrue: [aStream nextPut: $#; nextPutAll: fragment]!

printHostOn: aStream
	host notNil ifTrue: [aStream nextPutAll: host]!

printOn: aStream
	self
		printSchemeOn: aStream;
		printAuthorityOn: aStream;
		printHostOn: aStream;
		printPortOn: aStream;
		printPathOn: aStream;
		printQueryOn: aStream;
		printFragmentOn: aStream!

printPathOn: aStream
	self printSegments: segments on: aStream!

printPortOn: aStream
	(host notNil and: [port notNil])
		ifTrue: [aStream nextPut: $:; nextPutAll: port printString]!

printQueryOn: aStream
	self hasQuery ifFalse: [^self].
	aStream nextPut: $?.
	query isString ifTrue: [^aStream nextPutAll: query].
	self queryOptions
		do: [:option | self printQueryOption: option on: aStream]
		separatedBy: [aStream nextPut: $&]!

printQueryOption: option on: aStream
	| name value |
	name := self encodeString: option key asString.
	value := self encodeString: option value asString.
	aStream
		nextPutAll: name;
		nextPut: $=;
		nextPutAll: value!

printSchemeOn: aStream
	(host notNil and: [scheme notNil])
		ifTrue: [aStream nextPutAll: scheme; nextPutAll: '://']!

printSegments: aCollection on: aStream
	aCollection
		do: [:segment | segment ~= '/'
			ifTrue: [aStream nextPut: $/; nextPutAll: segment]].
	(aCollection notEmpty and: [aCollection last = '/'])
		ifTrue: [aStream nextPut: $/]!

printWithoutPort
	^String streamContents: [:strm | 
		self
			printSchemeOn: strm;
			printAuthorityOn: strm;
			printHostOn: strm;
			printPathOn: strm;
			printQueryOn: strm;
			printFragmentOn: strm]!

protocol
	^scheme!

protocol: aString
	self scheme: aString!

query
	^query!

query: anOrderedCollection
	anOrderedCollection isNil ifTrue: [^self].
	query := anOrderedCollection!

queryAt: aString
	| option |
	option := query detect: [:o | o key = aString] ifNone: [].
	^option notNil ifTrue: [option value]!

queryAt: name put: value
	query add: name trimBlanks -> value trimBlanks!

queryOptions
	^query!

queryString
	query isString ifTrue: [^query].
	^String streamContents: [:strm | self printQueryOn: strm]!

relativeUri
	segments size < 2 ifTrue: [^'/'].
	^String
		streamContents: [:strm | self printSegments: segments allButFirst on: strm]!

removeLastSegment
	self hasPath ifTrue: [segments removeLast]!

removeSegment: aString
	segments remove: aString ifAbsent: []!

removeSubpath: aString
	| subpath index |
	subpath := $/ split: aString.
	subpath := subpath reject: [:s | s isEmpty].
	index := segments indexOfSubCollection: subpath.
	index = 0 ifTrue: [^self].
	segments := (segments copyFrom: 1 to: index - 1)
		, (segments copyFrom: index + subpath size)!

removeTrailingSlash
	self hasTrailingSlash ifTrue: [segments removeLast]!

replaceQueryAt: name with: value
	| option |
	option := query detect: [:o | o key = name] ifNone: nil.
	^option notNil ifTrue: [option value: value]!

root
	^self class new
		scheme: scheme;
		host: host;
		port: port!

scheme
	^scheme!

scheme: aString
	scheme := aString notNil ifTrue: [aString asLowercase]!

segmentAt: anInteger
	^segments at: anInteger!

segmentAt: anInteger put: aString
	| segment |
	segment := (aString first = $/ and: [aString ~= '/'])
		ifTrue: [aString allButFirst]
		ifFalse: [aString].
	segments at: anInteger put: segment!

segments
	^segments!

segments: aCollection
	segments := aCollection asOrderedCollection!

user
	^user!

user: aString
	user := aString! !

!URL categoriesForMethods!
,!public!services! !
/!public!services! !
=!comparing!public! !
addPath:!accessing!public! !
addSegment:!accessing!public! !
addSegments:!accessing!public! !
addTrailingSlash!accessing!public! !
asHttp!converting!public! !
asJson!converting!public! !
asURL!converting!public! !
asWebSocketURL!converting!public! !
asWs!converting!public! !
asWss!converting!public! !
baseUri!inquiries!public! !
baseUri:!accessing!public! !
baseUrl!inquiries!public! !
beHttp!accessing!public! !
beHttps!accessing!public! !
beWs!accessing!public! !
beWss!accessing!public! !
decodeString:!private! !
defaultPort!public!services! !
encodedFragment:!accessing!public! !
encodedPassword:!accessing!public! !
encodedPath:!accessing!public! !
encodedQueryAt:put:!accessing!public! !
encodedUser:!accessing!public! !
encodeString:!private! !
firstSegment!inquiries!public! !
fragment!accessing!public! !
fragment:!accessing!public! !
hasExplicitPort!public!testing! !
hash!comparing!public! !
hashFragment!inquiries!public! !
hasPath!public!testing! !
hasQuery!public!testing! !
hasScheme!public!testing! !
hasTrailingSlash!public!testing! !
host!accessing!public! !
host:!accessing!public! !
initialize!initializing!public! !
isAbsolute!public!testing! !
isEmpty!public!testing! !
isHttp!public!testing! !
isHttps!public!testing! !
isRelative!public!testing! !
isSecure!public!testing! !
isSlash!public!testing! !
isWebSocketURL!public!testing! !
isWs!public!testing! !
isWss!public!testing! !
parseAuthorityFrom:!parsing!private! !
parseFrom:!parsing!private! !
parseHostPortFrom:!parsing!private! !
parsePathFrom:!parsing!private! !
parseQueryFrom:!parsing!private! !
parseUserInfoFrom:!parsing!private! !
password!accessing!public! !
password:!accessing!public! !
path!inquiries!public! !
path:!accessing!public! !
pathAndQuery!inquiries!public! !
port!accessing!public! !
port:!accessing!public! !
postCopy!copying!public! !
printAuthorityOn:!printing!public! !
printFragmentOn:!printing!public! !
printHostOn:!printing!public! !
printOn:!printing!public! !
printPathOn:!printing!public! !
printPortOn:!printing!public! !
printQueryOn:!printing!public! !
printQueryOption:on:!printing!public! !
printSchemeOn:!printing!public! !
printSegments:on:!printing!public! !
printWithoutPort!printing!public! !
protocol!accessing!public! !
protocol:!accessing!public! !
query!private! !
query:!accessing!public! !
queryAt:!inquiries!public! !
queryAt:put:!accessing!public! !
queryOptions!accessing!public! !
queryString!inquiries!public! !
relativeUri!inquiries!public! !
removeLastSegment!accessing!public! !
removeSegment:!accessing!public! !
removeSubpath:!accessing!public! !
removeTrailingSlash!accessing!public! !
replaceQueryAt:with:!accessing!public! !
root!inquiries!public! !
scheme!accessing!public! !
scheme:!accessing!public! !
segmentAt:!inquiries!public! !
segmentAt:put:!accessing!public! !
segments!accessing!public! !
segments:!accessing!public! !
user!accessing!public! !
user:!accessing!public! !
!

!URL class methodsFor!

fromString: aString
	^self new parseFrom: aString!

new
	^super new initialize! !

!URL class categoriesForMethods!
fromString:!instance creation!public! !
new!instance creation!public! !
!

