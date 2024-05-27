"Filed out from Dolphin Smalltalk 7"!

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
!URL categoriesFor: #,!public!services! !
!URL categoriesFor: #/!public!services! !
!URL categoriesFor: #=!comparing!public! !
!URL categoriesFor: #addPath:!accessing!public! !
!URL categoriesFor: #addSegment:!accessing!public! !
!URL categoriesFor: #addSegments:!accessing!public! !
!URL categoriesFor: #addTrailingSlash!accessing!public! !
!URL categoriesFor: #asHttp!converting!public! !
!URL categoriesFor: #asJson!converting!public! !
!URL categoriesFor: #asURL!converting!public! !
!URL categoriesFor: #asWebSocketURL!converting!public! !
!URL categoriesFor: #asWs!converting!public! !
!URL categoriesFor: #asWss!converting!public! !
!URL categoriesFor: #baseUri!inquiries!public! !
!URL categoriesFor: #baseUri:!accessing!public! !
!URL categoriesFor: #baseUrl!inquiries!public! !
!URL categoriesFor: #beHttp!accessing!public! !
!URL categoriesFor: #beHttps!accessing!public! !
!URL categoriesFor: #beWs!accessing!public! !
!URL categoriesFor: #beWss!accessing!public! !
!URL categoriesFor: #decodeString:!private! !
!URL categoriesFor: #defaultPort!public!services! !
!URL categoriesFor: #encodedFragment:!accessing!public! !
!URL categoriesFor: #encodedPassword:!accessing!public! !
!URL categoriesFor: #encodedPath:!accessing!public! !
!URL categoriesFor: #encodedQueryAt:put:!accessing!public! !
!URL categoriesFor: #encodedUser:!accessing!public! !
!URL categoriesFor: #encodeString:!private! !
!URL categoriesFor: #firstSegment!inquiries!public! !
!URL categoriesFor: #fragment!accessing!public! !
!URL categoriesFor: #fragment:!accessing!public! !
!URL categoriesFor: #hasExplicitPort!public!testing! !
!URL categoriesFor: #hash!comparing!public! !
!URL categoriesFor: #hashFragment!inquiries!public! !
!URL categoriesFor: #hasPath!public!testing! !
!URL categoriesFor: #hasQuery!public!testing! !
!URL categoriesFor: #hasScheme!public!testing! !
!URL categoriesFor: #hasTrailingSlash!public!testing! !
!URL categoriesFor: #host!accessing!public! !
!URL categoriesFor: #host:!accessing!public! !
!URL categoriesFor: #initialize!initializing!public! !
!URL categoriesFor: #isAbsolute!public!testing! !
!URL categoriesFor: #isEmpty!public!testing! !
!URL categoriesFor: #isHttp!public!testing! !
!URL categoriesFor: #isHttps!public!testing! !
!URL categoriesFor: #isRelative!public!testing! !
!URL categoriesFor: #isSecure!public!testing! !
!URL categoriesFor: #isSlash!public!testing! !
!URL categoriesFor: #isWebSocketURL!public!testing! !
!URL categoriesFor: #isWs!public!testing! !
!URL categoriesFor: #isWss!public!testing! !
!URL categoriesFor: #parseAuthorityFrom:!parsing!private! !
!URL categoriesFor: #parseFrom:!parsing!private! !
!URL categoriesFor: #parseHostPortFrom:!parsing!private! !
!URL categoriesFor: #parsePathFrom:!parsing!private! !
!URL categoriesFor: #parseQueryFrom:!parsing!private! !
!URL categoriesFor: #parseUserInfoFrom:!parsing!private! !
!URL categoriesFor: #password!accessing!public! !
!URL categoriesFor: #password:!accessing!public! !
!URL categoriesFor: #path!inquiries!public! !
!URL categoriesFor: #path:!accessing!public! !
!URL categoriesFor: #pathAndQuery!inquiries!public! !
!URL categoriesFor: #port!accessing!public! !
!URL categoriesFor: #port:!accessing!public! !
!URL categoriesFor: #postCopy!copying!public! !
!URL categoriesFor: #printAuthorityOn:!printing!public! !
!URL categoriesFor: #printFragmentOn:!printing!public! !
!URL categoriesFor: #printHostOn:!printing!public! !
!URL categoriesFor: #printOn:!printing!public! !
!URL categoriesFor: #printPathOn:!printing!public! !
!URL categoriesFor: #printPortOn:!printing!public! !
!URL categoriesFor: #printQueryOn:!printing!public! !
!URL categoriesFor: #printQueryOption:on:!printing!public! !
!URL categoriesFor: #printSchemeOn:!printing!public! !
!URL categoriesFor: #printSegments:on:!printing!public! !
!URL categoriesFor: #printWithoutPort!printing!public! !
!URL categoriesFor: #protocol!accessing!public! !
!URL categoriesFor: #protocol:!accessing!public! !
!URL categoriesFor: #query!private! !
!URL categoriesFor: #query:!accessing!public! !
!URL categoriesFor: #queryAt:!inquiries!public! !
!URL categoriesFor: #queryAt:put:!accessing!public! !
!URL categoriesFor: #queryOptions!accessing!public! !
!URL categoriesFor: #queryString!inquiries!public! !
!URL categoriesFor: #relativeUri!inquiries!public! !
!URL categoriesFor: #removeLastSegment!accessing!public! !
!URL categoriesFor: #removeSegment:!accessing!public! !
!URL categoriesFor: #removeSubpath:!accessing!public! !
!URL categoriesFor: #removeTrailingSlash!accessing!public! !
!URL categoriesFor: #replaceQueryAt:with:!accessing!public! !
!URL categoriesFor: #root!inquiries!public! !
!URL categoriesFor: #scheme!accessing!public! !
!URL categoriesFor: #scheme:!accessing!public! !
!URL categoriesFor: #segmentAt:!inquiries!public! !
!URL categoriesFor: #segmentAt:put:!accessing!public! !
!URL categoriesFor: #segments!accessing!public! !
!URL categoriesFor: #segments:!accessing!public! !
!URL categoriesFor: #user!accessing!public! !
!URL categoriesFor: #user:!accessing!public! !

!URL class methodsFor!

fromString: aString
	^self new parseFrom: aString!

new
	^super new initialize! !
!URL class categoriesFor: #fromString:!instance creation!public! !
!URL class categoriesFor: #new!instance creation!public! !

