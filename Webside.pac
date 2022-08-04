| package |
package := Package name: 'Webside'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #HttpRequestRouter;
	add: #MatchAlgorithm;
	add: #MatchToken;
	add: #PercentEncoder;
	add: #StarToken;
	add: #StringPattern;
	add: #URL;
	add: #URLTemplate;
	add: #URLTemplateTest;
	add: #URLTest;
	add: #WebsideAPI;
	add: #WebsideResource;
	add: #WebsideServer;
	add: #WebsideWorkspace;
	yourself.

package methodNames
	add: #AddClassChange -> #asWebsideJson;
	add: #AddClassChange -> #fromWebsideJson:;
	add: #AddMethodChange -> #asWebsideJson;
	add: #AddMethodChange -> #fromWebsideJson:;
	add: #Character -> #isAsterisk;
	add: #Character -> #isQuestionMark;
	add: #ClassDescription -> #asWebsideJson;
	add: #Collection -> #anyone;
	add: #Collection -> #gather:;
	add: #Collection -> #gather:in:;
	add: #Collection -> #groupBy:;
	add: #CompiledMethod -> #asWebsideJson;
	add: #Object -> #asWebsideJson;
	add: #Package -> #asWebsideJson;
	add: #RefactoryChange -> #asWebsideJson;
	add: #RefactoryChange -> #fromWebsideJson:;
	add: #RefactoryClassChange -> #asWebsideJson;
	add: #RefactoryClassChange -> #fromWebsideJson:;
	add: #RefactoryVariableChange -> #asWebsideJson;
	add: #RefactoryVariableChange -> #fromWebsideJson:;
	add: #RemoveMethodChange -> #asWebsideJson;
	add: #RemoveMethodChange -> #fromWebsideJson:;
	add: #RenameClassChange -> #asWebsideJson;
	add: #RenameClassChange -> #fromWebsideJson:;
	add: #RenameVariableChange -> #asWebsideJson;
	add: #RenameVariableChange -> #fromWebsideJson:;
	add: #StAssignmentNode -> #asWebsideJson;
	add: #StCascadeNode -> #asWebsideJson;
	add: #StLiteralToken -> #asWebsideJson;
	add: #StLiteralValueNode -> #asWebsideJson;
	add: #StMessageNode -> #asWebsideJson;
	add: #StMethodNode -> #asWebsideJson;
	add: #StProgramNode -> #asWebsideJson;
	add: #StProgramNode -> #websideType;
	add: #StReturnNode -> #asWebsideJson;
	add: #String -> #asURL;
	add: #String -> #indexOfString:from:to:;
	add: #StSequenceNode -> #asWebsideJson;
	add: #StVariableNode -> #asWebsideJson;
	add: #Symbol -> #evaluateWith:;
	add: 'AddClassChange class' -> #websideType;
	add: 'AddClassVariableChange class' -> #websideType;
	add: 'AddInstanceVariableChange class' -> #websideType;
	add: 'DolphinAddMethodChange class' -> #websideType;
	add: 'RefactoryChange class' -> #acceptsWebsideJson:;
	add: 'RefactoryChange class' -> #classForWebsideJson:;
	add: 'RefactoryChange class' -> #fromWebsideJson:;
	add: 'RefactoryChange class' -> #websideType;
	add: 'RemoveClassChange class' -> #websideType;
	add: 'RemoveClassVariableChange class' -> #websideType;
	add: 'RemoveInstanceVariableChange class' -> #websideType;
	add: 'RemoveMethodChange class' -> #websideType;
	add: 'RenameClassChange class' -> #websideType;
	add: 'RenameClassVariableChange class' -> #websideType;
	add: 'RenameInstanceVariableChange class' -> #websideType;
	add: 'StProgramNode class' -> #websideType;
	add: 'String class' -> #fromUTF8:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Core\Object Arts\Dolphin\IDE\Base\Development System'
	'..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\DolphinHttpServer\DolphinHttpServer\DolphinHttpServer\Dolphin Http Server'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Core\Object Arts\Dolphin\ActiveX\COM\OLE COM'
	'..\Core\Contributions\Refactory\Refactoring Browser\Change Objects\RBChangeObjects'
	'..\Core\Object Arts\Dolphin\System\Compiler\Smalltalk Parser'
	'..\Core\Contributions\svenc\STON\STON-Core'
	'..\Core\Contributions\Camp Smalltalk\SUnit\SUnit').

package!

"Class Definitions"!

Object subclass: #HttpRequestRouter
	instanceVariableNames: 'routes receiver'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MatchAlgorithm
	instanceVariableNames: 'string pattern start stop tokens failure ranges'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MatchToken
	instanceVariableNames: 'string start stop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #PercentEncoder
	instanceVariableNames: 'reserved'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StarToken
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
Object subclass: #StringPattern
	instanceVariableNames: 'stream tokens'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #URL
	instanceVariableNames: 'scheme user password host port segments query fragment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #URLTemplate
	instanceVariableNames: 'raw pattern parameters sample description'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WebsideAPI
	instanceVariableNames: 'server request'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WebsideResource
	instanceVariableNames: 'id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WebsideServer
	instanceVariableNames: 'server router baseUri port resources'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #URLTemplateTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #URLTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WebsideResource subclass: #WebsideWorkspace
	instanceVariableNames: 'bindings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!AddClassChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'definition' put: definition;
		yourself!

fromWebsideJson: json
	| superclass msg keywords index |
	super fromWebsideJson: json.
	definition := json at: 'definition'
				ifAbsent: 
					[superclass := json at: 'superclass'.
					superclass , ' subclass: #' , className
						, ' 
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	classInstanceVariableNames: '''''].
	className isNil
		ifTrue: 
			[msg := SmalltalkParser parseExpression: definition.
			keywords := msg selector keywords collect: #value.
			index := keywords indexOf: 'subclass:'
						ifAbsent: [keywords indexOf: 'variableByteSubclass:' ifAbsent: [keywords indexOf: 'variableSubclass:']].
			className := (msg arguments at: index) value asString]! !
!AddClassChange categoriesFor: #asWebsideJson!public! !
!AddClassChange categoriesFor: #fromWebsideJson:!public! !

!AddClassChange class methodsFor!

websideType
	^'AddClass'! !
!AddClassChange class categoriesFor: #websideType!public! !

!AddClassVariableChange class methodsFor!

websideType
	^'AddClassVariable'! !
!AddClassVariableChange class categoriesFor: #websideType!public! !

!AddInstanceVariableChange class methodsFor!

websideType
	^'AddInstanceVariable'! !
!AddInstanceVariableChange class categoriesFor: #websideType!public! !

!AddMethodChange methodsFor!

asWebsideJson
	| json |
	json := super asWebsideJson.
	source ifNotNil: [:s | json at: 'sourceCode' put: s].
	^json
		at: 'selector' put: self selector;
		at: 'category' put: self protocol;
		yourself!

fromWebsideJson: json
	| category |
	super fromWebsideJson: json.
	selector := json at: 'selector' ifAbsent: [].
	selector ifNotNil: [selector := selector asSymbol].
	source := json at: 'sourceCode' ifAbsent: [].
	category := json at: 'category' ifAbsent: [].
	category ifNil: [category := MethodCategory unclassified asString].
	self protocols: {category asSymbol}! !
!AddMethodChange categoriesFor: #asWebsideJson!public! !
!AddMethodChange categoriesFor: #fromWebsideJson:!public! !

!Character methodsFor!

isAsterisk
	^code = 42!

isQuestionMark
	^code = 63! !
!Character categoriesFor: #isAsterisk!public!testing! !
!Character categoriesFor: #isQuestionMark!public!testing! !

!ClassDescription methodsFor!

asWebsideJson
	| definition |
	definition := self definition copyWithout: 10 asCharacter.
	^super asWebsideJson
		at: 'name' put: self name;
		at: 'definition' put: definition;
		at: 'superclass'
		put: (self superclass ifNotNil: [:c | c name]);
		at: 'comment' put: self comment;
		at: 'variable' put: self isVariable;
		at: 'project' put: (self owningPackage ifNotNil: [:p | p name]);
		yourself! !
!ClassDescription categoriesFor: #asWebsideJson!converting!public! !

!Collection methodsFor!

anyone
	^self detect: [:one | true] ifNone: []!

gather: aBlock
	^self gather: aBlock in: (OrderedCollection new: self size)
!

gather: aBlock in: collection
	self do: [:each | | things |
		things := aBlock value: each.
		collection addAll: things].
	^collection!

groupBy: aspect
	| answer key copy remove |
	answer := Dictionary new.
	(aspect numArgs = 0 or: [aspect class == BlockClosure and: [aspect numArgs = 1]]) ifTrue: [
		self do: [:each | 
			key := aspect evaluateWith: each.
			(answer at: key ifAbsentPut: [OrderedCollection new]) add: each].
		^answer].
	copy := IdentitySet withAll: self.
	remove := IdentitySet new.
	self do: [:each | 
		copy do: [:e | 
			(aspect value: each value: e) ifTrue: [
				remove add: e.
				(answer at: each ifAbsentPut: [OrderedCollection new]) add: e]].
		copy removeAll: remove.
		remove removeAll].
	^answer! !
!Collection categoriesFor: #anyone!adding!public! !
!Collection categoriesFor: #gather:!enumerating!public! !
!Collection categoriesFor: #gather:in:!enumerating!public! !
!Collection categoriesFor: #groupBy:!operations!public! !

!CompiledMethod methodsFor!

asWebsideJson
	| source category |
	source := self getSource copyWithout: 10 asCharacter.
	category := self categories detect: [:c | c isPrivacy not and: [c isVirtual not]] ifNone: [].
	^Dictionary new
		at: 'selector' put: selector;
		at: 'class' put: self methodClass name;
		at: 'category' put: (category ifNotNil: [:c | c name]);
		at: 'source' put: source;
		at: 'author' put: 'self author';
		at: 'timestamp' put: 'self timestamp';
		at: 'project' put: (self owningPackage ifNotNil: [:p | p name]);
		at: 'overriding' put: self isOverride;
		at: 'overriden' put: self isOverridden;
		yourself! !
!CompiledMethod categoriesFor: #asWebsideJson!converting!public! !

!DolphinAddMethodChange class methodsFor!

websideType
	^'AddMethod'! !
!DolphinAddMethodChange class categoriesFor: #websideType!public! !

!Object methodsFor!

asWebsideJson
	| printed |
	printed := [self printString] on: Error
				do: [:e | 'Error while printing ' , self class name , ' instance'].
	^Dictionary new
		at: 'class' put: self class name;
		at: 'indexable' put: self isIndexable;
		at: 'size' put: (self isIndexable ifTrue: [self size] ifFalse: [0]);
		at: 'printString' put: printed;
		yourself! !
!Object categoriesFor: #asWebsideJson!converting!public! !

!Package methodsFor!

asWebsideJson
	| methods |
	methods := Dictionary new.
	self methodNames
		do: [:a | (methods at: a key asString ifAbsentPut: [OrderedCollection new]) add: a value].
	methods keysAndValuesDo: [:k :v | methods at: k put: v asArray].
	^super asWebsideJson
		at: 'name' put: name;
		at: 'classes' put: self classNames asArray;
		at: 'methods' put: methods;
		yourself! !
!Package categoriesFor: #asWebsideJson!converting!private! !

!RefactoryChange methodsFor!

asWebsideJson
	^Dictionary new
		at: 'type' put: self class websideType printString;
		at: 'label' put: self changeString;
		at: 'package' put: nil;
		at: 'timestamp' put: DateAndTime now printString;
		at: 'author' put: nil;
		yourself!

fromWebsideJson: json! !
!RefactoryChange categoriesFor: #asWebsideJson!initialize/release!public! !
!RefactoryChange categoriesFor: #fromWebsideJson:!initialize/release!public! !

!RefactoryChange class methodsFor!

acceptsWebsideJson: json
	| type |
	type := json at: 'type' ifAbsent: nil.
	^self websideType = type!

classForWebsideJson: json
	^self allSubclasses detect: [:c | c acceptsWebsideJson: json] ifNone: []!

fromWebsideJson: json
	| class |
	class := self classForWebsideJson: json.
	^class ifNotNil: [class new fromWebsideJson: json]!

websideType
	^nil! !
!RefactoryChange class categoriesFor: #acceptsWebsideJson:!public! !
!RefactoryChange class categoriesFor: #classForWebsideJson:!public! !
!RefactoryChange class categoriesFor: #fromWebsideJson:!public! !
!RefactoryChange class categoriesFor: #websideType!public! !

!RefactoryClassChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'class' put: self changeClass name asString;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	className := json at: 'class' ifAbsent: [].
	isMeta := className notNil and: [className endsWith: ' class'].
	(className notNil and: [isMeta])
		ifTrue: [className := (className copyFrom: 1 to: className size - 6) asSymbol]! !
!RefactoryClassChange categoriesFor: #asWebsideJson!accessing!public! !
!RefactoryClassChange categoriesFor: #fromWebsideJson:!accessing!public! !

!RefactoryVariableChange methodsFor!

asWebsideJson
	^super asWebsideJson at: 'variable' put: variable; yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	variable := json at: 'variable' ifAbsent: nil! !
!RefactoryVariableChange categoriesFor: #asWebsideJson!initialize/release!public! !
!RefactoryVariableChange categoriesFor: #fromWebsideJson:!initialize/release!public! !

!RemoveClassChange class methodsFor!

websideType
	^'RemoveClass'! !
!RemoveClassChange class categoriesFor: #websideType!public! !

!RemoveClassVariableChange class methodsFor!

websideType
	^'RemoveClassVariable'! !
!RemoveClassVariableChange class categoriesFor: #websideType!public! !

!RemoveInstanceVariableChange class methodsFor!

websideType
	^'RemoveInstanceVariable'! !
!RemoveInstanceVariableChange class categoriesFor: #websideType!public! !

!RemoveMethodChange methodsFor!

asWebsideJson
	^super asWebsideJson at: 'selector' put: selector; yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	selector := json at: 'selector' ifAbsent: [].
	selector notNil ifTrue: [selector := selector asSymbol]! !
!RemoveMethodChange categoriesFor: #asWebsideJson!public! !
!RemoveMethodChange categoriesFor: #fromWebsideJson:!public! !

!RemoveMethodChange class methodsFor!

websideType
	^'RemoveMethod'! !
!RemoveMethodChange class categoriesFor: #websideType!public! !

!RenameClassChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'class' put: oldName;
		at: 'newName' put: newName;
		at: 'renameReferences' put: true;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	oldName := json at: 'class' ifAbsent: [].
	newName := json at: 'newName' ifAbsent: []! !
!RenameClassChange categoriesFor: #asWebsideJson!initialize/release!public! !
!RenameClassChange categoriesFor: #fromWebsideJson:!initialize/release!public! !

!RenameClassChange class methodsFor!

websideType
	^'RenameClass'! !
!RenameClassChange class categoriesFor: #websideType!instance creation!public! !

!RenameClassVariableChange class methodsFor!

websideType
	^'RenameClassVariable'! !
!RenameClassVariableChange class categoriesFor: #websideType!public! !

!RenameInstanceVariableChange class methodsFor!

websideType
	^'RenameInstanceVariable'! !
!RenameInstanceVariableChange class categoriesFor: #websideType!public! !

!RenameVariableChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'class' put: className;
		at: 'variable' put: oldName;
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	className := json at: 'class' ifAbsent: [].
	isMeta := className notNil and: [className endsWith: ' class'].
	oldName := json at: 'variable' ifAbsent: [].
	newName := json at: 'newName' ifAbsent: []! !
!RenameVariableChange categoriesFor: #asWebsideJson!printing!public! !
!RenameVariableChange categoriesFor: #fromWebsideJson:!printing!public! !

!StAssignmentNode methodsFor!

asWebsideJson 	^super asWebsideJson at: 'children' put: { variable asWebsideJson . value asWebsideJson }! !
!StAssignmentNode categoriesFor: #asWebsideJson!accessing!public! !

!StCascadeNode methodsFor!

asWebsideJson
	| children |
	children := messages collect: [:n | n asWebsideJson].
	^super asWebsideJson
		at: 'children' put: children asArray;
		yourself! !
!StCascadeNode categoriesFor: #asWebsideJson!public!visitor! !

!StLiteralToken methodsFor!

asWebsideJson
	^value asString! !
!StLiteralToken categoriesFor: #asWebsideJson!printing!public! !

!StLiteralValueNode methodsFor!

asWebsideJson
	| value |
	value := token value isString ifTrue: [token value] ifFalse: [token printString].
	^super asWebsideJson
		at: 'value' put: value;
		yourself! !
!StLiteralValueNode categoriesFor: #asWebsideJson!matching!public! !

!StMessageNode methodsFor!

asWebsideJson
	| s children |
	s := Dictionary new
				at: 'type' put: 'Selector';
				at: 'value' put: self selector;
				at: 'start' put: selectorParts first start;
				at: 'stop' put: selectorParts last stop;
				yourself.
	children := OrderedCollection with: receiver asWebsideJson with: s.
	arguments do: [:n | children add: n asWebsideJson].
	^super asWebsideJson
		at: 'children' put: children asArray;
		yourself! !
!StMessageNode categoriesFor: #asWebsideJson!matching!public! !

!StMethodNode methodsFor!

asWebsideJson	| children |	children := OrderedCollection with: selector asWebsideJson.	arguments do: [ :n | children add: n asWebsideJson  ].	children add: body asWebsideJson .	^super asWebsideJson at: 'children' put: children asArray; yourself ! !
!StMethodNode categoriesFor: #asWebsideJson!public!visitor! !

!StProgramNode methodsFor!

asWebsideJson	^ Dictionary new		at: 'type' put: self websideType;		at: 'start' put: self start;		at: 'end' put: self stop;		yourself!

websideType	^self class websideType! !
!StProgramNode categoriesFor: #asWebsideJson!public!replacing! !
!StProgramNode categoriesFor: #websideType!public!replacing! !

!StProgramNode class methodsFor!

websideType	^self name copyFrom: 3 to: self name size - 4! !
!StProgramNode class categoriesFor: #websideType!constants!public! !

!StReturnNode methodsFor!

asWebsideJson 	^super asWebsideJson at: 'children' put: { value asWebsideJson  }; yourself! !
!StReturnNode categoriesFor: #asWebsideJson!public!visitor! !

!String methodsFor!

asURL
	^URL fromString: self trimBlanks!

indexOfString: aString from: start to: stop
	| n limit base i |
	n := aString size.
	limit := stop - n.
	base := start - 1.
	i := 1.
	[
		base > limit ifTrue: [^0].
		i <= n]
		whileTrue: [
			i := (self at: base + i) = (aString at: i) ifTrue: [i + 1] ifFalse: [
				base := base + 1.
				1]].
	^i > 1 ifTrue: [base + 1] ifFalse: [0]

! !
!String categoriesFor: #asURL!converting!public! !
!String categoriesFor: #indexOfString:from:to:!comparing!public! !

!String class methodsFor!

fromUTF8: aString
	^(aString anySatisfy: [:byte | byte asInteger >= 128])
		ifTrue: [aString asUtf8String]
		ifFalse: [aString]! !
!String class categoriesFor: #fromUTF8:!instance creation!public! !

!StSequenceNode methodsFor!

asWebsideJson 	| children |	children := OrderedCollection new.	temporaries do: [ :n | children add: n asWebsideJson  ].	statements do: [ :n |  children add: n asWebsideJson].	^super asWebsideJson at: 'children' put: children asArray ; yourself! !
!StSequenceNode categoriesFor: #asWebsideJson!public!visitor! !

!StVariableNode methodsFor!

asWebsideJson	^ super asWebsideJson		at: 'value' put: name;		yourself! !
!StVariableNode categoriesFor: #asWebsideJson!public!visitor! !

!Symbol methodsFor!

evaluateWith: anObject
	^anObject perform: self! !
!Symbol categoriesFor: #evaluateWith:!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

HttpRequestRouter guid: (GUID fromString: '{1ca3c460-0b47-4f5c-b9fb-c6bb4cce7d94}')!
HttpRequestRouter comment: ''!
!HttpRequestRouter categoriesForClass!Unclassified! !
!HttpRequestRouter methodsFor!

actionFor: route verb: verb
	^(routes at: verb) at: route!

initialize
	super initialize.
	routes := Dictionary new.
	self supportedVerbs
		do: [:verb | routes at: verb put: Dictionary new]!

performAction: action for: anHttpRequest
	| target selector arguments |
	(action isKindOf: MessageSend) ifTrue: [
		target := action receiver value.
		(target respondsTo: #request:) ifTrue: [target request: anHttpRequest].
		selector := action selector.
		arguments := anHttpRequest propertyAt: #arguments.
		^target notNil
			ifTrue: [
				(selector numArgs = arguments size and: [target respondsTo: selector])
					ifTrue: [target perform: selector withArguments: arguments asArray]
					ifFalse: [target perform: selector]]
			ifFalse: [action value]].
	^action value: anHttpRequest!

receiver: evaluable
	receiver := evaluable!

route: anHttpRequest
	| route url arguments action |
	route := self routeFor: anHttpRequest.
	route isNil ifTrue: [^HttpServerResponse notFound].
	url := anHttpRequest fullUrl asURL.
	arguments := route argumentsFrom: url.
	anHttpRequest propertyAt: #arguments put: arguments.
	action := self actionFor: route verb: anHttpRequest verb.
	^self performAction: action for: anHttpRequest
!

route: uri verb: method action: selector
	self route: uri verb: method to: selector!

route: uri verb: method to: evaluable
	| template action |
	template := URLTemplate on: uri.
	action := evaluable isSymbol
		ifTrue: [MessageSend new receiver: receiver; selector: evaluable]
		ifFalse: [evaluable].
	(routes at: method) at: template put: action!

routeAllTo: uri to: action
	self supportedVerbs do: [:verb | self route: uri verb: verb action: action]!

routeDELETE: uri to: action
	self route: uri verb: 'DELETE' action: action!

routeFor: anHttpRequest
	| candidates url |
	candidates := routes at: anHttpRequest verb ifAbsent: [^nil].
	url := anHttpRequest fullUrl asString asURL.
	(url hasTrailingSlash and: [url segments size > 2])
		ifTrue: [url removeTrailingSlash]
		ifFalse: [url path = url baseUri ifTrue: [url addTrailingSlash]].
	^candidates keys detect: [:template | template matches: url] ifNone: []!

routeGET: uri to: action
	self route: uri verb: 'GET' action: action!

routeHEAD: uri to: action
	self route: uri verb: 'HEAD' action: action!

routeOPTIONS: uri to: action
	self route: uri verb: 'OPTIONS' action: action!

routePOST: uri to: action
	self route: uri verb: 'POST' action: action!

routePUT: uri to: action
	self route: uri verb: 'PUT' action: action!

supportedVerbs
	^#('GET' 'POST' 'DELETE' 'HEAD' 'PUT' 'OPTIONS')! !
!HttpRequestRouter categoriesFor: #actionFor:verb:!private! !
!HttpRequestRouter categoriesFor: #initialize!initializing!public! !
!HttpRequestRouter categoriesFor: #performAction:for:!private! !
!HttpRequestRouter categoriesFor: #receiver:!accessing!public! !
!HttpRequestRouter categoriesFor: #route:!actions!public! !
!HttpRequestRouter categoriesFor: #route:verb:action:!private! !
!HttpRequestRouter categoriesFor: #route:verb:to:!private! !
!HttpRequestRouter categoriesFor: #routeAllTo:to:!public!services! !
!HttpRequestRouter categoriesFor: #routeDELETE:to:!public!services! !
!HttpRequestRouter categoriesFor: #routeFor:!private! !
!HttpRequestRouter categoriesFor: #routeGET:to:!public!services! !
!HttpRequestRouter categoriesFor: #routeHEAD:to:!public!services! !
!HttpRequestRouter categoriesFor: #routeOPTIONS:to:!public!services! !
!HttpRequestRouter categoriesFor: #routePOST:to:!public!services! !
!HttpRequestRouter categoriesFor: #routePUT:to:!public!services! !
!HttpRequestRouter categoriesFor: #supportedVerbs!public! !

!HttpRequestRouter class methodsFor!

new
	^super new initialize! !
!HttpRequestRouter class categoriesFor: #new!public! !

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
!MatchAlgorithm categoriesFor: #failBecause:!private! !
!MatchAlgorithm categoriesFor: #failure!outputs!public! !
!MatchAlgorithm categoriesFor: #hasMatched!public!testing! !
!MatchAlgorithm categoriesFor: #hasTokens!public!testing! !
!MatchAlgorithm categoriesFor: #index:!inputs!public! !
!MatchAlgorithm categoriesFor: #initialize!initializing!public! !
!MatchAlgorithm categoriesFor: #matchFirstStar!computing!public! !
!MatchAlgorithm categoriesFor: #matchFirstToken!computing!public! !
!MatchAlgorithm categoriesFor: #matchFirstTokenAfterStar!computing!public! !
!MatchAlgorithm categoriesFor: #matchLastToken!computing!public! !
!MatchAlgorithm categoriesFor: #matchNextToken!computing!public! !
!MatchAlgorithm categoriesFor: #matchRange!outputs!public! !
!MatchAlgorithm categoriesFor: #pattern:!inputs!public! !
!MatchAlgorithm categoriesFor: #privateMatchLastToken!computing!public! !
!MatchAlgorithm categoriesFor: #reset!private! !
!MatchAlgorithm categoriesFor: #run!computing!public! !
!MatchAlgorithm categoriesFor: #string:!inputs!public! !

!MatchAlgorithm class methodsFor!

new
	^super new initialize! !
!MatchAlgorithm class categoriesFor: #new!public! !

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
!PercentEncoder categoriesFor: #decode:!public!services! !
!PercentEncoder categoriesFor: #encode:!public!services! !
!PercentEncoder categoriesFor: #initialize!initializing!public! !
!PercentEncoder categoriesFor: #initializeReserved!initializing!public! !

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
!PercentEncoder class categoriesFor: #current!accessing!public! !
!PercentEncoder class categoriesFor: #decode:!public!services! !
!PercentEncoder class categoriesFor: #encode:!public!services! !
!PercentEncoder class categoriesFor: #new!instance creation!public! !

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

printParametersOn: rtf
	self hasQuery ifFalse: [^self].
	rtf
		cr;
		cr;
		useColor: Color darkGray while: [rtf bold: 'Query parameters'];
		cr.
	self queryOptions
		do: [:option | 
			rtf
				bold: option key;
				nextPutAll: ': ' , option value]
		separatedBy: [rtf cr]!

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
!URL categoriesFor: #postCopy!converting!public! !
!URL categoriesFor: #printAuthorityOn:!printing!public! !
!URL categoriesFor: #printFragmentOn:!printing!public! !
!URL categoriesFor: #printHostOn:!printing!public! !
!URL categoriesFor: #printOn:!printing!public! !
!URL categoriesFor: #printParametersOn:!printing!public! !
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

initializeParamenters
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
	self initializeParamenters; initializeSample; initializePattern!

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
!URLTemplate categoriesFor: #=!comparing!public! !
!URLTemplate categoriesFor: #argumentsFrom:!private! !
!URLTemplate categoriesFor: #description!accessing!public! !
!URLTemplate categoriesFor: #description:!accessing!public! !
!URLTemplate categoriesFor: #hasDescription!private! !
!URLTemplate categoriesFor: #hash!comparing!public! !
!URLTemplate categoriesFor: #initializeParamenters!private! !
!URLTemplate categoriesFor: #initializePattern!private! !
!URLTemplate categoriesFor: #initializeSample!private! !
!URLTemplate categoriesFor: #matches:!private! !
!URLTemplate categoriesFor: #on:!private! !
!URLTemplate categoriesFor: #parameters!accessing!public! !
!URLTemplate categoriesFor: #pattern!accessing!public! !
!URLTemplate categoriesFor: #printOn:!printing!public! !
!URLTemplate categoriesFor: #trimSlashesFrom:!private! !

!URLTemplate class methodsFor!

on: aString
	^self new on: aString! !
!URLTemplate class categoriesFor: #on:!instance creation!public! !

WebsideAPI guid: (GUID fromString: '{0d468ed7-e8b5-4ad5-a744-fe615bb74dcb}')!
WebsideAPI comment: 'WebsideAPI startServer

WebsideAPI stopServer'!
!WebsideAPI categoriesForClass!Unclassified! !
!WebsideAPI methodsFor!

activeDebuggers
	^self debuggers associations collect: 
			[:a |
			a value asWebsideJson
				at: 'id' put: a key asString;
				at: 'description' put: a value name;
				yourself]!

activeEvaluation	| id evaluation |	id := self requestedId.	evaluation := self evaluations at: id ifAbsent: [^self notFound].	^self newJsonObject		at: 'id' put: id asString;		yourself!

activeEvaluations	^self evaluations associations collect: 			[:a |			self newJsonObject				at: 'id' put: a key asString;				yourself]!

activeWorkspaces	^ self workspaces associations		collect: [ :a | 			a value asWebsideJson				at: 'id' put: a key asString;				yourself ]!

addChange
	| change |
	change := self requestedChange.
	change ifNil: [^self badRequest: 'Change not supported'].
	[change execute] on: Error do: [:e | ^self compilationError: e].
	^change asWebsideJson!

badRequest: aString
	^HttpServerResponse badRequest content: aString!

bodyAt: aString	^ self bodyAt: aString ifAbsent: []!

bodyAt: aString ifAbsent: aBlock	| json |	json := STONJSON fromString: request body.	^ json at: aString ifAbsent: aBlock!

cancelEvaluation
	| id evaluation |
	id := self requestedId.
	evaluation := self evaluations at: id ifAbsent: [^self notFound].
	evaluation terminate.
	self evaluations removeKey: id.
	^nil!

categories
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^(class realMethodCategories collect: [:c | c name]) asArray!

changes
	| author package changes |
	author := self queryAt: 'author'.
	package := self queriedPackage ifNil: [self systemPackage].
	changes := package changes currentChanges.
	author ifNotNil: [changes := changes select: [:ch | ch author = author]].
	^changes collect: [:ch | ch asWebsideJson]!

classDefinition
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^class asWebsideJson!

classes
	| root tree classes names depth json |
	root := self queryAt: 'root'.
	root := root notNil ifTrue: [self classNamed: root] ifFalse: [self defaultRootClass].
	root ifNil: [^self notFound].
	tree := self queryAt: 'tree'.
	tree = 'true'
		ifTrue: 
			[depth := self queryAt: 'depth'.
			depth := depth notNil ifTrue: [depth := depth asNumber].
			json := self classTreeFrom: root depth: depth.
			^Array with: json].
	classes := root withAllSubclasses asArray.
	names := self queryAt: 'names'.
	names = 'true' ifTrue: [^(classes collect: [:c | c name]) sort].
	^classes collect: [:c | c asWebsideJson]!

classNamed: aString
	| name metaclass class |
	name := aString.
	metaclass := name endsWith: ' class'.
	metaclass ifTrue: [name := name truncateTo: name size - 6].
	class := Smalltalk at: name asSymbol ifAbsent: [^nil].
	^metaclass ifTrue: [class class] ifFalse: [class]!

classTreeFrom: aClass depth: anInteger	| json subclasses depth names |	names := self queryAt: 'names'.	json := names = 'true'				ifTrue: 					[self newJsonObject						at: 'name' put: aClass name;						at: 'superclass' put: (aClass superclass ifNotNil: [:c | c name]);						yourself]				ifFalse: [aClass asWebsideJson].	(anInteger notNil and: [anInteger = 0]) ifTrue: [^json].	depth := anInteger notNil ifTrue: [anInteger - 1].	subclasses := (aClass subclasses asSortedCollection: [:a :b | a name <= b name])				collect: [:c | self classTreeFrom: c depth: depth].	json at: 'subclasses' put: subclasses asArray.	^json!

classTreeFromClasses: aCollection	| roots json subclasses |	roots := Dictionary new.	aCollection		do: 				[:c |				json := self newJsonObject							at: 'name' put: c name;							yourself.				roots at: c name put: json];		do: 				[:c |				c superclass notNil					ifTrue: 						[roots at: c superclass							ifPresent: 								[:sc |								subclasses := sc at: 'subclasses'											ifAbsentPut: [SortedCollection new sortBlock: [:a :b | (a at: 'name') <= (b at: 'name')]].								subclasses add: (roots at: c name)]]];		do: 				[:c |				c superclass notNil					ifTrue: [(roots includesKey: c superclass name) ifTrue: [roots removeKey: c name]]].	^(roots asArray asSortedCollection: [:a :b | (a at: 'name') <= (b at: 'name')]) asArray!

classVariables	| class |	class := self requestedClass.	class ifNil: [^self notFound].	^(class withAllSuperclasses gather: 			[:c |			c classVarNames asArray sort collect: 					[:v |					self newJsonObject						at: 'name' put: v;						at: 'class' put: c name , ' class';						at: 'type' put: 'class';						yourself]])		asArray!

compilationError: aCompilationError
	| error |
	error := STONJSON toString: aCompilationError asWebsideJson.
	^HttpServerResponse new
		statusCode: 409;
		contentType: 'application/json; charset=utf-8';
		content: error asUtf8String!

compilerReceiver
	| context index debugger frame |
	context := self bodyAt: 'context' ifAbsent: [^nil].
	context at: 'class' ifPresent: [:name | ^self classNamed: name].
	context at: 'object' ifPresent: [:id | ^self objects at: (IID fromString: id) ifAbsent: []].
	context at: 'debugger'
		ifPresent: 
			[:id |
			index := context at: 'frame' ifAbsent: [^nil].
			debugger := self debuggers at: (IID fromString: id) ifAbsent: [^nil].
			frame := debugger stack at: index asInteger ifAbsent: nil.
			^frame ifNotNil: [frame receiver]].
	^nil!

createDebugger
	| id process exception context debugger |
	id := self bodyAt: 'evaluation' ifAbsent: [^self notFound].
	id := IID fromString: id.
	process := self evaluations at: id ifAbsent: [^self notFound].
	exception := process suspendedContext exception.
	context := exception signalerContext.
	"process suspendedContext: context."
	debugger := process newDebugSessionNamed: exception description startedAt: context.
	context selector == #doesNotUnderstand: ifTrue: [context := context sender].
	debugger restart: context.
	context selector == #halt
		ifTrue: 
			[debugger
				stepOver;
				stepOver].
	self debuggers at: id put: debugger.
	^debugger asWebsideJson
		at: 'id' put: id asString;
		at: 'description' put: debugger name;
		yourself!

createWorkspace	| id |	id := self newID.	self workspaces at: id put: WebsideWorkspace new.	^ id asString!

debugExpression	| expression method receiver process context debugger id |	expression := self bodyAt: 'expression' ifAbsent: [''].	method := self compiler compile: expression.	receiver := self compilerReceiver.	process := [ method valueWithReceiver: receiver arguments: #() ]		newProcess.	context := process suspendedContext.	debugger := process		newDebugSessionNamed: 'debug it'		startedAt: context.	debugger stepIntoUntil: [ :c | c method == method ].	id := self newID.	self evaluations at: id put: process.	self debuggers at: id put: debugger.	^ id asString!

debuggerFrame
	| debugger index frame interval |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	index := self requestedIndex.
	frame := debugger stack at: index ifAbsent: [^self notFound].
	interval := debugger pcRangeForContext: frame.
	interval := Dictionary new
				at: 'start' put: interval first;
				at: 'end' put: interval last;
				yourself.
	^frame asWebsideJson
		at: 'index' put: index;
		at: 'interval' put: interval;
		yourself!

debuggerFrames	| debugger |	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].	^debugger stack withIndexCollect: 			[:f :i |			self newJsonObject				at: 'index' put: i;				at: 'label' put: f method printString;				yourself]!

debuggers
	^ server debuggers!

defaultRootClass
	^Object!

deleteDebugger
	| id debugger |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [].
	debugger notNil
		ifTrue: 
			[debugger terminate.
			self debuggers removeKey: id ifAbsent: [].
			self evaluations removeKey: id ifAbsent: []].
	^id!

deleteWorkspace	self workspaces		removeKey: self requestedId		ifAbsent: [ ^ self notFound ].	^ nil!

dialect
	^'Dolphin'!

evaluateExpression
	| debug expression sync pin id semaphore object process block json |
	debug := self bodyAt: 'debug'.
	debug == true ifTrue: [^self debugExpression].
	expression := self bodyAt: 'expression'.
	sync := (self bodyAt: 'sync') ifNil: [true].
	pin := (self bodyAt: 'pin') ifNil: [false].
	id := self newID.
	semaphore := Semaphore new.
	block := 
			[[object := self evaluateExpression: expression] on: Exception
				do: 
					[:exception |
					semaphore signal.
					process suspend].
			self evaluations removeKey: id ifAbsent: nil.
			(sync not or: [pin]) ifTrue: [self objects at: id put: object].
			semaphore signal.
			object].
	process := block newProcess.
	self evaluations at: id put: process.
	process resume.
	sync
		ifTrue: 
			[semaphore wait.
			object ifNil: [^self evaluationError: id].
			json := object asWebsideJson.
			pin ifTrue: [json at: 'id' put: id asString].
			^json].
	^Dictionary new
		at: 'id' put: id asString;
		at: 'expression' put: expression;
		yourself!

evaluateExpression: aString
	^Compiler evaluate: aString for: self compilerReceiver!

evaluationError: id	| process json error |	process := self evaluations at: id.	json := self newJsonObject				at: 'description' put: process suspendedFrame exception description;				at: 'evaluation' put: id asString;				yourself.	error := STONJSON toString: json.	^HttpServerResponse new		statusCode: 409;		contentType: 'application/json; charset=utf-8';		content: error asUtf8String!

evaluations
	^server evaluations!

filterByCategory: aCollection
	| category |
	category := self queriedCategory.
	^(category notNil and: [category notEmpty])
		ifTrue: [aCollection select: [:m | m category = category]]
		ifFalse: [aCollection]!

filterByVariable: aCollection
	| grouped filtered variable filter |
	grouped := aCollection groupBy: #methodClass.
	filtered := OrderedCollection new.
	variable := self queriedReferencing.
	filter := false.
	variable ifNotNil: [
		filter := true.
		grouped keysAndValuesDo: [:class :methods | | slot var |
			slot := class allInstVarNames indexOf: variable ifAbsent: nil.
			slot notNil
				ifTrue: [methods
					select: [:m | m referencesInstanceVariable: slot]
					in: filtered].
			var := class classVariableAssociationAt: variable.
			var notNil
				ifTrue: [methods
					select: [:m | m referencesAssociation: slot]
					in: filtered]]].
	variable := self queriedUsing.
	variable ifNotNil: [
		filter := true.
		grouped keysAndValuesDo: [:class :methods | | slot var |
			slot := class allInstVarNames indexOf: variable ifAbsent: nil.
			slot notNil
				ifTrue: [methods
					select: [:m | m usesInstanceVariable: slot]
					in: filtered].
			var := class classVariableAssociationAt: variable.
			var notNil
				ifTrue: [methods select: [:m | m usesAssociation: slot] in: filtered]]].
	variable := self queriedAssigning.
	variable ifNotNil: [
		filter := true.
		grouped keysAndValuesDo: [:class :methods | | slot var |
			slot := class allInstVarNames indexOf: variable ifAbsent: nil.
			slot notNil
				ifTrue: [methods
					select: [:m | m assignsInstanceVariable: slot]
					in: filtered].
			var := class classVariableAssociationAt: variable.
			var notNil
				ifTrue: [methods select: [:m | m assignsAssociation: slot] in: filtered]]].
	^filter ifTrue: [filtered] ifFalse: [aCollection]!

frameBindings	| debugger frame |	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].	frame := debugger stack at: self requestedIndex ifAbsent: [^self notFound].	^#() collect: 			[:b |			self newJsonObject				at: 'name' put: b key asString;				at: 'value' put: b value printString;				yourself]!

implementorsOf: aSymbol
	| system search environment |
	system := SmalltalkSystem current.
	search :=MethodSearch newSelector: aSymbol.
	environment := system systemEnvironment.
	self queriedScope ifNotNil: [:class | environment := environment forClasses: {class}].
	^(system definitionsMatching: search in: environment) allMethods asArray!

indexedSlotsOf: anObject
	| from to slot |
	anObject class isVariable ifFalse: [^self notFound].
	from := self
				queryAt: 'from'
				ifPresent: [:f | self integerFrom: f]
				ifAbsent: [1].
	to := self
				queryAt: 'to'
				ifPresent: [:t | self integerFrom: t]
				ifAbsent: [anObject size].
	^(from to: to) collect: 
			[:i |
			slot := anObject basicAt: i.
			slot asWebsideJson
				at: 'slot' put: i;
				yourself]!

instanceVariables	| class |	class := self requestedClass.	class isNil ifTrue: [^self notFound].	^(class withAllSuperclasses gather: 			[:c |			c instVarNames collect: 					[:v |					self newJsonObject						at: 'name' put: v;						at: 'class' put: c name;						at: 'type' put: 'instance';						yourself]])		asArray!

instanceVariablesOf: anObject	^anObject class allInstVarNames		collect: [ :n | 			self newJsonObject				at: 'name' put: n;				yourself ]!

integerFrom: aString
	aString isInteger ifTrue: [^aString].
	^[Integer fromString: aString] on: Error do: [:e | ]!

method	| class selector method json |	class := self requestedClass.	class ifNil: [^self notFound].	selector := self requestedSelector.	selector ifNil: [^self notFound].	(class includesSelector: selector) ifFalse: [^self notFound]. 	method := class >> selector.	json := method asWebsideJson.   	(self queryAt: 'ast') = 'true'		ifTrue: [ json at: 'ast' put: method parseTree asWebsideJson ].	^json!

methods
	| selector methods senders global references class ast |
	selector := self queriedSelector.
	selector notNil ifTrue: [methods := self implementorsOf: selector].
	selector := self queriedSending.
	selector notNil
		ifTrue: 
			[senders := self sendersOf: selector.
			methods := methods ifNil: [senders] ifNotNil: [methods intersection: senders]].
	global := self queriedReferencingClass.
	global notNil
		ifTrue: 
			[references := self referencesTo: global.
			methods := methods ifNil: [references] ifNotNil: [methods intersection: references]].
	class := self requestedClass ifNil: [self queriedClass].
	(class notNil and: [methods notNil])
		ifTrue: [methods := methods select: [:m | m methodClass == class]].
	methods ifNil: [methods := (class ifNil: [self defaultRootClass]) methodDictionary asArray].
	methods := self filterByCategory: methods.
	methods := self filterByVariable: methods.
	ast := (self queryAt: 'ast') = 'true'.
	^methods collect: 
			[:m |
			| json |
			json := m asWebsideJson.
			ast ifTrue: [json at: 'ast' put: m parseTree asWebsideJson].
			json]!

namedSlotsOf: anObject	| slot |	^anObject class allInstVarNames collect: [ :n |		slot := self slot: n of: anObject ifAbsent: nil.		slot asWebsideJson at: 'slot' put: n; yourself  ]!

newID	^IID newUnique!

newJsonObject	^Dictionary new!

notFound
	^HttpServerResponse notFound!

objects
	^server objects!

package
	| package |
	package := self requestedPackage.
	package ifNil: [^self notFound].
	^package asWebsideJson!

packageClasses
	| package defined extended extra extensions tree names |
	package := self requestedPackage.
	package ifNil: [^self notFound].
	defined := OrderedCollection withAll: package classes.
	extended := self queryAt: 'extended'.
	extensions := extended = 'true'
				ifTrue: 
					[extra := Set new.
					package methods do: [:m | extra add: m methodClass instanceClass].
					extra]
				ifFalse: [#()].
	tree := self queryAt: 'tree'.
	tree = 'true'
		ifTrue: [^(self classTreeFromClasses: defined) , (self classTreeFromClasses: extensions)].
	names := self queryAt: 'names'.
	names = 'true' ifTrue: [^(defined , extensions collect: #name) sort].
	^defined , extensions collect: #asWebsideJson!

packageMethods
	| package |
	package := self requestedPackage.
	package ifNil: [^self notFound].
	^package methods collect: [:m | m asWebsideJson]!

packages
	| manager root packages names |
	manager := PackageManager current.
	root := self queryAt: 'root'.
	root := root notNil ifTrue: [manager packageNamed: root ifNone: []].
	packages := root notNil ifTrue: [{root}] ifFalse: [manager packages].
	names := self queryAt: 'names'.
	names = 'true' ifTrue: [^(packages collect: [:p | p name]) asArray sort].
	^(packages collect: [:p | p asWebsideJson]) asArray!

pinnedObject
	| id object |
	id := self requestedId.
	self evaluations at: id
		ifPresent: [:process | process isSuspended ifTrue: [^self evaluationError: id]].
	object := self objects at: id ifAbsent: [^self notFound].
	^object asWebsideJson
		at: 'id' put: id asString;
		yourself!

pinnedObjects
	^self objects associations collect: 
			[:a |
			a value asWebsideJson
				at: 'id' put: a key asString;
				yourself]!

pinnedObjectSlots
	| id object path index last |
	id := self requestedId.
	object := self objects at: id ifAbsent: [^self notFound].
	path := request fullUrl asURL segments.
	index := path indexOf: 'objects'.
	path
		from: index + 2
		to: path size - 1
		do: 
			[:s |
			object := self
						slot: s
						of: object
						ifAbsent: [^self notFound]].
	last := path last.
	last = 'instance-variables' ifTrue: [^self instanceVariablesOf: object].
	last = 'named-slots' ifTrue: [^self namedSlotsOf: object].
	last = 'indexed-slots' ifTrue: [^self indexedSlotsOf: object].
	object := self
				slot: last
				of: object
				ifAbsent: [^self notFound].
	^object asWebsideJson!

pinObjectSlot	| slot id |	slot := self requestedSlot.	slot ifNil: [ ^ self badRequest: 'Bad object slot URI' ].	id := self newID.	self objects at: id put: slot.	^ slot asWebsideJson		at: 'id' put: id asString;		yourself!

queriedAssigning
	^self queryAt: 'assigning'!

queriedCategory
	| category |
	category := self queryAt: 'category'.
	^category ifNotNil: [category asSymbol]!

queriedClass
	| name |
	name := self queryAt: 'class'.
	^name ifNotNil: [self classNamed: name]!

queriedReferencing
	^self queriedReferencingClass isNil ifTrue: [self queryAt: 'referencing']!

queriedReferencingClass
	| name |
	name := self queryAt: 'referencing'.
	^name notNil ifTrue: [self classNamed: name]!

queriedScope
	| scope |
	scope := self queryAt: 'scope'.
	^scope ifNotNil: [self classNamed: scope]!

queriedSelector
	| selector |
	selector := self queryAt: 'selector'.
	^selector "ifNotNil: [Compiler new findSelector: selector]"!

queriedSending
	| selector |
	selector := self queryAt: 'sending'.
	^selector ifNotNil: [selector asSymbol]!

queriedUsing
	^self queryAt: 'using'!

queryAt: aString
	^self queryAt: aString ifAbsent: nil!

queryAt: aString ifAbsent: aBlock	^self queryAt: aString ifPresent: nil ifAbsent: aBlock!

queryAt: aString ifPresent: aBlock	^self queryAt: aString ifPresent: aBlock ifAbsent: nil!

queryAt: aString ifPresent: aBlock ifAbsent: anotherBlock	| value |	value := request fullUrl asURL queryAt: aString.	value ifNil: [ ^ anotherBlock ifNotNil: [anotherBlock value ]].	^ aBlock notNil		ifTrue: [ aBlock value: value ]		ifFalse: [ value ]!

referencesTo: aClass
	| system global environment |
	system := SmalltalkSystem current.
	global := system globalVariableNamed: aClass name.
	environment := system systemEnvironment.
	self queriedClass ifNotNil: [:class | environment := environment forClasses: {class}].
	^(system referencesToVariable: global in: environment) allMethods asArray
!

request: anHttpRequest
	request := anHttpRequest!

requestedChange
	| json change |
	json := STONJSON fromString: request body.
	change := RefactoryChange fromWebsideJson: json.
	^change!

requestedClass
	| name |
	name := self urlAt: 'name'.
	^name ifNotNil: [self classNamed: name]!

requestedId
	| id |
	id := self urlAt: 'id'.
	^id ifNotNil: [IID fromString: id]!

requestedIndex
	| index |
	index := self urlAt: 'index'.
	^index ifNotNil: [self integerFrom: index]!

requestedPackage
	| name |
	name := self urlAt: 'name'.
	^name ifNotNil: [PackageManager current packageNamed: name ifNone: []]!

requestedSelector
	| selector |
	selector := self urlAt: 'selector'.
	^selector ifNotNil: [selector asSymbol]!

requestedSlot	| uri path index id slot |	uri := self bodyAt: 'uri' ifAbsent: [ ^ nil ].	path := uri subStrings: '/'.	index := path indexOf: 'objects' ifAbsent: [ ^ nil ].	id := path at: index + 1 ifAbsent: [ ^ nil ].	id := IID fromString: id.	slot := self objects at: id ifAbsent: [ ^ nil ].	path		from: index + 2		to: path size		do: [ :s | slot := self slot: s of: slot ifAbsent: [ ^ nil ] ].	^ slot!

restartDebugger
	| debugger context update method |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	context := debugger stack at: self requestedIndex ifAbsent: [^self notFound].
	update := self queryAt: 'update'.
	method := context method.
	(update = 'true' and: [method notNil])
		ifTrue: [context privRefreshWith: method classBinding value >> method selector].
	debugger restart: context.
	^nil!

resumeDebugger
	| id debugger |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [^self notFound].
	self debuggers removeKey: id.
	debugger resume.
	^nil!

sendersOf: aSymbol
	| system search environment |
	system := SmalltalkSystem current.
	search :=MethodSearch newSelector: aSymbol.
	environment := system systemEnvironment.
	self queriedScope ifNotNil: [:class | environment := environment forClasses: {class}].
	^(system referencesMatching: search in: environment) allMethods asArray!

server: aWebsideServer
	server := aWebsideServer!

slot: aString of: anObject ifAbsent: aBlock
	| index |
	(self integerFrom: aString) printString = aString
		ifTrue: 
			[index := self integerFrom: aString.
			(anObject isKindOf: SequenceableCollection)
				ifTrue: 
					[index > anObject size ifTrue: [^aBlock value].
					^[anObject at: index] on: Error do: [:e | anObject basicAt: index]]
				ifFalse: 
					[anObject class isVariable ifTrue: [^anObject at: index].
					index > anObject class instSize ifTrue: [^aBlock value].
					^anObject instVarAt: index]].
	^(anObject class allInstVarNames includes: aString)
		ifTrue: [anObject instVarNamed: aString]
		ifFalse: [aBlock value]!

stepIntoDebugger
	| debugger context |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	context := debugger stack at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepInto: context.
	^nil!

stepOverDebugger
	| debugger context |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	context := debugger stack at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepOver: context.
	^nil!

stepThroughDebugger
	| debugger context |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	context := debugger stack at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepThrough: context.
	^nil!

subclasses
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^(class subclasses collect: [:c | c asWebsideJson]) asArray!

terminateDebugger
	| id debugger |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [^self notFound].
	self debuggers removeKey: id.
	debugger terminate.
	^nil!

unpinAllObjects	self objects removeAll.	^ nil!

unpinObject
	self objects removeKey: self requestedId ifAbsent: [^self notFound].
	^nil!

urlAt: aString
	^(request propertyAt: #arguments) at: aString ifAbsent: []!

variables
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^self instanceVariables , self classVariables!

workspace	^ self workspaces		at: self requestedId		ifAbsent: [ self notFound ]!

workspaces
	^ server workspaces! !
!WebsideAPI categoriesFor: #activeDebuggers!debugging endpoints!public! !
!WebsideAPI categoriesFor: #activeEvaluation!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #activeEvaluations!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #activeWorkspaces!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #addChange!changes endpoints!public! !
!WebsideAPI categoriesFor: #badRequest:!private! !
!WebsideAPI categoriesFor: #bodyAt:!private! !
!WebsideAPI categoriesFor: #bodyAt:ifAbsent:!private! !
!WebsideAPI categoriesFor: #cancelEvaluation!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #categories!code endpoints!public! !
!WebsideAPI categoriesFor: #changes!changes endpoints!public! !
!WebsideAPI categoriesFor: #classDefinition!code endpoints!public! !
!WebsideAPI categoriesFor: #classes!code endpoints!public! !
!WebsideAPI categoriesFor: #classNamed:!private! !
!WebsideAPI categoriesFor: #classTreeFrom:depth:!private! !
!WebsideAPI categoriesFor: #classTreeFromClasses:!private! !
!WebsideAPI categoriesFor: #classVariables!code endpoints!public! !
!WebsideAPI categoriesFor: #compilationError:!private! !
!WebsideAPI categoriesFor: #compilerReceiver!private! !
!WebsideAPI categoriesFor: #createDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #createWorkspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #debugExpression!private! !
!WebsideAPI categoriesFor: #debuggerFrame!debugging endpoints!public! !
!WebsideAPI categoriesFor: #debuggerFrames!debugging endpoints!public! !
!WebsideAPI categoriesFor: #debuggers!private! !
!WebsideAPI categoriesFor: #defaultRootClass!private! !
!WebsideAPI categoriesFor: #deleteDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #deleteWorkspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #dialect!code endpoints!public! !
!WebsideAPI categoriesFor: #evaluateExpression!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #evaluateExpression:!private! !
!WebsideAPI categoriesFor: #evaluationError:!public! !
!WebsideAPI categoriesFor: #evaluations!private! !
!WebsideAPI categoriesFor: #filterByCategory:!private! !
!WebsideAPI categoriesFor: #filterByVariable:!private! !
!WebsideAPI categoriesFor: #frameBindings!debugging endpoints!public! !
!WebsideAPI categoriesFor: #implementorsOf:!private! !
!WebsideAPI categoriesFor: #indexedSlotsOf:!private! !
!WebsideAPI categoriesFor: #instanceVariables!code endpoints!public! !
!WebsideAPI categoriesFor: #instanceVariablesOf:!public! !
!WebsideAPI categoriesFor: #integerFrom:!private! !
!WebsideAPI categoriesFor: #method!code endpoints!public! !
!WebsideAPI categoriesFor: #methods!code endpoints!public! !
!WebsideAPI categoriesFor: #namedSlotsOf:!private! !
!WebsideAPI categoriesFor: #newID!private! !
!WebsideAPI categoriesFor: #newJsonObject!private! !
!WebsideAPI categoriesFor: #notFound!private! !
!WebsideAPI categoriesFor: #objects!private! !
!WebsideAPI categoriesFor: #package!code endpoints!public! !
!WebsideAPI categoriesFor: #packageClasses!code endpoints!public! !
!WebsideAPI categoriesFor: #packageMethods!code endpoints!public! !
!WebsideAPI categoriesFor: #packages!code endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObject!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObjects!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObjectSlots!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinObjectSlot!objects endpoints!public! !
!WebsideAPI categoriesFor: #queriedAssigning!private! !
!WebsideAPI categoriesFor: #queriedCategory!private! !
!WebsideAPI categoriesFor: #queriedClass!private! !
!WebsideAPI categoriesFor: #queriedReferencing!private! !
!WebsideAPI categoriesFor: #queriedReferencingClass!private! !
!WebsideAPI categoriesFor: #queriedScope!private! !
!WebsideAPI categoriesFor: #queriedSelector!private! !
!WebsideAPI categoriesFor: #queriedSending!private! !
!WebsideAPI categoriesFor: #queriedUsing!private! !
!WebsideAPI categoriesFor: #queryAt:!private! !
!WebsideAPI categoriesFor: #queryAt:ifAbsent:!private! !
!WebsideAPI categoriesFor: #queryAt:ifPresent:!private! !
!WebsideAPI categoriesFor: #queryAt:ifPresent:ifAbsent:!private! !
!WebsideAPI categoriesFor: #referencesTo:!private! !
!WebsideAPI categoriesFor: #request:!accessing!public! !
!WebsideAPI categoriesFor: #requestedChange!private! !
!WebsideAPI categoriesFor: #requestedClass!private! !
!WebsideAPI categoriesFor: #requestedId!private! !
!WebsideAPI categoriesFor: #requestedIndex!private! !
!WebsideAPI categoriesFor: #requestedPackage!private! !
!WebsideAPI categoriesFor: #requestedSelector!private! !
!WebsideAPI categoriesFor: #requestedSlot!private! !
!WebsideAPI categoriesFor: #restartDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #resumeDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #sendersOf:!private! !
!WebsideAPI categoriesFor: #server:!accessing!public! !
!WebsideAPI categoriesFor: #slot:of:ifAbsent:!private! !
!WebsideAPI categoriesFor: #stepIntoDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #stepOverDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #stepThroughDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #subclasses!code endpoints!public! !
!WebsideAPI categoriesFor: #terminateDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #unpinAllObjects!objects endpoints!public! !
!WebsideAPI categoriesFor: #unpinObject!objects endpoints!public! !
!WebsideAPI categoriesFor: #urlAt:!private! !
!WebsideAPI categoriesFor: #variables!code endpoints!public! !
!WebsideAPI categoriesFor: #workspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #workspaces!private! !

!WebsideAPI class methodsFor!

startServer
	"
	self startServer
	"

	^WebsideServer new
		baseUri: '/dolphin';
		port: 9002;
		start!

stopServer
	"
	self stopServer
	"

	WebsideServer allInstances do: [:s | s stop]! !
!WebsideAPI class categoriesFor: #startServer!public!services! !
!WebsideAPI class categoriesFor: #stopServer!public!services! !

WebsideResource guid: (GUID fromString: '{fb90d8b3-e1bf-4830-b05d-2d19107070b8}')!
WebsideResource comment: ''!
!WebsideResource categoriesForClass!Unclassified! !
!WebsideResource methodsFor!

asWebsideJson	^super asWebsideJson at: 'id' put: id asString; yourself!

id	^id!

id: anIID	id := anIID!

initialize	super initialize.	id := self class newId! !
!WebsideResource categoriesFor: #asWebsideJson!public! !
!WebsideResource categoriesFor: #id!public! !
!WebsideResource categoriesFor: #id:!public! !
!WebsideResource categoriesFor: #initialize!public! !

!WebsideResource class methodsFor!

new	^super new initialize!

newId	^IID newUnique! !
!WebsideResource class categoriesFor: #new!public! !
!WebsideResource class categoriesFor: #newId!public! !

WebsideServer guid: (GUID fromString: '{4ccc35e6-7d31-4554-8b09-5b16765a0d84}')!
WebsideServer comment: 'WebsideAPI startServer

WebsideAPI stopServer'!
!WebsideServer categoriesForClass!Unclassified! !
!WebsideServer methodsFor!

baseUri
	^ baseUri!

baseUri: aString
	baseUri := aString!

debuggers	^ self resourcesAt: #debuggers!

defaultBaseUri
	^ '/dolphin'!

defaultPort
	^ 9002!

evaluations	^ self resourcesAt: #evaluations!

handlePreflightRequest: request
	| response |
	(self isPreflight: request)
		ifFalse: [^HttpServerResponse internalServerError content: 'Cannot process this request'].
	response := HttpServerResponse ok content: ''.
	request headers headerAt: 'Origin'
		ifPresent: [:origin | response headerAt: 'Access-Control-Allow-Origin' put: origin].
	request headers headerAt: 'Access-Control-Request-Method'
		ifPresent: [:method | response headerAt: 'Access-Control-Allow-Methods' put: method].
	request headers headerAt: 'Access-Control-Request-Headers'
		ifPresent: [:headers | response headerAt: 'Access-Control-Allow-Headers' put: headers].
	^response!

handleRequest: anHttpRequest with: anHttpResponse
	| result error |
	result := [router route: anHttpRequest] on: Error do: [:e | error := e].
	error notNil
		ifTrue: 
			[anHttpResponse
				statusCode: 500;
				content: error description asUtf8String]
		ifFalse: 
			[(result isKindOf: HttpServerResponse)
				ifTrue: 
					[anHttpResponse
						statusCode: result statusCode;
						reason: result reason;
						headers: result headers;
						content: result content]
				ifFalse: 
					[| payload |
					payload := STON toJsonString: result.
					anHttpResponse
						statusCode: 200;
						reason: 'OK';
						contentType: 'application/json; charset=utf-8';
						content: payload asUtf8String]].
	anHttpResponse headerAt: 'Access-Control-Allow-Origin' put: '*'!

initialize	super initialize.	port := self defaultPort.	baseUri := self defaultBaseUri.	router := HttpRequestRouter new.	self initializeRoutes; initializeResources!

initializeChangesRoutes
	router
		routeGET: '/changes' to: #changes;
		routePOST: '/changes' to: #addChange!

initializeCodeRoutes
	router
		routeGET: '/dialect' to: #dialect;
		routeGET: '/packages' to: #packages;
		routeGET: '/packages/{name}' to: #package;
		routeGET: '/packages/{name}/classes'
		to: #packageClasses;
		routeGET: '/packages/{name}/methods'
		to: #packageMethods;
		routeGET: '/classes' to: #classes;
		routeGET: '/classes/{name}' to: #classDefinition;
		routeGET: '/classes/{name}/subclasses' to: #subclasses;
		routeGET: '/classes/{name}/variables' to: #variables;
		routeGET: '/classes/{name}/instance-variables'
			to: #instanceVariables;
		routeGET: '/classes/{name}/class-variables' to: #classVariables;
		routeGET: '/classes/{name}/categories' to: #categories;
		routeGET: '/classes/{name}/methods' to: #methods;
		routeGET: '/classes/{name}/methods/{selector}' to: #method;
		routeGET: '/methods' to: #methods!

initializeDebuggingRoutes
	router
		routePOST: '/debuggers' to: #createDebugger;
		routeGET: '/debuggers/{id}/frames' to: #debuggerFrames;
		routeGET: '/debuggers/{id}/frames/{index}' to: #debuggerFrame;
		routeGET: '/debuggers/{id}/frames/{index}/bindings'
			to: #frameBindings;
		routePOST: '/debuggers/{id}/frames/{index}/stepover'
			to: #stepOverDebugger;
		routePOST: '/debuggers/{id}/frames/{index}/stepinto'
			to: #stepIntoDebugger;
		routePOST: '/debuggers/{id}/frames/{index}/restart'
			to: #restartDebugger;
		routePOST: '/debuggers/{id}/resume' to: #resumeDebugger;
		routePOST: '/debuggers/{id}/terminate' to: #terminateDebugger;
		routeDELETE: '/debuggers/{id}' to: #deleteDebugger!

initializeEvaluationRoutes	router		routePOST: '/evaluations' to: #evaluateExpression;		routeGET: '/evaluations' to: #activeEvaluations;		routeGET: '/evaluations/{id}' to: #activeEvaluation;		routeDELETE: '/evaluations/{id}' to: #cancelEvaluation!

initializeObjectsRoutes            router		routeGET: '/objects' to: #pinnedObjects;		routeGET: '/objects/{id}' to: #pinnedObject;		routeDELETE: '/objects/{id}' to: #unpinObject;		routeGET: '/objects/{id}/*' to: #pinnedObjectSlots;		routePOST: '/objects' to: #pinObjectSlot;		routeDELETE: '/objects' to: #unpinAllObjects    !

initializePreflightRoutes
	router routeOPTIONS: '/*' to: [:request | self handlePreflightRequest: request]	"This is not that well"!

initializeResources	resources := Dictionary new.	resources    	at: #evaluations put: Dictionary new;		at: #objects put: Dictionary new;		at: #workspaces put: Dictionary new;		at: #debuggers put: Dictionary new; at: #testRuns put: Dictionary new!

initializeRoutes
	router receiver: [WebsideAPI new server: self].
	self
		initializePreflightRoutes;
		initializeCodeRoutes;
		initializeChangesRoutes;
		initializeEvaluationRoutes;
		initializeObjectsRoutes;
		initializeWorkspacesRoutes;
		initializeDebuggingRoutes!

initializeServer
	server := HttpServer new.
	server
		addListener: 'WebsideServer'
		at: 'http://localhost:', self port printString , self baseUri
		handler: [:req :res | self handleRequest: req with: res]!

initializeWorkspacesRoutes
	router
		routePOST: '/workspaces' to: #createWorkspace;
		routeGET: '/workspaces' to: #workspaces;
		routeGET: '/workspaces/{id}' to: #workspaces;
		routeDELETE: '/workspaces/{id}' to: #deleteWorkspace!

isPreflight: request
	^request verb = 'OPTIONS'
		and: [(request headerAt: 'origin') notNil or: [(request headerAt: 'Origin') notNil]]!

objects	^ self resourcesAt: #objects!

port
	^ port!

port: anInteger
	port := anInteger!

reset	self debuggers removeAll.	self evaluations removeAll.	self objects removeAll.	self workspaces removeAll!

resourcesAt: aSymbol	^ resources at: aSymbol ifAbsent: nil!

resourcesAt: aSymbol put: anObject	resources at: aSymbol put: anObject!

start
	self initializeServer.
	server start!

stop
	server stop!

testRuns	^ self resourcesAt: #testRuns!

workspaces	^ self resourcesAt: #workspaces! !
!WebsideServer categoriesFor: #baseUri!accessing!public! !
!WebsideServer categoriesFor: #baseUri:!accessing!public! !
!WebsideServer categoriesFor: #debuggers!accessing!public! !
!WebsideServer categoriesFor: #defaultBaseUri!accessing!public! !
!WebsideServer categoriesFor: #defaultPort!accessing!public! !
!WebsideServer categoriesFor: #evaluations!accessing!public! !
!WebsideServer categoriesFor: #handlePreflightRequest:!actions!public! !
!WebsideServer categoriesFor: #handleRequest:with:!actions!public! !
!WebsideServer categoriesFor: #initialize!initializing!public! !
!WebsideServer categoriesFor: #initializeChangesRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeCodeRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeDebuggingRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeEvaluationRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeObjectsRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializePreflightRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeResources!initializing!public! !
!WebsideServer categoriesFor: #initializeRoutes!initializing!public! !
!WebsideServer categoriesFor: #initializeServer!initializing!public! !
!WebsideServer categoriesFor: #initializeWorkspacesRoutes!initializing!public! !
!WebsideServer categoriesFor: #isPreflight:!public!testing! !
!WebsideServer categoriesFor: #objects!accessing!public! !
!WebsideServer categoriesFor: #port!accessing!public! !
!WebsideServer categoriesFor: #port:!accessing!public! !
!WebsideServer categoriesFor: #reset!actions!public! !
!WebsideServer categoriesFor: #resourcesAt:!accessing!public! !
!WebsideServer categoriesFor: #resourcesAt:put:!accessing!public! !
!WebsideServer categoriesFor: #start!actions!public! !
!WebsideServer categoriesFor: #stop!actions!public! !
!WebsideServer categoriesFor: #testRuns!accessing!public! !
!WebsideServer categoriesFor: #workspaces!accessing!public! !

!WebsideServer class methodsFor!

new
	 ^super new initialize! !
!WebsideServer class categoriesFor: #new!instance creation!public! !

URLTemplateTest guid: (GUID fromString: '{56dde46a-6c6a-4bea-93df-0767604a2c75}')!
URLTemplateTest comment: ''!
!URLTemplateTest categoriesForClass!Unclassified! !
!URLTemplateTest methodsFor!

testArguments
	| template url arguments |
	template := URLTemplate on: '/a/{var}/b'.
	url := 'http://host/app/a/26/b' asURL.
	arguments := template argumentsFrom: url.
	self
		assert: arguments size = 1;
		assert: (arguments at: 'var') = '26'!

testMatching
	| template |
	template := URLTemplate on: '/books/{title}/pages'.
	self
		deny: (template matches: '/library/books/hamlet/chapter/3/pages' asURL);
		assert: (template matches: '/library/books/hamlet/pages' asURL).
	template := URLTemplate on: '/books/{title}'.
	self
		deny: (template matches: '/library/books/hamlet/chapters/3/pages' asURL);
		deny: (template matches: '/library/books/hamlet/pages' asURL)!

testTrailingSlash
	| template arguments |
	template := URLTemplate on: '/books/{title}'.
	self assert: (template matches: '/library/books/' asURL).
	arguments := template argumentsFrom: '/library/books/' asURL.
	self assert: (arguments at: 'title') isNil!

testVariablePath
	| template |
	template := URLTemplate on: '/books/{title}/*'.
	self
		assert: (template matches: '/library/books/hamlet/pages' asURL);
		assert: (template matches: '/library/books/hamlet/pages/33' asURL);
		assert: (template
			matches: '/library/books/hamlet/pages/33/paragraphs/22' asURL)! !
!URLTemplateTest categoriesFor: #testArguments!public! !
!URLTemplateTest categoriesFor: #testMatching!public! !
!URLTemplateTest categoriesFor: #testTrailingSlash!public! !
!URLTemplateTest categoriesFor: #testVariablePath!public! !

URLTest guid: (GUID fromString: '{fd4eeb84-43db-47c1-893d-71cf8638bc94}')!
URLTest comment: ''!
!URLTest categoriesForClass!SUnit! !
!URLTest methodsFor!

testAddPath
	| url |
	url := 'http://server/a' asURL.
	url addPath: 'b'.
	self assert: url = 'http://server/a/b' asURL.
	url addPath: 'c/d'.
	self assert: url = 'http://server/a/b/c/d' asURL.
	url addPath: '/e'.
	self assert: url = 'http://server/a/b/c/d/e' asURL!

testBaseUri
	| url |
	url := 'http://server/base' asURL.
	self assert: url baseUri = '/base'.
	url baseUri: 'base2'.
	self assert: url baseUri = '/base2'.
	url := 'http://server/' asURL.
	self assert: url isSlash.
	url baseUri: 'base'.
	self
		deny: url isSlash;
		assert: url baseUri = '/base'.
	url := 'http://server' asURL.
	url baseUri: '/base'.
	self assert: url baseUri = '/base'!

testBaseUrl
	| base |
	base := 'http://server/home' asURL.
	self assert: 'http://server/home' asURL baseUrl = base.
	self assert: 'http://server/home/section' asURL baseUrl = base.
	self assert: 'http://server/home/section/subsection' asURL baseUrl = base.
	base port: 3333.
	self assert: 'http://server:3333/home/section' asURL baseUrl = base!

testConcatenation
	| url |
	url := 'http://host/my' asURL.
	url := url , '-site'.
	self assert: url path = '/my-site'.
	url := 'http://host' asURL.
	url := url , 'my-site'.
	self assert: url path = '/my-site'.
	url := 'relative/url' asURL.
	url := url / 'can/be/a/suffix'.
	self assert: url path = '/relative/url/can/be/a/suffix'!

testDefaultPort
	self
		assert: 'http://server' asURL = 'http://server:80' asURL;
		deny: 'http://server:555' asURL = 'http://server' asURL;
		assert: 'https://server' asURL = 'https://server:443' asURL;
		deny: 'https://server:555' asURL = 'https://server:443' asURL;
		deny: 'https://server:555' asURL = 'https://server' asURL!

testEncodedQuery
	| url |
	url := 'http://host?first%20option=value' asURL.
	self
		assert: (url queryAt: 'first option') = 'value'
		description: 'Query options are decoded by an URL';
		assert: url queryString = '?first%20option=value'
		description: 'Query options are printed encoded'!

testEquality
	'http://www.petrovr.com' asURL = 'HTTP://WWW.PETROVR.COM' asURL!

testFreeFormatter1
	| url |
	url := 'ftp://ftp.is.co.za/rfc/rfc1808.txt' asURL.
	self
		assert: url scheme = 'ftp';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'ftp.is.co.za';
		assert: url port isNil;
		assert: url path = '/rfc/rfc1808.txt';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter10
	| url |
	url := 'https://secured.com:443' asURL.
	self
		assert: url scheme = 'https';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'secured.com';
		assert: url port = 443;
		assert: url path = '';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter11
	| url |
	url := 'ftp://ftp.bogus.com/~some/path/to/a/file.txt' asURL.
	self
		assert: url scheme = 'ftp';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'ftp.bogus.com';
		assert: url port isNil;
		assert: url path = '/~some/path/to/a/file.txt';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter12
	| url |
	url := 'urn:ietf:rfc:2648' asURL.
	self
		assert: url scheme = 'urn';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/ietf:rfc:2648';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter13
	| url |
	url := 'urn:uuid:6e8bc430-9c3a-11d9-9669-0800200c9a66' asURL.
	self
		assert: url scheme = 'urn';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/uuid:6e8bc430-9c3a-11d9-9669-0800200c9a66';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter14
	| url |
	url := 'ftp://username@host.com/' asURL.
	self
		assert: url scheme = 'ftp';
		assert: url user = 'username';
		assert: url password isNil;
		assert: url host = 'host.com';
		assert: url port isNil;
		assert: url path = '/';
		deny: url hasQuery;
		assert: url hashFragment isNil
!

testFreeFormatter15
	| url |
	url := 'username:password@host.com/' asURL.
	self
		assert: url scheme = 'username';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/password@host.com/';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter16
	| url |
	url := 'x:subdomain.domain.com' asURL.
	self
		assert: url scheme = 'x';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/subdomain.domain.com';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter17
	| url |
	url := 'www.superaddress.com:8080' asURL.
	self
		assert: url scheme = 'www.superaddress.com';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/8080';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter18
	| url |
	url := 'http://www.foo.bar/?listings.html#section-2' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.foo.bar';
		deny: url hasExplicitPort;
		assert: url path = '/';
		assert: url queryString = 'listings.html';
		assert: url hashFragment = '#section-2'!

testFreeFormatter19
	| url |
	url := 'http://www.foo.bar/segment1/segment2/some-resource.html' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.foo.bar';
		deny: url hasExplicitPort;
		assert: url path = '/segment1/segment2/some-resource.html';
		deny: url hasQuery;
		assert: url hashFragment isNil!

testFreeFormatter2
	| url |
	url := 'http://www.ietf.org/rfc/rfc2396.txt' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.ietf.org';
		deny: url hasExplicitPort;
		assert: url path = '/rfc/rfc2396.txt';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter20
	| url |
	url := 'http://www.foo.bar/image-2.html?w=100&h=50' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.foo.bar';
		deny: url hasExplicitPort;
		assert: url path = '/image-2.html';
		deny: url queryString = 'w=100&h=50';
		assert: url fragment isNil!

testFreeFormatter21
	| url |
	url := 'ftp://ftp.foo.bar/~john/doe?w=100&h=50' asURL.
	self
		assert: url scheme = 'ftp';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'ftp.foo.bar';
		assert: url port isNil;
		assert: url path = '/~john/doe';
		deny: url queryString = 'w=100&h=50';
		assert: url fragment isNil!

testFreeFormatter22
	| url |
	url := 'http://www.foo.bar/image.jpg?height=150&width=100' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.foo.bar';
		deny: url hasExplicitPort;
		assert: url path = '/image.jpg';
		deny: url queryString = 'height=150&width=100';
		assert: url fragment isNil!

testFreeFormatter23
	| url |
	url := 'https://www.secured.com:443/resource.html?id=6e8bc430-9c3a-11d9-9669-0800200c9a66#some-header' asURL.
	self
		assert: url scheme = 'https';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.secured.com';
		assert: url port = 443;
		assert: url path = '/resource.html';
		deny: url queryString = 'id=6e8bc430-9c3a-11d9-9669-0800200c9a66';
		assert: url fragment = 'some-header'!

testFreeFormatter3
	| url |
	url := 'ldap://[2001:db8::7]/c=GB?objectClass?one' asURL.
	self
		assert: url scheme = 'ldap';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = '2001:db8::7';
		assert: url port isNil;
		assert: url path = '/c=GB';
		assert: url queryString = 'objectClass?one';
		assert: url fragment isNil!

testFreeFormatter4
	| url |
	url := 'news:comp.infosystems.www.servers.unix' asURL.
	self
		assert: url scheme = 'news';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/comp.infosystems.www.servers.unix';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter5
	| url |
	url := 'tel:+1-816-555-1212' asURL.
	self
		assert: url scheme = 'tel';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/+1-816-555-1212';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter6
	| url |
	url := 'telnet://192.0.2.16:80/' asURL.
	self
		assert: url scheme = 'telnet';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = '192.0.2.16';
		assert: url port = 80;
		assert: url path = '/';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter7
	| url |
	url := 'urn:oasis:names:specification:docbook:dtd:xml:4.1.2' asURL
	self
		assert: url scheme = 'urn';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/oasis:names:specification:docbook:dtd:xml:4.1.2';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter8
	| url |
	url := 'http://www.google.com' asURL.
	self
		assert: url scheme = 'http';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'www.google.com';
		deny: url hasExplicitPort;
		assert: url path = '';
		deny: url hasQuery;
		assert: url fragment isNil!

testFreeFormatter9
	| url |
	url := 'http://foo:bar@w1.superman.com/very/long/path.html?p1=v1&p2=v2#more-details' asURL.
	self
		assert: url scheme = 'http';
		assert: url user = 'foo';
		assert: url password  = 'bar';
		assert: url host = 'w1.superman.com';
		deny: url hasExplicitPort;
		assert: url path = '/very/long/path.html';
		assert: (url queryAt: 'p1') = 'v1';
		assert: (url queryAt: 'p2') = 'v2';
		assert: url hashFragment = '#more-details'!

testHost
	| url |
	url := 'http://host/' asURL.
	self
		assert: url notNil;
		assert: url host = 'host'.
	url := 'http://1.180.2.66/' asURL.
	self
		assert: url notNil;
		assert: url host = '1.180.2.66'!

testMakeAbsolute
	| relative absolute url |
	relative := URL fromString: '/people/juliette?age=26'.
	self assert: relative isRelative.
	absolute := URL fromString: 'http://server/root'.
	self assert: absolute isAbsolute.
	url := absolute / relative.
	self
		assert: url isAbsolute;
		assert: url host = 'server';
		assert: url path = '/root/people/juliette';
		assert: (url queryAt: 'age') = '26'!

testODataQueryName
	| url |
	url := 'http://host/people?$filter=Id eq 1' asURL.
	self
		assert: url notNil;
		assert: (url queryAt: '$filter') = 'Id eq 1'!

testPath
	| url |
	url := 'http://host/resource' asURL.
	self
		assert: url notNil;
		assert: url path = '/resource'.
	url := 'http://host/resource/property' asURL.
	self
		assert: url notNil;
		assert: url path = '/resource/property'!

testProtocol
	| url copy |
	url := 'http://server' asURL.
	self assert: url isHttp.
	url beHttps.
	self assert: url isHttps; assert: url isSecure.
	copy := url asHttp.
	self
		deny: copy == url;
		assert: copy isHttp;
		assert: url isHttps.
	url beWs.
	self assert: url isWs.
	url beWss.
	self assert: url isWss!

testQuery
	| url |
	url := 'http://host/people?name=William' asURL.
	self
		assert: url notNil;
		assert: (url queryAt: 'name') = 'William'.
	url := 'http://host/people?name=William%20Jr' asURL.
	self
		assert: url notNil;
		assert: (url queryAt: 'name') = 'William Jr'.
	url := 'http://host/people?name=William Jr' asURL.
	self
		assert: url notNil;
		assert: (url queryAt: 'name') = 'William Jr'.
	url := 'http://host/people?name=' asURL.
	self
		assert: url notNil;
		assert: (url queryAt: 'name') isEmpty!

testRelative
	| url |
	url := 'root/people/david' asURL.
	self
		assert: url notNil;
		assert: url path = '/root/people/david'.
	url := '/people?name=William' asURL.
	self
		assert: url notNil;
		assert: url path = '/people';
		assert: (url queryAt: 'name') = 'William'!

testRemoveSegment
	| url |
	url := 'http://server/a/b/c' asURL.
	url removeSegment: 'a'.
	self assert: url = 'http://server/b/c' asURL.
	url removeSegment: 'c'.
	self assert: url = 'http://server/b' asURL.
	url removeSegment: 'z'.
	self assert: url = 'http://server/b' asURL!

testRemoveSubpath
	| url |
	url := 'http://server/a/b/c' asURL.
	url removeSubpath: 'a/b'.
	self assert: url = 'http://server/c' asURL.
	url := 'http://server/a/b' asURL.
	url removeSubpath: 'a/b/c'.
	self assert: url = 'http://server/a/b' asURL.
	url := 'http://server/a/b/c' asURL.
	url removeSubpath: 'a/b/c'.
	self assert: url = 'http://server' asURL!

testRepeatedOption
	| url |
	url := 'http://hostname/test?option=1&option=2' asURL.
	self assert: url queryOptions size = 2!

testScheme
	| url |
	url := 'http://host/' asURL.
	self
		assert: url notNil;
		assert: url scheme = 'http'.
	url := 'ftp://1.180.2.66/' asURL.
	self
		assert: url notNil;
		assert: url scheme = 'ftp'!

testSlashConcatenation
	| url |
	url := 'http://host/a/' asURL.
	self assert: url / 'b' = 'http://host/a/b' asURL!

testSlashInBetween
	| url |
	url := 'http://server' asURL.
	url path: '/my/path/'.
	self assert: url path = '/my/path/'.
	url addSegment: 'end'.
	self assert: url path = '/my/path/end'!

testTrailingSlash
	| url |
	url := 'http://server/base' asURL.
	self deny: url hasTrailingSlash.
	url addTrailingSlash.
	self
		assert: url hasTrailingSlash;
		assert: url path = '/base/'.
	url addTrailingSlash.
	self assert: url path = '/base/'.
	url := 'http://server.com' asURL.
	self deny: url hasTrailingSlash!

testWikipedia1
	| url |
	url := 'abc://username:password@example.com:123/path/data?key=value&key2=value2#fragid1' asURL.
	self
		assert: url scheme = 'abc';
		assert: url user = 'username';
		assert: url password = 'password';
		assert: url host = 'example.com';
		assert: url port = 123;
		assert: url path = '/path/data';
		assert: (url queryAt: 'key') = 'value';
		assert: (url queryAt: 'key2') = 'value2';
		assert: url fragment = 'fragid1'!

testWikipedia2
	| url |
	url := 'urn:example:mammal:monotreme:echidna' asURL.
	self
		assert: url scheme = 'urn';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/example:mammal:monotreme:echidna';
		deny: url hasQuery;
		assert: url fragment isNil!

testWikipedia3
	"
	//example.org/scheme-relative/URI/with/absolute/path/to/resource.txt /relative/URI/with/absolute/path/to/resource.txt relative/path/to/resource.txt ../../../resource.txt ./resource.txt#frag01 resource.txt #frag01
	"
	| url |
	url := 'https://example.org/absolute/URI/with/absolute/path/to/resource.txt' asURL.
	self
		assert: url scheme = 'https';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'example.org';
		deny: url hasExplicitPort;
		assert: url path = '/absolute/URI/with/absolute/path/to/resource.txt';
		deny: url hasQuery;
		assert: url fragment isNil!

testWikipedia4
	| url |
	url := 'ftp://example.org/resource.txt' asURL.
	self
		assert: url scheme = 'ftp';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host = 'example.org';
		assert: url port isNil;
		assert: url path = '/resource.txt';
		deny: url hasQuery;
		assert: url fragment isNil!

testWikipedia5
	| url |
	url := 'urn:ISSN:1535-3613' asURL.
	self
		assert: url scheme = 'urn';
		assert: url user isNil;
		assert: url password isNil;
		assert: url host isNil;
		assert: url port isNil;
		assert: url path = '/ISSN:1535-3613';
		deny: url hasQuery;
		assert: url fragment isNil! !
!URLTest categoriesFor: #testAddPath!public! !
!URLTest categoriesFor: #testBaseUri!public! !
!URLTest categoriesFor: #testBaseUrl!public! !
!URLTest categoriesFor: #testConcatenation!public! !
!URLTest categoriesFor: #testDefaultPort!public! !
!URLTest categoriesFor: #testEncodedQuery!public! !
!URLTest categoriesFor: #testEquality!public! !
!URLTest categoriesFor: #testFreeFormatter1!public! !
!URLTest categoriesFor: #testFreeFormatter10!public! !
!URLTest categoriesFor: #testFreeFormatter11!public! !
!URLTest categoriesFor: #testFreeFormatter12!public! !
!URLTest categoriesFor: #testFreeFormatter13!public! !
!URLTest categoriesFor: #testFreeFormatter14!public! !
!URLTest categoriesFor: #testFreeFormatter15!public! !
!URLTest categoriesFor: #testFreeFormatter16!public! !
!URLTest categoriesFor: #testFreeFormatter17!public! !
!URLTest categoriesFor: #testFreeFormatter18!public! !
!URLTest categoriesFor: #testFreeFormatter19!public! !
!URLTest categoriesFor: #testFreeFormatter2!public! !
!URLTest categoriesFor: #testFreeFormatter20!public! !
!URLTest categoriesFor: #testFreeFormatter21!public! !
!URLTest categoriesFor: #testFreeFormatter22!public! !
!URLTest categoriesFor: #testFreeFormatter23!public! !
!URLTest categoriesFor: #testFreeFormatter3!public! !
!URLTest categoriesFor: #testFreeFormatter4!public! !
!URLTest categoriesFor: #testFreeFormatter5!public! !
!URLTest categoriesFor: #testFreeFormatter6!public! !
!URLTest categoriesFor: #testFreeFormatter7!public! !
!URLTest categoriesFor: #testFreeFormatter8!public! !
!URLTest categoriesFor: #testFreeFormatter9!public! !
!URLTest categoriesFor: #testHost!public! !
!URLTest categoriesFor: #testMakeAbsolute!public! !
!URLTest categoriesFor: #testODataQueryName!public! !
!URLTest categoriesFor: #testPath!public! !
!URLTest categoriesFor: #testProtocol!public! !
!URLTest categoriesFor: #testQuery!public! !
!URLTest categoriesFor: #testRelative!public! !
!URLTest categoriesFor: #testRemoveSegment!public! !
!URLTest categoriesFor: #testRemoveSubpath!public! !
!URLTest categoriesFor: #testRepeatedOption!public! !
!URLTest categoriesFor: #testScheme!public! !
!URLTest categoriesFor: #testSlashConcatenation!public! !
!URLTest categoriesFor: #testSlashInBetween!public! !
!URLTest categoriesFor: #testTrailingSlash!public! !
!URLTest categoriesFor: #testWikipedia1!public! !
!URLTest categoriesFor: #testWikipedia2!public! !
!URLTest categoriesFor: #testWikipedia3!public! !
!URLTest categoriesFor: #testWikipedia4!public! !
!URLTest categoriesFor: #testWikipedia5!public! !

WebsideWorkspace guid: (GUID fromString: '{dcba7ff2-0ed4-4a98-8467-b131777c73b8}')!
WebsideWorkspace comment: ''!
!WebsideWorkspace categoriesForClass!Unclassified! !
!WebsideWorkspace methodsFor!

declareVariable: aString	bindings at: aString reduced put: nil!

initialize	super initialize.	bindings := Dictionary new!

receiver	^nil! !
!WebsideWorkspace categoriesFor: #declareVariable:!public! !
!WebsideWorkspace categoriesFor: #initialize!public! !
!WebsideWorkspace categoriesFor: #receiver!public! !

"Binary Globals"!

