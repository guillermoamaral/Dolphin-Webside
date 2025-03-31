"Filed out from Dolphin Smalltalk"!

Object subclass: #HttpRequestRouter
	instanceVariableNames: 'routes receiver'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

!HttpRequestRouter categoriesForMethods!
actionFor:verb:!private! !
initialize!initializing!public! !
performAction:for:!private! !
receiver:!accessing!public! !
route:!actions!public! !
route:verb:action:!private! !
route:verb:to:!private! !
routeAllTo:to:!public!services! !
routeDELETE:to:!public!services! !
routeFor:!private! !
routeGET:to:!public!services! !
routeHEAD:to:!public!services! !
routeOPTIONS:to:!public!services! !
routePOST:to:!public!services! !
routePUT:to:!public!services! !
supportedVerbs!public! !
!

!HttpRequestRouter class methodsFor!

new
	^super new initialize! !

!HttpRequestRouter class categoriesForMethods!
new!public! !
!

