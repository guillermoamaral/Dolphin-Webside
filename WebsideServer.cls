﻿"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WebsideServer
	instanceVariableNames: 'server router baseUri port resources'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WebsideServer guid: (GUID fromString: '{4ccc35e6-7d31-4554-8b09-5b16765a0d84}')!
WebsideServer comment: 'WebsideAPI startServer

WebsideAPI stopServer'!
!WebsideServer categoriesForClass!Unclassified! !
!WebsideServer methodsFor!

baseUri
	^ baseUri!

baseUri: aString
	baseUri := aString!

debuggers

defaultBaseUri
	^ '/dolphin'!

defaultPort
	^ 9002!

evaluations

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

initialize

initializeChangesRoutes
	router
		routeGET: '/changes' to: #changes;
		routePOST: '/changes' to: #addChange!

initializeCodeRoutes

initializeDebuggingRoutes
	router
		routeGET: '/debuggers' to: #activeDebuggers;
		routePOST: '/debuggers' to: #createDebugger;
		routeGET: '/debuggers/{id}/frames' to: #debuggerFrames;
		routeGET: '/debuggers/{id}/frames/{index}' to: #debuggerFrame;
		routeGET: '/debuggers/{id}/frames/{index}/bindings' to: #frameBindings;
		routePOST: '/debuggers/{id}/frames/{index}/stepover' to: #stepOverDebugger;
		routePOST: '/debuggers/{id}/frames/{index}/stepinto' to: #stepIntoDebugger;
		routePOST: '/debuggers/{id}/frames/{index}/restart' to: #restartDebugger;
		routePOST: '/debuggers/{id}/resume' to: #resumeDebugger;
		routePOST: '/debuggers/{id}/terminate' to: #terminateDebugger;
		routeDELETE: '/debuggers/{id}' to: #deleteDebugger!

initializeEvaluationRoutes

initializeGeneralRoutes
	router
		routeGET: '/dialect' to: #dialect;
		routeGET: '/logo' to: #logo;
		routeGET: '/colors' to: #colors;
		routePOST: 'save' to: #saveImage!

initializeObjectsRoutes        

initializePreflightRoutes
	router routeOPTIONS: '/*' to: [:request | self handlePreflightRequest: request]	"This is not that well"!

initializeResources

initializeRoutes

initializeServer
	server := HttpServer new.
	server
		addListener: 'WebsideServer'
		at: 'http://localhost:', self port printString , self baseUri
		handler: [:req :res | self handleRequest: req with: res]!

initializeWorkspacesRoutes
	router
		routePOST: '/workspaces' to: #createWorkspace;
		routeGET: '/workspaces' to: #activeWorkspaces;
		routeGET: '/workspaces/{id}' to: #workspace;
		routeDELETE: '/workspaces/{id}' to: #deleteWorkspace!

isPreflight: request
	^request verb = 'OPTIONS'
		and: [(request headerAt: 'origin') notNil or: [(request headerAt: 'Origin') notNil]]!

objects

port
	^ port!

port: anInteger
	port := anInteger!

reset

resourcesAt: aSymbol

resourcesAt: aSymbol put: anObject

start
	self initializeServer.
	server start!

stop
	server stop!

testRuns

workspaces
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
!WebsideServer categoriesFor: #initializeGeneralRoutes!initializing!public! !
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
