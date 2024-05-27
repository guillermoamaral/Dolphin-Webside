﻿"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WebsideAPI
	instanceVariableNames: 'server request'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WebsideAPI guid: (GUID fromString: '{0d468ed7-e8b5-4ad5-a744-fe615bb74dcb}')!
WebsideAPI comment: 'WebsideAPI startServer

WebsideAPI stopServer'!
!WebsideAPI categoriesForClass!Unclassified! !
!WebsideAPI methodsFor!

activeDebuggers
	^self debuggers associations asArray collect: 
			[:a |
			a value asWebsideJson
				at: 'id' put: a key asString;
				at: 'description' put: a value name;
				yourself]!

activeEvaluation

activeEvaluations

activeWorkspaces
	^self workspaces associations asArray collect: 
			[:a |
			a value asWebsideJson
				at: 'id' put: a key asString;
				yourself]!

addChange
	| change |
	change := self requestedChange.
	change ifNil: [^self badRequest: 'Change not supported'].
	[change execute] on: Error do: [:e | ^self compilationError: e].
	^change asWebsideJson!

badRequest: aString
	^HttpServerResponse badRequest content: aString!

bodyAt: aString

bodyAt: aString ifAbsent: aBlock

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

classTreeFrom: aClass depth: anInteger

classTreeFromClasses: aCollection

classVariables

colors
	^self newJsonObject
		at: 'primary' put: '#1d7bb9';
		at: 'secondary' put: '#1565C0';
		yourself!

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

createWorkspace

debugExpression

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

debuggerFrames

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

deleteWorkspace

dialect

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

evaluationError: id

evaluations
	^server evaluations!

filterByCategory: aCollection
	| category |
	category := self queriedCategory.
	^(category notNil and: [category notEmpty])
		ifTrue: [aCollection select: [:m | m category = category]]
		ifFalse: [aCollection]!

filterByVariable: aCollection
	| grouped filtered variable filter var |
	grouped := aCollection groupBy: #methodClass.
	filtered := OrderedCollection new.
	variable := self queriedAccessing.
	filter := false.
	variable
		ifNotNil: 
			[filter := true.
			grouped keysAndValuesDo: 
					[:class :methods |
					filtered addAll: (methods select: [:m | m accessesInstVar: variable]).
					var := class bindingFor: variable.
					var notNil ifTrue: [filtered addAll: (methods select: [:m | m refersToLiteral: var])]]].
	variable := self queriedUsing.
	variable
		ifNotNil: 
			[filter := true.
			grouped keysAndValuesDo: 
					[:class :methods |
					filtered addAll: (methods select: [:m | m readsInstVar: variable]).
					var := class bindingFor: variable.
					var notNil ifTrue: [filtered addAll: (methods select: [:m | m refersToLiteral: var])]]].
	variable := self queriedAssigning.
	variable
		ifNotNil: 
			[filter := true.
			grouped keysAndValuesDo: 
					[:class :methods |
					filtered addAll: (methods select: [:m | m writesInstVar: variable]).
					var := class bindingFor: variable.
					var notNil ifTrue: [filtered addAll: (methods select: [:m | m refersToLiteral: var])]]].
	^filter ifTrue: [filtered asArray] ifFalse: [aCollection]!

frameBindings

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

instanceVariables

instanceVariablesOf: anObject

integerFrom: aString
	aString isInteger ifTrue: [^aString].
	^[Integer fromString: aString] on: Error do: [:e | ]!

logo
'!

method

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
	(self queryAt: 'count') = 'true' ifTrue: [^methods size].
	ast := (self queryAt: 'ast') = 'true'.
	^methods collect: 
			[:m |
			| json |
			json := m asWebsideJson.
			ast ifTrue: [json at: 'ast' put: m parseTree asWebsideJson].
			json]!

namedSlotsOf: anObject

newID

newJsonObject

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
	^self objects associations asArray collect: 
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

pinObjectSlot

queriedAccessing

queriedAssigning
	^self queryAt: 'assigning'!

queriedCategory

queriedClass

queriedReferencing
	^self queriedReferencingClass isNil ifTrue: [self queryAt: 'referencing']!

queriedReferencingClass

queriedReferencingString

queriedScope

queriedSelector

queriedSelectorMatching

queriedSending

queriedUsing
	^self queryAt: 'using'!

queryAt: aString
	^self queryAt: aString ifAbsent: nil!

queryAt: aString ifAbsent: aBlock

queryAt: aString ifPresent: aBlock

queryAt: aString ifPresent: aBlock ifAbsent: anotherBlock

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

requestedSlot

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

saveImage
	^true!

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

unpinAllObjects

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

workspace

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
!WebsideAPI categoriesFor: #colors!general endpoints!public! !
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
!WebsideAPI categoriesFor: #dialect!general endpoints!public! !
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
!WebsideAPI categoriesFor: #logo!general endpoints!public! !
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
!WebsideAPI categoriesFor: #queriedAccessing!private! !
!WebsideAPI categoriesFor: #queriedAssigning!private! !
!WebsideAPI categoriesFor: #queriedCategory!private! !
!WebsideAPI categoriesFor: #queriedClass!private! !
!WebsideAPI categoriesFor: #queriedReferencing!private! !
!WebsideAPI categoriesFor: #queriedReferencingClass!private! !
!WebsideAPI categoriesFor: #queriedReferencingString!private! !
!WebsideAPI categoriesFor: #queriedScope!private! !
!WebsideAPI categoriesFor: #queriedSelector!private! !
!WebsideAPI categoriesFor: #queriedSelectorMatching!private! !
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
!WebsideAPI categoriesFor: #saveImage!general endpoints!public! !
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
