| package |
package := Package name: 'Webside'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AddCategoryChange;
	add: #AddPackageChange;
	add: #ClassifyMethodChange;
	add: #CommentClassChange;
	add: #HttpRequestRouter;
	add: #MatchAlgorithm;
	add: #MatchToken;
	add: #PercentEncoder;
	add: #RefactoryCategoryChange;
	add: #RefactoryPackageChange;
	add: #RemoveCategoryChange;
	add: #RemovePackageChange;
	add: #RenameCategoryChange;
	add: #RenamePackageChange;
	add: #StarToken;
	add: #StringPattern;
	add: #URL;
	add: #URLTemplate;
	add: #URLTemplateTest;
	add: #URLTest;
	add: #WebsideAPI;
	add: #WebsideClient;
	add: #WebsideEvaluation;
	add: #WebsideResource;
	add: #WebsideServer;
	add: #WebsideWorkspace;
	yourself.

package methodNames
	add: #AddClassChange -> #asWebsideJson;
	add: #AddClassChange -> #fromWebsideJson:;
	add: #AddClassChange -> #websideExecute;
	add: #AddMethodChange -> #asWebsideJson;
	add: #AddMethodChange -> #fromWebsideJson:;
	add: #Character -> #isAsterisk;
	add: #Character -> #isQuestionMark;
	add: #ClassDescription -> #asWebsideJson;
	add: #Collection -> #anyone;
	add: #Collection -> #gather:;
	add: #Collection -> #gather:in:;
	add: #Collection -> #groupBy:;
	add: #CompiledCode -> #asWebsideJson;
	add: #CompiledMethod -> #asWebsideJson;
	add: #CompilerNotification -> #asWebsideJson;
	add: #Debugger -> #process;
	add: #Debugger -> #stepInto:;
	add: #Debugger -> #stepIntoBlock:;
	add: #Debugger -> #stepOver:;
	add: #Exception -> #asWebsideJson;
	add: #MethodCompileFailed -> #asWebsideJson;
	add: #MethodRefactoring -> #asWebsideJson;
	add: #MethodRefactoring -> #fromWebsideJson:;
	add: #Object -> #asWebsideJson;
	add: #Object -> #websideIconName;
	add: #Object -> #websideViews;
	add: #Package -> #asWebsideJson;
	add: #PullUpInstanceVariableRefactoring -> #fromWebsideJson:;
	add: #Refactoring -> #asWebsideJson;
	add: #Refactoring -> #fromWebsideJson:;
	add: #Refactoring -> #websideExecute;
	add: #RefactoryChange -> #asWebsideJson;
	add: #RefactoryChange -> #fromWebsideJson:;
	add: #RefactoryChange -> #websideExecute;
	add: #RefactoryClassChange -> #asWebsideJson;
	add: #RefactoryClassChange -> #fromWebsideJson:;
	add: #RefactoryVariableChange -> #asWebsideJson;
	add: #RefactoryVariableChange -> #fromWebsideJson:;
	add: #RemoveMethodChange -> #asWebsideJson;
	add: #RemoveMethodChange -> #fromWebsideJson:;
	add: #RenameClassChange -> #asWebsideJson;
	add: #RenameClassChange -> #fromWebsideJson:;
	add: #RenameMethodRefactoring -> #asWebsideJson;
	add: #RenameMethodRefactoring -> #fromWebsideJson:;
	add: #RenameVariableChange -> #asWebsideJson;
	add: #RenameVariableChange -> #fromWebsideJson:;
	add: #StackFrame -> #asWebsideJson;
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
	add: #String -> #includesString:;
	add: #String -> #indexOfString:from:to:;
	add: #StSequenceNode -> #asWebsideJson;
	add: #StVariableNode -> #asWebsideJson;
	add: #Symbol -> #evaluateWith:;
	add: #VariableRefactoring -> #asWebsideJson;
	add: #VariableRefactoring -> #fromWebsideJson:;
	add: 'AddClassChange class' -> #websideType;
	add: 'AddClassVariableChange class' -> #websideType;
	add: 'AddInstanceVariableChange class' -> #websideType;
	add: 'AddMethodChange class' -> #websideType;
	add: 'DolphinAddMethodChange class' -> #websideType;
	add: 'PullUpInstanceVariableRefactoring class' -> #websideType;
	add: 'PushDownInstanceVariableRefactoring class' -> #websideType;
	add: 'Refactoring class' -> #acceptsWebsideJson:;
	add: 'Refactoring class' -> #classForWebsideJson:;
	add: 'Refactoring class' -> #fromWebsideJson:;
	add: 'Refactoring class' -> #websideType;
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
	'..\Core\Object Arts\Dolphin\System\Base64\Dolphin Base64'
	'..\DolphinHttpServer\DolphinHttpServer\DolphinHttpServer\Dolphin Http Server'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Core\Object Arts\Dolphin\MVP\Icons\Dolphin Text Tile Icons'
	'..\Core\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus'
	'..\Core\Object Arts\Dolphin\ActiveX\COM\OLE COM'
	'..\Core\Contributions\Refactory\Refactoring Browser\Change Objects\RBChangeObjects'
	'..\Core\Contributions\Refactory\Refactoring Browser\Environments\RBEnvironments'
	'..\Core\Contributions\Refactory\Refactoring Browser\Refactorings\RBRefactorings'
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
Object subclass: #WebsideClient
	instanceVariableNames: 'url client'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WebsideEvaluation
	instanceVariableNames: 'id expression receiver context requestor priority process state result error'
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
RefactoryChange subclass: #RefactoryPackageChange
	instanceVariableNames: 'packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryClassChange subclass: #ClassifyMethodChange
	instanceVariableNames: 'selector category'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryClassChange subclass: #CommentClassChange
	instanceVariableNames: 'comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryClassChange subclass: #RefactoryCategoryChange
	instanceVariableNames: 'category'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryCategoryChange subclass: #AddCategoryChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryCategoryChange subclass: #RemoveCategoryChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryCategoryChange subclass: #RenameCategoryChange
	instanceVariableNames: 'newName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryPackageChange subclass: #AddPackageChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryPackageChange subclass: #RemovePackageChange
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RefactoryPackageChange subclass: #RenamePackageChange
	instanceVariableNames: 'newName'
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
	instanceVariableNames: 'contents bindings'
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
	| superclass msg keywords index instanceVariables classVariables poolDictionaries |
	super fromWebsideJson: json.
	definition := json at: 'definition' ifAbsent: [].
	definition
		ifNil: 
			[superclass := json at: 'superclass' ifAbsent: [].
			superclass
				ifNotNil: 
					[instanceVariables := json at: 'instanceVariables' ifAbsent: [#()].
					classVariables := json at: 'classVariables' ifAbsent: [#()].
					poolDictionaries := json at: 'poolDictionaries' ifAbsent: [#()].
					definition := String streamContents: 
									[:s |
									s
										nextPutAll: superclass;
										nextPutAll: ' subclass: ';
										nextPutAll: className printString;
										nextPutAll: ' instanceVariableNames: '''.
									instanceVariables do: [:n | s nextPutAll: n] separatedBy: [s space].
									s nextPutAll: ''' classVariableNames: '''.
									classVariables do: [:n | s nextPutAll: n] separatedBy: [s space].
									s nextPutAll: ''' poolDictionaries: '''.
									poolDictionaries do: [:n | s nextPutAll: n] separatedBy: [s space].
									s nextPut: $']]].
	className
		ifNil: 
			[msg := SmalltalkParser parseExpression: definition.
			keywords := msg selector keywords collect: [:s | s asString].
			index := keywords indexOf: 'subclass:'
						ifAbsent: [keywords indexOf: 'variableByteSubclass:' ifAbsent: [keywords indexOf: 'variableSubclass:']].
			className := (msg arguments at: index) value asString]!

websideExecute
	| class packageName package |
	super websideExecute.
	class := self changeClass.
	class ifNil: [^self].
	packageName := self propertyAt: #packageName ifAbsent: [^self].
	package := PackageManager current packageNamed: packageName ifNone: [^self].
	package ifNotNil: [package addClass: self changeClass]! !
!AddClassChange categoriesFor: #asWebsideJson!public! !
!AddClassChange categoriesFor: #fromWebsideJson:!public! !
!AddClassChange categoriesFor: #websideExecute!public! !

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

!AddMethodChange class methodsFor!

websideType
	^'AbstractAddMethod'! !
!AddMethodChange class categoriesFor: #websideType!instance creation!public! !

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

!CompiledCode methodsFor!

asWebsideJson
	| source package |
	source := self getSource copyWithout: 10 asCharacter.
	package := self owningPackage ifNotNil: [:p | p name].
	^super asWebsideJson
		at: 'selector' put: selector;
		at: 'methodClass' put: methodClass name;
		at: 'source' put: source;
		at: 'author' put: 'Unknown';
		at: 'timestamp' put: nil;
		at: 'package' put: package;
		yourself! !
!CompiledCode categoriesFor: #asWebsideJson!public! !

!CompiledMethod methodsFor!

asWebsideJson
	| category |
	category := self categories detect: [:c | c isPrivacy not and: [c isVirtual not]] ifNone: [].
	^super asWebsideJson
		at: 'category' put: (category ifNotNil: [:c | c name]);
		at: 'author' put: 'Unknown';
		at: 'timestamp' put: nil;
		at: 'overriding' put: self isOverride;
		at: 'overriden' put: self isOverridden;
		yourself! !
!CompiledMethod categoriesFor: #asWebsideJson!converting!public! !

!CompilerNotification methodsFor!

asWebsideJson
	| interval json |
	interval := Dictionary new
				at: 'start' put: position;
				at: 'end' put: position;
				yourself.
	json := super asWebsideJson.
	json
		at: 'fullDescription' put: self errorMessage;
		at: 'interval' put: interval.
	^json! !
!CompilerNotification categoriesFor: #asWebsideJson!accessing!public! !

!Debugger methodsFor!

process
	^process!

stepInto: aStackFrame
	"This is a handy service to enable interactions without UI (such as through WebsideAPI).
	Of course #stepInto could (and should) be implemented by calling this method, but we
	decided to preserve original Dolphin services untouched by the moment."

	self debugState: 'Step Into'.
	breakWhen removeAll.
	self stepInFrame: aStackFrame!

stepIntoBlock: aStackFrame
	"This is a handy service to enable interactions without UI (such as through WebsideAPI).
	Of course #stepIntoBlock could (and should) be implemented by calling this method, but we
	decided to preserve original Dolphin services untouched by the moment."

	self debugState: 'Step Into Block'.
	"Break when encountering a block homed in the current method, or when returning to the current frame without encountering one"
	self
		breakWhen: [:f | f index = aStackFrame index or: [f isBlockFrame and: [f method = aStackFrame method]]].
	self runDebuggedProcess!

stepOver: aStackFrame
	"This is a handy service to enable interactions without UI (such as through WebsideAPI).
	Of course #stepOver could (and should) be implemented by calling this method, but we
	decided to preserve original Dolphin services untouched by the moment."

	self beRunning.
	self debugState: 'Step over'.
	self breakFrame: aStackFrame.
	self makeDebugFrame: aStackFrame sender.
	self resume! !
!Debugger categoriesFor: #process!private!updating! !
!Debugger categoriesFor: #stepInto:!public! !
!Debugger categoriesFor: #stepIntoBlock:!public! !
!Debugger categoriesFor: #stepOver:!public! !

!DolphinAddMethodChange class methodsFor!

websideType
	^'AddMethod'! !
!DolphinAddMethodChange class categoriesFor: #websideType!public! !

!Exception methodsFor!

asWebsideJson
	| json |
	json := super asWebsideJson.
	json at: 'description' put: self description.
	^json! !
!Exception categoriesFor: #asWebsideJson!handling!public! !

!MethodCompileFailed methodsFor!

asWebsideJson
	^compilerErrorNotification asWebsideJson! !
!MethodCompileFailed categoriesFor: #asWebsideJson!accessing!private! !

!MethodRefactoring methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'className' put: class name asString;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	json at: 'className'
		ifPresent: [:n | Smalltalk at: n ifPresent: [:c | class := RBClass existingNamed: n]]! !
!MethodRefactoring categoriesFor: #asWebsideJson!private! !
!MethodRefactoring categoriesFor: #fromWebsideJson:!private! !

!Object methodsFor!

asWebsideJson
	| printed |
	printed := [self printString] on: Error
				do: [:e | 'Error while printing ' , self class name , ' instance'].
	^Dictionary new
		at: 'class' put: self class name;
		at: 'hasNamedSlots' put: self class isPointers;
		at: 'hasIndexedSlots' put: self isIndexable;
		at: 'size' put: (self class isVariable ifTrue: [self size] ifFalse: [0]);
		at: 'printString' put: printed;
		at: 'iconName' put: self websideIconName;
		yourself!

websideIconName
	| icon |
	icon := self icon.
	icon ifNil: [^nil].
	icon class == TextTileIcon ifTrue: [^icon text asString].
	^[icon identifier] on: Error do: [:e | ]!

websideViews
	^#()! !
!Object categoriesFor: #asWebsideJson!converting!public! !
!Object categoriesFor: #websideIconName!converting!public! !
!Object categoriesFor: #websideViews!exceptions!private! !

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

!PullUpInstanceVariableRefactoring methodsFor!

fromWebsideJson: json
	super fromWebsideJson: json.
	json at: 'target'
		ifPresent: 
			[:n |
			Smalltalk at: n
				ifPresent: 
					[:c |
					class := RBClass existingNamed: n.
					class model: model]]! !
!PullUpInstanceVariableRefactoring categoriesFor: #fromWebsideJson:!public! !

!PullUpInstanceVariableRefactoring class methodsFor!

websideType
	^'MoveUpInstanceVariable'! !
!PullUpInstanceVariableRefactoring class categoriesFor: #websideType!public! !

!PushDownInstanceVariableRefactoring class methodsFor!

websideType
	^'MoveDownInstanceVariable'! !
!PushDownInstanceVariableRefactoring class categoriesFor: #websideType!public! !

!Refactoring methodsFor!

asWebsideJson
	^Dictionary new
		at: 'type' put: self class websideType;
		at: 'label' put: self class websideType;
		at: 'package' put: (self propertyAt: #packageName ifAbsent: []);
		at: 'timestamp' put: DateAndTime now printString;
		at: 'author' put: nil;
		yourself!

fromWebsideJson: json
	json at: 'package' ifPresent: [:n | self propertyAt: #packageName put: n].
	self model	"Only to initialize it"!

websideExecute
	^self execute! !
!Refactoring categoriesFor: #asWebsideJson!public! !
!Refactoring categoriesFor: #fromWebsideJson:!public! !
!Refactoring categoriesFor: #websideExecute!public! !

!Refactoring class methodsFor!

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
	| type |
	type := self name asString.
	(type endsWith: 'Refactoring') ifTrue: [type := type copyFrom: 1 to: type size - 11].
	^type! !
!Refactoring class categoriesFor: #acceptsWebsideJson:!class hierarchy-removing!public! !
!Refactoring class categoriesFor: #classForWebsideJson:!class hierarchy-removing!public! !
!Refactoring class categoriesFor: #fromWebsideJson:!class hierarchy-removing!public! !
!Refactoring class categoriesFor: #websideType!class hierarchy-removing!public! !

!RefactoryChange methodsFor!

asWebsideJson
	^Dictionary new
		at: 'type' put: self class websideType;
		at: 'label' put: ([self changeString] on: Error do: [:e | self class websideType]);
		at: 'package' put: (self propertyAt: #packageName ifAbsent: []);
		at: 'timestamp' put: DateAndTime now printString;
		at: 'author' put: nil;
		yourself!

fromWebsideJson: json
	json at: 'package' ifPresent: [:n | self propertyAt: #packageName put: n]!

websideExecute
	^self execute! !
!RefactoryChange categoriesFor: #asWebsideJson!initialize/release!public! !
!RefactoryChange categoriesFor: #fromWebsideJson:!initialize/release!public! !
!RefactoryChange categoriesFor: #websideExecute!initialize/release!public! !

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
	| type |
	type := self name asString.
	(type endsWith: 'Change') ifTrue: [type := type copyFrom: 1 to: type size - 6].
	^type! !
!RefactoryChange class categoriesFor: #acceptsWebsideJson:!public! !
!RefactoryChange class categoriesFor: #classForWebsideJson:!public! !
!RefactoryChange class categoriesFor: #fromWebsideJson:!public! !
!RefactoryChange class categoriesFor: #websideType!public! !

!RefactoryClassChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'className' put: self changeClass name asString;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	className := json at: 'className' ifAbsent: [].
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
		at: 'className' put: oldName;
		at: 'newName' put: newName;
		at: 'renameReferences' put: true;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	oldName := json at: 'className' ifAbsent: [].
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

!RenameMethodRefactoring methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'selector' put: oldSelector;
		at: 'newSelector' put: newSelector;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	json at: 'selector' ifPresent: [:s | oldSelector := s asSymbol].
	json at: 'newSelector'
		ifPresent: 
			[:s |
			newSelector := s asSymbol.
			permutation := 1 to: newSelector argumentCount]! !
!RenameMethodRefactoring categoriesFor: #asWebsideJson!public!testing! !
!RenameMethodRefactoring categoriesFor: #fromWebsideJson:!public!testing! !

!RenameVariableChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'className' put: className;
		at: 'variable' put: oldName;
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	className := json at: 'className' ifAbsent: [].
	isMeta := className notNil and: [className endsWith: ' class'].
	oldName := json at: 'variable' ifAbsent: [].
	newName := json at: 'newName' ifAbsent: []! !
!RenameVariableChange categoriesFor: #asWebsideJson!printing!public! !
!RenameVariableChange categoriesFor: #fromWebsideJson:!printing!public! !

!StackFrame methodsFor!

asWebsideJson
	| label |
	label := String streamContents: [:s | self displayOn: s].
	^Dictionary new
		at: 'label' put: label;
		at: 'class' put: self receiver class asWebsideJson;
		at: 'method' put: self method asWebsideJson;
		yourself! !
!StackFrame categoriesFor: #asWebsideJson!accessing!private! !

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

asWebsideJson	| children |	children := OrderedCollection with: self selector asWebsideJson.	arguments do: [ :n | children add: n asWebsideJson  ].	children add: body asWebsideJson .	^super asWebsideJson at: 'children' put: children asArray; yourself ! !
!StMethodNode categoriesFor: #asWebsideJson!public!visitor! !

!StProgramNode methodsFor!

asWebsideJson	^ super asWebsideJson		at: 'type' put: self websideType;		at: 'start' put: self start;		at: 'end' put: self stop;		yourself!

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

includesString: aString
	^(self indexOfSubCollection: aString) > 0!

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
!String categoriesFor: #includesString:!comparing!public! !
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

!VariableRefactoring methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'className' put: class name asString;
		at: 'variable' put: variableName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	json at: 'className'
		ifPresent: 
			[:n |
			Smalltalk at: n
				ifPresent: 
					[:c |
					class := RBClass existingNamed: n.
					class model: model]].
	variableName := json at: 'variable' ifAbsent: []! !
!VariableRefactoring categoriesFor: #asWebsideJson!initialize/release!public! !
!VariableRefactoring categoriesFor: #fromWebsideJson:!initialize/release!public! !

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
!URLTemplate categoriesFor: #=!comparing!public! !
!URLTemplate categoriesFor: #argumentsFrom:!private! !
!URLTemplate categoriesFor: #description!accessing!public! !
!URLTemplate categoriesFor: #description:!accessing!public! !
!URLTemplate categoriesFor: #hasDescription!private! !
!URLTemplate categoriesFor: #hash!comparing!public! !
!URLTemplate categoriesFor: #initializeParameters!private! !
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
	^self debuggers associations asArray collect: 
			[:a |
			a value asWebsideJson
				at: 'id' put: a key asString;
				at: 'description' put: a value name;
				yourself]!

activeEvaluation
	| id object fake |
	id := self requestedId.
	self evaluations at: id ifPresent: [:e | ^e asWebsideJson].
	object := self objects at: id ifAbsent: [^self notFound].
	fake := WebsideEvaluation new.
	^fake
		id: id;
		result: object;
		finished;
		asWebsideJson!

activeEvaluations
	^self evaluations values asArray collect: [:e | e asWebsideJson]!

activeWorkspaces
	^self workspaces values asArray collect: [:w | w asWebsideJson]!

addChange
	| change |
	change := self requestedChange.
	change ifNil: [^self badRequest: 'Change not supported'].
	[change websideExecute] on: Exception do: [:e | ^self compilationError: e].
	"Temporary workaround until a better way of gathering system changes is implemented"
	server addChange: change.
	^change asWebsideJson!

badRequest: aString
	^HttpServerResponse badRequest content: aString!

bodyAt: aString	^ self bodyAt: aString ifAbsent: []!

bodyAt: aString ifAbsent: aBlock	| json |	json := STONJSON fromString: request body.	^ json at: aString ifAbsent: aBlock!

cancelEvaluation
	| id evaluation |
	id := self requestedId.
	evaluation := self evaluations at: id ifAbsent: [^self notFound].
	evaluation cancel.
	self evaluations removeKey: id.
	^evaluation asWebsideJson!

categories
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^(class realMethodCategories collect: [:c | c name]) asArray!

changes
	| author package changes |
	author := self queryAt: 'author'.
	package := self queriedPackage ifNil: [self systemPackage].
	"	changes := package changes currentChanges.
	author ifNotNil: [changes := changes select: [:ch | ch author = author]]."
	"Temporary workaround until a better way of gathering changes is implemented"
	changes := server changes asArray.
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

classTreeFrom: aClass depth: anInteger	| json subclasses depth names |	names := self queryAt: 'names'.	json := names = 'true'				ifTrue: 					[self newJsonObject						at: 'name' put: aClass name;						at: 'superclass' put: (aClass superclass ifNotNil: [:c | c name]);
						at: 'iconName' put: aClass websideIconName;						yourself]				ifFalse: [aClass asWebsideJson].	(anInteger notNil and: [anInteger = 0]) ifTrue: [^json].	depth := anInteger notNil ifTrue: [anInteger - 1].	subclasses := (aClass subclasses asSortedCollection: [:a :b | a name <= b name])				collect: [:c | self classTreeFrom: c depth: depth].	json at: 'subclasses' put: subclasses asArray.	^json!

classTreeFromClasses: aCollection
	| roots json subclasses |
	roots := Dictionary new.
	aCollection
		do: 
				[:c |
				json := self newJsonObject
							at: 'name' put: c name;
							at: 'iconName' put: c websideIconName;
							yourself.
				roots at: c name put: json];
		do: 
				[:c |
				c superclass notNil
					ifTrue: 
						[roots at: c superclass
							ifPresent: 
								[:sc |
								subclasses := sc at: 'subclasses'
											ifAbsentPut: [SortedCollection new sortBlock: [:a :b | (a at: 'name') <= (b at: 'name')]].
								subclasses add: (roots at: c name)]]];
		do: 
				[:c |
				c superclass notNil
					ifTrue: [(roots includesKey: c superclass name) ifTrue: [roots removeKey: c name]]].
	^(roots asArray asSortedCollection: [:a :b | (a at: 'name') <= (b at: 'name')]) asArray!

classVariables	| class |	class := self requestedClass.	class ifNil: [^self notFound].	^(class withAllSuperclasses gather: 			[:c |			c classVarNames asArray sort collect: 					[:v |					self newJsonObject						at: 'name' put: v;						at: 'class' put: c name , ' class';						at: 'type' put: 'class';						yourself]])		asArray!

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

created: aDictionary
	| payload |
	payload := STON toJsonString: aDictionary.
	^HttpServerResponse new
		statusCode: 201;
		reason: 'Created';
		contentType: 'application/json; charset=utf-8';
		content: payload asUtf8String!

createDebugger
	| id evaluation process frame description debugger |
	id := self bodyAt: 'evaluation' ifAbsent: [^self notFound].
	id := IID fromString: id.
	evaluation := self evaluations at: id ifAbsent: [^self notFound].
	process := evaluation process.
	frame := evaluation hasFailed
				ifTrue: [evaluation error raisingFrame]
				ifFalse: [process suspendedFrame].
	description := evaluation hasFailed
				ifTrue: [evaluation error description]
				ifFalse: ['Suspended process'].
	debugger := Smalltalk developmentSystem debuggerClass new.
	debugger
		caption: description;
		process: process topFrame: frame;
		resumable: process isTerminated not.
	"context selector == #halt
		ifTrue: 
			[debugger
				stepOver;
				stepOver]."
	self debuggers at: id put: debugger.
	^debugger asWebsideJson
		at: 'id' put: id asString;
		at: 'description' put: description;
		yourself!

createEvaluation
	| expression evaluation |
	expression := self bodyAt: 'expression' ifAbsent: [''].
	expression isEmpty ifTrue: [^nil].
	evaluation := WebsideEvaluation new.
	evaluation
		expression: expression;
		receiver: self requestedEvaluationReceiver;
		context: self requestedEvaluationContext.
	^self evaluations at: evaluation id put: evaluation!

createWorkspace
	| workspace |
	workspace := WebsideWorkspace new.
	self workspaces at: workspace id put: workspace.
	^workspace asWebsideJson!

customViewsOf: anObject
	^anObject websideViews!

debugExpression	| expression method receiver process context debugger id |	expression := self bodyAt: 'expression' ifAbsent: [''].	method := self compiler compile: expression.	receiver := self compilerReceiver.	process := [ method valueWithReceiver: receiver arguments: #() ]		newProcess.	context := process suspendedContext.	debugger := process		newDebugSessionNamed: 'debug it'		startedAt: context.	debugger stepIntoUntil: [ :c | c method == method ].	id := self newID.	self evaluations at: id put: process.	self debuggers at: id put: debugger.	^ id asString!

debuggerFrame
	| debugger index frame map interval source lfs |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	index := self requestedIndex.
	frame := debugger frames at: index ifAbsent: [^self notFound].
	map := frame textMap.
	interval := (map lookup: (frame findIP: frame ip inTextMap: map))
				ifNil: [1 to: 0]
				ifNotNil: [:e | e value].
	interval notEmpty
		ifTrue: 
			[source := frame method getSource.
			lfs := (source copyFrom: 1 to: interval start) count: [:ch | ch isLinefeed].
			interval := interval - lfs].
	interval := Dictionary new
				at: 'start' put: interval start;
				at: 'end' put: interval stop;
				yourself.
	^frame asWebsideJson
		at: 'index' put: index;
		at: 'interval' put: interval;
		yourself!

debuggerFrames
	| debugger frames |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frames := debugger frames.
	^(1 to: frames size) asArray collect: 
			[:i |
			| label |
			label := String streamContents: [:s | (frames at: i) displayOn: s].
			self newJsonObject
				at: 'index' put: i;
				at: 'label' put: label;
				yourself]!

debuggers
	^ server debuggers!

defaultRootClass
	^Object!

deleteDebugger
	| id debugger evaluation |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [^self notFound].
	evaluation := self evaluations detect: [:e | e process == debugger process] ifNone: [].
	evaluation
		ifNotNil: 
			[evaluation process terminate.
			self evaluations removeKey: evaluation id ifAbsent: []].
	self debuggers removeKey: id.
	^id asString!

deleteWorkspace	self workspaces		removeKey: self requestedId		ifAbsent: [ ^ self notFound ].	^ nil!

dialect	^'Dolphin'!

evaluateExpression
	| evaluation |
	(self bodyAt: 'debug' ifAbsent: [false]) ifTrue: [^self debugExpression].
	(self bodyAt: 'profile' ifAbsent: [false]) ifTrue: [^self profileExpression].
	evaluation := self createEvaluation.
	evaluation ifNil: [^nil].
	evaluation
		when: #finished
			send: #value
			to: 
				[self objects at: evaluation id put: evaluation result.
				self evaluations removeKey: evaluation id];
		evaluate.
	(self bodyAt: 'sync' ifAbsent: [false]) ifFalse: [^self created: evaluation asWebsideJson].
	evaluation waitForResult.
	evaluation hasFailed ifTrue: [^self evaluationError: evaluation].
	(self bodyAt: 'pin' ifAbsent: [false]) ifFalse: [self objects removeKey: evaluation id].
	^evaluation result asWebsideJson
		at: 'id' put: evaluation id asString;
		yourself!

evaluateExpression: aString
	^Compiler evaluate: aString for: self compilerReceiver!

evaluationError: aWebsideEvaluation	| json data |	json := self newJsonObject				at: 'description' put: aWebsideEvaluation error description;				at: 'evaluation' put: aWebsideEvaluation id asString;				yourself.	data := STONJSON toString: json.	^HttpServerResponse new		statusCode: 409;		contentType: 'application/json; charset=utf-8';		content: data asUtf8String!

evaluations
	^server evaluations!

extensions
	^#()!

filterByCategory: aCollection
	| category |
	category := self queriedCategory.
	^(category notNil and: [category notEmpty])
		ifTrue: [aCollection select: [:m | m categories anySatisfy: [:c | c name = category]]]
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
	| debugger frame bindings temps receiver extra class value base |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frame := debugger frames at: self requestedIndex ifAbsent: [^self notFound].
	temps := frame temps.
	receiver := frame receiver.
	extra := frame stackWorkspace.
	class := receiver basicClass.
	bindings := OrderedCollection new: 1 + temps size + class instSize + extra.
	bindings addLast: (Array with: 'self' with: receiver).
	class allInstVarNames keysAndValuesDo: 
			[:i :n |
			value := receiver instVarAt: i.
			bindings addLast: (Array with: '   ' , n with: value)].
	temps do: 
			[:t |
			t third > 0
				ifTrue: 
					[value := (frame getOuter: t second) at: t third.
					bindings addLast: (Array with: t first with: value)]].
	base := frame index + frame frameSize.
	0 to: extra - 1
		do: 
			[:i |
			value := debugger process at: base + i.
			bindings addLast: (Array with: '_stack' , i displayString with: value)].
	^bindings asArray collect: 
			[:b |
			self newJsonObject
				at: 'name' put: b first;
				at: 'value' put: b second printString;
				yourself]!

icons
	| icons |
	icons := Icon allInstances select: [:i | (i respondsTo: #identifier) and: [i identifier isString]].
	^icons , TextTileIcon allInstances collect: 
			[:i |
			| name data |
			name := i class == TextTileIcon ifTrue: [i text asString] ifFalse: [i identifier].
			data := self imageFromIcon: i.
			self newJsonObject
				at: 'name' put: name;
				at: 'data' put: data;
				yourself]!

imageFromIcon: icon
	| bmp tmp bytes base64 |
	bmp := GdiplusBitmap fromIcon: icon.
	tmp := File temporaryFilename.
	tmp := (tmp truncateTo: tmp size - 4) , '.png'.
	[bmp saveToFile: tmp encoderParams: nil] on: Error do: [^nil].
	bytes := File readAllBytes: tmp.
	base64 := '' asUtf8String writeStream.
	Base64Codec new
		lineLength: -4;
		encodeFrom: bytes readStream onto: base64.
	^base64 contents!

imageNamed: aString
	"
	WebsideAPI new imageNamed: 'DolphinPackage.ico'
	"

	| icon |
	icon := Icon allInstances detect: [:i | i identifier = aString] ifNone: [^nil].
	^self imageFromIcon: icon!

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

jsonFromMethods: aCollection
	| ast |
	(self queryAt: 'basic') = 'true'
		ifTrue: 
			[^aCollection collect: 
					[:m |
					| source package category |
					source := m getSource copyWithout: 10 asCharacter.
					package := m owningPackage ifNotNil: [:p | p name].
					category := m categories detect: [:c | c isPrivacy not and: [c isVirtual not]] ifNone: [].
					self newJsonObject
						at: 'selector' put: m selector;
						at: 'methodClass' put: (m methodClass ifNotNil: [:c | c name]);
						at: 'category' put: (category ifNotNil: [:c | c name]);
						at: 'package' put: package;
						at: 'source' put: source;
						yourself]].
	ast := (self queryAt: 'ast') = 'true'.
	^aCollection collect: 
			[:m |
			| json |
			json := m asWebsideJson.
			ast ifTrue: [json at: 'ast' put: m parseTree asWebsideJson].
			json]!

logo	^'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
'!

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
	(self queryAt: 'count') = 'true' ifTrue: [^methods size].
	^self jsonFromMethods: methods!

methodTemplate	^self newJsonObject		at: 'template' put: true;		at: 'selector' put: 'messagePattern';		at: 'source'		put: 'messagePattern	"comment"	| temporaries |	statements';		yourself!

namedSlotsOf: anObject	| slot |	^anObject class allInstVarNames collect: [ :n |		slot := self slot: n of: anObject ifAbsent: nil.		slot asWebsideJson at: 'slot' put: n; yourself  ]!

newID	^IID newUnique!

newJsonObject	^Dictionary new!

notFound
	^HttpServerResponse notFound!

objectFromPath: uri
	| path id slot |
	path := uri subStrings: '/'.
	id := IID fromString: path first.
	slot := self objects at: id ifAbsent: [^nil].
	slot isNil ifTrue: [^nil].
	path
		from: 2
		to: path size
		do: 
			[:s |
			slot := self
						slot: s
						of: slot
						ifAbsent: [^nil]].
	^slot!

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
	^package methods asArray collect: [:m | m asWebsideJson]!

packageNamed: aString
	^PackageManager current packageNamed: aString ifNone: []!

packages
	| manager root packages names |
	manager := PackageManager current.
	root := self queryAt: 'root'.
	root := root notNil ifTrue: [manager packageNamed: root ifNone: []].
	packages := root notNil ifTrue: [{root}] ifFalse: [manager packages].
	names := self queryAt: 'names'.
	names = 'true' ifTrue: [^(packages collect: [:p | p name]) asArray sort].
	^(packages collect: [:p | p asWebsideJson]) asArray!

pauseEvaluation
	| id evaluation |
	id := self requestedId.
	evaluation := self evaluations at: id ifAbsent: [^self notFound].
	evaluation pause.
	^evaluation asWebsideJson!

pinnedObject
	| id object |
	id := self requestedId.
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
	last = 'custom-views' ifTrue: [^self customViewsOf: object].
	object := self
				slot: last
				of: object
				ifAbsent: [^self notFound].
	^object asWebsideJson!

pinObjectSlot	| slot id |	slot := self requestedSlot.	slot ifNil: [ ^ self badRequest: 'Bad object slot URI' ].	id := self newID.	self objects at: id put: slot.	^ slot asWebsideJson		at: 'id' put: id asString;		yourself!

profileExpression
	^nil!

queriedAccessing	^ self queryAt: 'accessing'!

queriedAssigning
	^self queryAt: 'assigning'!

queriedCategory	^self queryAt: 'category' ifPresent: [:c | c asSymbol]!

queriedClass	^self queryAt: 'class' ifPresent: [:n | self classNamed: n]!

queriedPackage
	^self queryAt: 'package' ifPresent: [:n | self packageNamed: n]!

queriedReferencing
	^self queriedReferencingClass isNil ifTrue: [self queryAt: 'referencing']!

queriedReferencingClass	^self queryAt: 'referencingClass' ifPresent: [:n | self classNamed: n ]!

queriedReferencingString	^ self queryAt: 'referencingString'!

queriedScope	^self queryAt: 'scope' ifPresent: [:s | self classNamed: s ]!

queriedSelector	^self queryAt: 'selector' ifPresent: [:s | s asSymbol]!

queriedSelectorMatching	^self queryAt: 'selectorMatching'!

queriedSending	^self queryAt: 'sending' ifPresent: [:s | s asSymbol]!

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
	change ifNil: [change := Refactoring fromWebsideJson: json].
	^change!

requestedClass
	| name |
	name := self urlAt: 'name'.
	^name ifNotNil: [self classNamed: name]!

requestedEvaluationContext
	| context id debugger index |
	context := self bodyAt: 'context' ifAbsent: [^nil].
	id := context at: 'debugger' ifAbsent: [].
	id
		ifNotNil: 
			[id := IID fromString: id.
			debugger := self debuggers at: id ifAbsent: [^nil].
			index := context at: 'frame' ifAbsent: [^nil].
			^debugger stack at: index asInteger ifAbsent: nil].
	^nil!

requestedEvaluationReceiver
	| context name path id debugger index frame |
	context := self bodyAt: 'context' ifAbsent: [^nil].
	name := context at: 'class' ifAbsent: [].
	name ifNotNil: [^self classNamed: name].
	path := context at: 'object' ifAbsent: [].
	path ifNotNil: [^self objectFromPath: path].
	id := context at: 'debugger' ifAbsent: [].
	id
		ifNotNil: 
			[id := IID fromString: id.
			debugger := self debuggers at: id ifAbsent: [^nil].
			index := context at: 'frame' ifAbsent: [^nil].
			frame := debugger stack at: index asInteger ifAbsent: nil.
			^frame ifNotNil: [frame receiver]].
	^nil!

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
	^name ifNotNil: [self packageNamed: name]!

requestedSelector
	| selector |
	selector := self urlAt: 'selector'.
	^selector ifNotNil: [selector asSymbol]!

requestedSlot	| uri path index id slot |	uri := self bodyAt: 'uri' ifAbsent: [ ^ nil ].	path := uri subStrings: '/'.	index := path indexOf: 'objects' ifAbsent: [ ^ nil ].	id := path at: index + 1 ifAbsent: [ ^ nil ].	id := IID fromString: id.	slot := self objects at: id ifAbsent: [ ^ nil ].	path		from: index + 2		to: path size		do: [ :s | slot := self slot: s of: slot ifAbsent: [ ^ nil ] ].	^ slot!

restartDebugger
	| debugger frame update method |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frame := debugger frames at: self requestedIndex ifAbsent: [^self notFound].
	"update := self queryAt: 'update'.
	method := frame method.
	(update = 'true' and: [method notNil])
		ifTrue: [frame privRefreshWith: method classBinding value >> method selector]."
	debugger restartFrame: frame.
	^nil!

resumeDebugger
	| id debugger |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [^self notFound].
	self debuggers removeKey: id.
	debugger resume.
	^nil!

resumeEvaluation
	| id evaluation |
	id := self requestedId.
	evaluation := self evaluations at: id ifAbsent: [^self notFound].
	evaluation resume.
	^evaluation asWebsideJson!

saveImage	SessionManager current saveImage.
	^true!

search
	| text ignoreCase position condition type results |
	text := self queryAt: 'text' ifAbsent: [^self badRequest: 'missing text'].
	ignoreCase := (self queryAt: 'ignoreCase') = 'true'.
	position := (self queryAt: 'condition' ifAbsent: [#beginning]) asSymbol.
	condition := position == #beginning
				ifTrue: [#beginsWith:]
				ifFalse: [position == #ending ifTrue: [#endsWith:] ifFalse: [#includesString:]].
	type := self queryAt: 'type' ifAbsent: 'all'.
	results := OrderedCollection new.
	(type = 'all' or: [type = 'class'])
		ifTrue: 
			[results addAll: (self
						search: (BrowserEnvironment new classes reject: [:c | c isMeta])
						for: text
						with: #name
						condition: condition
						type: 'class'
						ignoreCase: ignoreCase)].
	(type = 'all' or: [type = 'selector'])
		ifTrue: 
			[results addAll: (self
						search: Smalltalk developmentSystem allSelectors
						for: text
						with: #asString
						condition: condition
						type: 'selector'
						ignoreCase: ignoreCase)].
	(type = 'all' or: [type = 'package'])
		ifTrue: 
			[results addAll: (self
						search: Package manager packages
						for: text
						with: #name
						condition: condition
						type: 'package'
						ignoreCase: ignoreCase)].
	^results asArray!

search: aCollection for: aString with: aSymbol condition: anotherSymbol type: anotherString ignoreCase: aBoolean
	^aCollection select: 
			[:element |
			| candidate |
			candidate := element perform: aSymbol.
			aBoolean ifTrue: [candidate := candidate asLowercase].
			candidate perform: anotherSymbol with: aString]
		thenCollect: 
			[:element |
			self newJsonObject
				at: 'type' put: anotherString;
				at: 'text' put: (element perform: aSymbol);
				at: 'iconName' put: element websideIconName;
				yourself]!

selectors
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^class selectors asArray!

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
	| debugger frame |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frame := debugger frames at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepInto: frame.
	^nil!

stepOverDebugger
	| debugger frame |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frame := debugger frames at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepOver: frame.
	^nil!

stepThroughDebugger
	| debugger frame |
	debugger := self debuggers at: self requestedId ifAbsent: [^self notFound].
	frame := debugger frame at: self requestedIndex ifAbsent: [^self notFound].
	debugger stepThrough.
	^nil!

subclasses
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^class subclasses asArray collect: [:c | c asWebsideJson]!

superclasses
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^class allSuperclasses asArray collect: [:c | c asWebsideJson]!

systemPackage
	^PackageManager current systemPackage!

terminateDebugger
	| id debugger evaluation |
	id := self requestedId.
	debugger := self debuggers at: id ifAbsent: [^self notFound].
	evaluation := self evaluations detect: [:e | e process == debugger process] ifNone: [].
	evaluation
		ifNotNil: 
			[evaluation process terminate.
			self evaluations removeKey: evaluation id ifAbsent: []].
	self debuggers removeKey: id.
	^nil!

unpinAllObjects	self objects removeAll.	^ nil!

unpinObject
	self objects removeKey: self requestedId ifAbsent: [^self notFound].
	^nil!

updateWorkspace
	| workspace source |
	workspace := self workspaces at: self requestedId ifAbsent: [^self notFound].
	source := self bodyAt: 'source' ifAbsent: ''.
	workspace contents: source.
	^workspace asWebsideJson!

urlAt: aString
	^(request propertyAt: #arguments) at: aString ifAbsent: []!

usedCategories
	^#()!

usualCategories
	^#()!

variables
	| class |
	class := self requestedClass.
	class ifNil: [^self notFound].
	^self instanceVariables , self classVariables!

version
	^SessionManager current productVersion printString!

workspace
	| workspace |
	workspace := self workspaces at: self requestedId ifAbsent: [^self notFound].
	^workspace asWebsideJson!

workspaceBindings
	| workspace |
	workspace := self workspaces at: self requestedId ifAbsent: [^self notFound].
	^workspace bindings associations collect: 
			[:a |
			self newJsonObject
				at: 'name' put: a key;
				at: 'value' put: a value printString;
				yourself]!

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
!WebsideAPI categoriesFor: #created:!private! !
!WebsideAPI categoriesFor: #createDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #createEvaluation!private! !
!WebsideAPI categoriesFor: #createWorkspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #customViewsOf:!private! !
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
!WebsideAPI categoriesFor: #extensions!extensions endpoints!public! !
!WebsideAPI categoriesFor: #filterByCategory:!public! !
!WebsideAPI categoriesFor: #filterByVariable:!private! !
!WebsideAPI categoriesFor: #frameBindings!debugging endpoints!public! !
!WebsideAPI categoriesFor: #icons!general endpoints!public! !
!WebsideAPI categoriesFor: #imageFromIcon:!private! !
!WebsideAPI categoriesFor: #imageNamed:!private! !
!WebsideAPI categoriesFor: #implementorsOf:!private! !
!WebsideAPI categoriesFor: #indexedSlotsOf:!private! !
!WebsideAPI categoriesFor: #instanceVariables!code endpoints!public! !
!WebsideAPI categoriesFor: #instanceVariablesOf:!public! !
!WebsideAPI categoriesFor: #integerFrom:!private! !
!WebsideAPI categoriesFor: #jsonFromMethods:!private! !
!WebsideAPI categoriesFor: #logo!general endpoints!public! !
!WebsideAPI categoriesFor: #method!code endpoints!public! !
!WebsideAPI categoriesFor: #methods!code endpoints!public! !
!WebsideAPI categoriesFor: #methodTemplate!code endpoints!public! !
!WebsideAPI categoriesFor: #namedSlotsOf:!private! !
!WebsideAPI categoriesFor: #newID!private! !
!WebsideAPI categoriesFor: #newJsonObject!private! !
!WebsideAPI categoriesFor: #notFound!private! !
!WebsideAPI categoriesFor: #objectFromPath:!private! !
!WebsideAPI categoriesFor: #objects!private! !
!WebsideAPI categoriesFor: #package!code endpoints!public! !
!WebsideAPI categoriesFor: #packageClasses!code endpoints!public! !
!WebsideAPI categoriesFor: #packageMethods!code endpoints!public! !
!WebsideAPI categoriesFor: #packageNamed:!private! !
!WebsideAPI categoriesFor: #packages!code endpoints!public! !
!WebsideAPI categoriesFor: #pauseEvaluation!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObject!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObjects!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinnedObjectSlots!objects endpoints!public! !
!WebsideAPI categoriesFor: #pinObjectSlot!objects endpoints!public! !
!WebsideAPI categoriesFor: #profileExpression!profiling endpoints!public! !
!WebsideAPI categoriesFor: #queriedAccessing!private! !
!WebsideAPI categoriesFor: #queriedAssigning!private! !
!WebsideAPI categoriesFor: #queriedCategory!private! !
!WebsideAPI categoriesFor: #queriedClass!private! !
!WebsideAPI categoriesFor: #queriedPackage!private! !
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
!WebsideAPI categoriesFor: #requestedEvaluationContext!private! !
!WebsideAPI categoriesFor: #requestedEvaluationReceiver!private! !
!WebsideAPI categoriesFor: #requestedId!private! !
!WebsideAPI categoriesFor: #requestedIndex!private! !
!WebsideAPI categoriesFor: #requestedPackage!private! !
!WebsideAPI categoriesFor: #requestedSelector!private! !
!WebsideAPI categoriesFor: #requestedSlot!private! !
!WebsideAPI categoriesFor: #restartDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #resumeDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #resumeEvaluation!evaluation endpoints!public! !
!WebsideAPI categoriesFor: #saveImage!general endpoints!public! !
!WebsideAPI categoriesFor: #search!code endpoints!public! !
!WebsideAPI categoriesFor: #search:for:with:condition:type:ignoreCase:!private! !
!WebsideAPI categoriesFor: #selectors!code endpoints!public! !
!WebsideAPI categoriesFor: #sendersOf:!private! !
!WebsideAPI categoriesFor: #server:!accessing!public! !
!WebsideAPI categoriesFor: #slot:of:ifAbsent:!private! !
!WebsideAPI categoriesFor: #stepIntoDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #stepOverDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #stepThroughDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #subclasses!code endpoints!public! !
!WebsideAPI categoriesFor: #superclasses!code endpoints!public! !
!WebsideAPI categoriesFor: #systemPackage!private! !
!WebsideAPI categoriesFor: #terminateDebugger!debugging endpoints!public! !
!WebsideAPI categoriesFor: #unpinAllObjects!objects endpoints!public! !
!WebsideAPI categoriesFor: #unpinObject!objects endpoints!public! !
!WebsideAPI categoriesFor: #updateWorkspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #urlAt:!private! !
!WebsideAPI categoriesFor: #usedCategories!code endpoints!public! !
!WebsideAPI categoriesFor: #usualCategories!code endpoints!public! !
!WebsideAPI categoriesFor: #variables!code endpoints!public! !
!WebsideAPI categoriesFor: #version!general endpoints!public! !
!WebsideAPI categoriesFor: #workspace!public!workspaces endpoints! !
!WebsideAPI categoriesFor: #workspaceBindings!public!workspaces endpoints! !
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

WebsideClient guid: (GUID fromString: '{01ff30e8-7918-4d1f-8703-ba7c4f350477}')!
WebsideClient comment: ''!
!WebsideClient categoriesForClass!Unclassified! !
WebsideEvaluation guid: (GUID fromString: '{b4a27f67-df46-4a72-a342-cea2fbac33b9}')!
WebsideEvaluation comment: ''!
!WebsideEvaluation categoriesForClass!Webside-Base! !
!WebsideEvaluation methodsFor!

asWebsideJson
	| json |
	json := super asWebsideJson.
	json
		at: 'id' put: id asString;
		at: 'expression' put: expression;
		at: 'state' put: state.
	error ifNotNil: [json at: 'error' put: error asWebsideJson].
	^json!

cancel	process ifNotNil: [process terminate].	self cancelled!

cancelled
	state := #cancelled.
	self
		trigger: #cancelled;
		trigger: #finalized!

context	^context!

context: anObject	context := anObject!

error	^ error!

evaluate
	self evaluateBlock: [result := Compiler evaluate: expression for: receiver]!

evaluateBlock: aBlock
	process := 
			[state := #evaluating.
			aBlock on: Exception
				do: 
					[:exception |
					error := exception.
					self failed.
					process suspend].
			self finished]
					newProcess.
"priority should be already set. In fact, it is initiallized as Processor userSchedulingPriority.
However, for some reason that should be understood/modified, HttpServer loop is scheduled at a lower priority,
making the resuming of the evaluation process to take control immediately when #resume is sent below, making synchronous
what otherwise would be asynchronous evaluation."
	process
		priority: Processor activeProcess priority - 1;
		name: 'Webside evaluation ' , id asString;
		resume!

expression	^expression!

expression: aString	expression := aString!

failed
	state := #failed.
	self
		trigger: #failed;
		trigger: #finalized!

finished
	state := #finished.
	self
		trigger: #finished;
		trigger: #finalized!

hasFailed	^ state == #failed!

hasFinished	^ state == #finished!

id	^id!

id: uuid	id := uuid!

initialize
	super initialize.
	id := IID newUnique.
	state := #pending.
	priority := Processor userSchedulingPriority!

isEvaluating	^ state == #evaluating!

isFinalized	^ self hasFinished or: [ self hasFailed or: [ self wasCancelled ] ]!

isPaused	^ state == #paused!

isPending	^ state == #pending!

pause	process suspend.	self paused!

paused	state := #paused.	self trigger: #paused!

priority: anInteger	priority := anInteger!

process	^process!

process: aProcess	process := aProcess!

receiver	^ receiver!

receiver: anObject	receiver := anObject!

requestor	^ requestor!

requestor: anObject	requestor := anObject!

result
	self hasFinished ifTrue: [^result]!

result: anObject	result := anObject!

resume	self resumed.	process resume.!

resumed
	state := #evaluating.
	self trigger: #resumed!

state
	 ^state!

waitForResult
	| semaphore |
	self isFinalized
		ifFalse: 
			[semaphore := Semaphore new.
			self
				when: #finalized
				send: #signal
				to: semaphore.
			semaphore wait].
	^self result!

wasCancelled	^ state == #cancelled! !
!WebsideEvaluation categoriesFor: #asWebsideJson!public! !
!WebsideEvaluation categoriesFor: #cancel!public! !
!WebsideEvaluation categoriesFor: #cancelled!public! !
!WebsideEvaluation categoriesFor: #context!public! !
!WebsideEvaluation categoriesFor: #context:!public! !
!WebsideEvaluation categoriesFor: #error!public! !
!WebsideEvaluation categoriesFor: #evaluate!public! !
!WebsideEvaluation categoriesFor: #evaluateBlock:!public! !
!WebsideEvaluation categoriesFor: #expression!public! !
!WebsideEvaluation categoriesFor: #expression:!public! !
!WebsideEvaluation categoriesFor: #failed!public! !
!WebsideEvaluation categoriesFor: #finished!public! !
!WebsideEvaluation categoriesFor: #hasFailed!public! !
!WebsideEvaluation categoriesFor: #hasFinished!public! !
!WebsideEvaluation categoriesFor: #id!public! !
!WebsideEvaluation categoriesFor: #id:!public! !
!WebsideEvaluation categoriesFor: #initialize!private! !
!WebsideEvaluation categoriesFor: #isEvaluating!public! !
!WebsideEvaluation categoriesFor: #isFinalized!public! !
!WebsideEvaluation categoriesFor: #isPaused!public! !
!WebsideEvaluation categoriesFor: #isPending!public! !
!WebsideEvaluation categoriesFor: #pause!public! !
!WebsideEvaluation categoriesFor: #paused!public! !
!WebsideEvaluation categoriesFor: #priority:!public! !
!WebsideEvaluation categoriesFor: #process!public! !
!WebsideEvaluation categoriesFor: #process:!public! !
!WebsideEvaluation categoriesFor: #receiver!public! !
!WebsideEvaluation categoriesFor: #receiver:!public! !
!WebsideEvaluation categoriesFor: #requestor!public! !
!WebsideEvaluation categoriesFor: #requestor:!public! !
!WebsideEvaluation categoriesFor: #result!public! !
!WebsideEvaluation categoriesFor: #result:!public! !
!WebsideEvaluation categoriesFor: #resume!public! !
!WebsideEvaluation categoriesFor: #resumed!public! !
!WebsideEvaluation categoriesFor: #state!public! !
!WebsideEvaluation categoriesFor: #waitForResult!public! !
!WebsideEvaluation categoriesFor: #wasCancelled!public! !

!WebsideEvaluation class methodsFor!

new
	 ^super new initialize! !
!WebsideEvaluation class categoriesFor: #new!public! !

WebsideResource guid: (GUID fromString: '{fb90d8b3-e1bf-4830-b05d-2d19107070b8}')!
WebsideResource comment: ''!
!WebsideResource categoriesForClass!Unclassified! !
!WebsideResource methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'id' put: id asString;
		yourself!

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

addChange: change
	self changes add: change!

baseUri
	^ baseUri!

baseUri: aString
	baseUri := aString!

changes	^ self resourcesAt: #changes!

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
		routeGET: '/packages' to: #packages;
		routeGET: '/packages/{name}' to: #package;
		routeGET: '/packages/{name}/classes' to: #packageClasses;
		routeGET: '/packages/{name}/methods' to: #packageMethods;
		routeGET: '/classes' to: #classes;
		routeGET: '/classes/{name}' to: #classDefinition;
		routeGET: '/classes/{name}/superclasses' to: #superclasses;
		routeGET: '/classes/{name}/subclasses' to: #subclasses;
		routeGET: '/classes/{name}/variables' to: #variables;
		routeGET: '/classes/{name}/instance-variables' to: #instanceVariables;
		routeGET: '/classes/{name}/class-variables' to: #classVariables;
		routeGET: '/classes/{name}/categories' to: #categories;
		routeGET: '/usual-categories' to: #usualCategories;
		routeGET: '/classes/{name}/used-categories' to: #usedCategories;
		routeGET: '/classes/{name}/methods' to: #methods;
		routeGET: '/classes/{name}/selectors' to: #selectors;
		routeGET: '/classes/{name}/methods/{selector}' to: #method;
		routeGET: '/methods' to: #methods;
		routeGET: '/methodtemplate' to: #methodTemplate;
		routeGET: '/search' to: #search!

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
	router
		routePOST: '/evaluations' to: #evaluateExpression;
		routeGET: '/evaluations' to: #activeEvaluations;
		routeGET: '/evaluations/{id}' to: #activeEvaluation;
		routeDELETE: '/evaluations/{id}' to: #cancelEvaluation;
		routePOST: '/evaluations/{id}/pause' to: #pauseEvaluation;
		routePOST: '/evaluations/{id}/resume' to: #resumeEvaluation!

initializeExtensionsRoutes
	router routeGET: '/extensions' to: #extensions!

initializeGeneralRoutes
	router
		routeGET: '/dialect' to: #dialect;
		routeGET: '/logo' to: #logo;
		routeGET: '/colors' to: #colors;
		routePOST: '/save' to: #saveImage;
		routeGET: '/icons' to: #icons!

initializeObjectsRoutes            router		routeGET: '/objects' to: #pinnedObjects;		routeGET: '/objects/{id}' to: #pinnedObject;		routeDELETE: '/objects/{id}' to: #unpinObject;		routeGET: '/objects/{id}/*' to: #pinnedObjectSlots;		routePOST: '/objects' to: #pinObjectSlot;		routeDELETE: '/objects' to: #unpinAllObjects    !

initializePreflightRoutes
	router routeOPTIONS: '/*' to: [:request | self handlePreflightRequest: request]	"This is not that well"!

initializeResources
	"Changes are stored here until a better way to gather system changes is implemented"
	resources := Dictionary new.
	resources
		at: #evaluations put: Dictionary new;
		at: #objects put: Dictionary new;
		at: #workspaces put: Dictionary new;
		at: #debuggers put: Dictionary new;
		at: #testRuns put: Dictionary new;
		at: #changes put: OrderedCollection new!

initializeRoutes
	router receiver: [WebsideAPI new server: self].
	self
		initializePreflightRoutes;
		initializeGeneralRoutes;
		initializeCodeRoutes;
		initializeChangesRoutes;
		initializeEvaluationRoutes;
		initializeObjectsRoutes;
		initializeWorkspacesRoutes;
		initializeDebuggingRoutes;
		initializeExtensionsRoutes!

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
		routePUT: '/workspaces/{id}' to: #updateWorkspace;
		routeDELETE: '/workspaces/{id}' to: #deleteWorkspace;
		routeGET: '/workspaces/{id}/bindings' to: #workspaceBindings!

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
!WebsideServer categoriesFor: #addChange:!accessing!public! !
!WebsideServer categoriesFor: #baseUri!accessing!public! !
!WebsideServer categoriesFor: #baseUri:!accessing!public! !
!WebsideServer categoriesFor: #changes!accessing!public! !
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
!WebsideServer categoriesFor: #initializeExtensionsRoutes!initializing!public! !
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

RefactoryPackageChange guid: (GUID fromString: '{a233a4fd-81a3-45c7-9a63-338591e63744}')!
RefactoryPackageChange comment: ''!
!RefactoryPackageChange categoriesForClass!Refactory-Change Objects! !
!RefactoryPackageChange methodsFor!

asUndoOperation!

asWebsideJson
	^super asWebsideJson
		at: 'name' put: packageName;
		yourself!

executeNotifying: aBlock 
	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self primitiveExecute.
	aBlock value.
	^undo!

fromWebsideJson: json
	super fromWebsideJson: json.
	packageName := json at: 'name' ifAbsent: []!

primitiveExecute
	^self subclassResponsibility! !
!RefactoryPackageChange categoriesFor: #asUndoOperation!public! !
!RefactoryPackageChange categoriesFor: #asWebsideJson!public! !
!RefactoryPackageChange categoriesFor: #executeNotifying:!public! !
!RefactoryPackageChange categoriesFor: #fromWebsideJson:!public! !
!RefactoryPackageChange categoriesFor: #primitiveExecute!public! !

ClassifyMethodChange guid: (GUID fromString: '{80aeef58-aca9-4378-b558-42c99c9ae553}')!
ClassifyMethodChange comment: ''!
!ClassifyMethodChange categoriesForClass!Refactory-Change Objects! !
!ClassifyMethodChange methodsFor!

asUndoOperation
	self changeClass removeSelector: selector fromCategory: category asMethodCategory!

asWebsideJson
	| json |
	json := super asWebsideJson.
	^json
		at: 'selector' put: selector;
		at: 'category' put: category;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	selector := json at: 'selector' ifAbsent: [].
	selector ifNotNil: [selector := selector asSymbol].
	category := json at: 'category' ifAbsent: []!

method
	^self changeClass >> selector!

primitiveExecute
	MethodCategory setMethod: self method categories: (Array with: category asMethodCategory)! !
!ClassifyMethodChange categoriesFor: #asUndoOperation!public! !
!ClassifyMethodChange categoriesFor: #asWebsideJson!public! !
!ClassifyMethodChange categoriesFor: #fromWebsideJson:!public! !
!ClassifyMethodChange categoriesFor: #method!public! !
!ClassifyMethodChange categoriesFor: #primitiveExecute!public! !

CommentClassChange guid: (GUID fromString: '{86fa936b-3c4d-4835-88a3-9df9f185b725}')!
CommentClassChange comment: ''!
!CommentClassChange categoriesForClass!Refactory-Change Objects! !
!CommentClassChange methodsFor!

asUndoOperation
	self class new comment: ''!

asWebsideJson
	^super asWebsideJson
		at: 'comment' put: comment;
		yourself!

comment: aString
	comment := aString!

fromWebsideJson: json
	super fromWebsideJson: json.
	comment := json at: 'comment' ifAbsent: ['']!

primitiveExecute
	self changeClass comment: comment! !
!CommentClassChange categoriesFor: #asUndoOperation!public! !
!CommentClassChange categoriesFor: #asWebsideJson!public! !
!CommentClassChange categoriesFor: #comment:!public! !
!CommentClassChange categoriesFor: #fromWebsideJson:!public! !
!CommentClassChange categoriesFor: #primitiveExecute!public! !

RefactoryCategoryChange guid: (GUID fromString: '{a56b47bc-9b75-4e97-b085-6127c9cb60eb}')!
RefactoryCategoryChange comment: ''!
!RefactoryCategoryChange categoriesForClass!Refactory-Change Objects! !
!RefactoryCategoryChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'category' put: category;
		yourself!

category: aString
	category := aString!

fromWebsideJson: json
	super fromWebsideJson: json.
	category := json at: 'category' ifAbsent: []! !
!RefactoryCategoryChange categoriesFor: #asWebsideJson!public! !
!RefactoryCategoryChange categoriesFor: #category:!public! !
!RefactoryCategoryChange categoriesFor: #fromWebsideJson:!public! !

AddCategoryChange guid: (GUID fromString: '{8eb4977e-9a49-4989-bafc-f2b365181fa2}')!
AddCategoryChange comment: ''!
!AddCategoryChange categoriesForClass!Refactory-Change Objects! !
!AddCategoryChange methodsFor!

asUndoOperation
	^RemoveCategoryChange new
		changeClass: self changeClass;
		category: category;
		yourself!

primitiveExecute
	| class catalogue existing |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	existing := catalogue keys detect: [:k | k name = category] ifNone: [].
	existing ifNil: [catalogue at: category asMethodCategory put: {}]! !
!AddCategoryChange categoriesFor: #asUndoOperation!public! !
!AddCategoryChange categoriesFor: #primitiveExecute!public! !

RemoveCategoryChange guid: (GUID fromString: '{ae6cec4f-39ab-4bad-a9c8-ebb8593dd62d}')!
RemoveCategoryChange comment: ''!
!RemoveCategoryChange categoriesForClass!Refactory-Change Objects! !
!RemoveCategoryChange methodsFor!

asUndoOperation
	^AddCategoryChange new
		changeClass: self changeClass;
		category: category;
		yourself!

primitiveExecute
	| class catalogue existing |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	existing := catalogue keys detect: [:k | k name = category] ifNone: [].
	existing ifNotNil: [catalogue removeKey: existing]! !
!RemoveCategoryChange categoriesFor: #asUndoOperation!public! !
!RemoveCategoryChange categoriesFor: #primitiveExecute!public! !

RenameCategoryChange guid: (GUID fromString: '{56140d1e-1ec2-4cd9-aa5a-abc85a9200d5}')!
RenameCategoryChange comment: ''!
!RenameCategoryChange categoriesForClass!Refactory-Change Objects! !
!RenameCategoryChange methodsFor!

asUndoOperation
	^RenameCategoryChange new
		changeClass: self changeClass;
		category: newName;
		newName: category;
		yourself!

asWebsideJson
	^super asWebsideJson
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	newName := json at: 'newName' ifAbsent: []!

newName: aString
	newName := aString!

primitiveExecute
	| class catalogue source target methods |
	class := self changeClass.
	catalogue := class methodsCatalogue.
	source := catalogue keys detect: [:k | k name = category] ifNone: [^self].
	target := catalogue keys detect: [:k | k name = newName] ifNone: [].
	methods := catalogue at: source.
	target notNil
		ifTrue: [methods := (catalogue at: target) copyWith: methods]
		ifFalse: [target := newName asMethodCategory].
	catalogue
		at: target put: methods;
		removeKey: source! !
!RenameCategoryChange categoriesFor: #asUndoOperation!public! !
!RenameCategoryChange categoriesFor: #asWebsideJson!public! !
!RenameCategoryChange categoriesFor: #fromWebsideJson:!public! !
!RenameCategoryChange categoriesFor: #newName:!public! !
!RenameCategoryChange categoriesFor: #primitiveExecute!public! !

AddPackageChange guid: (GUID fromString: '{a2e72d80-c0cb-4bf5-8398-127e871225f6}')!
AddPackageChange comment: ''!
!AddPackageChange categoriesForClass!Refactory-Change Objects! !
!AddPackageChange methodsFor!

primitiveExecute
	| pm filename |
	pm := Package manager.
	^pm packageNamed: packageName
		ifNone: 
			[filename := File composePath: PackageFolder dolphinRootPathname subPath: packageName.
			pm newPackage: filename]! !
!AddPackageChange categoriesFor: #primitiveExecute!public! !

RemovePackageChange guid: (GUID fromString: '{411f07c2-aecb-44a9-9d66-f292cff602fb}')!
RemovePackageChange comment: ''!
!RemovePackageChange categoriesForClass!Refactory-Change Objects! !
!RemovePackageChange methodsFor!

primitiveExecute
	| pm package |
	pm := Package manager.
	package := pm packageNamed: packageName.
	package ifNotNil: [pm removePackage: package]! !
!RemovePackageChange categoriesFor: #primitiveExecute!public! !

RenamePackageChange guid: (GUID fromString: '{64e42418-9e10-4e76-81bf-a783daad96b7}')!
RenamePackageChange comment: ''!
!RenamePackageChange categoriesForClass!Refactory-Change Objects! !
!RenamePackageChange methodsFor!

asWebsideJson
	^super asWebsideJson
		at: 'name' put: packageName;
		at: 'newName' put: newName;
		yourself!

fromWebsideJson: json
	super fromWebsideJson: json.
	newName := json at: 'newName' ifAbsent: []!

primitiveExecute
	| pm package |
	pm := Package manager.
	package := pm packageNamed: packageName.
	package ifNotNil: [pm renamePackage: package to: newName]! !
!RenamePackageChange categoriesFor: #asWebsideJson!public! !
!RenamePackageChange categoriesFor: #fromWebsideJson:!public! !
!RenamePackageChange categoriesFor: #primitiveExecute!public! !

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
	self assert: 'http://www.petrovr.com' asURL = 'HTTP://WWW.PETROVR.COM' asURL!

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

asWebsideJson
	| json |
	json := super asWebsideJson.
	json
		at: 'id' put: id asString;
		at: 'source' put: contents.
	^json!

bindings
	^bindings!

contents
	^contents!

contents: aString
	contents := aString!

declareVariable: aString	bindings at: aString reduced put: nil!

initialize	super initialize.	bindings := Dictionary new!

receiver	^nil! !
!WebsideWorkspace categoriesFor: #asWebsideJson!public! !
!WebsideWorkspace categoriesFor: #bindings!public! !
!WebsideWorkspace categoriesFor: #contents!public! !
!WebsideWorkspace categoriesFor: #contents:!public! !
!WebsideWorkspace categoriesFor: #declareVariable:!public! !
!WebsideWorkspace categoriesFor: #initialize!public! !
!WebsideWorkspace categoriesFor: #receiver!public! !

"Binary Globals"!

