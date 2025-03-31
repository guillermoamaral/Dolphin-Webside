"Filed out from Dolphin Smalltalk"!

Object subclass: #WebsideEvaluation
	instanceVariableNames: 'id expression receiver context requestor priority process state result error semaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

cancel
	process ifNotNil: [process terminate].
	self cancelled!

cancelled
	state := #cancelled.
	self
		trigger: #cancelled;
		finalized!

context	^context!

context: anObject	context := anObject!

debug
	self evaluateBlock: 
			[process suspend.
			result := Compiler evaluate: expression for: receiver]!

error
	^error!

error: anException
	error := anException!

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
	"Priority should be already set. In fact, it is initiallized as Processor userSchedulingPriority.
However, for some reason that should be understood/modified, HttpServer loop is scheduled at a lower priority,
making the resuming of the evaluation process to take control immediately when #resume is sent below, making synchronous
what otherwise would be asynchronous evaluation."
	process
		priority: Processor activeProcess priority - 1;
		name: 'Webside evaluation ' , id asString;
		resume!

expression
	^expression!

expression: aString
	expression := aString!

failed
	state := #failed.
	self
		trigger: #failed;
		finalized!

finalized
	semaphore notNil ifTrue: [semaphore signal].
	^self trigger: #finalized!

finished
	state := #finished.
	self
		trigger: #finished;
		finalized!

hasFailed
	^state == #failed!

hasFinished
	^state == #finished!

id	^id!

id: uuid
	id := uuid!

initialize
	super initialize.
	id := IID newUnique.
	state := #pending.
	priority := Processor userSchedulingPriority!

isEvaluating
	^state == #evaluating!

isFinalized
	^self hasFinished or: [self hasFailed or: [self wasCancelled]]!

isPaused
	^state == #paused!

isPending
	^state == #pending!

pause
	self paused.
	process suspend!

paused
	state := #paused.
	self trigger: #paused!

priority: anInteger	priority := anInteger!

process	^process!

process: aProcess
	process := aProcess!

receiver
	^receiver!

receiver: anObject
	receiver := anObject!

requestor
	^requestor!

requestor: anObject
	requestor := anObject!

result
	self hasFinished ifTrue: [^result]!

result: anObject
	result := anObject!

resume	self resumed.	process resume.!

resumed
	state := #evaluating.
	self trigger: #resumed!

state
	 ^state!

waitForResult
	self isFinalized
		ifFalse: 
			[semaphore := Semaphore new.
			semaphore wait].
	^self result!

wasCancelled
	^state == #cancelled! !

!WebsideEvaluation categoriesForMethods!
asWebsideJson!public! !
cancel!public! !
cancelled!public! !
context!public! !
context:!public! !
debug!public! !
error!public! !
error:!public! !
evaluate!public! !
evaluateBlock:!public! !
expression!public! !
expression:!public! !
failed!public! !
finalized!public! !
finished!public! !
hasFailed!public! !
hasFinished!public! !
id!public! !
id:!public! !
initialize!private! !
isEvaluating!public! !
isFinalized!public! !
isPaused!public! !
isPending!public! !
pause!public! !
paused!public! !
priority:!public! !
process!public! !
process:!public! !
receiver!public! !
receiver:!public! !
requestor!public! !
requestor:!public! !
result!public! !
result:!public! !
resume!public! !
resumed!public! !
state!public! !
waitForResult!public! !
wasCancelled!public! !
!

!WebsideEvaluation class methodsFor!

new
	 ^super new initialize! !

!WebsideEvaluation class categoriesForMethods!
new!public! !
!

