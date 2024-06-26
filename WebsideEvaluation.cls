"Filed out from Dolphin Smalltalk 7"!

Object subclass: #WebsideEvaluation
	instanceVariableNames: 'id expression receiver context requestor priority process state result error'
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

