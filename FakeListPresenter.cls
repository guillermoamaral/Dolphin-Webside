"Filed out from Dolphin Smalltalk"!

Object subclass: #FakeListPresenter
	instanceVariableNames: 'list selection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FakeListPresenter guid: (GUID fromString: '{73f8ea67-5b8b-47fb-9ab2-8018f4487c8d}')!

FakeListPresenter comment: ''!

!FakeListPresenter categoriesForClass!Unclassified! !

!FakeListPresenter methodsFor!

initialize
	super initialize.
	list := OrderedCollection new!

list
	 ^list!

list: aCollection
	 list := aCollection!

model
	 ^list!

selectableItems
	 ^self!

selection
	^selection!

selection: anObject 
	 selection := anObject!

selectionOrNil
	^self selection!

selectionOrNil: anObject 
	 self selection: anObject! !

!FakeListPresenter categoriesForMethods!
initialize!public! !
list!public! !
list:!public! !
model!public! !
selectableItems!public! !
selection!public! !
selection:!public! !
selectionOrNil!public! !
selectionOrNil:!public! !
!

!FakeListPresenter class methodsFor!

new
	 ^super new initialize! !

!FakeListPresenter class categoriesForMethods!
new!public! !
!

