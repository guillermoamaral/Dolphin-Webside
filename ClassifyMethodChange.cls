"Filed out from Dolphin Smalltalk 7"!

RefactoryClassChange subclass: #ClassifyMethodChange
	instanceVariableNames: 'selector category'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

