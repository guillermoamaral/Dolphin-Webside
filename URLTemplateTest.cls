"Filed out from Dolphin Smalltalk"!

TestCase subclass: #URLTemplateTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

!URLTemplateTest categoriesForMethods!
testArguments!public! !
testMatching!public! !
testTrailingSlash!public! !
testVariablePath!public! !
!

