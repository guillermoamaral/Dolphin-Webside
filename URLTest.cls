"Filed out from Dolphin Smalltalk"!

TestCase subclass: #URLTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

!URLTest categoriesForMethods!
testAddPath!public! !
testBaseUri!public! !
testBaseUrl!public! !
testConcatenation!public! !
testDefaultPort!public! !
testEncodedQuery!public! !
testEquality!public! !
testFreeFormatter1!public! !
testFreeFormatter10!public! !
testFreeFormatter11!public! !
testFreeFormatter12!public! !
testFreeFormatter13!public! !
testFreeFormatter14!public! !
testFreeFormatter15!public! !
testFreeFormatter16!public! !
testFreeFormatter17!public! !
testFreeFormatter18!public! !
testFreeFormatter19!public! !
testFreeFormatter2!public! !
testFreeFormatter20!public! !
testFreeFormatter21!public! !
testFreeFormatter22!public! !
testFreeFormatter23!public! !
testFreeFormatter3!public! !
testFreeFormatter4!public! !
testFreeFormatter5!public! !
testFreeFormatter6!public! !
testFreeFormatter7!public! !
testFreeFormatter8!public! !
testFreeFormatter9!public! !
testHost!public! !
testMakeAbsolute!public! !
testODataQueryName!public! !
testPath!public! !
testProtocol!public! !
testQuery!public! !
testRelative!public! !
testRemoveSegment!public! !
testRemoveSubpath!public! !
testRepeatedOption!public! !
testScheme!public! !
testSlashConcatenation!public! !
testSlashInBetween!public! !
testTrailingSlash!public! !
testWikipedia1!public! !
testWikipedia2!public! !
testWikipedia3!public! !
testWikipedia4!public! !
testWikipedia5!public! !
!

