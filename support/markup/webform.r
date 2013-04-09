REBOL [
	Title: "REBOL <-> Web Form"
	Author: "Christopher Ross-Gill"
	Date: 13-Jul-2011
	Version: 0.1.5
	Purpose: "Convert a Rebol block to a URL-Encoded Web Form string"
	Comment: "Conforms to application/x-www-form-urlencoded"
	File: %altwebform.r
	Type: 'module
	Exports: [url-decode load-webform url-encode to-webform]
	Example: [
		"a=3&aa.a=1&b.c=1&b.c=2"
		[a "3" aa [a "1"] b [c ["1" "2"]]]
	]
]

url-decode: use [deplus decrlf][
	deplus: func [text][
		parse/all text [any [to #"+" text: (change text #" ")] to end]
		head text
	]

	decrlf: func [text][
		parse/all text [any [to crlf text: (remove text)] to end]
		head text
	]

	func [text [any-string! none!]][
		decrlf dehex deplus either text [to-string text][""]
	]
]

load-webform: use [result path string pair as-path][
	result: copy []

	as-path: func [name [string!]][to-path to-block replace/all name #"." #" "]

	path: use [aa an wd][
		aa: #[bitset! 64#{AAAAAAAAAAD+//8H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
		an: #[bitset! 64#{AAAAAAAg/wP+//+H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
		wd: [aa 0 40 an] ; one alpha, any alpha/numeric/dash/underscore
		[wd 0 6 [#"." wd]]
	]

	string: use [ch hx][
		ch: charset {*-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz!$'(),/:;?@[\]^^`{|}~}
		hx: #[bitset! 64#{AAAAAAAA/wN+AAAAfgAAAAAAAAAAAAAAAAAAAAAAAAA=}]
		[any [ch | #"+" | #"%" 2 hx]] ; any [unreserved | percent-encoded]
	]

	pair: use [name value tree][
		[
			copy name path #"=" copy value string [#"&" | end]
			(
				tree: :result
				name: as-path name
				value: url-decode value

				until [
					tree: any [
						find/tail tree name/1
						insert tail tree name/1
					]

					name: next name

					switch type?/word tree/1 [
						none! [unless tail? name [insert/only tree tree: copy []]]
						string! [change/only tree tree: reduce [tree/1]]
						block! [tree: tree/1]
					]

					if tail? name [append tree value]
				]
			)
		]
	]

	func [
		[catch] "Loads Data from a URL-Encoded Web Form string"
		webform [string! none!]
	][
		webform: any [webform ""]
		result: copy []

		either parse/all webform [opt #"&" any pair][result][
			make error! "Not a URL Encoded Web Form"
		]
	]
]

url-encode: use [ch][
	ch: #[bitset! 64#{AAAAAABg/wP+//+H/v//RwAAAAAAAAAAAAAAAAAAAAA=}]
	func [text [any-string!]][
		either parse/all copy text [
			any [
				some ch | #" " text: (change back text #"+") |
				skip text: (change/part back text join "%" enbase/base to-string text/-1 16 1)
			]
		][head text][""]
	]
]

to-webform: use [
	webform form-key emit
	here path value block array object
][
	path: []
	form-key: does [
		remove head foreach key path [insert "" reduce ["." key]]
	]

	emit: func [data][
		repend webform ["&" form-key "=" url-encode data]
	]

	value: [
		  here: number! (emit form here/1)
		| [logic! | 'true | 'false] (emit form here/1)
		| [none! | 'none]
		| date! (replace form date "/" "T")
		| [any-string! | tuple! | money! | time! | pair!] (emit form here/1)
	]

	array: [any value end]

	object: [
		any [
			here: [word! | set-word!] (insert path to-word here/1)
			[value | block] (remove path)
		] end
	]

	block: [
		here: [
			  any-block! (change/only here copy here/1)
			| object! (change/only here third here/1)
		] :here into [object | array]
	]

	func [
		"Serializes block data as URL-Encoded Web Form string"
		data [block! object!] /prefix
	][
		clear path
		webform: copy ""
		data: either object? data [third data][copy data]
		if parse copy data object [
			either all [prefix not tail? next webform][back change webform "?"][remove webform]
		]
	]
]