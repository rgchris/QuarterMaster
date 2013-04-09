REBOL [
	title: "AltXML"
	file: %altxml.r
	author: "Christopher Ross-Gill"
	date: 7-June-2009
	version: 0.2.0
	type: 'module
	exports: [decode-xml load-xml]
]

decode-xml: use [nm hx ns mk rf word to-utf-char entity][
	nm: #[bitset! 64#{AAAAAAAA/wMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	hx: #[bitset! 64#{AAAAAAAA/wN+AAAAfgAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	ns: ["lt" 60 "gt" 62 "amp" 38 "quot" 34 "apos" 39]

	word: use [w1 w+][
		w1: #[bitset! 64#{AAAAAAAAAAD+//+H/v//B/////////////////////8=}]
		w+: #[bitset! 64#{AAAAAABg/wP+//+H/v//B/////////////////////8=}]
		[w1 any w+]
	]
	
	to-utf-char: use [os fc en][
		os: [0 192 224 240 248 252]
		fc: [1 64 4096 262144 16777216 1073741824]
		en: [127 2047 65535 2097151 67108863 2147483647]

		func [int [integer!] /local char][
			repeat ln 6 [
				if int <= en/:ln [
					char: reduce [os/:ln + to integer! (int / fc/:ln)]
					repeat ps ln - 1 [
						insert next char (to integer! int / fc/:ps) // 64 + 128
					]
					break
				]
			]

			to-string to-binary char
		]
	]

	entity: [
		mk: #"&" [
			  copy rf word ";" (rf: any [select ns rf 63])
			| #"#" [
				  #"x" copy rf 2 4 hx ";" (rf: to-integer to-issue rf)
				| copy rf 2 5 nm ";" (rf: to-integer rf)
			]
		] ex: (mk: change/part mk to-utf-char rf ex) :mk
	]

	func [text [string! none!]][
		either text [
			all [parse/all text [any [to "&" [entity | skip]] to end] text]
		][copy ""]
	]
]

load-xml: use [
	xml! doc make-node
	space word entity text name attribute element header content
][
	xml!: context [
		name: space: value: tree: branch: position: none

		flatten: use [xml path emit encode de-ns element attribute tag attr text][
			path: copy []
			emit: func [data][repend xml data]

			encode: use [ch tx][
				ch: #[bitset! 64#{/////7v//+////////////////////////////////8=}]
				; complement charset {<"&}
				tx: [
					some ch | text: skip (
						text: change/part text switch text/1 [
							#"<" ["&lt;"] #"^"" ["&quot;"] #"&" ["&amp;"]
						] 1
					)
				]
				func [text][parse/all text: copy text [some tx] head text]
			]

			de-ns: func [name [tag! issue!]][
				join "" [to-string copy/part head name name ":" to-string name]
			]

			attribute: [
				set attr issue! set text [any-string! | number! | logic!] (
					attr: either head? attr [to-string attr][de-ns attr]
					emit [" " attr {="} encode form text {"}]
				)
			]

			element: [
				set tag tag! (
					insert path tag: either head? tag [to-string tag][de-ns tag]
					emit ["<" either head? tag [tag][]]
				) [
					  none! (emit " />" remove path)
					| set text string! (emit [">" encode text "</" tag ">"] remove path)
					| into [
						any attribute [
							  end (emit " />" remove path)
							| (emit ">") some element end (emit ["</" take path ">"])
						]
					]
				]
				| %.txt set text string! (emit encode text)
				| attribute
			]

			does [
				xml: copy ""
				if parse tree element [xml]
			]
		]

		find-element: func [tag [tag! issue!]][
			find value tag
		]

		get-by-tag: func [tag /local rule hits hit][
			hits: copy []
			parse tree rule: [
				some [
					opt [hit: tag skip (append hits make-node hit) :hit]
					skip [into rule | skip]
				]
			] hits
		]

		get-by-id: func [id /local rule hit at][
			parse tree rule: [
				some [
					  at: tag! into [thru #id id to end] (hit: any [hit make-node at])
					| skip [into rule | skip]
				]
			] hit
		]

		text: has [rule text part][
			case/all [
				string? value [text: value]
				block? value [
					text: copy ""
					parse value rule: [
						any [[%.txt | tag!] set part string! (append text part) | skip into rule | 2 skip]
					]
					text: unless empty? text [trim/auto text]
				]
				string? text [trim/auto text]
			]
		]

		get: func [name [issue! tag!] /local hit at][
			if parse tree [
				tag! into [
					any [
						  at: name [block! (hit: make-node at) | set hit skip] to end
						| [issue! | tag! | file!] skip
					]
				]
			][hit]
		]

		sibling: func [/before /after][
			case [
				all [after find [tag! file!] type?/word position/3] [
					make-node skip position 2
				]
 				all [before find [tag! file!] type?/word position/-2] [
					make-node skip position -2
				]
			]
		]

		parent: has [branch]["Need Branch" none]

		children: has [hits at][
			hits: copy []
			parse case [
				block? value [value] string? value [reduce [%.txt value]] none? value [[]]
			][
				any [issue! skip]
				any [at: [tag! | file!] skip (append hits make-node at)]
			]
			hits
		]

		attributes: has [hits at][
			hits: copy []
			parse either block? value [value][[]] [
				any [at: issue! skip (append hits make-node at)] to end
			]
			hits
		]

		clone: does [make-node tree]

		append-child: func [name data /local at][
			case [
				none? position/2 [value: tree/2: position/2: copy []]
				string? position/2 [
					new-line value: tree/2: position/2: compose [%.txt (position/2)] true
				]
			]

			either issue? name [
				parse position/2 [any [issue! skip] at:]
			][at: tail position/2]

			insert at reduce [name data]
			new-line at true
		]

		append-text: func [text][
			case [
				none? position/2 [value: tree/2: position/2: text]
				string? position/2 [append position/2 text]
				%.txt = pick tail position/2 -2 [append last position/2 text]
				block? position/2 [append-child %.txt text]
			]
		]

		append-attr: func [name value][
			name: any [remove find name: to-issue name ":" name]
			append-child name value
		]
	]

	doc: make xml! [
		branch: make block! 10
		document: true
		new: does [clear branch tree: position: reduce ['document none]]

		open-tag: func [tag][
			insert/only branch position
			tag: any [remove find tag: to-tag tag ":" tag]
			tree: position: append-child tag none
		]

		close-tag: func [tag][
			tag: any [remove find tag: to-tag tag ":" tag]
			while [tag <> position/1][
				; probe reform ["No End Tag:" position/1]
				if empty? branch [make error! "End tag error!"]
				take branch
			]
			tree: position: take branch
		]
	]

	make-node: func [here /base][
		make either base [doc][xml!][
			position: here
			name: here/1
			space: all [any-string? name not head? name copy/part head name name]
			value: here/2
			tree: reduce [name value]
		]
	]

	space: use [space][
		space: charset "^-^/^M "
		[some space]
	]

	word: use [w1 w+][
		w1: #[bitset! 64#{AAAAAAAAAAD+//+H/v//B/////////////////////8=}]
		w+: #[bitset! 64#{AAAAAABg/wP+//+H/v//B/////////////////////8=}]
		[w1 any w+]
	]

	entity: use [nm hx][
		nm: #[bitset! 64#{AAAAAAAA/wMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]
		hx: #[bitset! 64#{AAAAAAAA/wN+AAAAfgAAAAAAAAAAAAAAAAAAAAAAAAA=}]
		[#"&" [word | #"#" [1 5 nm | #"x" 1 4 hx]] ";" | #"&"]
	]

	text: use [char value][
		; intersect charset ["^-^/^M" #" " - #"^(FF)"] complement charset [#"^(00)" - #"^(20)" "&<"]
		char: #[bitset! 64#{AAAAAL7//+////////////////////////////////8=}]
		[
			copy value [
				opt space [char | entity]
				any [char | entity | space]
			] (doc/append-text decode-xml value)
		]
	]

	name: [word opt [":" word]]

	attribute: use [q1 q2 attr value][
		; intersect charset ["^-^/^M" #" " - #"^(FF)"] complement charset {"&<}
		q1: #[bitset! 64#{ACYAALv//+////////////////////////////////8=}]
		; intersect charset ["^-^/^M" #" " - #"^(FF)"] complement charset {&'<}
		q2: #[bitset! 64#{ACYAAD///+////////////////////////////////8=}]
		[	opt space copy attr name opt space "=" opt space [
				; lone ampersand is 'loose' not 'strict'
				  {"} copy value any [q1 | entity | "&"] {"}
				| {'} copy value any [q2 | entity | "&"] {'}
			] (doc/append-attr attr decode-xml value)
		]
	]

	element: use [tag value][
		[	#"<" [
				copy tag name (doc/open-tag tag) any attribute opt space [
					  "/>" (doc/close-tag tag)
					| #">" content "</" copy tag name (doc/close-tag tag) opt space #">"
				]
				| #"!" [
					  "--" copy value to "-->" 3 skip ; (doc/append-child /comment value)
					| "[CDATA[" copy value to "]]>" 3 skip (doc/append-text value)
				]
			]
		]
	]

	header: [
		opt [#{efbbbf}] any [
			  space 
			| "<" ["?xml" thru "?>" | "!" ["--" thru "-->" | thru ">"] | "?" thru "?>"]
		]
	]

	content: [any [text | element | space]]

	load-xml: func [
		"Transform an XML document to a REBOL block"
		document [any-string!] "An XML string/location to transform"
		/dom "Returns an object with DOM-like methods to traverse the XML tree"
		/local root
	][
		if any [file? document url? document][document: read document]
		root: doc/new
		parse/all/case document [header element to end]
		doc/tree: any [root/document []]
		doc/value: doc/tree/2
		either dom [make-node/base doc/tree][doc/tree]
	]
]