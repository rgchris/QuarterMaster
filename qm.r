#!/usr/local/bin/rebol -c

REBOL [
	Title: "QuarterMaster"
	Author: "Christopher Ross-Gill"
	Version: 0.3.12
	Notes: {Warning: Work-In-Progress - no liabilities for damage, etc.}
	License: http://creativecommons.org/licenses/by-sa/3.0/
]

config: construct [
	public-key: "a key that will be visible - for cookie ids"
	private-key: "a private key - for encrypting passwords"
	session-timeout: 0:02:00
	zone: -6:00
	post-limit: 500 ;-- not active yet; can be altered per controller/action
	default-controller: "blog"
	spaces: [
		;-- QM requires entries for "system" "data" "site" "support"
		"system"  %/Path/To/app/application/
		"space"   %/Path/To/app/space/
		"site"    %/Path/To/HTTPD/root/
		"support" %/Path/To/support/

		;-- Add more for your convenience
	]
]

;--## APPLICATION BEYOND THIS POINT
;-------------------------------------------------------------------##
system/options/binary-base: 64
system/error/user/type: "QuarterMaster Error"
date: now - now/zone + config/zone
range!: :pair! ; until REBOL v3
else: #[true] ; for 'case statements

random/seed to-integer checksum/secure form now/precise ; any better?
if all [_dbg: find/tail/last any [system/options/cgi/query-string ""] "&debug=" _dbg: tail? _dbg] [
	print "Content-Type: text/plain; charset=utf-8^/"
]

;--## APPLICATION NAMESPACE
;-------------------------------------------------------------------##
qm: context [
	binding: 'self
	controller: metadata: action: #[none]
	models: request: response: #[none]
	alerts: []
	errors: []
	notices: []
	handler: %.rsp
	view-path: #[none]
	title: ""
	code: []
]

;--## EXTENDED CORE FUNCTIONS
;-------------------------------------------------------------------##
context [
	func: make function! [spec [block!] body [block!]][make function! spec body]
	does: func [body [block!]][make function! [] body]

	uses: func [proto [block!] spec [block!]][
		proto: context proto
		func [args [block! object!]] compose/only [
			args: make (proto) args
			do bind (spec) args
		]
	]

	try-else: func [[throw] 'try-first [any-block!] on-fail [block!] /local reason][
		either error? reason: try :try-first bind :on-fail 'reason [:reason]
	]

	assert-all: func [[throw] cases [block!] /local value][
		until [
			set [value cases] do/next cases
			unless value cases/1
			cases: next cases
			any [not value tail? cases]
		]
		any [value]
	]

	; step-through: try-each: func ['steps [block!] else [block!] /local reason][
	; 	foreach [block err-code] steps [
	; 		either error? reason: try :block [do bind else 'reason return err-code][:reason]
	; 	]
	; ]

	fortype: func [[throw] type [datatype!] block [block!] f [any-function!] /local val][
		parse block [some [to type set val type (f :val)]]
	]

	with: func [object [any-word! object! port!] block [any-block!] /only][
		block: bind block object
		either only [block] :block
	]

	envelope: envelop: func [val [any-type!]][either any-block? val [val][reduce [val]]]

	raise: func [[catch] reason][throw make error! rejoin envelop reason]

	export: func [words [word! block!] /to dest [object!] /local word][
		dest: any [dest system/words]
		fortype word! to-block words func [word] [
			set/any in dest word get/any word
			; protect in dest word
		]
	]

	true?: func [test][either test [#[true]][#[false]]]

	export [func does uses try-else assert-all fortype export envelop envelope raise with true?]
]

;--## SERIES HELPERS
;-------------------------------------------------------------------##
context [
	push: func [stack [series! port!] value [any-type!] /only][
		head either only [insert/only stack :value][insert stack :value]
	]

	pop: take: func [series [series! port! none!] /last /part range [integer!] /local result][
		range: any [range 1]
		case [
			none? series [return none]
			last [series: skip tail series negate abs range]
		]
		either part [
			result: copy/part series range
			remove/part series range
		][
			result: pick series 1
			remove series
		]
		result
	]

	pop*: func [stack [series! port!] /local val][
		val: pick stack 1
		remove stack
		:val
	]

	flatten: func [block [any-block!] /once][
		once: either once [
			[(block: insert block pop block)]
		][
			[(insert block pop block)]
		]
		parse block [
			any [block: any-block! (insert block pop block) :block | skip]
		]
		head block
	]

	map: func [series [any-block! port!] action [any-function!] /only /copy /local new][
		if copy [series: system/words/copy/deep series]
		while [not tail? series][
			series: either only [
				change/part/only series action series/1 1
			][
				change/part series action series/1 1
			]
		]
		head series
	]

	map-each: func [[catch throw] 'word [word! block!] series [any-block!] body [block!] /copy /local new][
		case/all [
			word? word [word: envelop word]
			not parse word [some word!][
				raise "WORDS argument should be a word or block of words"
			]
			copy [series: system/words/copy/deep series]
		]
		use word compose/deep [
			while [not tail? series][
				set [(word)] series (body)
				series: change/part series reduce [(word)] (length? word)
			]
		]
		head series
	]

	get-choice: func [word [string! word!] words [any-block!]][
		all [
			word: attempt [to-word word]
			find words word
			word
		]
	]

	get-class: func [classes [block!] item /local type][
		all [
			type: type? classes/1
			classes: find classes item
			first find/reverse classes type
		]
	]

	link-to: func ['path [any-block!] /local out][
		out: copy %""
		path: compose to-block path
		foreach val path [
			either issue? val [append out mold val][repend out ["/" form val]]
		]
	]

	compose-path: func ['path [path! lit-path! word! lit-word!]][
		to-path new-line/all compose to-block envelop path none
	]

	paginate: func [series [series! port!] page [integer! none!] /window padding /size length][
		page: any [page 1]
		length: any [length 15]
		padding: any [padding 2]

		context [
			last: max 1 to-integer (length? series) - 1 / length + 1
			current: max 1 min last page
			next: either last > current [current + 1][false]
			previous: either 1 < current [current - 1][false]
			records: copy/part skip series offset: current - 1 * length length
			upper: copy [] lower: copy []
			repeat cnt padding [
				insert lower current - cnt
				append upper current + cnt
			]
			remove-each val lower [val <= 1]
			remove-each val upper [val >= last]
			start: current - 2 <= padding
			end: last - current - 1 <= padding
		]
	]

	prepare: use [rule flat nest val][
		rule: [val: paren! (do val/1 remove val) :val | get-word! (change/part val get/any val/1 1)]
		flat: [some [rule | skip]]
		nest: [some [rule | into nest | skip]]

		func [block [any-block!] /deep][
			parse block: copy/deep block either deep [nest][flat]
			block
		]
	]

	export [push take pop flatten map map-each get-choice get-class compose-path prepare link-to paginate]
]

;--## KEY-VALUE HELPERS
;-------------------------------------------------------------------##
context [
	add-to: func [ser key val][
		key: envelop key
		map key func [key][as word! key]
		if find key none! [return none]
		until [
			ser: any [
				find/tail ser key/1
				insert tail ser key/1
			]

			key: next key

			switch type?/word ser/1 [
				none! [unless tail? key [insert/only ser ser: copy []]]
				string! [change/only ser ser: envelop ser/1]
				block! [ser: ser/1]
			]

			if tail? key [append ser val]
		]
	]

	get-from: func [series 'key][
		key: copy envelop key
		while [all [not tail? key any-block? series]][
			series: select series pop key
		]
		all [tail? key series]
	]

	export [add-to get-from]
]

;--## CHARACTER SETS
;-------------------------------------------------------------------##
context [
	comment {
	Will consider the possibility of using more bnf friendly words...
	[
		snip: difference charset [#"^(20)" - #"^(7F)"] charset [{:*.<>=} #"{" #"}"]
		chars-n:  charset [#"0" - #"9"]   ; digit
		chars-la: charset [#"a" - #"z"]   ; lower-alpha
		chars-ua: charset [#"A" - #"Z"]   ; upper-alpha
		chars-a:  union chars-la chars-ua ; alpha
		chars-an: union chars-a chars-n   ; alphanumeric
		chars-hx: union chars-n charset [#"A" - #"F" #"a" - #"f"] ; hexdig
		chars-ud: union chars-an charset "*-._!~',"               ; url decode
		chars-u:  union chars-ud charset ":+%&=?"                 ; url
		chars-f:  union chars-an charset "-_"                     ; file
		chars-w1: union chars-a charset "*-._!+?&|"
		chars-w*: union chars-w1 chars-n
		chars-p:  union chars-an charset "-_!+%"   ; path
		chars-sp: charset " ^-"                    ; space
		chars-as: charset ["^/^-" #"^(20)" - #"^(7F)"] ; ascii
		chars-up: charset [#"^(80)" - #"^(FF)"]    ; above ascii
		; chars-ht: exclude union chars-as chars-up charset {&<>"}
		chars-ht: exclude chars-as charset {&<>"}
		chars: complement nochar: charset " ^-^/"
	]
	}

	chars-n:  #[bitset! 64#{AAAAAAAA/wMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	chars-la: #[bitset! 64#{AAAAAAAAAAAAAAAA/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-ua: #[bitset! 64#{AAAAAAAAAAD+//8HAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	chars-a:  #[bitset! 64#{AAAAAAAAAAD+//8H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-an: #[bitset! 64#{AAAAAAAA/wP+//8H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-hx: #[bitset! 64#{AAAAAAAA/wN+AAAAfgAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	chars-ud: #[bitset! 64#{AAAAAIJ0/wP+//+H/v//RwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-u:  #[bitset! 64#{AAAAAKJ8/wf+//+H/v//RwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-f:  #[bitset! 64#{AAAAAAAg/wP+//+H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-w1: #[bitset! 64#{AAAAAEJsAID+//+H/v//FwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-w*: #[bitset! 64#{AAAAAEJs/4P+//+H/v//FwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-p:  #[bitset! 64#{AAAAAKJo/wP+//+H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]
	chars-sp: #[bitset! 64#{AAIAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]
	chars-as: #[bitset! 64#{AAYAAP///////////////wAAAAAAAAAAAAAAAAAAAAA=}]
	chars-up: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAP////////////////////8=}]
	; chars-ht: #[bitset! 64#{AAYAALv//6////////////////////////////////8=}]
	chars-ht: #[bitset! 64#{AAYAALv//6///////////wAAAAAAAAAAAAAAAAAAAAA=}]
	chars:    #[bitset! 64#{//n///7///////////////////////////////////8=}]
	nochar:   #[bitset! 64#{AAYAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=}]

	export [
		chars-n  chars-la chars-ua chars-a chars-an chars-hx
		chars-w1 chars-w* chars-f  chars-p chars-u chars-ud chars-sp
		chars-up chars-as chars-ht chars   nochar
	]
]

;--## UTF-8
;-------------------------------------------------------------------##
context [
	utf-2: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////wAAAAA=}]
	utf-3: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//AAA=}]
	utf-4: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/wA=}]
	utf-5: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA8=}]
	utf-b: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAP//////////AAAAAAAAAAA=}]

	utf-8: [utf-2 1 utf-b | utf-3 2 utf-b | utf-4 3 utf-b | utf-5 4 utf-b]

	utf-os: [0 192 224 240 248 252]
	utf-fc: [1 64 4096 262144 16777216]

	get-ucs-code: func [char /local int][
		int: 0
		char: change char char/1 xor pick utf-os length? char
		forskip char 1 [change char char/1 xor 128]
		char: head reverse head char
		forskip char 1 [int: (to-integer char/1) * (pick utf-fc index? char) + int]
		all [int > 127 int <= 65535 int]
	]

	export [utf-8 get-ucs-code]
]

;--## STRING HELPERS
;-------------------------------------------------------------------##
context [
	pad: func [text length [integer!] /with padding [char!]][
		padding: any [padding #"0"]
		text: form text
		skip tail insert/dup text padding length negate length
	]

	url-encode: func [text [any-string!]][
		parse/all copy text [
			copy text any [
				some chars-ud |
				#" " text: (change back text #"+") |
				skip text: (change/part back text join "%" enbase/base to-string text/-1 16 1)
			]
		]
		text
	]

	deplus: func [text][
		parse/all text [some [to #"+" text: (text: change text #" ") :text] to end]
		head text
	]

	decrlf: func [text][
		parse/all text [some [to crlf text: (text: change/part text #"^/" 2) :text] to end]
		head text
	]

	url-decode: func [text [any-string!]][decrlf dehex deplus to-string text]

	decode-query: func [query [string! none!] /local result name value][
		query: any [query ""]
		result: copy []

		if query [
			remove-each value query: parse/all query "&" [empty? value]
			map/only query func [value][parse/all value "="]
			foreach value query [change/only value parse/all value/1 "."]
		]

		parse query [
			any [
				into [
					set name block! set value opt string! (
						add-to result name any [all [value url-decode value] ""]
					)
				]
			]
		]

		result
	]

	; doesn't work right, yet
	decode-options: func [options [string! none!] type [datatype!]][
		options: any [options ""]
		options: parse lowercase options ";,"
		map options func [val [string!] /local weight][
			either parse/all val ["q=" weight: "0." integer!][
				load weight
			][
				as :type val
			]
		]
	]

	compose-tags: func [body [string!] callback [any-function!] /local out tag block][
		out: make string! length? body
		while [tag: find body "=["][
			insert/part tail out body offset? body tag
			body: either error? err: try [
				block: load/next tag: next tag
			][
				append out "**Tag Loading Error: #"
				tag
			][
				append out any [callback first block ""]
				second block
			]
		]
		append out body
	]

	interpolate: func [body [string!] escapes [any-block!] /local out][
		body: out: copy body

		parse/all body [
			any [
				to #"%" body: (
					body: change/part body reduce any [
						select/case escapes body/2 body/2
					] 2
				) :body
			]
		]

		out
	]

	sanitize: func [text [any-string!] /local char][
		parse/all copy text [
			copy text any [
				text: some chars-ht
				| #"&" (text: change/part text "&amp;" 1) :text
				| #"<" (text: change/part text "&lt;" 1) :text
				| #">" (text: change/part text "&gt;" 1) :text
				| #"^"" (text: change/part text "&quot;" 1) :text
				| #"^M" (remove text) :text 
				| copy char utf-8 (text: change/part text rejoin ["&#" get-ucs-code char ";"] length? char)
				| skip (text: change/part text rejoin ["#(" to-integer text/1 ")"] 1) :text
				; | skip (text: change text "#") :text
			]
		]
		any [text ""]
	]

	; sanitize: func [text][
	; 	replace/all text: copy text #"&" "&amp;"
	; 	replace/all text #"<" "&lt;"
	; 	replace/all text #">" "&gt;"
	; 	replace/all text #"^"" "&quot;"
	; ]

	load-multipart: func [
		[catch] data [binary!] boundary
		/local store name content filetype filename
		file-prototype qchars nchars dchars
	][
		store: copy []
		file-prototype: context [name: data: type: meta: #[none]]

		qchars: #[bitset! 64#{//////v///////////////////////////////////8=}]
		nchars: #[bitset! 64#{//////////f///////////////////////////////8=}]
		
		unless parse/all/case data [boundary data: to end][
			raise "Postdata not Multipart"
		]

		boundary: join crlf boundary

		unless parse/all/case data [
			some [
				"--" crlf end |
				(name: content: filemime: filetype: filename: none)
				crlf {Content-Disposition: form-data; name=}
				[{"} copy name some qchars {"} | copy name some nchars]
				(name: parse/all name ".")
				opt [
					{; filename=} [
						[{"} copy filename any qchars {"} | copy filename any nchars]
						crlf {Content-Type: } copy filetype to crlf
					]
				]
				crlf crlf copy content to boundary boundary (
					content: any [content ""]
					either all [filetype filetype: as path! filetype][
						filename: either filename [
							as file! any [
								find/last/tail filename #"/"
								find/last/tail filename #"\"
								find/last/tail filename #":"
								filename
							]
						][%file.dat]
						content: either filetype/1 = 'text [decrlf content][to-binary content]
						add-to store name make file-prototype [
							name: :filename type: :filetype data: :content
						]
					][
						add-to store name to-string decrlf content
					]
				)
			]
		][
			raise "Invalid Multipart Postdata"
		]
		
		store
	]

	export [
		pad url-encode url-decode decode-query decode-options
		load-multipart compose-tags interpolate sanitize
	]
]

;--## PORT HELPERS
;-------------------------------------------------------------------##
context [
	add-protocol: func ['name id handler /with block][
		unless in system/schemes name [
			system/schemes: make system/schemes compose [
				(to-set-word name) #[none]
			]
		]
		set in system/schemes name make system/standard/port compose [
			scheme: name
			port-id: (id)
			handler: (handler)
			passive: #[none]
			cache-size: 5
			proxy: make object! [host: port-id: user: pass: type: bypass: #[none]]
			(block)
		]
	]

	to-header: func [object [object!] /local header][
		header: make string! (20 * length? first object)
		foreach word next first object [
			if get word: in object word [
				insert tail header reduce [word ": " get word newline]
			]
		]
		header
	]

	codes: [read 1 write 2 append 4 new 8 binary 32 lines 64 direct 524288]
	get-port-flags: func [port words][
		remove-each word copy words [
			word: select codes word
			word <> (port/state/flags and word)
		]
	]

	chars: ; charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" "-_!+%"]
	#[bitset! 64#{AAAAACIo/wP+//+H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]

	space!: context [
		root: domain: path: target: folder: file: suffix: #[none]
	]

	get-space: func [base [url!] location [url!] /local space][
		base: form base
		space: make space! [uri: :location]

		if all with/only space [
			parse/all uri [
				base
				copy domain some chars #"/"
				copy path any [some [some chars | #"."] #"/"]
				copy target opt [any chars #"." 1 10 chars]
			]
			root: select config/spaces domain
		] with/only space [
			path: all [path to-file path]
			target: all [target to-file target]
			folder: join root any [path ""]
			file: join folder any [target ""]
			suffix: suffix? file
			self
		]
	]

	export [add-protocol to-header get-port-flags get-space]
]

;--## VALUES HELPERS
;-------------------------------------------------------------------##
context [
	pad-zone: func [time /flat][
		rejoin [
			pick "-+" time/hour < 0
			pad abs time/hour 2
			either flat [""][#":"]
			pad time/minute 2
		]
	]

	get-iso-year: func [year [integer!] /local d1 d2][
		d1: to-date join "4-1-" year
		d2: to-date join "28-12-" year
		return reduce [d1 + 1 - d1/weekday d2 + 7 - d2/weekday]
	]

	to-iso-week: func [date [date!] /local out d1 d2][
		out: 0x0
		set [d1 d2] get-iso-year out/y: date/year

		case [
			date < d1 [d1: first get-iso-year out/y: date/year - 1]
			date > d2 [d1: first get-iso-year out/y: date/year + 1]
		]

		out/x: date + 8 - date/weekday - d1 / 7
		out
	]

	date-codes: [
		#"a" [copy/part pick system/locale/days date/weekday 3]
		#"A" [pick system/locale/days date/weekday]
		#"b" [copy/part pick system/locale/months date/month 3]
		#"B" [pick system/locale/months date/month]
		#"C" [to-integer date/year / 100]
		#"d" [pad date/day 2]
		#"D" [date/year #"/" pad date/month 2 #"/" pad date/day 2]
		#"e" [date/day]
		#"g" [pad (second to-iso-week date) // 100 2]
		#"G" [second to-iso-week date]
		#"h" [time/hour + 11 // 12 + 1]
		#"H" [pad time/hour 2]
		#"i" [any [get-class [st 1 21 31 nd 2 22 rd 3 23] date/day "th"]]
		#"I" [pad time/hour + 11 // 12 + 1 2]
		#"j" [pad date/julian 3]
		#"J" [date/julian]
		#"m" [pad date/month 2]
		#"M" [pad time/minute 2]
		#"p" [pick ["am" "pm"] time/hour < 12]
		#"P" [pick ["AM" "PM"] time/hour < 12]
		#"S" [pad round time/second 2]
		#"t" [#"^-"]
		#"T" [pad time/hour 2 #":" pad time/minute 2 #":" pad round time/second 2]
		#"u" [date/weekday]
		#"U" [pad to-integer date/julian + 6 - (date/weekday // 7) / 7 2]
		#"V" [pad first to-iso-week date 2]
		#"w" [date/weekday // 7]
		#"W" [pad to-integer date/julian + 7 - date/weekday / 7 2]
		#"y" [pad date/year // 100 2]
		#"Y" [date/year]
		#"z" [pad-zone/flat zone]
		#"Z" [pad-zone zone]
	]

	form-date: func [date [date!] format [any-string!] /gmt /local time zone nyd second][
		all [
			date/time date/zone
			date/time: date/time - date/zone
			date/time: date/time + date/zone: either gmt [0:00][config/zone]
		]

		time: round any [date/time 0:00]
		zone: any [date/zone config/zone 0:00]
		interpolate format bind date-codes 'date
	]

	color-codes: [
		#"r" [color/1] #"1" [to-char color/1]
		#"g" [color/2] #"2" [to-char color/2]
		#"b" [color/3] #"3" [to-char color/3]
		#"a" [color/4] #"4" [to-char color/4]
		#"R" [skip tail to-hex color/1 -2]
		#"G" [skip tail to-hex color/2 -2]
		#"B" [skip tail to-hex color/3 -2]
		#"A" [skip tail to-hex color/4 -2]
	]

	form-color: func [color [tuple!] format [any-string!]][
		bind color-codes 'color
		color: 0.0.0.0 + color
		interpolate format color-codes
	]

	pluralize: func [string [string!] count [number!]][
		unless any [count = 1 count = -1][string: join string "s"]
		reform [count string]
	]

	export [form-date to-local-time form-color pluralize]
]

;--## VALUES FILTER
;-------------------------------------------------------------------##
context [
	id: [chars-la 0 15 chars-f]
	word: [chars-w1 0 25 chars-w*]
	number: [integer!]
	integer: [opt #"-" number]

	masks: reduce [
		issue!    [some chars-u]
		logic!    ["true" | "on" | "yes" | "1"]
		word!     [word]
		url!      [id #":" some [chars-u | #":" | #"/"]]
		email!    [some chars-u #"@" some chars-u]
		path!     [word 1 5 [#"/" [word | integer]]]
		integer!  [integer]
		string!   [some [some chars-as | utf-8]]
		'positive [number]
		'id       [id]
		'key      [word 0 6 [#"." word]]
	]

	as: func [
		[catch] type [datatype!] value [any-type!]
		/where format [none! block! any-word!]
	][
		case/all [
			none? format [format: select masks type]
			none? format [if type = type? value [return value]]
			any-word? format [format: select masks to-word format]
			block? format [
				unless parse/all form value format [return none]
			]
			type = path! [return load value]
		]

		attempt [make type value]

	]

	export [as]
]

;--## IMPORT
;-------------------------------------------------------------------##
context [
	result: errors: #[none]

	messages: [
		not-included "is not included in the list"
		excluded "is reserved"
		invalid "is missing or invalid"
		not-confirmed "doesn't match confirmation"
		not-accepted "must be accepted"
		empty "can't be empty"
		blank "can't be blank"
		too-long "is too long (maximum is %d characters)"
		too-short "is too short (minimum is %d characters)"
		wrong-length "is the wrong length (should be %d characters)"
		not-a-number "is not a number"
		too-many "has too many arguments"
	]

	datatype: [
		'any-string! | 'binary! | 'block! | 'char! | 'date! | 'decimal! | 'email! | 'file! |
		'get-word! | 'integer! | 'issue! | 'lit-path! | 'lit-word! | 'logic! | 'money! |
		'none! | 'number! | 'pair! | 'paren! | 'path! | 'range! | 'refinement! |
		'set-path! | 'set-word! | 'string! | 'tag! | 'time! | 'tuple! | 'url! | 'word!
	]

	else: #[none]
	otherwise: [
		['else | 'or][
			set else string! | copy else any [word! string!]
		] | (else: #[none])
	]

	source: key: value: target: type: format: constraints: else: none

	constraint: use [is is-not? is-or-length-is op val val-type range group][
		op: val: val-type: none
		is: ['is | 'are]
		is-or-length-is: [
			[
				['length | 'size] (val: length? value val-type: integer!)
				| (val: :value val-type: :type)
			] is
		]
		is-not?: ['not (op: false) | (op: true)]

		[
			is [
				'accepted otherwise (
					unless true = value [report not-accepted]
				) |
				'confirmed opt 'by set val get-word! otherwise (
					val: to-word val
					unless value = as/where :type get-from source :val format [
						report not-confirmed
					]
				) |
				is-not? 'within set group any-block! otherwise (
					either found? find group value [
						unless op [report excluded]
					][
						if op [report not-included]
					]
				)
			] |
			is-or-length-is [
				is-not? 'between [set range [range! | into [2 val-type]]] otherwise (
					either op [
						case [
							val < target: range/1 [report too-short]
							val > target: range/2 [report too-long]
						]
					][
						unless any [
							val < range/1
							val > range/2
						][report excluded]
					]
				) |
				'more-than set target val-type otherwise (
					unless val > target [report too-short]
				) |
				'less-than set target val-type otherwise (
					unless val < target [report too-long]
				) |
				set target val-type otherwise (
					unless val = target [report wrong-length]
				)
			]
		]
	]

	do-constraints: does [constraints: [any constraint]]
	no-constraints: does [constraints: [to set-word! | to end]]

	humanize: func [word][uppercase/part replace/all form word "-" " " 1]

	report: func ['message [word!]][
		message: any [
			all [string? else else]
			all [block? else select else message]
			reform [humanize key any [select messages message ""]]
		]
		unless select errors :key [repend errors [:key copy []]]
		append select errors :key interpolate message [
			#"w" [form key]
			#"W" [humanize key]
			#"d" [form target]
			#"t" [form type]
		]
	]

	import: func [
		[catch] source* [block! none!] spec [block!]
		/report-to errs [block!]
		/block /again /local required present
	][
		unless source* [return none]

		errors: copy []
		result: copy []

		source: source* []

		unless parse compose/deep/only spec [
			any [
				set key [set-word! | refinement!] (key: to-word key)
				set required opt 'opt (required: required <> 'opt)
				set type datatype (type: get type)
				set format opt [block! | get-word!]
				otherwise

				(
					value: either block [
						pick source 1
					][
						get-from source :key
					]

					present: not any [
						none? value
						empty? trim/head/tail form value
					]

					either all [
						present
						value: either :type = block! [
							either block? format [
								import value format
							][value]
						][
							as/where :type value format
						]
					][
						if block [source: next source]
						do-constraints
						repend result [key value]
					][
						no-constraints
						case [
							all [present not block] [report invalid]
							required [report blank]
							not required [repend result [key none]]
						]
					]
				)

				constraints
			]
		][raise "Could not parse Import specification"]

		all [block? errs insert clear errs errors]
		unless qm/errors: all [not empty? errors errors][result]
	]

	get-one: func [data type /local res][
		parse data [some [res: type to end break | skip]]
		unless tail? res [take res]
	]

	get-some: func [data type /local pos res][
		res: make block! length? data
		parse data [some [pos: type (append/only res take pos) :pos | skip]]
		unless empty? res [res]
	]

	match: func [
		[catch] source* [block!] spec [block!]
		/report-to errs [block!]
		/local required type
	][
		source: copy :source*
		errors: copy []
		result: context append remove-each item copy spec [not set-word? item] none

		unless parse spec [
			some [
				set key set-word! (key: to-word key)
				set required ['opt | 'any | 'some | none]
				copy type [datatype any ['| datatype]]
				otherwise

				(
					switch/default required [
						any [
							value: get-some source type
							either value [do-constraints][no-constraints]
						]
						opt [
							value: get-one source type
							either value [do-constraints][no-constraints]
						]
						some [
							value: get-some source type
							either value [do-constraints][no-constraints report invalid]
						]
					][
						value: get-one source type
						either value [do-constraints][no-constraints report invalid]
					]

					result/(key): value
				)

				constraints
			]
		][raise "Could not parse Match specification"]

		all [block? errs insert clear errs errors]
		unless empty? source [key: 'match report too-many]
		unless qm/errors: all [not empty? errors errors][result]
	]

	export [import match]
]

;--## Q(uick)TAG DIALECT
;-------------------------------------------------------------------##
context [
	qtags: []
	form-val: func [val /local value][
		val: switch/default type?/word val [
			get-word! [
				if value: get/any :val [
					rejoin [{ } val {="} sanitize form value {"}]
				]
			]
			word! [
				all [val: get val sanitize form val]
			]
			none! [val]
			string! [val]
		][sanitize form val]
		any [val ""]
	]

	add-qtag: func ['name [word!] format [block!] spec [block!] prep [block!]][
		repend qtags [
			name
			func [[catch] spec [block!]] compose/deep/only [
				throw-on-error [
					either spec: match spec (spec) [
						spec: make spec (prep)
						rejoin map/copy with/only spec (format) :form-val
					][
						raise (rejoin ["!!!Invalid " uppercase form name " tag."])
					]
				]
			]
		]
	]

	add-qtag a [
		"<a" :id :href :rel :class :title :accesskey ">"
	][
		href: file! | url! | path!
		id: opt issue!
		class: any refinement!
		title: opt string!
		accesskey: opt char!
		rel: any lit-word!
	][
		all [path? href href: link-to :href]
	]

	add-qtag div ["<div" :id :class ">"][
		id: opt issue!
		class: any refinement!
	][]

	add-qtag img [
		"<img" :id :width :height :src :class :alt :title " />"
	][
		src: file! | url! | path!
		size: opt pair!
		alt: string!
		title: opt string!
		id: opt issue!
		class: any refinement!
	][
		all [path? src src: link-to :src]
		size: any [size -1x-1]
		set [width height] reduce [size/x size/y]
		all [width < 0 width: none] all [height < 0 height: none]
	]

	add-qtag form [
		"<form" :method :action :enctype :id :class ">"
	][
		method: opt word! is within [get post upload]
		action: file! | url! | path!
		id: opt issue!
		class: any refinement!
	][
		enctype: none
		case/all [
			path? action [action: link-to :action]
			none? method [method: 'post]
			method = 'upload [method: 'post enctype: 'multipart/form-data]
		]
	]

	to-key: func [key [path! word!]][replace/all form compose-path :key #"/" #"."]

	add-qtag label [
		"<label" :for :accesskey :title :class ">"
	][
		title: opt string!
		for: opt issue!
		class: any refinement!
		accesskey: opt char!
	][]

	add-qtag hidden [
		{<input type="hidden"} :name :value " />"
	][
		name: word! | path!
		value: opt any-string! | number! | none!
	][
		name: to-key name
		value: any [value ""]
	]

	field: [
		name: word! | path!
		id: opt issue!
		size: opt integer! | pair!
		value: opt any-string! | number! | none!
		class: any refinement!
	]

	add-qtag field [
		{<input type="text"} :name :value :id :size :class " />"
	] field [
		name: to-key name
		class: append any [class copy []] /field
		value: any [value ""]
	]

	add-qtag password [
		{<input type="password"} :name :value :id :size :class " />"
	] field [
		name: to-key name
		class: append any [class copy []] /field
		value: any [value ""]
	]

	add-qtag area [
		{<textarea} :name :id :cols :rows :class ">" value "</textarea>"
	] field [
		name: to-key name
		class: append any [class copy []] /field
		size: 0x0 + any [size 12x50]
		cols: abs size/x
		rows: abs size/y
		value: any [value ""]
	]

	check-options: [
		name: word! | path!
		id: opt issue!
		value: any-string! | number!
		checked: opt logic! | none! | any-string! | number!
		class: any refinement!
	]

	check-line-options: [
		name: word! | path!
		id: issue!
		accesskey: opt char!
		label: string!
		value: any-string! | number!
		checked: opt logic! | none! | any-string! | number!
		class: any refinement!
	]

	check-action: [
		for: :id
		name: to-key name
		value: any [value ""]
		checked: if any [value = checked true? checked]["checked"]
	]

	add-qtag check [
		{<input type="checkbox"} :name :value :checked :id :class " />"
	] :check-options :check-action

	add-qtag check-line [
		{<label} :for :accesskey :class {><input type="checkbox"} :name :value :checked :id " /> " label "</label>"
	] :check-line-options :check-action

	add-qtag radio [
		{<input type="radio"} :name :value :checked :id :class " />"
	] :check-options :check-action

	add-qtag radio-line [
		{<label} :for :accesskey {><input type="radio"} :name :value :checked :id :class " /> " label "</label>"
	] :check-line-options :check-action

	[
		| 'select [
			'many
			| opt 'one
		]
	]

	add-qtag get-file [
		{<input type="file"} :name :id :class " />"
	][
		name: word! | path!
		id: opt issue!
		class: any refinement!
	][
		name: to-key name
		class: append any [class copy []] /get-file
	]

	add-qtag submit [
		{<input type="submit"} :name :value :id :size :class " />"
	] field [
		name: to-key name
		value: any [value ""]
		class: append any [class copy []] /submit
	]

	build-tag: func [[catch] spec [block!] /local cmd action][
		either action: select qtags cmd: pop spec: compose spec [
			action spec
		][
			rejoin ["!!! Invalid QuickTag Type: &lt;" cmd "&gt;"]
		]
	]

	export [add-qtag build-tag]
]

;--## FILESYSTEM
;-------------------------------------------------------------------##
context [
	sw*: system/words
	rights: [
		folder [
			owner-read: group-read: world-read:
			owner-write: group-write: world-write:
			owner-execute: group-execute: world-execute: #[true]
		]
		file [
			owner-read: group-read: world-read:
			owner-write: group-write: world-write: #[true]
			owner-execute: group-execute: world-execute: #[false]
		]
	]

	set-rights: func [file access][
		unless find [1 3] system/version/4 [
			attempt [set-modes file rights/:access]
			; not perfect
		]
	]

	break-path: func [[catch] target [file!] base [file!] /local path mk][
		path: make block! []
		either parse/all target: form target [
			base some [
				thru #"/" mk: (append path to-file copy/part target mk)
			] end
		][return path][
			raise compose [access invalid-path (target)]
		]
	]

	make-dir: func [[catch] path [file!] /root base [file!] /deep /local dirs][
		all [empty? path return path]
		if exists? path [
			return either dir? path [path][false]
		]
		either deep [
			close throw-on-error [open/new path]
			any [
				find [1 3] system/version/4
				throw-on-error [set-rights path 'folder]
			]
		][
			dirs: break-path path base
			foreach path dirs [make-dir/deep path]
		]
		path
	]

	get-subfolders: func [folder /deep /local tree files][
		tree: []
		unless deep [clear tree]
		insert tree folder
		files: read folder
		foreach file files [
			if equal? last folder/:file #"/" [
				get-subfolders/deep folder/:file
			]
		]
		tree
	]

	delete: func [[catch] target [url!] /pare /local path folder err][
		either error? set/any 'err try [
			; Delete Children
			if dir? target [
				folder: get-subfolders dirize target
				foreach path folder [close clear open path]
			]

			; Delete Target
			set [path target] split-path target
			folder: open path
			remove find folder target
			close folder

			; Delete Empty Parents
			if pare [
				while [
					pare: empty? folder: open path
					close folder
					pare
				][
					set [path target] split-path path
					folder: open path
					remove find folder target
					close folder
				]
			]
		][throw err][path]
	]

	dir?: func [[catch] target [file! url!]][
		throw-on-error [
			target: make port! target
			query target
		]
		target/status = 'directory
	]

	touch: func [[catch] target [file! url!]][
		throw-on-error [
			target: make port! target
			switch target/scheme [
				qm [target: make port! target/locals/file]
			]
			query target
			switch target/status [
				file [set-modes target [modification-date: now]]
				#[none] [close open/new target]
			]
		]
		exit
	]

	export [delete make-dir touch]

	; INTERFACE
	add-protocol qm 0 context [
		port-flags: system/standard/port-flags/pass-thru

		init: func [port url /local spec][
			unless all [
				url? url
				spec: get-space qm:// url
			][
				raise ["Filesystem URL <" url "> is invalid."]
			]

			with port [
				set [url host path target] reduce bind [uri domain path target] spec

				locals: context [
					flags: []
					root: spec/root
					folder: spec/folder
					file: spec/file
					suffix: spec/suffix
					open: #[none]
				]

				sub-port: make port! spec/file
			]
		]

		open: func [port][
			with port [
				locals/flags: get-port-flags port [read write append new binary lines]

				all [
					sw*/find locals/flags 'new
					not dir? locals/folder
					make-dir/root locals/folder locals/root
				]

				either all [
					any [
						exists? locals/file
						sw*/find locals/flags 'new
					]
					sw*/open/mode sub-port locals/flags
				][
					locals/open: true
					state/tail: sub-port/state/tail
				][
					state/tail: 0
				]

				state/index: 0
				state/flags: state/flags or port-flags
			]
		]

		copy: func [port][
			if port/locals/open with/only port [
				user-data: sw*/copy skip sub-port state/index
				all [
					block? user-data block? state/custom
					remove-each file user-data [not parse file state/custom]
				]
				user-data
			]
		]

		insert: func [port data][
			if port/locals/open with/only port [
				foreach [test onfail][
					[sw*/insert sub-port data]
					["Could not write <" url ">"]
					[set-rights sub-port 'file]
					["Could not set permissions <" url ">"]
				][
					if error? try :test [raise :onfail]
				]
				self
			]
		]

		remove: func [port][
			either port/locals/open with/only port [
				sub-port: skip sub-port state/index
				system/words/remove/part sub-port state/num
				self
			][]
		]

		find: func [port value][
			if port/locals/open with/only port [
				if value: system/words/find sub-port value [
					sub-port: :value
					self
				]
			]
		]

		close: func [port][
			either port/locals/open with/only port [
				any [locals/open exit]
				system/words/close sub-port
				self
			][]
		]

		query: func [port][
			with port [
				system/words/query sub-port
				size: sub-port/size
				date: sub-port/date
				status: sub-port/status
			]
		]
	]
]

;--## EXTERNAL HELPERS
;-------------------------------------------------------------------##
context [
	root: qm://support/
	cache: []

	require: know: func [[catch] location [file!] /reset /local helper][
		if reset [remove/part find cache location 2]
		any [
			select cache location
			if all [
				helper: attempt [load/header root/:location]
				helper: context compose [header: (helper)]
			][
				repend cache [location helper]
				if block? get in helper/header 'exports [
					export bind helper/header/exports helper
				]
				helper
			]
			throw raise ["Missing support file: %" location]
		]
	]

	export [require know]
]

;--## RENDER
;-------------------------------------------------------------------##
context [
	root: qm://system/views/

	load-rsp: func [[catch] body [string!] /local code mk][
		code: make string! length? body

		append code "out*: make string! {}^/"
		parse/all body [
			any [
				end (append code "out*") break
				| "<%" [
					"=" copy mk to "%>" (repend code ["prin (" mk "^/)^/"])
					| [#":" | #"!"] copy mk to "%>" (repend code ["prin build-tag [" mk "^/]^/"])
					| copy mk to "%>" (repend code [mk newline])
					| (raise "Expected '%>'")
				] 2 skip
				| copy mk [to "<%" | to end] (repend code ["prin " mold mk "^/"])
			]
		]

		try-else [
			code: bind load code qm/binding
			bind code 'self
			uses [
				out*: "" prin: func [val][repend out* val]
				print: func [val][prin val prin newline]
			] code
		][throw reason]
	]

	depth*: 0 ;-- to break recursion
	scope: #[none]

	clean-view-path: func [
		[catch] path [file!]
		partial? [logic! none!]
	][
		path: split-path path
		change path next split-path path/1
		case/all [
			none? path/2 [raise "Not a Valid Filename to Render"]
			none? path/1 [change path any [scope qm/view-path]]
			none? path/1 [raise "No View Scope"]
			partial? [insert path/2 %_]
		]
		return rejoin path
	]

	render3p: func [[catch] format [file!] body [string!] locals [block!]][
		all [
			format: rejoin [%render/ next format %.r]
			format: know format
			function? format: get in format 'renders
			format: format body locals
			format locals
		]
	]

	render: build: func [
		[catch] body [file! string!]
		/partial /with locals [block!]
		/type format /local out
	][
		if depth* > 20 [return ""]
		depth*: depth* + 1

		locals: either locals [
			map/copy locals func [word][
				reduce [to-set-word word get/any word]
			]
		][[]]

		out: case/all [
			file? body [
				format: suffix? body: clean-view-path body partial
				body: read root/:body
			]
			string? body [
				either any [none? format format = %.rsp][
					format: load-rsp body
					format locals
				][
					throw-on-error [render3p format body locals]
				]
			]
		]

		depth*: depth* - 1
		return out
	]

	render-each: func [
		'items [word! block!]
		source [series!]
		body [file! string!]
		/whole /with locals /local out
	][
		out: copy ""
		locals: append any [locals []] items: envelop items
		foreach :items source compose/only [
			append out do either whole ['render/with]['render/partial/with] body (locals)
		]
		return out
	]

	export [clean-view-path render build]
]

;--## ROUGHCUTDB
;-------------------------------------------------------------------##
context [
	space: qm://space/
	; locate: func [port id][space/(port/locals/locate id)]

	sw*: get in system 'words

	table!: context [
		name: header: spec: index: root: path: changed: #[none]
		locate: func [id][form id]
		record: context [
			id: new?: owner: root: path: #[none] data: []
			on-load: on-save: on-create: on-change: on-delete: #[none]
			get: func [key [word!]][select data key]
			set: func [key [word!] val][unset key val repend data [key val] val]
			unset: func [key [word!]][remove-each [k v] data [k = key]]
			store: func [[catch]][
				case [
					not new? [change owner self self]
					not unique? [errors: [id ["Record ID already exists."]] none]
					else [append owner self self]
				]
			]
			destroy: does [unless new? [remove find head owner id]]
			inject: func [pending][
				unless block? :pending [return none]
				foreach [key val] pending [set key val]
			]
			unique?: does [not find owner/locals/index get 'id]
			injects: func [spec [block!] /local out][
				func [args] compose/only/deep [
					case [
						not block? :args [none]
						out: import/report-to args (spec) errors [
							inject out out
						]
						parse args [any [word! skip]][
							inject args none
						]
					]
				]
			]
			errors: []
		]
	]

	; INTERFACE
	add-protocol roughcut 0 context [
		port-flags: system/standard/port-flags/pass-thru

		shift: func [port][
			port/locals/index: skip head port/locals/index port/state/index
			port
		]

		commit: func [[catch] block /local status][
			loop 30 [
				if status: not exists? space/lock.r [break]
				wait 0.005
			]

			either status [
				touch space/lock.r
				do block
				delete space/lock.r
			][
				delete space/lock.r
				raise "Database locked"
			]
		]

		init: func [port spec [url!]][
			unless port/locals: make table! port/locals [
				raise ["Could not load model <" port/locals/name ">"]
			]
			with port/locals [
				root: path: (any [port/locals/header/home dirize space/:name])
				changed: (modified? space/:name/index.r)
			]
		]

		open: func [[catch] port [port!]][
			update port

			with port [
				with state [
					index: 0
					flags: flags or port-flags
				]
			]
		]

		select: func [port criteria /local result record][
			case [
				criteria = 'new [
					result: make port/locals/record compose [
						new?: #[true]
						owner: :port
						data: sw*/copy data
					]
					result/on-create
					result
				]
				block? criteria [
					result: sw*/copy []
					forall port [
						record: first port
						if with record criteria [
							append result record
						]
					]

					; while [port: find port criteria][
					; 	append result first port
					; 	port: next port
					; ]

					:result
				]
				criteria [
					all [
						port: find port criteria
						; index? port
						first port
					]
				]
			]
		]

		find: func [port [port!] criteria /local index][
			shift port

			case [
				block? criteria [ ; reserved for query dialect
					; none
				]
				criteria [
					all [
						index: sw*/find head port/locals/index criteria
						port: at head port index? index
					]
				]
			]
		]

		pick: func [port [port!] /local id root][
			shift port

			all [
				id: sw*/pick port/locals/index 1
				root: dirize join port/locals/root port/locals/locate id
				make port/locals/record compose [
					id: (id)
					owner: port
					root: path: (root)
					data: load/all root/index.r
					on-load
				]
			]
		]

		insert: func [port [port!] record [object!] /local id root][
			update port

			unless case/all [
				not object? record [raise "Not a RoughCut Active Record"]
				not id: record/get 'id [raise "Active Record needs an ID"]
				sw*/find head port/locals/index id [
					raise "ID already exists"
				]
				error? root: try [port/locals/locate id][:root]
			][
				record/id: :id
				record/root: record/path: port/locals/root/:root
				record/new?: false

				commit [
					record/on-save
					new-line/all/skip record/data true 2

					save/all record/root/index.r record/data
					head sw*/insert port/locals/index record/id
					update/set port
				]
			]
		]

		change: func [port [port!] record [object!]][
			unless case/all [
				not object? record [raise "Not a RoughCut Active Record"]
				not sw*/find head port/locals/index record/id [
					raise "Active Record ID not found"
				]
			][
				commit [
					record/on-change
					record/on-save

					new-line/skip/all record/data true 2
					save/all record/root/index.r record/data
				]
			]
		]

		remove: func [port [port!] /local record][
			update port

			loop port/state/num [
				record: first port

				commit [
					record/on-delete

					delete/pare record/root
					sw*/remove port/locals/index
					update/set port
				]
			]
		]

		update: func [port [port!] /set][
			with port/locals [
				either set [
					save/all root/index.r new-line/all head index true
					changed: now
				][
					unless all [
						exists? root/index.r
						index: load root/index.r
					][
						write root/index.r ""
						changed: now
						index: sw*/copy []
					]
				]

				port/state/tail: length? head index
			]

			shift port
		]

		copy: func [port [port!]][
			update port
			map sw*/copy/part port/locals/index port/state/num func [id][
				select port id
			]
		]

		close: func [port [port!]][port/locals/index: none]
	]
]

;--## MODELS
;-------------------------------------------------------------------##
context [
	root: qm://system/models/

	engage-model: has [specs type][
		specs: map read/custom root [chars-a any chars-f %.r] func [spec][
			reduce [to-word form copy/part spec find spec %.r spec]
		]

		qm/models: context map-each [name file] specs [
			name: to-set-word name
		]

		map-each [name spec] specs [
			spec: bind load/header root/:spec qm/models
			type: get in spec/1 'type
			spec: compose [
				name: (to-lit-word name)
				header: (spec)
			]
			spec: switch type?/word type [
				word! [
					spec: compose/only [scheme: (to-lit-word type) locals: (spec)]
					unless attempt [spec: open spec][
						raise ["Error opening Model: %models/" name ".r <" type "::" name ">"]
					]
					spec
				]
				none! [context bind spec qm/models]
			]
			set in qm/models name spec
		]

		qm/models
	]

	disengage-model: does [
		foreach spec next first qm/models [
			if port? spec: get in qm/models spec [close spec]
		]
	]
	export [engage-model disengage-model]
]

;--## CONSOLE SESSION STOPS HERE
;-------------------------------------------------------------------##
unless system/options/cgi/request-method [
	all [system/product = 'base know %core/help.r]
	recycle halt
]


;--## REQUEST
;-------------------------------------------------------------------##
qm/request: make system/options/cgi [
	controller: action: input: none
	remote-addr: as tuple! remote-addr

	get-header: func [name][select other-headers form name]

	clear find request-path: copy request-uri: as file! any [
		get-env "REQUEST_URI"
		get-header "HTTP_INTERNAL_REFERER"
		"/"
	] "?"

	path-info: parse/all remove copy request-path "/"

	query: decode-query query-string

	cookies: all [
		cookies: select other-headers "HTTP_COOKIE"
		parse cookies ";="
	]

	cookies: any [cookies []]

	map cookies :url-decode

	; Must fix 'decode-options
	; accept-types: decode-options select other-headers "HTTP_ACCEPT" path!
	; accept-languages: decode-options select other-headers "HTTP_ACCEPT_LANGUAGE" word!
	; accept-encodings: decode-options select other-headers "HTTP_ACCEPT_ENCODING" word!
	; accept-charsets: decode-options select other-headers "HTTP_ACCEPT_CHARSET" word!

	content-limit: config/post-limit
	content-boundary: #[none]

	content-type: as path! content-type
	type: none

	all [
	  ;--- content length ---
		content-length: any [as integer! content-length 0]
		0 < content-length

	  ;--- content type ---
		parse form content-type [
			[
				"text/" [
					  opt "x-" "rebol" (type: /rebol)
					| "xml" (type: /xml)
					| opt "x-" "json" (type: /json)
				]
				| "application/" [
					  "x-www-form-urlencoded" (type: /url-encoded)
					| "xml" (type: /xml)
				]
				| "multipart/form-data;" "boundary=" content-boundary: some chars (
					type: /multipart
					content-boundary: join "--" content-boundary
				)
			]
		]
	]

	;-- body is the raw data from the HTTP request
	body: func [/binary /local tmp][
		body: copy either binary [#{}][{}]
		set-modes system/ports/input [lines: false]
		unless binary [set-modes system/ports/input [binary: false]]
		body: copy system/ports/input
		; body: copy/part system/ports/input content-length
		; ^^^^^^^^ problem, not sure why...
	]

	if find server-software "Cheyenne" [
		input: get in system/words 'input

		body: func [/binary][
		 	body: either all [
				body: :input
				content-length = length? body
			][body][#{}]
			either binary [body][to-string body]
		]
	]

	;-- content is the body 'loaded' according to the content-type header
	content: func [[catch]][
		throw-on-error [
			content: switch type [
				/url-encoded [decode-query body]
				/rebol [attempt [load/header body]]
				/multipart [
					; know %qm/multipart.r
					load-multipart body/binary content-boundary
				]
			]
		]
		any [content []]
	]

	get-cookie: func [name][select cookies form name]
	get-param: func ['name /query /body /local result][
		result: copy [query #[none] body #[none]]
		unless body [result/query: get-from self/query :name]
		unless query [result/body: get-from self/content :name]
		any [result/query result/body]
	]

	export [get-header get-cookie get-param]
]

;--## RESPONSE
;-------------------------------------------------------------------##
qm/response: context [
	status: 200
	headers: make string! ""
	type: 'text/html
	charset: "utf-8"

	template: length: body: #[none]

	set-header: func [header value][
		repend headers [header ": " value newline]
	]

	clear-cookie: func [key /path root /domain base][
		key: join form key "=; expires=Thu, 01 Jan 1970 00:00:00 GMT"
		repend key ["; path=" any [root %/]]
		if domain [repend key ["; domain=" base]]
		set-header 'Set-Cookie key
	]

	set-cookie: func [key value /expires on [date!] /path root /domain base][
		value: rejoin [form key "=" url-encode value]
		if expires [append value form-date/gmt on "; expires=%a, %d %b %Y %T GMT"]
		repend value ["; path=" any [root %/]]
		if domain [repend value ["; domain=" base]]
		set-header 'Set-Cookie value
	]

	export [set-header set-cookie clear-cookie]
]

;--## VIEW
;-------------------------------------------------------------------##
context [
	status-codes: [
		200 "OK" 201 "Created" 204 "No Content"
		301 "Moved Permanently" 302 "Moved temporarily"
		400 "Bad Request" 401 "No Authorization" 403 "Forbidden" 404 "Not Found" 411 "Length Required"
		500 "Internal Server Error" 503 "Service Unavailable"
	]

	log: #[none]

	probe: func [data][
		log: append any [log "^/"] mold :data
		append log newline
		return :data
	]

	send-response: func [body [string! binary!]][
		system/ports/output/state/with: "^/"
		; body: join body ["<div><b>" difference now/precise st "</b></div>"]
		write-io system/ports/output body length? body
		close system/ports/output
		body
	]

	if find qm/request/server-software "Cheyenne" [
		send-response: func [body [string! binary!]][
			prin to-string body
		]
	]

	publish: has [yield][
		with qm/response [
			either yield: body: case [
				any [none? body empty? body][" "]
				string? body [render body]
				file? body [render body]
				binary? body [body]
			][
				body: any [
					all [
						any [file? template string? template]
						render/with template [yield]
					]	
					yield
				]
			][
				status: 404 body: any [render %errors/notfound.rsp "Not Found..."]
			]

			case/all [
				any [none? body empty? body][body: " "]

				log [
					either find body </body> [
						insert find body </body> rejoin [
							{^/<div id="_log"><h1>Log</h1><pre><code>}
							remove sanitize log
							{</code></pre></div>^/}
						]
					][
						repend body ["^/" log]
					]
				]

				; find request/accept-encodings 'deflate [
				; 	set-header 'Content-Encoding "deflate"
				; 	; IE and Safari need deflate header and footer removed
				; 	clear skip tail remove remove body: compress body -8
				; ]
			]

			unless find status-codes status [status: 500]
			status: reform ["Status:" status select status-codes status]

			set-header 'Content-Type either find [text application] type/1 [
				rejoin [form type "; charset=" charset]
			][form type]

			set-header 'Content-Length length? body

			insert body reduce [status newline headers newline]

			send-response body
		]
	]

	export [publish probe]
]

;--## CONTROLLER
;-------------------------------------------------------------------##
context [
	root: qm://system/controllers/

	rendered?: #[false]

	redirect-to: func [[catch] 'url [file! url! path! none!] /back /status response-code [integer!]][
		if rendered? [raise "Already Rendered!"]

		qm/response/status: any [response-code 302]
		qm/response/template: #[none]
		case [
			all [back back: as url! get-header "HTTP_REFERER"][url: :back]
			none? url [raise "Redirect requires a valid URL"]
			all [file? url not #"/" = first url][insert url #"/"]
			path? url [url: link-to :url]
		]
		qm/response/set-header 'Location url

		rendered?: #[true]
	]

	render: func [
		[catch]
		body [file! string! url! binary! none!]
		/status code [integer!]
		/as 'type [path!]
		/template master [file! string! url! binary! none!]
		/partial
		/charset encoding
		/local path
	][
		if rendered? [raise "Already Rendered!"]

		qm/response/body: :body

		case/all [
			status [qm/response/status: :code]
			as [qm/response/type: :type]
			file? body [
				qm/response/body: clean-view-path body partial
			]
		]

		qm/response/template: case [
			binary? body [none]
			all [partial not template][none]
			not template [qm/response/template]
			file? master [clean-view-path master false]
			string? master [master]
		]

		rendered?: #[true]
	]

	print: func [value][render/as/template reform value text/plain none]

	load-controller: use [actions event action filter current][
		actions: []

		action: use [name class options][
			class: context options: [spec: code: none]
			with/only class [
				(with class options)
				'action set name string! set spec opt block!
				'does set code block! (repend actions ['action name make class []])
			]
		]

		event: use [name code][
			[
				'event set name string! 'does set code block!
				(repend actions ['event name code])
			]
		]

		filter: use [names test code][
			[
				'protect copy names some string!
				set test paren! set code block!
				(repend actions ['filter names reduce [test code]])
			]
		]

		func [[catch] name /local file meta][
			actions: copy []

			unless all [
				(all [name name <> ""])
				(exists? file: join root [name %.r])
				(block? file: load/header file)
				('controller = get in meta: pop file 'type)
			][
				return none
			]

			insert actions meta
			unless parse/all file [some [action | event | filter]][
				name: rejoin ["Invalid Controller Spec: %controllers/" name ".r"]
				raise :name
			]

			actions
		]
	]

	route: func [request [object!] /local actions file default args code] with/only qm [
		controller: any [
			pop request/path-info
			config/default-controller
		]

		if all [
			controller controller/1 = #"_"
		][
			render "Invalid Controller"
			exit
		]

		unless actions: load-controller controller [
			; render "No Controller"
			render/status %errors/notfound.rsp 404
			exit
		]

		response/template: get in actions/1 'template

		use [default][
			request/action: pick request/path-info 1
			default: get in actions/1 'default

			case [
				find actions reduce ['action request/action][
					remove request/path-info
					action: request/action
				]
				find actions reduce ['action default][
					action: default
				]
				else [
					raise "No Action"
					; render/status %errors/notfound.rsp 404
					exit
				]
			]
		]

		use [startup][
			all [
				startup: load-controller "_startup"
				insert tail actions next startup
			]
		]

		foreach [type scope details] next actions with/only events: context [
			start: prepare: filter: this: none
		][
			case [
				type = 'event [
					switch scope [
						"web-start" [start: :details]
						"prepare" [prepare: any [prepare details]]
					]
				]
				all [
					type = 'filter
					find scope action
				][
					filter: append any [filter []] details
				]
				scope = action [
					this: any [this details]
				]
			]
		]

		unless events/this [
			response/template: none
			render/status %errors/notfound.rsp 404
			exit
		]

		unless args: import/block request/path-info any [events/this/spec []][
			response/template: none
			render/status %errors/badrequest.rsp 400
			; print "Bad Request" exit
			exit
		]

		view-path: dirize to-file controller

		qm: make qm context with/only qm with/only 'route compose/deep [
			(third models)
			header: (first actions)
			title: (rejoin [uppercase/part form controller 1 " :: " uppercase/part form action 1])
			(events/start)
			(events/prepare)
			(map-each [word value] args [word: to-set-word word])
			case [
				(any [events/filter []])
				else [(events/this/code)]
			]
		]

		qm/binding: in qm 'self
		either rendered? [#[true]][
			if action [
				render head insert %.rsp action
			]
		]
	]

	export [route]
]

;--## ENGAGE
;-------------------------------------------------------------------##
try-else [
	with qm [
		engage-model
		route request
		disengage-model
		publish response
	]
][
	reason: make disarm reason []

	qm/binding: 'reason
	qm/handler: %.rsp

	reason-type: sanitize system/error/(reason/type)/type
	reason-message: sanitize reform bind envelop system/error/(reason/type)/(reason/id) reason
	reason-where: sanitize mold reason/where

	with qm/response [
		status: 500
		type: 'text/html
		template: #[none]
		body: trim/head trim/with {
			<html><head>
			<title>Error: <%= reason-type %></title>
			<link href="/styles/anywhere.css" rel="stylesheet" type="text/css" /></head>
			<body>
			<h1>QuarterMaster</h1>
			<h2>Error Message</h2>
			<pre><code>** <%= reason-type %>: <%= reason-message %>
			** Where: <%= reason-where %>
			** Near: <%= sanitize mold reason/near %></code></pre>
			</body>
			</html>
		} #"^-"
	]

	publish
]