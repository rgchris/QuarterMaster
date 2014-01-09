REBOL [
	Title: "QuarterMaster"
	Author: "Christopher Ross-Gill"
	Version: 0.7.5
	Notes: {Warning: Work-In-Progress - no liabilities for damage, etc.}
	License: http://creativecommons.org/licenses/by-sa/3.0/
	Needs: [2.7.8 shell]
]

;--## APPLICATION NAMESPACE
;-------------------------------------------------------------------##
qm: context [
	binding: 'self
	profile: settings:
	controller: metadata: action:
	models: db: request: response: none
	alerts: []
	errors: []
	notices: []
	handler: %.rsp
	view-path: none
	title: ""
	date: now
	code: []
	live?: string? system/options/cgi/server-software
	cheyenne?: parse any [system/options/cgi/server-software ""][thru "Cheyenne" to end]
	probe: func [data /with prefix][
		either with [
			with: join data mold prefix
			data: :prefix
		][
			with: mold data
		]
		if response [
			response/log: append any [response/log "^/"] :with
			append response/log newline
		]
		return :data
	]
]

;--## SETTINGS
;-------------------------------------------------------------------##
if all [qm/live? parse/all system/options/cgi/query-string [thru "?" any [thru "&"] "debug=" end]][
	print "Content-Type: text/plain; charset=utf-8^/"
]

system/options/binary-base: 64
system/error/user/type: "QuarterMaster Error"
range!: :pair! ; until REBOL v3
else: true ; for 'case statements

qm/profile: any [
	system/script/args
	system/script/parent/header
]

settings: qm/settings: construct/with any [
	qm/profile/settings
	make error! "No Settings Provided"
] context [
	get: none
]

settings/get: func [key [word!]][
	all [not key = 'self key: in settings key get key]
]

date: qm/date: qm/date - qm/date/zone + settings/zone

parse settings/spaces use [location][
	[some [string! [
		location: url! |
		file! (change location clean-path location/1)
	]]]
]

use [seed][
	seed: either any [
		not qm/live?
		settings/get 'no-mod-unique
	][""][
		any [
			get-env "UNIQUE_ID"
			make error! "Missing Apache Mod_Unique_ID"
		]
	]

	append seed now/precise
	random/seed to integer! checksum/secure seed
]

;--## EXTENDED CORE FUNCTIONS
;-------------------------------------------------------------------##
context [
	func: make function! [spec [block!] body [block!]][make function! spec body]
	does: func [body [block!]][make function! [] body]

	uses: func [
		"Defines a function with a finite context"
		proto [block!]
		spec [block!] "Function Body"
	][
		proto: context proto
		func [args [block! object!]] compose/only [
			args: make (proto) args
			do bind (spec) args
		]
	]

	try-else: func [
		"Tries to DO a block, returns its value or DOes the fallback block."
		[throw] 'block [block!] fallback [block!] /local reason
	][
		either error? reason: try :block bind :fallback 'reason [:reason]
	]

	verify: assert-all: func [
		"Steps through a series of cases/resolutions. Returns last case result where all cases are positive."
		[throw] cases [block!] /local value
	][
		until [
			set [value cases] do/next cases
			unless value cases/1
			cases: next cases
			any [not value tail? cases]
		]
		any [value]
	]

	with: func [
		"Binds and evaluates a block to a specified context."
		object [any-word! object! port!] "Target context."
		block [any-block!] "Block to be bound."
		/only "Returns the block unevaluated."
	][
		block: bind block object
		either only [block] :block
	]

	envelop: func [
		"Returns a block, encloses any value not already of any-block type."
		values [any-type!]
	][
		case [
			any-block? values [values]
			none? values [make block! 0]
			else [reduce [values]]
		]
	]

	press: func [
		"Evaluates and joins a block of values omitting unset and none values."
		values [any-block! string! none!]
		/local out
	][
		any [values return none]
		values: reduce envelop values
		remove-each value values [any [unset? get/any 'value none? value]]
		append copy "" values
	]

	raise: func [[throw] reason][throw make error! press reason]

	form-error: func [reason [error!] /local type message][
		reason: make disarm reason []

		type: system/error/(reason/type)/type
		message: reform bind envelop system/error/(reason/type)/(reason/id) reason
		reason: rejoin [
			"** " type ": " message
			"^/** Where: " mold reason/where
			"^/** Near: " mold reason/near
		]
	]

	true?: func [test][not not test]

	export: func [words [word! block!] /to dest [object!] /local word][
		dest: any [dest system/words]
		foreach word words [if word? word [set/any in dest word get/any word]]
	]

	export [func does uses try-else verify assert-all with envelop press raise form-error true? export]
]

;--## SERIES HELPERS
;-------------------------------------------------------------------##
context [
	push: func [stack [series! port!] value [any-type!] /only][
		head either only [insert/only stack :value][insert stack :value]
	]

	append: func [
		[catch]
		{Appends a value to the tail of a series and returns the series head.} 
		series [series! port!] value 
		/only "Appends a block value as a block"
	][
		throw-on-error [
			head either only [
				insert/only tail series :value
			][
				insert tail series :value
			]
		]
	]

	flatten: func [block [any-block!] /once][
		once: either once [
			[(block: insert block take block)]
		][
			[(insert block take block)]
		]
		parse block [
			any [block: any-block! (insert block take block) :block | skip]
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

	each: func [[catch throw] 'word [word! block!] series [any-block!] body [block!] /copy /local new][
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

	categorize: func [items [block!] test [any-function! block!] /local out value target][
		out: copy []
		if block? :test [test: func [item] :test]
		foreach item items [
			value: test item
			unless target: select out value [
				repend out [value target: copy []]
			]
			append target item
		]
		foreach [value items] out [new-line/all items true]
		new-line/all/skip out true 2
	]

	get-choice: func [word [string! word!] words [any-block!]][
		all [
			word: attempt [to word! word]
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

	link-to: func ['path [any-block!] /full /local out][
		out: copy %""
		path: compose to block! path
		foreach val path [
			case [
				issue? val [append out mold val]
				get-word? val [repend out ["/" get/any :val]]
				parse/all form val [["." | ","] to end][append out form val]
				parse/all form val [["`" | "!"] to end][append out back change form val ","]
				refinement? val [append out replace mold val "/" OOGIEBOOGIE]
				val [repend out ["/" form val]]
			]
		]
		either full [join settings/home either find/match out %/ [next out][out]][out]
	]

	compose-path: func [
		"Evaluates a path and reduces contained paren values"
		'path [path! lit-path! word! lit-word!]
	][
		to path! new-line/all compose to block! path none
	]

	paginate: func [
		"Paginate a Series of Known Length"
		series [series! port!]
		page [integer! none!]
		/window padding /size length
	][
		page: any [page 1]
		length: any [length 15]
		padding: any [padding 2]

		context [
			last: max 1 to integer! (length? series) - 1 / length + 1
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

	some: func [series [block!] block [block!] /empty else [block!]][
		else: any [else [none]]
		either empty? series :else :block
	]

	change-status: func [current target conditions [block!]][
		foreach [old new permission action] :conditions [
			if all [
				old = current
				new = target
				all to block! :permission
			][
				break/return do action
			]
		]
	]

	neaten: func [block [block!] /pairs /flat][
		new-line/all/skip block not flat either pairs [2][1]
	]

	export [
		push append flatten map each categorize
		get-choice get-class compose-path
		prepare link-to paginate some change-status neaten
	]
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
			series: select series take key
		]
		all [tail? key series]
	]

	export [add-to get-from]
]

;--## GRAMMAR SETS
;-------------------------------------------------------------------##
context [
	ascii: charset ["^/^-" #"^(20)" - #"^(7E)"]
	digit: charset [#"0" - #"9"]
	upper: charset [#"A" - #"Z"]
	lower: charset [#"a" - #"z"]
	alpha: union upper lower
	alphanum: union alpha digit
	hex: union digit charset [#"A" - #"F" #"a" - #"f"]

	symbol: file*: union alphanum charset "_-"
	url-: union alphanum charset "!'*,-._~" ; "!*-._"
	url*: union url- charset ":+%&=?"

	space: charset " ^-"
	ws: charset " ^-^/"

	word1: union alpha charset "!&*+-.?_|"
	word*: union word1 digit
	html*: exclude ascii charset {&<>"}

	para*: path*: union alphanum charset "!%'+-._"
	extended: charset [#"^(80)" - #"^(FF)"]

	chars: complement nochar: charset " ^-^/^@^M"
	ascii+: charset [#"^(20)" - #"^(7E)"]
	wiki*: complement charset [#"^(00)" - #"^(1F)" {:*.<>} #"{" #"}"]
	name: union union lower digit charset "*!',()_-"
	wordify-punct: charset "-_()!"

	ucs: charset ""
	utf-8: use [utf-2 utf-3 utf-4 utf-5 utf-b][
		utf-2: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////wAAAAA=}]
		utf-3: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP//AAA=}]
		utf-4: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/wA=}]
		utf-5: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA8=}]
		utf-b: #[bitset! 64#{AAAAAAAAAAAAAAAAAAAAAP//////////AAAAAAAAAAA=}]

		[utf-2 1 utf-b | utf-3 2 utf-b | utf-4 3 utf-b | utf-5 4 utf-b]
	]

	get-ucs-code: decode-utf: use [utf-os utf-fc int][
		utf-os: [0 192 224 240 248 252]
		utf-fc: [1 64 4096 262144 16777216]

		func [char][
			int: 0
			char: change char char/1 xor pick utf-os length? char
			forskip char 1 [change char char/1 xor 128]
			char: head reverse head char
			forskip char 1 [int: (to integer! char/1) * (pick utf-fc index? char) + int]
			all [int > 127 int <= 65535 int]
		]
	]

	inline: [ascii+ | utf-8]
	text-row: [chars any [chars | space]]
	text: [ascii | utf-8]

	ident: [alpha 0 14 file*]
	wordify: [alphanum 0 99 [wordify-punct | alphanum]]
	word: [word1 0 25 word*]
	number: [some digit]
	integer: [opt #"-" number]
	wiki: [some [wiki* | utf-8]]
	ws*: white-space: [some ws]

	encode-utf8: func [
		"Encode a code point in UTF-8 format" 
		char [integer!] "Unicode code point"
	][
		as-string to binary! reduce case [
			char <= 127 [[char]]

			char <= 2047 [[
				char and 1984 / 64 + 192 
				char and 63 + 128
			]]

			char <= 65535 [[
				char and 61440 / 4096 + 224 
				char and 4032 / 64 + 128 
				char and 63 + 128
			]]

			char <= 2097151 [[
				char and 1835008 / 262144 + 240 
				char and 258048 / 4096 + 128 
				char and 4032 / 64 + 128 
				char and 63 + 128
			]]

			; true [[]]
			true [[40 63 41]]
		]
	]

	amend: func [rule [block!]][
		bind rule 'self
	]

	export [get-ucs-code decode-utf encode-utf8 amend]
]

;--## STRING HELPERS
;-------------------------------------------------------------------##
context [
	pad: func [text length [integer!] /with padding [char!]][
		padding: any [padding #"0"]
		text: form text
		skip tail insert/dup text padding length negate length
	]

	url-encode: use [ch sp encode][
		ch: charset {!'*-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz~}
		encode: func [text][insert next text enbase/base form text/1 16 change text "%"]

		func [text [any-string!] /wiki][
			sp: either wiki [#"_"][#"+"]

			parse/all copy text [
				copy text any [
					  text: some ch | #" " (change text sp)
					| [#"_" | #"." | #","] (all [wiki encode text]) | skip (encode text)
				]
			]
			text
		]
	]

	url-decode: use [deplus sp decrlf][
		deplus: func [text][
			parse/all text [
				some [to sp text: (text: change text #" ") :text] to end
			]
			head text
		]

		decrlf: func [text][
			parse/all text [
				some [to crlf text: (text: change/part text #"^/" 2) :text] to end
			]
			head text
		]

		func [text [any-string!] /wiki][
			sp: either wiki [#"_"][#"+"]
			decrlf dehex deplus to string! text
		]
	]

	load-webform: func [query [string! none!] /loose /local result name value][
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
			| [any-string! | tuple! | money! | time!] (emit form here/1)
		]

		array: [any value end]

		object: [
			any [
				here: [word! | set-word!] (insert path to word! here/1)
				[value | block] (remove path)
			] end
		]

		block: [
			here: [
				  any-block! (change/only here copy here/1)
				| object! (change/only here body-of here/1)
			] :here into [object | mk: array]
		]

		func [
			"Serializes block data as URL-Encoded Web Form string"
			data [block! object!] /prefix
		][
			clear path
			webform: copy ""
			data: either object? data [body-of data][copy data]
			if parse copy data object [
				either prefix [back change webform "?"][remove webform]
			]
		]
	]

	; doesn't work right, yet
	; text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5
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

	prep: func [value [any-type!]][
		form any [value ""]
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

	sanitize: func [text [any-string!] /local char] amend [
		parse/all copy text [
			copy text any [
				text: some html*
				| #"&" (text: change/part text "&amp;" 1) :text
				| #"<" (text: change/part text "&lt;" 1) :text
				| #">" (text: change/part text "&gt;" 1) :text
				| #"^"" (text: change/part text "&quot;" 1) :text
				| #"^M" (remove text) :text 
				| copy char utf-8 (text: change/part text rejoin ["&#" get-ucs-code char ";"] length? char)
				| skip (text: change/part text rejoin ["#(" to integer! text/1 ")"] 1) :text
				; | skip (text: change text "#") :text
			]
		]
		any [text make string! 32]
	]

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
						content: either filetype/1 = 'text [decrlf content][to binary! content]
						add-to store name make file-prototype [
							name: :filename type: :filetype data: :content
						]
					][
						add-to store name to string! decrlf content
					]
				)
			]
		][
			raise "Invalid Multipart Postdata"
		]
	
		store
	]

	string-length?: func [[catch] string [any-string!] /local counter][
		either parse/all string amend [
			(counter: 0)
			any [[ascii | utf-8] (counter: counter + 1)]
		][counter][raise "String contains invalid characters."]
	]

	export [
		pad url-encode url-decode load-webform to-webform decode-options
		load-multipart compose-tags prep interpolate sanitize string-length?
	]
]

;--## PORT HELPERS
;-------------------------------------------------------------------##
context [
	add-protocol: func ['name id handler /with block][
		unless in system/schemes name [
			system/schemes: make system/schemes compose [
				(to set-word! name) #[none]
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
		header: make string! (20 * length? words-of object)
		foreach word words-of object [
			if get :word [
				insert tail header reduce [word ": " get :word newline]
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

	chars: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" "!%()+,-_"]
	; #[bitset! 64#{AAAAACIo/wP+//+H/v//BwAAAAAAAAAAAAAAAAAAAAA=}]

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
				copy path any [some chars opt [#"." 1 10 chars] #"/"]
				copy target opt [any chars 1 2 [#"." 1 10 chars]]
			]
			root: select settings/spaces domain
		] with/only space [
			path: all [path to file! path]
			target: all [target to file! target]
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

	pad-precise: func [seconds [number!] /local out][
		seconds: form make time! seconds
		head change copy "00.000000" find/last/tail form seconds ":"
	]

	to-iso-week: use [get-iso-year][
		get-iso-year: func [year [integer!] /local d1 d2][
			d1: to-date join "4-Jan-" year
			d2: to-date join "28-Dec-" year
			reduce [d1 + 1 - d1/weekday d2 + 7 - d2/weekday]
		]

		func [date [date!] /local out d1 d2][
			out: 0x0
			set [d1 d2] get-iso-year out/y: date/year

			case [
				date < d1 [d1: first get-iso-year out/y: date/year - 1]
				date > d2 [d1: first get-iso-year out/y: date/year + 1]
			]

			out/x: date + 8 - date/weekday - d1 / 7
			out
		]
	]

	to-epoch-time: func [date [date!]][
		; date/time: date/time - date/zone
		date: form any [
			attempt [to integer! difference date 1-Jan-1970/0:0:0]
			date - 1-Jan-1970/0:0:0 * 86400.0
		]
		clear find/last date "."
		date
	]

	date-codes: [
		#"a" [copy/part pick system/locale/days date/weekday 3]
		#"A" [pick system/locale/days date/weekday]
		#"b" [copy/part pick system/locale/months date/month 3]
		#"B" [pick system/locale/months date/month]
		#"C" [to integer! date/year / 100]
		#"d" [pad date/day 2]
		#"D" [date/year #"-" pad date/month 2 #"-" pad date/day 2]
		#"e" [date/day]
		#"f" [find/tail pad-precise time/second "."]
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
		#"s" [to-epoch-time date]
		#"S" [pad to integer! time/second 2]
		#"t" [#"^-"]
		#"T" [pad time/hour 2 #":" pad time/minute 2 #":" pad round time/second 2]
		#"u" [date/weekday]
		#"U" [pad to integer! date/julian + 6 - (date/weekday // 7) / 7 2]
		#"V" [pad first to-iso-week date 2]
		#"w" [date/weekday // 7]
		#"W" [pad to integer! date/julian + 7 - date/weekday / 7 2]
		#"y" [pad date/year // 100 2]
		#"Y" [date/year]
		#"z" [pad-zone/flat zone]
		#"Z" [pad-zone zone]

		#"c" [
			date/year #"-" pad date/month 2 "-" pad date/day 2 "T"
			pad time/hour 2 #":" pad time/minute 2 #":" pad to integer! time/second 2 
			either gmt ["Z"][pad-zone zone]
		]
	]

	form-date: func [date [date!] format [any-string!] /gmt /local time zone nyd][
		either date/time [
			if date/zone [date/time: date/time - date/zone]
			date/zone: either gmt [0:00][settings/zone]
			date/time: round date/time + date/zone
		][
			date/time: 0:00
			date/zone: either gmt [0:00][settings/zone]
		]

		time: date/time
		zone: date/zone
		interpolate format bind date-codes 'date
	]

	form-time: func [time [time!] format [any-string!] /local date zone][
		date: now/date zone: 0:00
		interpolate format bind date-codes 'time
	]

	color-codes: [
		#"r" [color/1] #"1" [to char! color/1]
		#"g" [color/2] #"2" [to char! color/2]
		#"b" [color/3] #"3" [to char! color/3]
		#"a" [color/4] #"4" [to char! color/4]
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

	export [form-date form-time to-local-time form-color pluralize]
]

;--## VALUES FILTER
;-------------------------------------------------------------------##
context [
	masks: reduce amend [
		issue!    [some url*]
		logic!    ["true" | "on" | "yes" | "1"]
		word!     [word]
		url!      [ident #":" some [url* | #":" | #"/"]]
		email!    [some url* #"@" some url*]
		path!     [word 1 5 [#"/" [word | integer]]]
		integer!  [integer]
		string!   [some [some ascii | utf-8]]
		'positive [number]
		'id       [ident]
		'key      [word 0 6 [#"." word]]
	]

	load-rfc3339: func [date [string!]][
		date: replace copy date "T" "/"
		replace date "Z" "+0:00"
		attempt [to-date date]
	]

	load-rfc822: use [day month][
		day: remove collect [
			foreach day system/locale/days [
				keep '|
				keep copy/part day 3
			]
		]

		month: remove collect [
			foreach month system/locale/months [
				keep '|
				keep copy/part month 3
			]
		]

		; "Tue, 08 Jan 2013 15:19:11 UTC"
		func [date [string!] /local part checked][
			date: collect [
				checked: parse/all date amend [
					day ", "
					copy part 2 digit (keep part)
					" " (keep "-")
					copy part month (keep part)
					" " (keep "-")
					copy part 4 digit (keep part)
					" " (keep "/")
					copy part [
						2 digit ":" 2 digit opt [":" 2 digit]
					] (keep part)
					" "
					[
						  ["UTC" | "GMT"]
						| copy part [["+" | "-"] 2 digit ":" 2 digit] (keep part)
						| copy part [["+" | "-"] 4 digit] (
							insert at part 4 ":"
							keep part
						)
						| to end
					]
				]
			]
			if checked [to date! rejoin date]
		]
	]

	as: func [
		[catch] type [datatype!] value [any-type!]
		/where format [none! block! any-word!]
	][
		case/all [
			none? value [return none]
			all [string? value any [type <> string! any-word? format]][value: trim value]
			type = logic! [if find ["false" "off" "no" "0" 0 false off no] value [return false]]
			all [string? value type = date!][
				value: any [
					load-rfc3339 value
					load-rfc822 value
				]
			]
			block? format [format: amend bind format 'value]
			none? format [format: select masks type]
			none? format [if type = type? value [return value]]
			any-word? format [format: select masks to word! format]
			block? format [
				unless parse/all value: form value format [return none]
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
	filter: [
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

		source: spec: rule: key: value: required: present: target: type: format: constraints: else: none

		constraint: use [is is-not? is-or-length-is op val val-type range group][
			op: val: val-type: none
			is: ['is | 'are]
			is-or-length-is: [
				[
					['length | 'size] (val: string-length? form value val-type: integer!)
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
						val: to word! val
						unless value = as/where :type get-from source :val format [
							report not-confirmed
						]
					) |
					is-not? 'within set group [any-block! | get-word!] otherwise (
						if get-word? group [
							unless all [
								function? group: get :group
								block? group: group
							][
								group: []
							]
						]

						either case [
							block? value [value = intersect value group]
							true [found? find group value]
						][
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
		skip-constraints: does [constraints: [to set-word! | to end]]

		valid?: func [value][any [value find type 'none!]]
		humanize: func [key /local initial][
			initial: true
			if parse form key amend [
				copy key any [
					key: alpha (if initial [uppercase/part key 1] initial: false)
					| ["-" | "_" | " "] (change key " " initial: true)
					| skip (initial: false)
				]
			][
				rejoin ["'" key "'"]
			]
		]

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

		engage: does [parse spec rule]
	]

	make-filter: func [source spec rule][
		spec: context compose/deep [
			(filter)
			errors: copy []
			result: copy []
			rule: [(copy/deep rule)]
			spec: [(spec)]
		]
		spec/source: copy source
		spec
	]

	validate: import: func [
		[catch] source [any-type!] spec [block!]
		/block /report-to errs [block!]
	][
		unless block? source [return none]

		spec: make-filter source compose/deep/only spec [
			any [
				set key set-word! (key: to word! key)
				set required opt 'opt (required: required <> 'opt)
				[
					set type datatype (type: get type)
					| set type ['dialect | 'match | 'object!]
				]
				set format opt [block! | get-word!]
				otherwise

				(
					value: either block [
						pick source 1
					][
						get-from source :key
					]

					either all [
						present: not any [
							all [
								none? value
								not find reduce [
									logic! block! 'object!
								] :type
							]
							empty? trim form value
						]

						not none? value: case [
							find [dialect match] :type [
								use [values][
									all [
										block? format
										values: attempt [to block! value]
										switch type [
											dialect [parse values format]
											match [match values format]
										]
										value
									]
								]
							]

							:type = 'object! [
								value: envelop value
								either block? format [
									import value format
								][value]
							]

							:type = block! [
								value: envelop value
								either block? format [
									all [parse value format value]
								][value]
							]

							:type = logic! [
								either as logic! value [true][false]
							]

							true [as/where :type value format]
						]
					][
						do-constraints
						if block [source: next source]
						repend result [key value]
					][
						skip-constraints
						case [
							all [present not block] [report invalid]
							required [report blank]
							not required [repend result [key none]]
						]
					]
				)

				constraints
			]

			end (if all [block not tail? source empty? errors][key: 'import report too-many])
		]

		unless spec/engage [raise "Could not parse Import specification"]

		all [block? errs insert clear errs spec/errors]
		unless qm/errors: all [not empty? spec/errors spec/errors][spec/result]
	]

	loose-import: func [[catch] source [block! none!] spec [block!]][
		unless source [return none]

		spec: make-filter source compose/deep/only spec [
			(skip-constraints)
			any [
				set key set-word! (key: to word! key)
				opt 'opt
				[set type datatype (type: get type)
				| set type ['dialect | 'match | 'object!] (type: :block!)]
				set format opt [block! | get-word!]
				otherwise

				(
					value: get-from source :key

					present: not any [
						none? value
						empty? trim form value
					]

					if all [
						present
						not none? value: case [
							:type = block! [value]
							:type = logic! [as logic! value]
							value [form value]
						]
					][
						repend result [key value]
					]
				)

				constraints
			]
		]

		either spec/engage [spec/result][
			raise "Could not parse Import specification"
		]
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
		[catch] source [block!] spec [block!]
		/report-to errs [block!]
		/loose "Ignore unmatched values"
	][
		spec: make-filter source spec [
			(result: context append remove-each item copy spec [not set-word? item] none)

			some [
				set key set-word! (key: to word! key)
				set required ['opt | 'any | 'some | none]
				copy type [lit-word! any ['| lit-word!] | datatype any ['| datatype]]
				otherwise

				(
					switch/default required [
						any [
							value: get-some source type
							either valid? value [do-constraints][skip-constraints]
						]
						opt [
							value: get-one source type
							either valid? value [do-constraints][skip-constraints]
						]
						some [
							value: get-some source type
							either valid? value [do-constraints][skip-constraints report invalid]
						]
					][
						value: get-one source type
						either valid? value [do-constraints][skip-constraints report invalid]
					]

					result/(key): value
				)

				constraints
			]

			end (
				if all [
					not loose
					not empty? source
					empty? errors
				][key: 'match report too-many]
			)
		]

		unless spec/engage [raise "Could not parse Match specification"]

		all [block? errs insert clear errs spec/errors]
		unless qm/errors: all [not empty? spec/errors spec/errors][spec/result]
	]

	export [import validate loose-import match]
]

;--## FILESYSTEM
;-------------------------------------------------------------------##
context [
	sw*: system/words
	rights: [ ; Permissions can be problematic.
		folder [
			owner-read: group-read:
			owner-write: group-write:
			owner-execute: group-execute: #[true]
			world-read: world-write: world-execute: #[true] ; #[false]
		]
		file [
			owner-read: group-read:
			owner-write: group-write: #[true]
			owner-execute: group-execute: #[false]
			world-read: world-write: #[true] world-execute: #[false]
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
				thru #"/" mk: (append path to file! copy/part target mk)
			] end
		][return path][
			throw make error! compose [access invalid-path (target)]
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
				wrt [target: make port! target/locals/file]
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
	add-protocol wrt 0 context [
		port-flags: system/standard/port-flags/pass-thru

		init: func [port url /local spec][
			unless all [
				url? url
				spec: get-space wrt:// url
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
	root: wrt://support/
	app: wrt://system/support/

	cache: []

	require: know: func [[catch] location [file!] /reset /args arg /local helper][
		if reset [remove/part find cache location 2]
		any [
			select cache location
			if all [
				helper: any [
					attempt [load/header app/:location]
					attempt [load/header root/:location]
				]
				helper: context compose [
					system/script: make system/script compose/only [
						title: helper/1/title
						header: helper/1
						parent: system/script
						args: (all [arg envelop arg])
					]
					header: (helper)
					system/script: system/script/parent
				]
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
						rejoin map/copy compose with/only spec (format) :form-val
					][
						raise (rejoin ["!!!Invalid " uppercase form name " tag."])
					]
				]
			]
		]
	]

	add-qtag a [
		"<a" :id :href :rel :class :title :accesskey :target ">"
	][
		href: file! | url! | path! | email!
		id: opt issue!
		class: any refinement!
		title: opt string!
		accesskey: opt char!
		target: opt 'new-tab
		rel: any lit-word!
	][
		all [email? href href: append to url! "mailto:" href]
		all [path? href href: link-to :href]
		target: switch/default target [new-tab ["_blank"]][none]
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
		method: opt word! is within [get post upload put delete]
		action: file! | url! | path!
		id: opt issue!
		class: any refinement!
	][
		enctype: none
		case/all [
			path? action [action: link-to :action]
			none? method [method: 'post]
			method = 'put [method: 'post action: join action %?put]
			method = 'delete [method: 'post action: join action %?delete]
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
		value: opt any-string! | number! | money! | tuple! | none!
	][
		name: to-key name
		value: any [value ""]
	]

	field: [
		required: opt 'opt | 'required
		type: opt 'email! | 'word! | 'url! | 'search! | 'date!
		name: word! | path!
		id: opt issue!
		size: opt integer! | pair!
		maxlength: opt integer!
		value: opt any-string! | date! | time! | none!
		class: any refinement!
		placeholder: opt any-string! | number! | date! | time! | none!
	]

	add-qtag field [
		{<input} :type :name :value :id :size :class :maxlength :placeholder :required " />"
	] field [
		type: switch/default type [
			email! ["email"] url! ["url"] date! ["date"] search! ["search"]
		]["text"]
		name: to-key name
		class: append any [class copy []] either type = "date" [/date][/text]
		if date? value [value: form-date/gmt value "%Y-%m-%d"]
		value: any [value ""]
		required: unless required = 'opt ["required"]
	]

	add-qtag number [
		{<input} :type :name :value :id :class :min :max :placeholder :required " />"
	][
		required: opt 'opt | 'required
		type: opt 'range
		name: word! | path!
		id: opt issue!
		value: integer! | none!
		range: opt pair!
		class: any refinement!
		placeholder: opt integer! | none!
	][
		type: form any [type 'number]
		name: to-key name
		class: append any [class copy []] /number
		value: any [value ""]
		min: max: none
		if range [min: range/x max: range/y]
		required: unless required = 'opt ["required"]
	]

	add-qtag password [
		{<input type="password"} :name :value :id :size :class :required " />"
	] field [
		name: to-key name
		class: append any [class copy []] /text
		value: any [value ""]
		required: unless required = 'opt ["required"]
	]

	add-qtag area [
		{<textarea} :name :id :cols :rows :class :maxlength :placeholder :required ">" value "</textarea>"
	] field [
		name: to-key name
		class: append any [class copy []] /text
		size: 0x0 + any [size 12x50]
		cols: abs size/x
		rows: abs size/y
		; if value [placeholder: none]
		value: any [value ""]
		required: unless required = 'opt ["required"]
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
		checked: if any [value = checked as logic! checked]["checked"]
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

	add-qtag select [
		{<select} :name :id :size :required :multiple {>}
	][
		required: opt 'opt | 'required
		name: word! | path!
		id: opt issue!
		size: opt integer! is between 1x10
		multiple: opt 'multiple
	][
		name: to-key name
		required: switch/default required [
			opt [none]
		]["required"]
		multiple: all [multiple "multiple"]
	]

	add-qtag option [
		{<option} :value :id :selected {>} label {</option>}
	][
		id: opt issue!
		value: any-string! | number!
		label: string!
		selected: opt logic! | none! | any-string! | number!
	][
		value: any [value ""]
		selected: if any [value = selected as logic! selected]["selected"]
		label: any [label ""]
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

	add-qtag time [
		{<time} :datetime {>} format {</time>}
	][
		datetime: date!
		format: opt string!
	][
		format: form-date datetime any [format "%D"]
		datetime: form-date datetime case [
			date/zone ["%Y-%m-%dT%T%Z"]
			date/time ["%Y-%m-%dT%TZ"]
			date ["%Y-%m-%d"]
		]
	]

	add-qtag button [
		(value) {<button type="submit"} :id :class ">"
	][
		name: opt word! | path! | none!
		value: opt any-string! | date! | time! | none!
		id: opt issue!
		class: any refinement!
	][
		value: either name [
			; a hack to accommodate IE6's broken button handling
			build-tag [hidden (name) (value)]
		][""]
	]

	add-qtag script [{<script} :src :type ">"][
		src: opt file! | url! | path!
		type: opt path!
	][
		case/all [
			all [none? src none? type][type: "text/javascript"]
			path? src [src: link-to :src]
			path? type [type: form type]
		]
	]

	add-qtag em [{<em} :class {>} label {</em>}][label: string! class: any refinement!][]

	build-tag: func [[catch] spec [block!] /local cmd action][
		either action: select qtags cmd: take spec: compose spec [
			throw-on-error [action spec]
		][
			rejoin ["!!! Invalid QuickTag Type: &lt;" cmd "&gt;"]
		]
	]

	export [add-qtag build-tag]
]

;--## RENDER
;-------------------------------------------------------------------##
context [
	root: wrt://system/views/

	load-rsp: func [[catch] body [string!] /local code mk][
		code: make string! length? body

		append code "REBOL [Title: {RSP Output}]^/out*: make string! {}^/"
		parse/all body [
			any [
				end (append code "out*") break
				| "<%" [
					  "==" copy mk to "%>" (repend code ["prin sanitize form (" mk "^/)^/"])
					| "=" copy mk to "%>" (repend code ["prin (" mk "^/)^/"])
					| [#":" | #"!"] copy mk to "%>" (repend code ["prin build-tag [" mk "^/]^/"])
					| copy mk to "%>" (repend code [mk newline])
					| (raise "Expected '%>'")
				] 2 skip
				| copy mk [to "<%" | to end] (repend code ["prin " mold/all mk "^/"])
			]
		]

		try-else [
			code: bind load code qm/binding
			bind code 'self
			uses [
				out*: "" prin: func [val][repend out* any [val ""] ()]
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
			all [
				partial?
				not suffix? path/2
			][repend path/2 [%.part qm/handler]]
			not suffix? path/2 [
				repend path/2 [
					
					any [qm/response/format qm/request/format %.html]
					qm/handler
				]
			]
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
		/partial /with locals [block!] /use args
		/type format /local out
		/only
	][
		if depth* > 20 [return ""]
		depth*: depth* + 1

		insert locals: any [locals []] 'args

		locals: collect [
			foreach word locals [
				keep reduce [to set-word! word get/any word]
			]
		]

		out: case/all [
			file? body [
				format: suffix? body: clean-view-path body partial
				body: read root/:body
			]
			string? body [
				case [
					only [body]
					any [none? format format = %.rsp][
						format: load-rsp body
						format locals
					]
					else [
						throw-on-error [render3p format body locals]
					]
				]
			]
		]

		depth*: depth* - 1
		out
	]

	render-each: func [
		'items [word! block!]
		source [series!]
		body [file! string!]
		/whole /with locals /local out
	][
		out: copy ""
		locals: append any [locals copy []] items: envelop items
		foreach :items source compose/only [
			append out do either whole ['render/with]['render/partial/with] body (locals)
		]
		out
	]

	export [clean-view-path render render-each build]
]

;--## MySQL
;-------------------------------------------------------------------##
context [
	sw*: get in system 'words
	net-log: func [msg][net-utils/net-log reform join ["MySQL:"] msg]

	system/error: make system/error [
		mysql: make object! [
			code: 3036
			type: "MySQL Error"
			error: none
		]
	]

	raise: func [[catch] reason][
		system/error/mysql/error: press envelop reason
		throw make error! [mysql error]
	]

	std-header-length: 4
	port-flags: system/standard/port-flags/pass-thru or 32 ; /binary

	last-packet?: func [status [integer!]][status = 254]
	closed?: func [port [port!]][not zero? port/state/flags and 1024]
	write?: func [port [port!]][port/state/flags and 2 = 2]

	throws: [closed "closed"]

	alive?: func [block [block!]][
		throws/closed <> catch block
	]

	reopen: func [[catch] port [port!]][
		net-log "Connection closed by server! Reopening"
		if throws/closed = catch [
			open port
		][throw raise "Server down!"]
	]

	assert-open: func [[catch] port [port!]][
		net-log "ASSERT OPEN"
		if closed? port/sub-port [
			net-log "CONNECTION CLOSED"
			port/state/flags: 1024
			throw-on-error [reopen port]
		]
		unless closed? port/sub-port [true]
	]

	get-flags: func [
		flags [integer!] codes [block!]
		/local list
	][
		list: copy []
		foreach [name value] codes [
			if equal? value flags and value [append list name]
		]
		list
	]

	scramble: use [
		to-pair xor-pair or-pair and-pair remainder-pair floor
		hash-v9 hash-v10 crypt-v9 crypt-v10 crypt-v11
	][
		to-pair: func [value [integer!]][to pair! reduce [value 1]]
		xor-pair: func [p1 p2][to-pair p1/x xor p2/x]
		or-pair: func [p1 p2][to-pair p1/x or p2/x]
		and-pair: func [p1 p2][to-pair p1/x and p2/x]

		remainder-pair: func [val1 val2 /local new][
			val1: either negative? val1/x [abs val1/x + 2147483647.0][val1/x]
			val2: either negative? val2/x [abs val2/x + 2147483647.0][val2/x]
			to-pair to integer! val1 // val2
		]

		floor: func [value][
			value: to integer! either negative? value [value - .999999999999999][value]
			either negative? value [complement value][value]
		]

		hash-v9: func [data [string!] /local nr nr2 byte][
			nr: 1345345333x1
			nr2: 7x1
			foreach byte data [
				if all [byte <> #" " byte <> #"^(tab)"][
					byte: to-pair to integer! byte
					nr: xor-pair nr (((and-pair 63x1 nr) + nr2) * byte) + (nr * 256x1)
					nr2: nr2 + byte
				]
			]
			nr
		]

		hash-v10: func [data [string!] /local nr nr2 adding byte][
			nr: 1345345333x1
			adding: 7x1
			nr2: to-pair to integer! #12345671
			foreach byte data [
				if all [byte <> #" " byte <> #"^(tab)"][
					byte: to-pair to integer! byte
					nr: xor-pair nr (((and-pair 63x1 nr) + adding) * byte) + (nr * 256x1)
					nr2: nr2 + xor-pair nr (nr2 * 256x1)
					adding: adding + byte
				]
			]
			nr: and-pair nr to-pair to integer! #7FFFFFFF
			nr2: and-pair nr2 to-pair to integer! #7FFFFFFF
			reduce [nr nr2]
		]

		crypt-v9: func [
			data [string!] seed [string!] /local
			new max-value clip-max hp hm nr seed1 seed2 d b i
		][
			new: make string! length? seed
			max-value: to-pair to integer! #01FFFFFF
			clip-max: func [value][remainder-pair value max-value]
			hp: hash-v9 seed
			hm: hash-v9 data	
			nr: clip-max xor-pair hp hm
			seed1: nr
			seed2: nr / 2x1

			foreach i seed [
				seed1: clip-max ((seed1 * 3x1) + seed2)
				seed2: clip-max (seed1 + seed2 + 33x1)
				d: seed1/x / to decimal! max-value/x
				append new to char! floor (d * 31) + 64
			]
			new
		]

		crypt-v10: func [
			data [string!] seed [string!] /local
			new max-value clip-max pw msg seed1 seed2 d b i
		][
			new: make string! length? seed
			max-value: to-pair to integer! #3FFFFFFF
			clip-max: func [value][remainder-pair value max-value]
			pw: hash-v10 seed
			msg: hash-v10 data	

			seed1: clip-max xor-pair pw/1 msg/1
			seed2: clip-max xor-pair pw/2 msg/2

			foreach i seed [
				seed1: clip-max ((seed1 * 3x1) + seed2)
				seed2: clip-max (seed1 + seed2 + 33x1)
				d: seed1/x / to decimal! max-value/x
				append new to char! floor (d * 31) + 64
			]
			seed1: clip-max (seed1 * 3x1) + seed2
			seed2: clip-max seed1 + seed2 + 33x0
			d: seed1/x / to decimal! max-value/x
			b: to char! floor (d * 31)

			forall new [new/1: new/1 xor b]
			head new
		]
	
		;--- New 4.1.0+ authentication scheme ---
		crypt-v11: func [data [string!] seed [string!] /local key1 key2][
			key1: checksum/secure data
			key2: checksum/secure key1
			to string! key1 xor checksum/secure join seed key2
		]
	
		scramble: func [data [string!] port [port!] /v10 /local seed][
			if any [none? data empty? data][return ""]
			seed: port/locals/crypt-seed
			if v10 [return crypt-v10 data copy/part seed 8]
			either port/locals/protocol > 9 [
				either port/locals/auth-v11 [
					crypt-v11 data seed
				][
					crypt-v10 data seed
				]
			][
				crypt-v9 data seed
			]
		]
	]

	transcribe: use [
		null string byte int int24 long long64 length nbytes field
		value byte-char null? b0 b1 b2 b3 mk remnants
	][
		b0: b1: b2: b3: value: mk: none
		; byte-char: complement charset []
		byte-char: [skip]

		null: to char! 0

		string: [copy value to null null | copy value to end]

		byte: [copy value byte-char (value: to integer! to char! :value)]

		int: [
			byte (b0: value)
			byte (b1: value value: b0 + (256 * b1))
		]

		int24: [
			byte (b0: value)
			byte (b1: value)
			byte (b2: value value: b0 + (256 * b1) + (65536 * b2))
		]

		long: [
			byte (b0: value)
			byte (b1: value)
			byte (b2: value) 
			byte (
				b3: value
				value: b0 + (256 * b1) + (65536 * b2) + (16777216.0 * b3)
				value: any [attempt [to integer! value] value]
			)
		]

		long64: [
			long skip 4 b3 (net-log "Warning: long64 type detected !")
		]

		length: [
			#"^(FB)" (value: 0 null?: true) |
			#"^(FC)" int | #"^(FD)" int24 | #"^(FE)" long | byte
		]

		nbytes: [
			#"^(01)" byte | #"^(02)" int | #"^(03)" int24 | #"^(04)" long | none (value: 255)
		]

		field: [
			(null?: false)
			length copy value value skip
		]

		remnant: [(value: none) copy value some skip]

		transcribe: func [message [string!] data [series!] rule [block!] /local complete][
			complete: false
			rule: bind rule 'value
			parse/all/case data [rule end (complete: true)]
			net-log reform ["TRANSCRIBE" message to tag! complete]
			unless complete [
				net-log ["Bad Packet:" mold enbase/base to binary! data 16]
			]
			complete
		]
	]

	retrieve: func [[throw] port [port!] buffer [binary!] size [integer!]][
		size: read-io port/sub-port buffer size
		unless positive? size [
			close port/sub-port
			throw throws/closed
		]		
		net-log ["low level read of" size "bytes"] 
		size
	]

	read-packet: use [defrag-read][
		defrag-read: func [port [port!] buffer [binary!] expected [integer!]][
			clear buffer
			while [expected > length? buffer][
				retrieve port buffer expected - length? buffer
			]
		]

		read-packet: func [[catch] port [port!] /local packet-size wire status old-cache][
			net-log "READ PACKET"
			wire: port/locals
			wire/stream-end?: false
	
		;--- reading header ---
			defrag-read port wire/buffer std-header-length

			transcribe "Read Packet" wire/buffer [
				int24 (packet-size: value)
				byte  (wire/seq-num: value net-log ["Seq." mold to issue! value "of" to tag! packet-size])
			]

		;--- reading data ---
			if packet-size > wire/buffer-size [
				net-log ["Expanding buffer, OLD:" wire/buffer-size "NEW:" packet-size]
				old-cache: wire/cache
				wire/buffer: make binary! wire/buffer-size: packet-size + (length? old-cache) + length? wire/buffer
				wire/cache: make binary! wire/cache-size: wire/buffer-size
				insert tail wire/cache old-cache
			]

			defrag-read port wire/buffer packet-size

			if packet-size <> length? wire/buffer [
				raise "Error: inconsistent packet length !"
			]	

			wire/last-status: status: to integer! wire/buffer/1
			wire/error-code: wire/error-msg: none

			net-log ["Status" to tag! status]

			switch status [
				255 [ ; exception
					transcribe "Recover Exception" next wire/buffer case [
						find wire/capabilities 'protocol-41 [
							[
								int    (wire/error-code: value)
								6 skip
								string (wire/error-msg: value)
							]
						]

						any [
							none? wire/protocol
							wire/protocol > 9
						][
							[
								int    (wire/error-code: value)
								string (wire/error-msg: value)
							]
						]

						true [
							wire/error-code: 0
							[string (wire/error-msg: value)]
						]
					]
					wire/stream-end?: true
					throw-on-error [raise [wire/error-msg " <" any [wire/error-code "--"] ">"]]
				]

				254 [ ; eof
					case [
						transcribe "EOF" next wire/buffer [
							int (net-log ["Warnings:" value])
							int (net-log ["Status:" value])
						][
							wire/more-results?: not zero? wire/buffer/4 and 8
							wire/stream-end?: true
						]
					]
				]

				0 [ ; ok
					if none? wire/expecting [
						transcribe "Ok" next wire/buffer [
							length
							length
							int (wire/more-results?: not zero? value and 8 net-log ["Ok/Status:" to tag! value])
							int (net-log ["Ok/Warnings:" to tag! value])
							opt remnant (
								if value [net-log ["Ok/Message:" value]]
								wire/matched-rows: value
							)
						]
						wire/stream-end?: true
					]
				]
			]

			net-log to string! wire/buffer

			wire/buffer
		]
	]

	flush-pending: func [port [port!] /local wire chunk-size][
		wire: port/locals
		unless wire/stream-end? [
			net-log "Flushing Unread Data"
			until [
				clear wire/buffer
				chunk-size: retrieve port wire/buffer wire/buffer-size
				all [wire/buffer-size > chunk-size last-packet? last wire/buffer]
			]
			net-log "Flush End."
			wire/stream-end?: true
		]
	]

	send-packet: use [
		to-byte to-int to-int24 to-long to-string to-binary form send-packet
	][
		form: func [value [any-string!]][to string! value]

		to-byte: func [value [integer!]][to char! value]

		to-int: func [value [integer!]][
			join to char! value // 256 to char! value / 256
		]

		to-int24: func [value [integer!]][
			rejoin [
				to char! value // 256
				to char! (to integer! value / 256) and 255
				to char! (to integer! value / 65536) and 255
			]
		]

		to-long: func [value [integer!]][
			rejoin [
				to char! value // 256
				to char! (to integer! value / 256) and 255
				to char! (to integer! value / 65536) and 255
				to char! (to integer! value / 16777216) and 255
			]
		]

		to-string: func [value [string!]][
			join value to char! 0
		]

		to-binary: func [value [any-string!] /local length][
			length: to char! length? value: form value
			; either length > 0 [join length value][""]
			join length value
		]

		send-packet: func [port [port!] data [string! block!]][
			net-log ["SEND PACKET" to tag! port/locals/seq-num: port/locals/seq-num + 1]
			if block? data [data: rejoin bind data 'send-packet]
			net-log ["Packet" mold data]

			data: join #{} [
				to-int24 length? data
				to-byte port/locals/seq-num
				data
			]

			write-io port/sub-port data length? data
			not port/locals/stream-end?: false
		]
	]

	send-command: use [commands encode-refresh][
		commands: [
			;sleep			0
			quit			1
			init-db			2
			query			3
			;field-list		4
			create-db		5
			drop-db			6
			reload			7
			shutdown		8
			statistics		9
			;process-info	10
			;connect		11
			process-kill	12
			debug			13
			ping			14
			;time			15
			;delayed-insert	16
			change-user		17
		]

		encode-refresh: use [codes][
			codes: [
				grant		1	; Refresh grant tables
				log			2	; Start on new log file
				tables		4	; Close all tables 
				hosts		8	; Flush host cache
				status		16	; Flush status variables
				threads		32	; Flush status variables
				slave		64	; Reset master info and restart slave thread
				master		128 ; Remove all bin logs in the index
			]					; and truncate the index

			encode-refresh: func [block [block!] /local total name value][
				total: 0
				foreach name block [
					either value: select codes :name [
						total: total + value
					][
						raise ["Unknown argument: " :name]
					]
				]
				total
			]
		]

		send-command: func [[throw] 'command [word!] port [port!] statement [string! block!] /response][
			net-log ["SEND COMMAND" to tag! :command]
			unless find commands command [raise ["Unknown Command: " command]]

			port/locals/seq-num: -1
			send-packet port [
				to-byte select commands command
				switch/default command [
					quit shutdown statistics debug ping [""]
					reload [to-byte encode-refresh statement]
					process-kill [to-long pick statement 1]
					change-user [
						rejoin [
							to-string pick statement 1
							to-string scramble pick statement 2 port
							to-string pick statement 3
						]
					]
				][
					either string? statement [statement][pick statement 1]
				]
			]

			if response [
				net-log "Fetching Response"
				response: read-packet port
				port/locals/stream-end?: true
				switch/default command [
					statistics [to string! response]
					ping [
						either zero? port/locals/last-status [
							net-log "Ping Response OK."
							true
						][
							net-log ["BAD PING RESPONSE" to tag! port/locals/last-status]
							false
						]
					]
				][true]
			]
		]
	]

	bind-query: use [escape to-sql][
		escape: use [cut safe escapes][
			safe: complement cut: charset {^(00)^/^-^M^(08)'"\}
			escapes: make hash! [
				#"^(00)"	"\0"
				#"^/" 		"\n"
				#"^-" 		"\t"
				#"^M" 		"\r"
				#"^(08)" 	"\b"
				#"'" 		"\'"
				#"^""		{\"}
				#"\" 		"\\"
			]

			escape: func [value [string!] /local mk][
				parse/all value [
					any [
						some safe |
						mk: cut (mk: change/part mk select escapes mk/1 1) :mk
					] end
				]
				value
			]
		]

		to-sql: func [value [any-type!] /local res][
			switch/default type?/word value [
				none!	["NULL"]
				date!	[form-date value either value/time [{'%Y-%m-%d %H:%M:%S'}][{'%Y-%m-%d'}]]
				time!	[form-time value {'%H:%M:%S'}]
				money!	[head remove find mold value "$"]
				string!	[join "'" [escape copy value "'"]]
				binary!	[to-sql to string! value]
				; block!	[
				; 	if empty? value: reduce value [return "()"]
				; 	res: append make string! 100 #"("
				; 	forall value [repend res [to-sql value/1 #","]]
				; 	head change back tail res #")"
				; ]
				word!	[escape replace/all form value "-" "_"]
				path!	[either parse value [some word!][remove press map-each word to block! value [join "." form word]][form value]]
				logic!  [either value [1][0]]
				tuple! pair! tag! issue! email! url! file! block! [to-sql mold/all value]
			][
				either any-string? value [to-sql form value][form value]
			]
		]

		bind-query: func [statement [string! block!] /local args mk ex][
			if string? statement [return statement]
			statement: take args: reduce statement

			press collect [
				parse/all statement [
					mk: any [
						to #"?" ex: (
							keep copy/part mk ex
							keep to-sql take args
						) #"?" mk:
					][end | to end ex: (keep copy/part mk ex)]
				]
			]
		]
	]

	send-query: use [get-column-count get-column-headers][
		get-column-count: func [port [port!] /local wire count][
			wire: port/locals
			transcribe "Get Column Count" read-packet port [
				#{00} (count: 0) remnant ; OK packet, already processed by 'read-packet
				|
				length (count: value)
			]
			net-log ["Column Count:" to tag! count]
			count
		]

		get-column-headers: use [type? header-flags header!][
			type?: use [types][
				types: [
					0    decimal      1    tiny       2    short
					3    long         4    float      5    double
					6    null         7    timestamp  8    longlong
					9    int24        10   date       11   time
					12   datetime     13   year       14   newdate
					15   var-char     16   bit        246  new-decimal
					247  enum         248  set        249  tiny-blob
					250  medium-blob  251  long-blob  252  blob
					253  var-string   254  string     255  geometry
				]

				type?: func [code [integer!]][any [select types code 'unknown]]
			]

			header-flags: [
				not-null        1      ; field can't be NULL
				primary-key     2      ; field is part of a primary key
				unique-key      4      ; field is part of a unique key
				multiple-key    8      ; field is part of a key
				blob            16
				unsigned        32
				zero-fill       64
				binary          128
				enum            256    ; field is an enum
				auto-increment  512    ; field is a autoincrement field
				timestamp       1024   ; field is a timestamp
				set             2048   ; field is a set
				no-default      4096
				; other1          8192 
				; other2          16384
				num             32768  ; field is num (for clients)
			]

			header!: make object! [
				table: name: length: code: type: flags: decimals: charset: none
			]

			get-column-headers: func [
				port [port!] count [integer!]
				/local wire column
			][
				wire: port/locals
				wire/columns: make block! count

				loop count [
					header: make header! []
					transcribe join "Get Column Header #" count read-packet port [
						field  ; catalog
						field  ; db
						field  (header/table: value)
						field  ; org-table
						field  (header/name: to word! value)
						field  ; org-name
						byte   ; filler
						int    (header/charset: value)
						long   (header/length: value)
						byte   (header/type: type? header/code: value)
						int    (header/flags: get-flags value header-flags)
						byte   (header/decimals: value)
						int    ; filler
						nbytes ; default
					]
					append wire/columns :header
					net-log header
				]


				read-packet port			; check the ending flag
				unless wire/stream-end? [
					flush-pending port
					raise "Error: end of columns stream not found"
				]
				wire/stream-end?: false		; prepare correct state for 
				clear wire/cache			; rows reading.
				wire/columns
			]
		]

		send-query: func [port [port!] statement [string! block!] /local count][
			net-log "SEND QUERY"
			net-log statement: bind-query statement
			send-command query port statement
			count: get-column-count port
			unless port/locals/stream-end? [
				get-column-headers port count
			]
			none
		]
	]

	send: use [send-queries auto-ping][
		send-queries: use [statement space not-squote not-dquote delimiter][
			statement: make string! 1024

			space: charset " ^-^M^/"
			not-squote: complement charset "'"
			not-dquote: complement charset {"}

			send-queries: func [
				port [port!] statements [string!]
				/local mk ex
			][
				net-log "SEND QUERIES"
				delimiter: port/locals/delimiter

				parse/all statements [
					mk: any [
						#"#" thru newline
						| #"'" any ["\'" | "''" | some not-squote] #"'"
						| {"} any [{\"} | {""} | some not-dquote] {"}
						| #"`" thru #"`"
						| ex: delimiter (
							insert/part clear statement mk ex
							send-query port statement
						) any space mk:
						| skip
					]
				]
				unless tail? mk [send-query port mk]
			]
		]

		auto-ping: func [[catch] port [port!] /local response][
			if port/locals/auto-ping? [
				net-log "Sending Ping"
				response: catch [send-command/response ping port []]
				if any [
					throws/closed = response
					not response
				][
					net-log "PING FAILED"
					throw-on-error [reopen port]
				]
			]
		]

		send: func [[catch] port [port!] statement [string! block!] /local response command][
			net-log "SEND"
			port/locals/columns: none

			unless statement = [ping] [auto-ping port]

			if throws/closed = catch [
				case/all [
					string? statement [
						either statement/1 = #"[" [
							statement: load statement
						][
							send-queries port statement
						]
					]

					block? statement [
						parse statement [
							  string! (send-query port statement)
							| set command word! (
								response: send-command/response :command port next statement
							)
							| (raise "Not a Valid MySQL Statement")
						]
					]
				]
			][raise "Connection lost - Port closed!"]

			response
		]
	]

	traverse: use [map-rows read-packet-via][
		map-rows: use [to-date to-datetime try-load type-handlers][
			to-date: func [v][any [attempt [to date! v] 1-jan-0000]]

			to-datetime: func [val][
				all [
					val: any [attempt [to date! val] 1-jan-0000/00:00]
					val/zone: settings/zone
					val
				]
			]

			try-load: func [value [none! string!]][
				all [
					value
					value: any [attempt [load value] value]
				]
				if any-word? value [value: form value]
				value
			]

			type-handlers: [
				decimal			[to money!]
				tiny			[to integer!]
				short			[to integer!]
				long			[to integer!]
				float			[to decimal!]
				double			none
				null			none
				timestamp		[to-datetime]
				longlong		[to integer!]
				int24			[to integer!]
				date			[to-date]
				time			[to time!]
				datetime		[to-datetime]
				year			[to integer!]
				newdate			none
				var-char		none
				bit				none
				new-decimal		[to money!]
				enum			none
				set				none
				tiny-blob		none
				medium-blob		none
				long-blob		none
				blob			none
				var-string		none
				string			none
				geometry		none
			]

			map-rows: func [
				port [port!] rows [block!]
				/local row index convert-body action columns handler tmp
			][
				columns: port/locals/columns
				convert-body: make block! 1
				action: [if tmp: pick row (index)]

				foreach column columns [
					index: index? find columns column

					if case [
						all [
							find [text blob var-string] column/type
							find column/flags 'binary
						][handler: [try-load]]

						all [
							column/type = 'tiny
							column/length = 1
						][handler: [equal? "1"]]

						handler: select type-handlers column/type [
							handler <> 'none
						]
					][
						append convert-body append/only compose action head insert at compose [
							change/only at row (index) :tmp
						] 5 handler
					]
				]

				if not empty? convert-body [
					either port/locals/flat? [
						while [not tail? rows][
							row: rows
							do convert-body
							rows: skip rows length? columns
						]
					][
						foreach row rows :convert-body
					]
				]
			]
		]

		read-packet-via: func [port [port!] /local wire][
			wire: port/locals

			if empty? wire/cache [
				read-packet port
				if wire/stream-end? [return #{}]	; empty set !
			]

			local: wire/cache			; swap cache<=>buffer		
			wire/cache: wire/buffer
			wire/buffer: :local

			local: wire/cache-size
			wire/cache-size: wire/buffer-size
			wire/buffer-size: :local

			read-packet port
			wire/cache
		]

		traverse: func [
			port [port!] limit [none! integer!]
			/local wire row-data row rows column-count count last-row
		][
			net-log "TRAVERSE"
			wire: port/locals

			either wire/stream-end? [
				net-log "Traverse Truncated"
				make block! 0
			][
				rows: make block! max any [limit 0] wire/rows
				column-count: length? wire/columns
				count: 0

				until [
					row-data: read-packet-via port

					if empty? row-data [return rows]		; empty set

					row: make block! column-count

					unless transcribe "Traverse" row-data [
						any [field (append row value)]
					][
						probe to string! wire/buffer
						probe to string! wire/cache
					]

					either wire/flat? [
						insert tail rows row
					][
						insert/only tail rows row
					]

					any [
						wire/stream-end?
						limit = count: count + 1
					]	; end of stream or rows # reached
				]

				if wire/load? [map-rows port rows]

				if wire/newlines? [
					either wire/flat? [
						new-line/skip rows true column-count
					][
						new-line/all rows true
					]
				]

				rows
			]
		]
	]

	assert-handshake: use [handshake connect][
		; Modified Open-Proto minus Proxy:
		connect: func [
			{Open the socket connection and confirm server response.} 
			port "Initalized port spec" 
			/sub-protocol subproto 
			/secure 
			/local sub-port data in-bypass find-bypass bp
		][
			if not sub-protocol [subproto: 'tcp] 
			net-log reduce ["Opening" to string! subproto "for" to string! port/scheme] 
			net-log	reduce ["connecting to:" port/host]

			port/sub-port: sub-port: system/words/open/lines compose [
				scheme: (to lit-word! subproto) 
				host: port/host
				user: port/user
				pass: port/pass
				port-id: port/port-id
			]

			if all [secure find [ssl tls] subproto][
				system/words/set-modes sub-port [secure: true]
			]

			sub-port/timeout: port/timeout
			sub-port/user: port/user
			sub-port/pass: port/pass
			sub-port/path: port/path
			sub-port/target: port/target
			net-utils/confirm/multiline sub-port none
			set-modes sub-port [keep-alive: true]
			sub-port/state/flags: 524835	; force /direct/binary mode
			port/state/flags: port/state/flags or port-flags
			port
		]

		handshake: use [wire! client-flags handshake][
			wire!: make object! [
			;--- Internals (do not touch!)---
				seq-num: 0
				buffer-size: cache-size: 10'000
				last-status:
				stream-end?:
				more-results?:
				expecting: none
				buffer: none
				cache: none
			;-------
				auto-commit: on		; not used, just reserved for /Command compatibility.
				rows: 10			; not used, just reserved for /Command compatibility.
				load?: on
				auto-ping?: on
				flat?: off
				delimiter: #";"
				newlines?: value? 'new-line
				init:
				matched-rows:
				columns:
				protocol:
				version:
				thread-id:
				crypt-seed:
				capabilities:
				error-code:
				error-msg:
				conv-list: 
				character-set:
				server-status:
				auth-v11:
				native-password: none
			]

			client-flags: [
				long-password		1		; new more secure passwords
				found-rows			2		; Found instead of affected rows
				long-flag			4		; Get all column flags
				connect-with-db		8		; One can specify db on connect
				no-schema			16		; Don't allow db.table.column
				compress			32		; Can use compression protcol
				odbc				64		; Odbc client
				local-files			128		; Can use LOAD DATA LOCAL
				ignore-space		256		; Ignore spaces before '('
				protocol-41			512		; Use new protocol (was "Support the mysql_change_user()")
				interactive			1024	; This is an interactive client
				ssl					2048	; Switch to SSL after handshake
				ignore-sigpipe		4096	; IGNORE sigpipes
				transactions		8196	; Client knows about transactions
				reserved			16384	; for 4.1.0 only
				secure-connection	32768	; use new hashing algorithm
				multi-queries		65536	; enable/disable multiple queries support
	    		multi-results		131072	; enable/disable multiple result sets
			]

			handshake: func [[catch] port [port!] /local wire capabilities key error response][
				either wire: port/locals [
					clear wire/cache
					clear wire/buffer
					wire/seq-num: 0
					wire/last-status:
					wire/stream-end?: none
				][
					wire: port/locals: make wire! []
					wire/buffer: make binary! wire/buffer-size
					wire/cache: make binary! wire/buffer-size
				]

				transcribe "Handshake" read-packet port [
					byte   (wire/protocol: value)
					string (wire/version: value)
					long   (wire/thread-id: value)
					string (wire/crypt-seed: value)
					int    (wire/capabilities: get-flags value client-flags)
					byte   (wire/character-set: value)
					int    (wire/server-status: value)
					13 skip		; reserved for future use
						; 2 byte - server capabilities (two upper bytes)
						; byte - length of the scramble
						; 10 #"^(a0)"
					string	(
						if value [
							wire/crypt-seed: join copy wire/crypt-seed value
							wire/auth-v11: yes
						]
					)
					opt [
						string (wire/native-password: value = "mysql_native_password")
					]
					to end
				]

				if wire/protocol = -1 [
					close port/sub-port
					raise "Server configuration denies access to locals source^/Port closed!"
				]

				; found-rows, connect-with-db, protocol-41, multi-queries
				; multi-results, long-password, secure-connection
				capabilities: 229899

				capabilities: either wire/protocol > 9 [
					capabilities or client-flags/long-password
				][
					capabilities and complement client-flags/long-password
				]

				send-packet port [ ; Version 4.1
					to-long capabilities
					to-long (length? port/user) + (length? port/pass)
						+ 7 + std-header-length
					to-byte 8 ; wire/character-set
					"^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@"
					to-string port/user
					to-binary key: scramble port/pass port
					to-string any [port/path "^@"]
				]

				either error? set/any 'error try [
					response: read-packet port
				][
					any [all [find key #{00} wire/error-code] error]	; -- detect the flaw in the protocol
				][
					if response = #{FE} [
						net-log "Switching to old password mode!"
						send-packet port [to-string scramble/v10 port/pass port]
						read-packet port
					]
					net-log "Connected to server. Handshake OK"
					none
				]
			]
		]

		assert-handshake: func [[catch] port [port!] /local error][
			repeat tries 10 [
				either handshake connect port [
					close port/sub-port
				][
					return port/locals/stream-end?: true	; force stream-end, so 'copy won't timeout !
				]
			]

			raise "Cannot handshake with server"
		]
	]

	add-protocol mysql 3306 context [
	;------ Public interface ------
		init: func [port [port!] spec /local args][
			if url? spec [
				net-utils/url-parser/parse-url port spec
			]
			port/url: spec
			if none? port/host [
				raise ["No network server for " scheme " is specified"]
			] 
			if none? port/port-id [
				raise ["No port address for " scheme " is specified"]
			]
			if all [none? port/path port/target][
				port/path: port/target
				port/target: none
			]
			if all [port/path slash = find/last port/path slash][
				remove back tail port/path
			]
			if none? port/user [port/user: make string! 0]
			if none? port/pass [port/pass: make string! 0]
			if port/pass = "?" [port/pass: ask/hide "Password: "]
		]

		open: func [[catch] port [port!] /local statement on-connect][
			net-log "OPEN HANDLER"
			throw-on-error [assert-handshake port]

			insert port "SET collation_connection = utf8_unicode_ci; SET NAMES utf8;"

			unless write? port [
				either statement: port/state/custom [
					unless string? statement: first sql [raise "invalid query"]
					insert port statement
				][
					insert port either port/target [
						join "DESC " port/target
					][
						port/locals/flat?: on
						join "SHOW " sw*/pick ["DATABASES" "TABLES"] not port/path
					]
				]
			]

			if on-connect: port/locals/init [
				net-log ["Sending Init String:" on-connect]
				insert port on-connect
			]

			port/state/tail: 10		; for 'pick to work properly
		]

		insert: func [port [port!] statement [block! string!]][
			net-log "INSERT HANDLER"
			assert-open port
			flush-pending port
			send port statement
		]

		pick: func [port [port!] data [none! integer!]][
			net-log "PICK HANDLER"
			if any [
				none? data
				data = 1
			][
				unless port/locals/stream-end? [copy/part port 1]
			]
		]

		copy: func [port [port!] /part limit [integer!]][
			net-log "COPY HANDLER"
			assert-open port
			traverse port limit
		]

		select: func [port [port!] data [string! binary!]][
			net-log "SELECT HANDLER"
			any [
				insert port data
				copy port
			]
		]

		close: func [port [port!]][
			port/sub-port/timeout: 4
			either error? try [
				flush-pending port
				send-command quit port []
			][net-log "Error on closing port!"][net-log "Close ok."]
			sw*/close port/sub-port
			port/state/flags: 1024
		]
	]

	query-db: use [name-fields][
		name-fields: func [db [port!] record [block!]][
			new-line/all/skip collect [
				repeat os length? record [
					keep db/locals/columns/:os/name
					keep/only record/:os
				]
			] true 2
		]

		query-db: func [
			[catch]
			data [string! block!] /other db [port!]
			/flat /raw /named /first /local result wire
		][
			unless other [db: qm/models/database]
			wire: db/locals
			wire/flat?: true? flat
			wire/load?: not true? raw
			result: try-else [select db data][throw :reason]
			wire/flat?: off
			wire/load?: on
			if block? result [
				case/all [
					first [flat: none clear next result]
					all [flat named][
						flat: named: first: false
						if greater? length? result length? wire/columns [
							make error! "/flat and /named not allowed in this case!"
						]
						name-fields db result
					]
					flat [first: none]
					named [forall result [change/only result name-fields db result/1]]
					block? result [new-line/all result true]
					first [result: result/1]
				]
			]

			result
		]
	]

	;--- Register ourselves. 
	; net-utils/net-install MySQL self 3306

	export [to-sql query-db] ; [send-sql name-fields]
]

;--## ArrowDB
;-------------------------------------------------------------------##
context [
	space: wrt://space/ ; for storing files related to a record

	sw*: get in system 'words

	insert-db: func [table [word! port!] packet [object!] /local keys values id][
		if port? table [table: table/locals/name]
		id: get in packet 'id
		keys: words-of packet
		packet: body-of packet
		values: remove rejoin collect [loop length? keys [keep ",?"]]
		keys: remove rejoin collect [foreach key keys [keep "," keep form key]]

		query-db compose [
			(press ["INSERT INTO " table " (" keys ") VALUES (" values ")"])
			(extract/index packet 2 2)
		]

		either id <> 0 [id][
			pick query-db/first "SELECT LAST_INSERT_ID()" 1
		]
	]

	update-db: func [table [word! port!] id [any-type!] packet [object!] /local keys][
		if port? table [table: table/locals/name]

		keys: next press collect [
			foreach key words-of packet [keep reduce [", " form key "=?"]]
		]
		packet: values-of packet

		query-db compose [
			(rejoin ["UPDATE " table " SET" keys " WHERE id = ?"])
			(packet) (id)
		]
	]

	delete-db: func [table [word! port!] id [any-type!]][
		if port? table [table: table/locals/name]
		query-db compose [(rejoin ["DELETE FROM " table " WHERE id = ?"]) id]
	]

	export [insert-db update-db delete-db]

	table!: context [
		name: header: spec: index: root: path: packet: filter: changed: #[none]

		locate: func [id][form id]

		record: context [
			id: new?: unique?: header: owner: root: path: packet: #[none]

			data: []
			errors: []

			get: func [key [word!]][foreach [k v] data [if k = key [break/return v]]]
			set: func [key [word!] value][unset key value repend data [key value] value]
			unset: func [key [word!]][remove-each [k v] data [k = key]]
			inject: func [pending [block! none!] /only keys [block!] /except exceptions [block!]][
				unless block? :pending [return none]
				if only [remove-each [key value] pending [not find keys key]]
				if except [remove-each [key value] pending [find exceptions key]]
				foreach [key val] pending [set key val]
				data
			]

			submit: func [[catch] 'form [word!] args [block! none!] /local][
				case with/only owner/locals [
					not block? args [none]
					not local: in forms :form [raise ["Bad Submission Request: " mold form]]
					local: import/report-to args forms/:local errors [
						inject local
						local
					]
					parse args [any [word! skip]][
						inject args
						none
					]
				]
			]

			increment: func [key [word!]][
				set key 1 + any [get key 0]
			]

			decrement: func [key [word!]][
				set key -1 + any [get key 0]
			]

			toggle: func [key [word!]][set key not get key]

			touch: func [key [word!]][set key now]

			unique?: does [not find owner get 'id]

			store: func [
				[catch] "Saves record to database. Returns record on successful store."
				/only fields [block!] "Save only these fields."
			][
				; throw-on-error [
				
				case [
					not new? [ ; record already exists--UPDATE
						change owner self 
						self
					]
					not unique? [ ; new record has existing id--fail
						errors: [id ["Record ID already exists."]] 
						none
					]
					else [ ; record does not exist--INSERT
						append owner self 
						self
					]
				]
				; ]  ; throw
			]

			destroy: does [
				on-delete
				unless new? [delete-db owner id]
				self
			]

			on-load: on-save: on-create: on-change: on-delete: none
		]

		forms: context [
			inherit: func [base [block!] spec [block!] /local rule /new][
				if new [spec: copy spec]
				if parse base [
					some [
						copy rule [set-word! [to set-word! | to end]]
						(unless find spec rule/1 [append spec rule])
					]
				][spec]
			]
		]

		queries: context []
	]

	create-record: func [table [port!]][
		make table/locals/record compose [
			new?: true
			data: copy []
			owner: (table)
			header: (table/locals/header)
			on-create
		]
	]

	load-record: func [table [port!] record [block!]][
		make table/locals/record compose/only [
			data: (record)
			id: get 'id
			owner: (table)
			header: (table/locals/header)
			root: path: dirize join owner/locals/root owner/locals/locate :id
			on-load
		]
	]

	record?: func [record][
		all [
			object? record
			find/match words-of record [
				id new? unique? header owner root path packet data errors
			]
			block? record/data
			record
		]
	]

	prepare: use [make-packet populate][
		make-packet: use [column detect-type][
			column: context [name: type: required: default: within: role: size: increment: none]

			detect-type: use [digit in-squote][
				digit: charset "0123456789"
				in-squote: complement charset "'"

				detect-type: func [column [object!]][
					parse/all column/type with/only column [
						  "int" opt ["(" copy size some digit ")"] end (type: integer! size: all [size load size])
						| "varchar(" copy size some digit ")" (type: string! size: to integer! size)
						| "decimal(" copy size some digit ",2)" end (type: money! size: 2 + to integer! size)
						| "decimal" end (type: money!)
						| "float" end (type: decimal!)
						| "date" ["time" | "stamp" |] end (type: date!)
						| "timestamp" end (type: date!)
						| "time" end (type: time!)
						| "tinyint(1)" end (type: logic!)
						| "enum(" (within: copy []) some [
							"'" copy local some [in-squote | "''"] "'" ["," | ")" end]
							(append within replace/all local "''" "'")
						] (type: string!)
						| "varbinary(" copy size some digit ")" (type: any-type! size: to integer! size)
						| "text" (type: string!)
						| "blob" (type: any-type!)
					]

					column
				]
			]

			make-packet: func [[catch] table [port!] /local][
				unless object? table/locals/packet [
					unless local: query-db join "SHOW COLUMNS FROM " table/locals/name [
						raise ["Unable to Access Table " table/locals/name]
					]

					unless empty? local: collect [
						parse local with/only column [
							some [
								(set column none)
								into [
									"uid" to end |
									set name string!
									set type string!
									["YES" (required: false) | "NO" (required: true)] ; null accepted?
									set role [string! | none!] ; key
									set default [string! | none!]
									set increment [string! | none!] ; extra
									(
										keep to set-word! name
										detect-type column
										keep make column []
									)
								]
							]
						]
					][
						table/locals/packet: context local
					]
				]
				make table/locals/packet []
			]
		]

		populate: use [errors report][
			report: func [key code message][
				unless select errors :key [repend errors [:key copy []]]
				repend select errors :key [:code press message]
			]


			populate: func [[catch] packet [object!] record [object!] /local value spec][
				clear errors: record/errors
				
				; if record/partial-store? [
				; 	spec: body-of packet
				; 	remove-each [word val] spec [
				; 		not find record/partial-store? to-word word
				; 	]
				; 	packet: context spec
				; ]

				foreach [key spec] body-of packet [
					key: to word! key
					value: record/get key

					if all [
						value = <auto>
						spec/increment = "auto_increment"
					][value: 0]

					verify [
						any [
							not spec/required
							not none? value
						][
							report :key 'required [uppercase form key " is Required"]
						]

						any [
							spec/type = any-type!
							spec/type = type? value
							none? value
						][
							report :key 'wrong-type [uppercase form key " is not of type " uppercase form spec/type]
						]

						any [
							not spec/size
							spec/size >= either string! = spec/type [
								string-length? form value
							][
								length? form value
							]
							none? value
						][
							report :key 'too-long [uppercase form key " is too long"]
						]

						any [
							not spec/within
							find spec/within value
							none? value
						][
							report :key 'not-valid [uppercase form key " is not an accepted value"]
						]
					]

					packet/(key): either none? value [spec/default][value]
				]

				either empty? errors [packet][
					;if settings/debug [
					;	raise mold errors
					;]

					raise "Record Data Does Not Match Database Spec"
				]
			]
		]

		prepare: func [[catch] record [object!]][
			populate make-packet record/owner record
		]
	]

	; INTERFACE
	add-protocol roughcut 0 context [
		port-flags: system/standard/port-flags/pass-thru

		init: func [table spec [url!]][
			unless table/locals: make table! table/locals [
				raise ["Could not load model <" table/locals/name ">"]
			]

			with table/locals [
				root: path: (any [table/locals/header/home dirize space/:name])
				port: (table)
				changed: (modified? space/:name/index.r)
			]
		]

		open: func [[catch] table [port!]][
			update table

			with table [
				with state [
					index: 0
					flags: flags or port-flags
				]
			]
		]

		select: func [
			[catch] table [port!] criteria
			part aperture only? case? any? with* wild skip* offset
			/local statement result record
		][
			case [
				criteria = 'new [
					create-record table
				]

				block? criteria [
					either all [
						word? sw*/pick criteria 1
						statement: get in table/locals/queries criteria/1
					][
						result: throw-on-error [query-db/named attempt [join envelop statement next criteria]]
						collect [foreach record result [keep load-record table record]]
					][
						throw-on-error [raise "Not a Valid Query"]
					]
				]

				criteria [
					all [
						result: query-db/named/first [
							"SELECT * FROM ? WHERE id = ?" to word! table/locals/name criteria
						]
						load-record table result
					]
				]
			]
		]

		find: func [table [port!] id /local index][
			if all [
				id
				query-db/first ["SELECT id FROM ? WHERE id = ?" to word! table/locals/name form id]
			][id]
		]

		pick: func [table [port!] /local record][
			all [
				record: query-db/named/first [
					"SELECT * FROM ? LIMIT ?,1" to word! table/locals/name 1 + table/state/index
				]
				load-record table record
			]
		]

		insert: func [table [port!] record [object!] /local id root][
			unless case/all [
				not record? record [raise "Not a RoughCut Active Record"]
				not id: record/get 'id [raise "Active Record needs an ID"]
			][
				record/on-save

				return if id: insert-db table prepare record [
					record/new?: false
					record/set 'id record/id: id
					attempt [
						record/root: record/path: table/locals/root/(table/locals/locate id)
					]
					update table
				]
			]
		]

		change: func [
			table [port!] 
			record [object!]
		][
			unless case/all [
				not record? record [raise "Not a RoughCut Active Record"]
				record/unique? [raise "Active Record ID not found"]
			][
				record/on-change
				record/on-save

				if update-db table record/id prepare record [table]
			]
		]

		update: func [table [port!] /local][
			local: query-db/first ["SELECT COUNT(*) AS length FROM ?" table/locals/name]

			unless all [
				block? local
				parse local [set local integer! end (table/state/tail: :local)]
			][raise ["Weird SQL Result:" mold local]]

			unless port? table [
				raise "Table No Longer Port!"
			]

			return table
		]

		copy: func [table [port!]][
			collect [
				foreach record query-db/named [
					"SELECT * FROM ? LIMIT ?,?"
					to word! table/locals/name
					table/state/index
					table/state/num
				][
					keep load-record table record
				]
			]
		]

		close: func [table [port!]][table/locals/index: none]
	]
]

;--## MODELS
;-------------------------------------------------------------------##
context [
	root: wrt://system/models/

	engage-model: func [[catch] /local specs type][
		specs: map read/custom root amend [alpha any file* %.r] func [spec][
			reduce [to set-word! form copy/part spec find spec %.r spec]
		]

		qm/models: qm/db: context compose [
			(specs)
			; (each [name file] specs [name: to set-word! name])
			database: all [
				settings/database
				any [
					attempt [open settings/database]
					attempt [open settings/database]
					throw-on-error [open settings/database]
				]
			]
		]

		throw-on-error [
			each [name spec] specs [
				spec: bind load/header root/:spec qm/models
				type: get in spec/1 'type
				spec: compose [
					name: (to lit-word! name)
					header: (spec)
				]
				spec: switch type?/word type [
					word! [
						spec: compose/only [scheme: (to lit-word! type) locals: (spec)]
						if error? spec: try [open spec][
							raise ["Error opening Model: %models/" name ".r <" type "::" name ">"]
						]
						spec
					]
					none! [context bind spec qm/models]
				]
				set in qm/models name spec
			]
		]

		qm/models
	]

	disengage-model: does [
		foreach spec words-of qm/models [
			if port? spec: get :spec [close spec]
		]
	]

	export [engage-model disengage-model]
]

;--## CONSOLE SESSION STOPS HERE
;-------------------------------------------------------------------##
unless system/options/cgi/request-method [
	all [system/product = 'base know %core/help.r]
]

;--## VIEW
;-------------------------------------------------------------------##
context [
	status-codes: [
		200 "OK" 201 "Created" 204 "No Content"
		301 "Moved Permanently" 302 "Moved temporarily" 303 "See Other" 307 "Temporary Redirect"
		400 "Bad Request" 401 "No Authorization" 403 "Forbidden" 404 "Not Found" 411 "Length Required"
		500 "Internal Server Error" 503 "Service Unavailable"
	]

	send-response: func [body [string! binary!]][
		system/ports/output/state/with: "^/"
		; body: join body ["<div><b>" difference now/precise st "</b></div>"]
		write-io system/ports/output body length? body
		close system/ports/output
		body
	]

	if qm/cheyenne? [
		send-response: func [body [string! binary!]][
			prin to string! body
		]
	]

	publish: func [response [object!] /local yield][
		with response [
			either yield: body: case [
				any [none? body empty? body][" "]
				string? body [render body]
				file? body [render body]
				binary? body [body]
			][
				body: any [
					all [
						find [text/html application/xhtml+xml] reduce [type]
						any [file? template string? template]
						render/with template [yield]
					]
					yield
				]
			][
				status: 404
				body: switch/default type [
					text/plain ["Not Found"]
				][any [render %errors/notfound.rsp "Not Found."]]
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

				; should be response/deflate? or something
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

	export [publish]
]

;--## CONTROLLER
;-------------------------------------------------------------------##
context [
	root: wrt://system/controllers/

	rendered?: #[false]

	redirect-to: func [
		[catch]
		'url [file! url! path! paren! none!]
		/back /status response-code [integer!]
		/format extension [file!]
		/content body [file! string! url! binary! none!]
	][
		if rendered? [raise "Already Rendered!"]
		if paren? url compose [url: (url)]

		qm/response/status: any [response-code 303]
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
		/only
		/local path
	][
		if rendered? [raise "Already Rendered!"]

		qm/response/body: :body

		case/all [
			only [qm/response/body: to binary! body]
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

		rendered?: true
	]

	reject: func [status [integer!] body [file! string!]][
		render/status/template :body :status none
	]

	print: func [value][render/as/template to binary! reform value text/plain none]

	format: does [
		any with/only qm [
			response/format
			request/format
			%.html
		]
	]

	aspect: does [
		any with/only qm [
			response/aspect
			request/aspect
		]
	]

	submit: func [record [object!] 'name [word!] values [any-type!]][
		record/submit :name values
	]

	where: func [condition [file!] then [block!] /else otherwise [block!]][
		either any [
			condition = format
			condition = aspect
		] :then any [:otherwise [true]]
	]

	load-controller: use [ctrl route event words][
		route: use [name route options path method where action][
			route: context options: [
				view: verify: none
				actions: reduce [
					"get" make block! 4
					"post" make block! 2
					"put" make block! 2
					"delete" make block! 2
					"head" make block! 0
				]
			]

			with/only route [
				'route (with route options)
				set path paren! 'to set view [file! | word!] (view: to file! view) into [
					some [
						(method: where: action: none)
						set method ['get | 'post | 'put | 'delete | 'head]
						copy where any file! set action block!
						(
							append route/actions/(form method) new-line compose/deep [
								(any [where 'default]) [(action)]
							] true
						)
						| ['verify | 'assert | 'assert-all] set verify block!
					]
				] (repend ctrl/routes [path make route []])
			]
		]
	
		event: use [event code][
			[
				'event set event ["before" | "after" | "finish"] 'does set code block! (
					event: to word! event
					any [block? ctrl/(event) ctrl/(event): code]
				)
			]
		]

		words: use [value mk][
			[
				any [
					  set value set-word! (insert ctrl/locals value)
					| mk: [block! | paren!] :mk into words
					| skip
				]
			]
		]

		func [[catch] name /local file values][
			ctrl: context [
				name: header: none routes: copy [] before: after: none
				locals: copy [none]
			]

			unless all [
				(all [ctrl/name: name name <> ""])
				(exists? file: join root [name %.r])
				(block? values: load/header file)
				('controller = get in ctrl/header: take values 'type)
			][
				return none
			]

			either parse/all values [
				some [route | event | 'comment skip]
			][
				new-line/all/skip ctrl/routes true 2
			][
				name: rejoin ["Invalid Controller Spec: %controllers/" name ".r"]
				raise :name
			]

			parse values [words (ctrl/locals: unique ctrl/locals)]

			ctrl
		]
	]

	route: func [request [object!] response [object!] /local route action file args code] with/only qm [
		unless controller: any [
			request/controller
			settings/default-controller
		][
			render/status %errors/notfound.rsp 404
			exit
		]

		unless controller: load-controller controller [
			; render "No Controller<br/><pre>"
			render/status %errors/notfound.rsp 404
			exit
		]

		response/template: get in controller/header 'template

		route: foreach [path action] controller/routes [
			args: request/path-info
			path: to block! path

			case/all [
				string? path/1 [
					args: either path/1 = args/1 [next args][none]
					path: next path
				]
				args: import/block args to block! path [
					break/return action
				]
			]
		]

		unless all [
			object? route
			action: select route/actions request/action
			parse action compose/deep [
				[thru request/aspect | thru request/format | thru 'default]
				to block! set code block! to end
			]
		][
			response/template: none
			render/status %errors/notfound.rsp 404
			exit
		]

		view-path: dirize to file! controller/name

		qm: make qm context with/only qm with/only 'root collect [
			comment {header}
			keep [header:]
			keep controller/header
			comment {/header}

			comment {words}
			keep controller/locals
			comment {/words}

			comment {models}
			keep body-of models
			comment {/models}

			keep [title:]
			keep rejoin [uppercase/part form controller/name 1 " :: " uppercase/part form route/view 1]
			keep load wrt://system/events/startup.r
			keep controller/before

			foreach [word value] args [keep to set-word! word keep value]
			foreach [word] any [get in controller/header 'locals []][
				keep to set-word! word
			]
			keep none

			keep either block? route/verify [
				compose/only [if verify (route/verify) (code)]
			][code]

			keep controller/after
			keep load wrt://system/events/close-models.r
		]

		qm/binding: in qm 'self

		unless rendered? [
			switch request/format [
				%.r %.txt [response/type: 'text/plain]
			]

			render rejoin [
				any [response/body route/view]
				any [aspect ""]
				format handler
			]
		]

		rendered?
	]

	protect [root redirect-to render reject print format aspect where load-controller route]

	export [route]
]

;--## ENGAGE
;-------------------------------------------------------------------##
if qm/live? [
	qm/request: make system/options/cgi [
		cgi?: false

		controller: action: input: target: format: aspect: none
		remote-addr: as tuple! remote-addr
		query: []

		get-header: func [name][select other-headers form name]

		clear find request-path: copy request-uri: as file! any [
			get-header "HTTP_INTERNAL_REFERER" ; Cheyenne
			get-env "REQUEST_URI"
			"/"
		] "?"

		path-info: parse/all remove copy request-path "/"

		if target: pick path-info length? path-info [
			use [chars name part][
				name: amend [1 20 symbol]
				chars: complement charset ".,"

				parse/all target [
					any chars target: any [
						  copy part ["." name] (format: part)
						| copy part ["," name] (aspect: part)
					][
						end (
							format: all [format to file! format]
							aspect: all [aspect to file! aspect]
						) | (format: aspect: none)
					]
				]

				target: head clear target
			]
		]

		controller: take path-info

		map cookies: any [
			all [
				cookies: select other-headers "HTTP_COOKIE"
				parse cookies ";="
			]
			[]
		] :url-decode

		; Must fix 'decode-options
		; accept-types: decode-options select other-headers "HTTP_ACCEPT" path!
		; accept-languages: decode-options select other-headers "HTTP_ACCEPT_LANGUAGE" word!
		; accept-encodings: decode-options select other-headers "HTTP_ACCEPT_ENCODING" word!
		; accept-charsets: decode-options select other-headers "HTTP_ACCEPT_CHARSET" word!

		case/all [
			"get" = action: request-method [
				query: load-webform query-string
			]

			"post" = action [
				if find ["put" "delete"] query-string [action: query-string]
				; unless empty? query-string [action: query-string]
			]
		]

		action: form as word! lowercase action

		content-limit: settings/post-limit
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
						  opt "x-" "rebol" (type: %.r)
						| "xml" (type: %.xml)
						| opt "x-" "json" (type: %.json)
						| "html" (type: %.html)
					]
					| "application/" [
						  "x-www-form-urlencoded" (type: %.url)
						| "xml" (type: %.xml)
					]
					| "multipart/form-data;" "boundary=" content-boundary: some chars (
						type: %.formdata
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

		if qm/cheyenne? [
			input: get in system/words 'input

			body: func [/binary][
			 	body: either all [
					body: input
					content-length = length? body
				][body][#{}]
				either binary [body][to string! body]
			]
		]

		;-- content is the body 'loaded' according to the content-type header
		content: func [[catch]][
			throw-on-error [
				content: switch type [
					%.url [load-webform body]
					%.r [attempt [load/header body]]
					%.formdata [
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

	qm/response: context [
		status: 200
		headers: make string! ""
		type: 'text/html
		charset: "utf-8"
		scripts: []

		template: length: body: log: view: format: aspect: none

		set-header: func [header value][
			repend headers [header ": " value newline]
		]

		clear-cookie: func [key /path root /domain base][
			key: join form key "=; expires=Thu, 01 Jan 1970 00:00:00 GMT"
			repend key ["; path=" any [root %/]]
			if domain [repend key ["; domain=" base]]
			set-header 'Set-Cookie key
		]

		set-cookie: func [key value /expires on [date!] /path root [file!] /domain base [string!] /open][
			value: rejoin collect [
				keep [form key "=" url-encode value]
				if expires [keep form-date/gmt on "; expires=%a, %d %b %Y %T GMT"]
				keep ["; path=" any [root %/]]
				if domain [keep ["; domain=" base]]
				unless open [keep "; HttpOnly"]
			]
			set-header 'Set-Cookie value
		]

		export [set-header set-cookie clear-cookie]
	]

	try-else [
		with qm [
			export [probe]
			protect 'request
			protect 'response
			engage-model
			route request response
			disengage-model
			publish response
		]
	][
		; print "Content-Type: text/plain^/"
		reason: make disarm reason []

		reason-type: system/error/(reason/type)/type
		reason-message: reform bind envelop system/error/(reason/type)/(reason/id) reason
		reason: rejoin [
			"** " reason-type ": " reason-message
			"^/** Where: " mold get in reason 'where
			"^/** Near: " mold get in reason 'near
		]

		trims: func [string [string!]][
			trim/tail trim/auto string
		]

		qm/binding: 'reason
		qm/handler: %.rsp

		with qm/response [
			status: 500
			type: 'text/html
			template: none
			body: trims settings/exceptions
		]

		publish qm/response

		attempt [
			save to url! form-date/gmt now "wrt://space/crash/err%Y%m%d-%H%M%S.txt" reduce [
				now qm/request/remote-addr
				join qm/request/server-name form qm/request/request-uri
				select qm/request/other-headers "HTTP_USER_AGENT"
				reason
			]
		]
	]
]