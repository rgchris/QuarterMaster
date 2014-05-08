Rebol [
	Title: "Color Code"
	Date: 21-Oct-2013
	Author: "Christopher Ross-Gill"
	File: %color-code.r
	Type: 'module
	Exports: [script? load-header color-code]
	Purpose: {
		Colorize source code based on datatype.
		Result is HTML <pre> block.
		Works with R3
		Sample CSS: http://reb4.me/s/rebol.css
	}
	Version: 2.1.1
	History: [
		23-Oct-2009 2.1.0 "First QM Module" "Christopher Ross-Gill"
		29-May-2003 1.0.0 "Fixed deep parse rule bug." "Carl Sassenrath"
	]
]

system/standard/script: make system/standard/script [Type: none]

script?: use [space id mark type][
	space: charset " ^-"
	id: [
		any space mark: 
		any ["[" mark: (mark: back mark) any space]
		copy type ["REBOL" | "Red" opt "/System" | "World" | "Topaz" | "Freebell"]
		any space
		"[" to end
	]

	func [source [string! binary!] /language][
		if all [
			parse/all source [
				some [
					id break |
					(mark: none)
					thru newline opt #"^M"
				]
			]
			mark
		][either language [type][mark]]
	]
]

load-header: func [[catch] source [string! binary!] /local header][
	source: to string! source
	unless header: script? source [make error! "Source does not contain header."]
	header: find next header "["
	unless header: attempt [load/next header][make error! "Header is incomplete."]
	reduce [construct/with header/1 system/standard/script header/2]
]

color-code: use [out emit emit-var emit-header rule value][
	out: none
	emit: func [data][
		data: reduce envelop data until [append out take data empty? data]
	]

	emit-var: func [value start stop /local type out][
		either none? :value [type: "cmt"][
			if path? :value [value: first :value]

			type: either word? :value [
				any [
					all [find [Rebol Red Topaz Freebell World] value "rebol"]
					all [value? :value any-function? get :value "function"]
					all [value? :value datatype? get :value "datatype"]
					"word"
				]
			][
				any [replace to string! type?/word :value "!" ""]
			]
		]

		out: sanitize copy/part start stop

		either type [
			[{<var class="dt-} type {">} out {</var>}]
		][
			out
		]
	]

	rule: use [str new rule hx percent][
		hx: charset "0123456789abcdefABCDEF"
		
		percent: use [dg nm sg sp ex][
			dg: charset "0123456789"
			nm: [dg any [some dg | "'"]]
			sg: charset "-+"
			sp: charset ".,"
			ex: ["E" opt sg some dg]

			[opt sg [nm opt [ex | sp nm opt ex] | sp nm opt ex] "%"]
		]

		rule: [
			some [
				str:
				some [" " | tab] new: (emit copy/part str new) |
				[crlf | newline] (emit "^/") |
				#";" [thru newline | to end] new:
					(emit-var none str new) |
				[#"[" | #"("] (emit first str) rule |
				[#"]" | #")"] (emit first str) break |
				[8 hx | 4 hx | 2 hx] #"h" new:
					(emit-var 0 str new) |
				percent new: (emit-var 0.1 str new) |
				skip (
					set [value new] load/next str
					emit-var :value str new
				) :new
			]
		]

		[
			rule [end | str: to end (emit sanitize str)]
		]
	]

	func [
		[catch] "Return color source code as HTML."
		text [string!] "Source code text"
	][
		out: make binary! 3 * length? text

		unless text: script? detab text [
			make error! "Not a REBOL script."
		]

		unless head? text [
			emit [
				{<var class="dt-preamble">}
				sanitize copy/part head text text
				"</var>"
			] 
		]

		parse/all text [rule]

		insert out {<pre class="code rebol">}
		to string! append out {</pre>}
	]
]
