REBOL [
	Title: "Color REBOL Code in HTML"
	Date: 23-Oct-2009
	File: %color-code.r
	Author: "Carl Sassenrath"
	Type: 'module
	Exports: [color-code]
	Purpose: {
		Colorize source code based on datatype.
		Result is HTML <pre> block.
		Works with R3
		Sample CSS: http://www.ross-gill.com/styles/rebol-code.css
	}
	History: [
		29-May-2003 "Fixed deep parse rule bug."
	]
]

color-code: use [out emit emit-var rule value][
	out: none
	envelop: func [data][either block? data [data][compose [(data)]]]
	emit: func [data][data: reduce envelop data until [append out take data empty? data]]

	emit-var: func [value start stop /local type][
		either none? :value [
			type: either = start/1 #";" ["cmt"]["none"]][
			if path? :value [value: first :value]
			type: either word? :value [
				any [
					all [value? :value any-function? get :value "function"]
					all [value? :value datatype? get :value "datatype"]
				]
			][
				any [replace to-string type?/word :value "!" ""]
			]
		]
		either type [ ; (Done this way so script can color itself.)
			emit ["-[" {-var class="dt-} type {"-} "]-"
			copy/part start stop "-[" "-/var-" "]-"]
		][
			emit copy/part start stop
		]
	]

	rule: use [str new][
		[
			some [
				str:
				some [" " | tab] new: (emit copy/part str new) |
				newline (emit "^/") |
				#";" [thru newline | to end] new:
					(emit-var none str new) |
				[#"[" | #"("] (emit first str) rule |
				[#"]" | #")"] (emit first str) break |
				skip (
					set [value new] load/next str
					emit-var :value str new
				) :new
			]
		]
	]

	func [
		[catch]
		"Return color source code as HTML."
		text [string!] "Source code text"
	][
		out: make binary! 3 * length? text
		set [value text] load/next/header detab text
		emit copy/part head text text
		parse/all text rule
		out: to-string out

		foreach [from to] reduce [ ; (join avoids the pattern)
			"&" "&amp;" "<" "&lt;" ">" "&gt;"
			join "-[" "-" "<" join "-" "]-" ">"
		][
			replace/all out from to
		]

		insert out {<pre class="code rebol">}
		append out {</pre>}
	]
]

;Example: write %color-code.html color-code read %color-code.r
