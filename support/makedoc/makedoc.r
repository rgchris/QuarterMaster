REBOL [
	Title: "Makedoc"
	Date: 26-Oct-2011
	Author: ["Gabriele Santilli" "Christopher Ross-Gill"]
	License: %license.r ; fsm!
	Type: 'module
	Root: wrt://system/makedoc/
	Exports: [
		load-doc make-doc load-para make-para
	]
]

load-scanpara: use [para!][
	para!: context amend [
		para: copy []
		emit: use [prev][
			func [data][
				prev: pick back tail para 1
				case [
					not string? data [append/only para data]
					not string? prev [append para data]
					true [append prev data para]
				]
			]
		]

		text: char: values: none

		in-word?: false
		in-word: [(in-word?: true)]
		not-in-word: [(in-word?: false)]

		string: use [mk ex][
			[
				mk: {"} (
					either error? try [
						mk: load/next ex: mk
					][
						values: "="
					][
						ex: mk/2
						values: reduce ['wiki mk/1]
					]
				) :ex
			]
		]

		block: use [mk ex][
			[
				mk: #"[" (
					either error? try [
						mk: load/next ex: mk
					][
						ex
						values: "="
					][
						ex: mk/2
						values: mk/1
					]
				) :ex ; ]
			]
		]

		rule: none

		scanpara: func [paragraph [string!]][
			clear para
			parse/all paragraph rule
			new-line/all para false
			; probe para
			copy para
		]
	]

	func [scanpara [file!]][
		if all [
			exists? scanpara: header/root/(scanpara)
			scanpara: load/header scanpara
			'paragraph = get in take scanpara 'type
		][
			make para! compose/only [rule: (amend scanpara)]
		]
	]
]

load-scanner: use [para! scanner!][
	scanner!: context amend [
		doc: []
		emit: func ['style data /verbatim][
			if string? data [
				trim/tail data
				unless verbatim [data: inline/scanpara data]
				; unless verbatim [data: envelop data]
			]
			repend doc [style data]
		]

		inline: text: para: values: url-mark: none
		term: [any space [newline | end]]
		trim-each: [(foreach val values [trim/head/tail val])]
		options: []

		line:       [any space copy text text-row term (trim/head/tail text)]
		paragraph:  [copy para [text-row any [newline text-row]] term]
		lines:      [any space paragraph]
		indented:   [some space opt text-row]
		example:    [
			copy para some [indented | some newline indented]
			(para: trim/auto para)
		]
		define:     [copy text to " -" 2 skip [newline | any space] paragraph]
		commas:     [line (values: parse/all text ",") trim-each]
		pipes:      [line (values: parse/all text "|") trim-each]
		block:      [term (values: copy []) | line (values: any [attempt [load/all text] copy []])]
		url-start:  [url-mark: "http" opt "s" "://" opt "www."]
		url-block:  [:url-mark line (values: any [attempt [load/all text] copy []])]

		rules: none
	
		scandoc: func [document [string!]][
			clear doc
			emit options options
			parse/all document rules
			new-line/skip/all doc true 2
			doc
		]
	]

	func [scandoc [file!] scanpara [file!]][
		if all [
			exists? scandoc: header/root/(scandoc)
			scandoc: load/header scandoc
			'document = get in take scandoc 'type
		][
			scandoc: make scanner! compose/only [rules: (scandoc)]
			if scandoc/inline: load-scanpara scanpara [
				scandoc
			]
		]
	]
]

fsm!: context [
	initial: state: none
	state-stack: [ ]

	goto-state: func [new-state [block!] retact [paren! none!]] [
		insert/only insert/only state-stack: tail state-stack :state :retact
		state: new-state
	]

	return-state: has [retact [paren! none!]] [
		set [state retact] state-stack
		state: any [state initial]
		do retact
		state-stack: skip clear state-stack -2
	]

	rewind-state: func [up-to [block!] /local retact stack] [
		if empty? state-stack [return false]
		stack: tail state-stack
		retact: make block! 128
		until [
			stack: skip stack -2
			append retact stack/2
			if same? up-to stack/1 [
				state: up-to
				do retact
				state-stack: skip clear stack -2
				return true
			]
			head? stack
		]
		false
	]

	event: func [evt /local val ovr retact done?] [
		if not block? state [exit]
		until [
			done?: yes
			local: any [
				find state evt
				find state to-get-word type?/word evt
				find state [default:]
			]
			if local [
				parse local [
					any [any-string! | set-word! | get-word!]
					set val opt paren! (do val) [
						'continue (done?: no)
						|
						'override set ovr word! (evt: to set-word! ovr done?: no)
						|
						none
					] [
						'return (return-state)
						|
						'rewind? copy val some word! (
							if not foreach word val [
								if block? get/any word [
									if rewind-state get word [break/return true]
								]
								false
							] [
								done?: yes
							]
						)
						|
						set val word! set retact opt paren! (
							either block? get/any val [goto-state get val :retact][
								done?: yes
							]
						)
						|
						none (done?: yes)
					]
				]
			]
			done?
		]
	]

	init: func [initial-state [word! block!]] [
		; _t_ "fsm_init"
		if word? initial-state [
			unless block? initial-state: get/any :initial-state [
				make error! "Not a valid state"
			]
		]
		clear state-stack: head state-stack
		initial: state: initial-state
	]

	end: does [
		; _t_ "fsm_end"
		foreach [retact state] head reverse head state-stack [do retact]
	]
]

load-emitter: use [emitter! para!][
	emitter!: context [
		sections: context [
			this: 0.0.0.0
			reset: does [this: 0.0.0.0]
			step: func [level /local bump mask] [
				set [bump mask] pick [
					[1.0.0.0 1.0.0.0]
					[0.1.0.0 1.1.0.0]
					[0.0.1.0 1.1.1.0]
					[0.0.0.1 1.1.1.1]
				] level
				level: form this: this + bump * mask
				clear find level ".0"
				level
			]
		]

		outline: func [doc [block!]][
			remove-each style copy doc [
				not find [sect1 sect2 sect3 sect4] style
			]
		]

		init-emitter: func [doc] [
			sections/reset

			foreach [word str] doc [
				if w: find [sect1 sect2 sect3 sect4] word [
					w: index? w
					if w <= toc-levels [
						sn: sections/step w
						insert insert tail toc capture [make-heading/toc w sn copy/deep str] "<br>^/"
					]
				]
			]

			sections/reset

			if no-title [emit toc state: normal]
		]

		toc: none

		initialize: func [para [block!]][
			if string? pick para 1 [
				insert para reduce [<initial> take pick para 1 </initial>]
			]
			para
		]

		no-indent: true
		no-nums: true
		make-heading: func [level num str /toc /local lnk][
			lnk: replace/all join "section-" num "." "-"
			num: either no-nums [""] [join num pick [". " " "] level = 1]
			either toc [
				emit [{<a class="toc} level {" href="#} lnk {">}] emit-inline str emit [</a> newline]
			][
				emit [{<h} level { id="} lnk {">}] emit-inline str emit [{</h} level {>}]
			]
		]

		emit-sect: func [level str /local sn] [
			sn: sections/step level
			make-heading level sn str
		]

		hold-values: []
		hold: func [value [any-type!]][insert hold-values value value]
		release: does [take hold-values]

		out: {}
		emit: func [value][
			insert tail out reduce value
		]

		states: data: word: value: options: none

		inline: make fsm! []

		emit-inline: func [para [block!] /with state [word! block!]] [
			unless block? state [
				state: get in states any [:state 'inline]
			]
			inline/init state
			foreach part para [
				set 'value part
				inline/event value
			]
			inline/end
		]

		raise: func [msg] [emit ["Emitter error: " msg]]

		escape-html: :sanitize

		inherit: func [parent-state new-directives][
			append new-directives parent-state
		]
	
		raise: func [msg][
			emit compose [{<ul class="attention"><li>Document error: } (msg) {</li></ul>}]
		]

		outline: make fsm! []

		outline-do: func [doc [block!] state [block!]][
			outline/init state
			forskip doc 2 [
				set [word data] doc
				outline/event to set-word! word
			]
			outline/end
		]

		generate: func [doc [block!]] [
			clear hold-values
			clear out
			sections/reset
			outline-do doc get in states 'initial
			copy out
		]
	]

	func [makedoc [file!]][
		if all [
			exists? makedoc: header/root/(makedoc)
			makedoc: load/header makedoc
			'emitter = get in take makedoc 'type
		][
			makedoc: make emitter! compose/only [states: context (makedoc)]
		]
	]
]

grammar!: context [
	document: %article.r
	paragraph: %paragraph.r
	markup: %html.r
]

make-doc: func [
	document [string! block!]
	/custom options [block! object!]
	/local scanner emitter
][
	options: make grammar! any [options []]
	case/all [
		string? document [
			; _t_ "md_sc"
			if scanner: load-scanner options/document options/paragraph [
				document: scanner/scandoc document
			]
		]
		block? document [
			; _t_ "md_em"
			if emitter: load-emitter options/markup [
				emitter/generate document
			]
		]
	]
]

load-doc: use [document!][
	document!: context [
		options: none
		text: none
		document: none
		outline: func [/level depth [integer!]][
			level: copy/part [sect1 sect2 sect3 sect4] min 1 max 4 any [depth 2]
			remove-each [style para] copy document [
				not find level style
			]
		]
		render: does [
			make-doc/custom document options
		]
	]

	func [
		[catch] document [string!]
		/with model [none! block! object!]
		/custom options [none! block! object!]
		/local doc [none!]
	][
		options: make grammar! any [options []]
		model: make document! any [model []]
		model/options: options
		model/text: :document

		if doc: load-scanner options/document options/paragraph [
			model/document: doc/scandoc document
			model
		]
	]
]
