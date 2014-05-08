Rebol [
	Title: "MakeDoc"
	Date: 7-May-2014
	Author: [
		"Gabriele Santilli" "Finite State Machine"
		"Christopher Ross-Gill" "Module Structure"
	]
	License: [
		"Finite State Machine"
		http://www.colellachiara.com/soft/MD3/fsm.html
	]
	Type: 'module
	Root: wrt://system/makedoc/
	Version: 3.2.2 ; CRG Version
	Exports: [load-doc make-doc form-para]
]

make-doc: context [
	root: system/script/header/root

	load-next: func [string [string!]][
		load/next string
	]

	load-scanpara: use [para!][
		para!: context amend [
			para: copy []
			emit: use [prev][
				func [data /after alt][
					all [after in-word? data: alt]
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

			string: use [mk ex then][
				[
					mk: {"} (
						either error? try [
							mk: load-next ex: mk
						][
							then: [end skip]
						][
							ex: mk/2
							then: [:ex]
							values: reduce ['wiki mk/1]
						]
					) then
				]
			]

			block: use [mk ex then][
				[
					mk: #"[" (
						either error? try [
							mk: load-next ex: mk
						][
							then: [end skip]
						][
							ex: mk/2
							values: mk/1
							then: [:ex]
						]
					) then ; ]
				]
			]

			paren: use [mk ex then][
				[
					mk: #"(" (
						either error? try [
							mk: load-next ex: mk
						][
							then: [end skip]
						][
							ex: mk/2
							values: mk/1
							then: [:ex]
						]
					) then ; )
				]
			]

			rule: none

			scanpara: func [paragraph [string!]][
				clear para
				parse/all paragraph rule
				new-line/all para false
				copy para
			]
		]

		load-scanpara: func [scanpara [file! url!]][
			if all [
				scanpara: attempt [read scanpara]
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

		load-scanner: func [scandoc [file! url!] scanpara [file! url!]][
			if all [
				scandoc: attempt [read scandoc]
				scandoc: load/header scandoc
				'document = get in take scandoc 'type
			][
				scandoc: make scanner! compose/only [rules: (amend scandoc)]
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
							some ['return (return-state)]
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
			document: position: word: data: none

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

			flags: [
				flags: []
				get: func [key [word!]][
					foreach [k value] flags [if k = key [break/return value]]
				]
				unset: func [key [word!]][
					remove-each [k value] flags [k = key]
				]
				set: func [key [word!] value [any-type!]][
					unset key repend flags [key value] value
				]
			]

			; init-emitter: func [doc] [
			; 	sections/reset
			; 
			; 	foreach [word str] doc [
			; 		if w: find [sect1 sect2 sect3 sect4] word [
			; 			w: index? w
			; 			if w <= toc-levels [
			; 				sn: sections/step w
			; 				insert insert tail toc capture [make-heading/toc w sn copy/deep str] "<br>^/"
			; 			]
			; 		]
			; 	]
			; 
			; 	sections/reset
			; 
			; 	if no-title [emit toc state: normal]
			; ]

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

			form-url: func [url [url!]][
				if parse url: form url amend [
					copy url some [
						some ascii
						| url: extended (change/part url join "%" enbase/base form url/1 16 1)
						| skip
					]
				][url]
			]

			stack: []
			hold: func [value [any-type!]][insert stack value value]
			release: does [take stack]

			cursor: context [
				stack: []

				here: out: {}
				reset: does [clear stack here: out: copy {}]
				mark: does [insert stack here here: copy {}]
				unmark: does [here: insert take stack head here]
				close: does [while [not empty? stack][unmark] copy out]
			]

			emit: func [value /at-mark][
				either at-mark [
					cursor/stack/1: insert cursor/stack/1 reduce value
				][
					cursor/here: insert cursor/here reduce value
				]
			]

			states: data: word: value: options: none

			inline: make fsm! []

			emit-inline: func [
				para [block!]
				/with state [word! block!]
				/local doc-position
			][
				doc-position: :position
				unless block? state [
					state: get in states any [:state 'inline]
				]
				inline/init state
				forall para [
					position: :para
					set 'value para/1
					inline/event :value
				]
				position: :doc-position
				inline/end
			]

			raise: func [msg] [emit ["Emitter error: " msg]]

			escape-html: :sanitize

			inherit: func [parent-state new-directives][
				append new-directives parent-state
			]

			raise: func [msg][
				emit compose [{<ul class="attention"><li>} (msg) {</li></ul>}]
			]

			outline: make fsm! []

			generate: func [doc [block!]] [
				clear stack
				cursor/reset
				sections/reset
				outline/init get in states 'initial
				forskip doc 2 [
					position: doc
					set [word data] doc
					outline/event to set-word! word
				]
				outline/end
				cursor/close
			]
		]

		load-emitter: func [makedoc [file! url!]][
			if all [
				makedoc: attempt [read makedoc]
				makedoc: load/header makedoc
				'emitter = get in take makedoc 'type
			][
				makedoc: make emitter! compose/only [
					flags: context flags
					states: context (makedoc)
				]
			]
		]
	]

	grammar!: context [
		root: none
		template: none
		document: %document.r
		paragraph: %paragraph.r
		markup: %html.r
		model: none
	]

	resolve: use [resolve-path][
		resolve-path: func [root [file! url!] target [none! file! url!]][
			case [
				none? target [target]
				url? target [target]
				url? root [root/:target]
				find/match target root [target]
				target [root/:target]
			]
		]

		resolve: func [options [object!]][
			options/root: any [options/root root]
			options/document: resolve-path options/root options/document
			options/paragraph: resolve-path options/root options/paragraph
			options/markup: resolve-path options/root options/markup
			if any [file? options/template url? options/template][
				options/template: resolve-path options/root options/template
			]
			if any [file? options/model url? options/model][
				options/model: resolve-path options/root options/model
			]
			options
		]
	]

	load-doc: use [document!][
		document!: context [
			options: source: text: document: values: none

			render: func [/custom options [block! object! none!]][
				make-doc/custom self make self/options any [options []]
			]

			title: has [title][
				if parse document [opt ['options skip] 'para set title block! to end][
					form-para title
				]
			]

			outline: func [/level depth [integer!]][
				level: copy/part [sect1 sect2 sect3 sect4] max 1 min 4 any [depth 2]
				remove-each [style para] copy document [
					not find level style
				]
			]

			boiler: has [boiler][
				collect [
					if parse document [opt ['options skip] 'para skip 'code set boiler string! to end][
						foreach para parse/all boiler "^/" [
							keep form-para para
						]
					]
				]
			]
		]

		load-doc: func [
			[catch] document [file! url! string! binary! block!]
			/with model [none! block! object!]
			/custom options [none! block! object!]
			/local scanner
		][
			options: make grammar! any [options []]
			resolve options

			model: make document! any [
				model
				switch type?/word options/model [
					object! block! [model]
					file! url! [attempt [load options/model]]
				]
				[]
			]

			model/options: options
			model/values: copy []

			case/all [
				any [file? document url? document][
					model/source: document
					document: any [read document ""]
				]
				binary? document [
					document: to string! document
				]
				string? document [
					model/text: document
					if scanner: load-scanner options/document options/paragraph [
						document: scanner/scandoc document
					]
				]
				block? document [
					model/document: :document
					model
				]
			]
		]
	]

	make-doc: func [
		document [file! url! string! binary! block! object!]
		/with model [block! object!]
		/custom options [block! object!]
		/local template emitter
	][
		options: make grammar! any [options []]
		resolve options

		unless object? document [
			document: load-doc/with/custom document model options
		]

		if object? document [
			case [
				all [
					template: options/template
					template: case/all [
						file? template [
							template: attempt [read template]
						]
						url? template [
							template: attempt [read template]
						]
						binary? template [
							template: to string! template
						]
						string? template [template]
					]
				][
					document/options/template: none
					render/with template [document]
				]

				emitter: load-emitter options/markup [
					emitter/document: document
					emitter/generate document/document
				]
			]
		]
	]

	form-para: use [encode-utf8][
		encode-utf8: func [
			"Encode a code point in UTF-8 format" 
			char [integer!] "Unicode code point"
		][
			if char <= 127 [
				return as-string to binary! reduce [char]
			] 
			if char <= 2047 [
				return as-string to binary! reduce [
					char and 1984 / 64 + 192 
					char and 63 + 128
				]
			] 
			if char <= 65535 [
				return as-string to binary! reduce [
					char and 61440 / 4096 + 224 
					char and 4032 / 64 + 128 
					char and 63 + 128
				]
			] 
			if char > 2097151 [return ""] 
			as-string to binary! reduce [
				char and 1835008 / 262144 + 240 
				char and 258048 / 4096 + 128 
				char and 4032 / 64 + 128 
				char and 63 + 128
			]
		]

		func [paragraph [block! string!] /local pos][
			join "" collect [
				foreach part envelop paragraph [
					case [
						string? part [keep part]
						integer? part [keep encode-utf8 part]
						switch part [
							<quot> [part: #{E2809C}]
							</quot> [part: #{E2809D}]
							<apos> [part: #{E28098}]
							</apos> [part: #{E28099}]
							<sb> [part: "["]
							<sb> [part: "]"]
						][
							keep to string! part
						]
						char? part [keep part]
					]
				]
			]
		]
	]
]

form-para: get in make-doc 'form-para
load-doc: get in make-doc 'load-doc
make-doc: get in make-doc 'make-doc