REBOL [
	Title: "Schema Handler for MySQL"
	Date:  5-Aug-2012
	Author: "Christopher Ross-Gill"
	Type: 'module
	Exports: [
		load-schema
		analyse-schema
		apply-schema
	]
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

assert-all: func [[throw] cases [block!] /local value][
	until [
		set [value cases] do/next cases
		unless value cases/1
		cases: next cases
		any [not value tail? cases]
	]
	any [value]
]

load-schema: use [parse-schema result database table field view queries to-parse-set][
	to-parse-set: func [block [block!]][
		block: copy block
		remove head forskip block 2 [insert block '|]
	]

	result: context [
		out: copy []
		position: database: table: view: field: query: none

		add: func [name [set-word!] 'type [word!]][
			name: to-word name

			switch type compose/deep [
				database [
					position: form name
					append out name
					append out database: context [
						name: (to-lit-word name)
						tables: copy []
						views: copy []
						options: copy []
					]
					database
				]
				table [
					position: rejoin [form database/name "/" name]
					append database/tables name
					append database/tables table: context [
						name: (to-lit-word name)
						fields: copy []
						options: copy []
						primary: none
						indices: copy []
						uniquity: copy []
						parent: :database
						increment: none
					]
					table
				]
				field [
					position: rejoin [form database/name "/" table/name "/" name]
					append table/fields name
					append table/fields field: context [
						name: (to-lit-word name)
						type: rel: width: required: default: increment: none
						parent: :table
					]
					field
				]
				view [
					position: rejoin [form database/name "/" name]
					append database/views name
					append database/views view: context [
						name: (to-lit-word name)
						tables: copy []
						fields: copy []
						expressions: none
						order: direction: none
						parent: :database
					]
					view
				]
			]
		]

		return: has [rel][
			foreach [name database] out [
				foreach [name table] database/tables [
					foreach [name field] table/fields [
						if rel: field/rel [
							unless rel: all [
								find out/(database/name)/tables rel/1
								find out/(database/name)/tables/(rel/1)/fields rel/2
								out/(database/name)/tables/(rel/1)/fields/(rel/2)
							][throw make error! join "Could Not Find Relation: " [field/rel/1 "/" field/rel/2]]
							field/type: rel/type
							field/width: rel/width
						]
					]
				]
			]

			out
		]

		reset: does [
			out: clear out
			database: table: view: field: query: none
			position: "!START!"
			self
		]
	]

	database: use [name rule this][
		rule: [
			into [some [table | view]]
		]

		database: [
			set name set-word! 'database
			(bind rule this: result/add name database)
			rule
		]
	]

	table: use [name rule this][
		rule: [
			into [some field]
		]

		table: [
			set name set-word! 'table
			(bind rule this: result/add name table)
			rule
		]
	]

	field: use [name rule this string binary date logic integer decimal][
		integer: ['integer!]
		decimal: ['decimal!]
		logic: ['logic!]
		string: ['string!]
		binary: ['block! | 'binary! | 'email! | 'url! | 'tuple! | 'issue! | 'money!]
		date: ['date!]

		rule: [
			set required opt 'opt (required: required <> 'opt)
			[
				[
					  set type integer set width integer!
					| set type [decimal | logic (required: true) | date]
					| set type string set width opt [integer! | into [some string!]]
					| set type binary set width opt integer!
				] (type: get :type)
				| set rel [get-word! | into [get-word! word!]] (
					case [
						get-word? rel [rel: to-path reduce [to-word rel 'id]]
						any-block? rel [rel: to-path reduce [to-word rel/1 to-word rel/2]]
					]
				)
			]
			any [
				  'primary (parent/primary: name)
				| 'index (append parent/indices name)
				| 'unique (append parent/uniquity name)
				| 'increment set increment opt integer!
				  (parent/increment: :increment increment: true)
				| 'init set default any-type! (
					unless type = type? default [
						throw make error! rejoin [
							"Default Value Does Not Match Field Type: "
							parent/parent/name "/" parent/name "/" name
						]
					]
				)
			]
		]

		field: [
			set name set-word!
			(bind rule this: result/add name field)
			rule
		]
	]

	view: use [name rule this][
		rule: [into queries]

		view: [
			set name set-word! 'view
			(bind rule this: result/add name view)
			rule
		]
	]

	queries: use [expression rule term table fieldset ops mk ex][
		ops: to-parse-set map ["<>" "<" ">" ">=" "<="] :to-lit-word

		wrap: use [ex][
			[ex: (mk: change/part/only mk to-paren copy/part mk ex ex) :mk]
		]

		expression: [
			mk: 'find into [some string! | some integer!] [word! | into [word! word!]] wrap
			|
			[word! | path!] ['= | ops] [
				  [word! | path!]
				| integer! | decimal! | logic! | string! | date!
				| block! | binary! | email! | url! | tuple! | issue! | money!
			] wrap
			|
			['all | 'any] into [some expression]
		]

		rule: [
			(result/position: rejoin [form result/database/name "/" result/view/name " Query Start"])

			[
				some [
					set term set-word!
					set table word!
					set fieldset ['* | into [some word!]]
					(
						repend tables [term table]
						foreach field envelop fieldset [
							append/only fields to-path new-line/all reduce [term: to-word term field] false
						]
					)
				]
				|
				set table word!
				set fieldset ['* | into [some word!]]
				(
					append tables table
					append fields fieldset
				)
			]

			(result/position: rejoin [form result/database/name "/" result/view/name " Query Clause"])

			'where copy expressions expression

			(result/position: rejoin [form result/database/name "/" result/view/name " Query Order"])

			opt [
				['order | into ['order set direction ['ascending | 'descending]]]
				copy order some [word! | into [word! word!]]
			]
		]

		queries: [
			'select
			(bind rule result/view)
			into rule
		]
	]

	parse-schema: func [
		[catch]
		schema [block!]
		/local table field
	][
		result/reset
		either parse schema [
			object!
			some database
			to end
		][
			result/return
		][
			throw make error! join "Unable to Parse Schema at: " result/position
		]
	]

	load-schema: func [
		[catch]
		schema [file! url! string! block! none!]
	][
		case/all [
			any [file? schema url? schema][
				schema: assert-all [
					exists? schema [
						throw make error! "Unable to Locate Schema"
					]
					attempt [load/header schema][
						throw make error! "Schema does not contain header"
					]
				]
			]
			string? schema [
				unless schema: attempt [load/header schema][
					throw make error! "Schema does not contain header"
				]
			]
			block? schema [
				unless schema: all [
					object? pick schema 1
					in schema/1 'type
					schema/1/type = 'schema
					schema
				][
					throw make error! "Not marked as Schema data"
				]
			]
			not block? schema [throw make error! "Unknown Schema Error"]
		]

		throw-on-error [parse-schema schema]
	]
]
