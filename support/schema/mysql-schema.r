REBOL [
	Title: "MySQL Schema Tools"
	Date:  8-Aug-2012
	Author: "Christopher Ross-Gill"
	Type: 'module
	Exports: [schema-create schema-get]
]

schema-get: use [result new-lines to-key load-list][
	new-lines: func [block [block!] /local pos][
		pos: block
		while [pos: find pos set-word!][
			new-line pos true
			pos: next pos
		]
		block
	]

	load-list: use [
		comma qmark quoted-chars
		quoted-value out current values
	][
		comma: ","
		qmark: "'"

		quoted-chars: complement charset reduce [qmark]

		current: none
		quoted-value: use [chunk][
			[
				qmark (current: copy "")
				any [
					copy chunk some quoted-chars (append current chunk)
					|
					qmark qmark (append current qmark)
				]
				qmark (append out current)
			]
		]

		values: [(out: copy []) quoted-value any [comma quoted-value]]

		func [stream [string!]][
			if parse/all stream values [out]
		]
	]

	to-key: func [key [word! string!]][
		to set-word! replace/all form key "_" "-"
	]

	schema-get: func ['database [word!] /local out][
		database: new-line/all/skip out: reduce [
			to-key :database 'database

			new-line/all/skip map query-db ["SHOW FULL TABLES FROM ?" database] func [table [block!]][
				switch table/2 [
					"BASE TABLE" [
						table: to word! table/1
						reduce [
							to-key table 'table
							new-lines map query-db [
								"SHOW COLUMNS FROM ?" to-path reduce [database table]
							] func [column /local type size][
								; probe column
								remove-each val reduce [
									to-key column/1
									if column/3 = "YES" ['opt]
									if parse/all column/2 amend [
										(size: none)
										  "varchar(" copy size some digit ")"
										  (type: 'string! size: load size)
										| "text" (type: 'string!)
										| "enum(" copy size to ")" skip
										  (type: string! size: load-list size)
										| "int(" copy size some digit ")"
										  (type: integer! size: load size)
										| "float" (type: 'decimal!)
										| "datetime" (type: 'date!)
										| "tinyint(1)" (type: 'logic!)
										| "varbinary(" copy size some digit ")"
										  (type: 'any-type! size: load size)
										| "blob" (type: 'any-type!)
									][
										type
									]
									size
									switch column/4 [
										"PRI" ['primary]
									]
									switch column/6 [
										"auto_increment" ['increment]
									]
									if column/5 [reduce ['init column/5]]
								][none? val]
							]
						]
					]

					"VIEW" [
						table: to word! table/1
						reduce [
							to-key table 'view
							map query-db ["SHOW CREATE VIEW ?" table] func [table /local value current][
								table: old: second table
								parse/all table amend [
									(table: copy [] current: copy "")
									to "VIEW" some [
										  some [" " | ","] (append current " ")
										| "`" copy value word "`" (append current value)
										| "'" copy value word "'" (append current mold value)
										| "." (append current "/")
										| "AS `" copy value word "`" (
											repend current [value ":"]
											append table trim current
											current: copy ""
										)
										| "by " | copy value word (
											unless empty? trim current [append table current]
											current: copy ""
											append current value
										)
										| copy value ["(" | ")" | "=" | "<" | ">"] (append current value)
									]
									(append table trim current)
								]
								; load form 
								new-line/all table true
							]
						]
					]
				]
			] true 3
		] true 3

		out: copy #{}
		save/header out database compose [
			Title: "Detected Database Schema"
			Type: 'schema
			Date: (now/date)
		]
		to-string replace/all out "    " "^-"
	]
]

schema-create: use [result to-key escape form-value listify][
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
			parse/all value: copy value [
				any [
					mk: some safe |
					cut (mk: change/part mk select escapes mk/1 1) :mk
				] end
			]
			value
		]
	]

	listify: func [list [block!] /with comma][
		comma: any [comma ", "]
		press remove collect [
			foreach value list [
				keep comma
				keep form-value value
			]
		]
	]

	form-value: func [value [any-type!]][
		switch/default type?/word value [
			path! [listify/with to-block value "."]
			paren! [listify/with to-block value " "]
			word! [
				either value = '* [form value][
					press ["`" replace/all form value "-" "_" "`"]
				]
			]
			get-word! [form-value get :value]
			string! [press ["'" escape value "'"]]
			integer! decimal! [form value]
			money! [head remove find mold value "$"]
			block! [listify value]

			none! ["NULL"]
			date! [form-date value either value/time [{'%Y-%m-%d %H:%M:%S'}][{'%Y-%m-%d'}]]
			time! [form-time value {'%H:%M:%S'}]
			binary! [press ["'" escape to string! value "'"]]
			logic! [either value [1][0]]
			tuple! pair! tag! issue! email! url! file! block! [
				press ["'" escape mold/all value "'"]
			]
		][
			"[UNSUPPORTED TYPE]"
		]
	]

	result: context [
		out: copy ""
		reset: does [out: copy ""]
		emit: func [data][repend out data]

		depth: 0
		comma: ""
		feed: does [emit "^/"]
		indent: does [emit head insert/dup copy "" "^-" depth]

		open-paren: does [
			depth: depth + 1
			comma: ""
			"("
		]

		close-paren: does [
			depth: depth - 1
			comma: ","
			")"
		]

		add: func [fragment][
			foreach part fragment [
				switch/default type?/word part [
					string! [emit part]
					get-word! [emit form-value get :part]
				][
					switch/default part [
						feed [feed]
						indent [indent]
						open [emit open-paren]
						close [emit close-paren]
						_ [emit " "]
						comma [emit comma]
						next [
							switch depth [
								0 [emit ";^/"]
								1 [emit ",^/"]
								2 [emit ","]
							]
						]
					][make error! join "Don't Know What This Is: " mold part]
				]
			] 
		]

		add-field: func [field][
			collect with/only field [
				add [comma feed indent :name _]
				comma: ","

				emit switch/default to-word type [
					string! [
						case [
							integer? width [
								press ["VARCHAR(" width ")"]
							]
							block? width [
								press ["ENUM" open-paren listify width close-paren]
							]
							true ["TEXT"]
						]
					]
					integer! [press ["INT(" width ")"]]
					decimal! ["FLOAT"]
					date! ["DATETIME"]
					logic! [required: false "TINYINT(1)"]
					url! email! block! tuple! issue! [
						either integer? width [
							press ["VARBINARY(" width ")"]
						]["BLOB"]
					]
					money! [press ["DECIMAL(" width ",2)"]]
				][make error! join "Unknown Type: " type]

				case/all [
					required [emit " NOT NULL"]
					increment [emit " auto_increment"]
					default [emit " DEFAULT " emit form-value default]
				]
			]
		]

		add-expression: use [prepare][
			prepare: func [expression /with wrappers /local ref dv][
				wrappers: any [wrappers ["" "" ""]]
				dv: ""
				press collect [
					parse expression [
						(keep wrappers/1)
						some [
							'all set expression block!
							(keep dv dv: wrappers/2)
							(keep prepare/with expression ["(" " AND " ")"])
							|
							'any set expression block!
							(keep dv dv: wrappers/2)
							(keep prepare/with expression ["(" " OR " ")"])
							|
							into [
								(keep dv dv: wrappers/2)
								'find set expression block! set ref [path! | word!]
								(keep reduce [form-value ref " IN (" listify expression ")"])
								|
								set ref [word! | path!]
								set with word!
								set expression skip
								(
									keep reduce [
										form-value ref
										" " form with " "
										form-value expression
									]
								)
							]
						]
						(keep wrappers/3)
					]
				]
			]

			add-expression: func [expression][
				emit "WHERE "
				parse expression [
					'all set expression block!
					(expression: prepare/with expression ["" " AND " ""])
					|
					'any set expression block!
					(expression: prepare/with expression ["" " OR " ""])
					|
					paren! (expression: prepare expression)
				]
				emit expression
				feed
			]
		]

		add-tables: func [from /comma][
			emit "FROM"
			emit press collect [
				parse from [
					some [
						set from [word! | path!] (
							keep reduce [comma " " form-value from]
							comma: ","
						)
						|
						copy from [set-word! [word! | path!]] (
							keep reduce [comma " " form-value from/2]
							keep reduce [" " form-value to-word from/1]
							comma: ","
						)
					]
				]
			]
			feed
		]
	]

	schema-create: func [schema][
		result/reset

		foreach [name database] schema [
			result/add [
				"##^/## NEW DATABASE^/##^/^/"
				"DROP DATABASE IF EXISTS " :name next
				"CREATE DATABASE IF NOT EXISTS " :name 
				" CHARACTER SET utf8 COLLATE utf8_general_ci" next
				"USE " :name next feed
			]

			foreach [name table] database/tables [
				result/add [
					"DROP TABLE IF EXISTS" _ :name next
					"CREATE TABLE" _ :name _ open
				]

				foreach [name field] table/fields [
					result/add-field field
				]

				if word? table/primary [
					result/add with/only table [
						comma feed indent "PRIMARY KEY (" :primary ")"
					]
				]

				unless empty? table/indices [
					result/add with/only table [
						comma feed indent "INDEX (" :indices ")"
					]
				]

				unless empty? table/uniquity [
					result/add with/only table [
						comma feed indent "UNIQUE (" :uniquity ")"
					]
				]

				result/add [feed close]
				if integer? table/increment [
					result/add with/only table [_ "auto_increment=" :increment]
				]
				result/add [next feed]
			]

			foreach [name view] database/views [
				result/add with/only view [
					"DROP VIEW IF EXISTS" _ :name next
					"CREATE VIEW" _ :name _ "AS" feed
					"SELECT" _ :fields feed
				]

				result/add-tables view/tables

				result/add-expression view/expressions

				if view/order with/only view [
					result/add [
						"ORDER BY" _ :order
					]
					switch view/direction [
						ascending [result/emit " ASC"]
						descending [result/emit " DESC"]
					]
				]

				result/add [next feed]
			]
		]

		result/out
	]
]