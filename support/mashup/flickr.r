REBOL [
	Title: "Flickr API Client"
	Date: 15-Jul-2010
	Author: "Christopher Ross-Gill"
	File: %flickr.r
	Version: 0.3.0
	History: [
		16-Nov-2008 0.1.0 "Proof of Concept QM Module"
		5-Jul-2010 0.2.0 "Standalone Version"
		15-Jul-2011 0.3.0 "Added OAuth Authentication"
	]
	Comment: "User authentication is not required for all methods, ymmv."
	; Target: https://secure.flickr.com/services/
	Target: http://api.flickr.com/services/
	Exports: [flickr]
	Flickr-URLs: http://www.flickr.com/services/api/misc.urls.html

	Settings: [
		Consumer-Key:    <Consumer-Key>
		Consumer-Secret: <Consumer-Secret>
		User-Store:      none
		User:            none
	]
]

require %markup/json.r

flickr: context bind [
	echo: func ["Echo" [catch]][engage [method: "flickr.test.echo" echo: true engage: "REBOL"]]
	null: func ["Null" [catch]][engage [method: "flickr.test.null"]]

	as: func [
		"Set current user" [catch]
		user [string!] "Twitter user name"
	][
		either user: select users user [
			persona: make persona user
		][
			either not error? user: try [register][
				repend users [
					user/name
					new-line/skip/all third user true 2
				]
				user
			][throw :user]
		]
	]

	save-users: func [
		"Saves authorized users"
		/to location [file! url!] "Alternate Storage Location"
	][
		location: any [location settings/user-store]
		unless any [file? location url? location][
			make error! "No Storage Location Provided"
		]
		save/header location new-line/skip/all users true 2 context [
			Title: "Flickr Authorized Users"
			Date: now/date
		]
	]

	authorized-users: func ["Lists authorized users"][extract users 2] 

	search: find: func ["Find Photos" term /window size][
		engage [
			method: "flickr.photos.search"
			text: term
			extras: "date_taken"
			per_page: any [size 10]
		]
	]

	get-photosets: func [
		"Get all photosets for a given user" [catch]
		/for user
	][
		engage [
			method: "flickr.photosets.getList"
			user_id: any [user settings/user]
		]
	]

	get-photos-for: func [
		"Get photos for a given photoset ID" [catch]
		id [string! issue!] /privacy level [integer!]
	][
		engage [
			method: "flickr.photosets.getPhotos"
			photoset_id: id
			privacy_filter: level
			extras: "date_taken"
			media: "photos"
		]
	]

	get-info-for: func [
		"Get information about a photo" [catch]
		id [string! issue!]
	][
		engage [
			method: "flickr.photos.getInfo"
			photo_id: id
		]
	]

	make-url: func [
		"Provides a shortened link to a photo page" [catch]
		photo [object!]
	][
		all [
			in photo 'id
			get-short-url photo/id
		]
	]

] context [
	lookup: none

	flickr: system/script/header/target

	settings: make context [
		consumer-key: consumer-secret:
		key: secret: auth: user: none
	] any [
		system/script/args
		system/script/header/settings
	]

	get-http-response: func ["HTTP Hack" port [port!]][
		reform next parse do bind [response-line] last second get in port/handler 'open none
	]

	raise: use [error][
		unless in system/error 'flickr [system/error: make system/error [flickr: none]]

		error: system/error/flickr: context [
			code: 7787167 ; checksum http://www.flickr.com/
			type: "Flickr Error"
			message: none
		]

		func [[throw] result [object! string! block!]][
			case [
				object? result [result: result/message]
				block? result [result: rejoin result]
			]
			error/message: result
			throw make error! [flickr message]
		]
	]

	users: any [attempt [load settings/user-store] []]

	persona: context [
		id: name: fullname: none
		token: secret: none
	]

	get-short-url: use [to-base58][
		to-base58: use [ch dv md][
			ch: "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
			func [id /local out][
				id: load form id
				out: copy ""
				while [id > 0][
					insert out ch/(to-integer id // 58 + 1)
					id: to-integer id / 58
				]
				out
			]
		]

		func [photo [object! issue! string!]][
			case/all [
				object? photo [photo: get in photo 'id]
				any-string? photo [join http://flic.kr/p/ to-base58 photo]
			]
		]
	]

	oauth!: context [
		oauth_callback: none
		oauth_consumer_key: settings/consumer-key
		oauth_token: oauth_nonce: none
		oauth_signature_method: "HMAC-SHA1"
		oauth_timestamp: none
		oauth_version: 1.0
		oauth_verifier: oauth_signature: none
	]

	default: context [
		api_key: settings/consumer-key
		format: "json"
		nojsoncallback: 1
		method: "flickr.test.echo"
	]

	send: use [make-nonce timestamp params sign][
		make-nonce: does [
			enbase/base checksum/secure join now/precise settings/consumer-key 64
		]

		timestamp: func [/for date [date!]][
			date: any [date now]
			date: form any [
				attempt [to integer! difference date 1-Jan-1970/0:0:0]
				date - 1-Jan-1970/0:0:0 * 86400.0
			]
			clear find/last date "."
			date
		]

		sign: func [
			"Returns parameters including OAuth Signature"
			method [word!]
			lookup [url!]
			params [object! block!]
			/local out
		][
			out: copy ""

			params: make oauth! any [params []]
			params/oauth_nonce: make-nonce
			params/oauth_timestamp: timestamp
			params/oauth_token: persona/token
			params: context sort/skip third params 2

			params/oauth_signature: enbase/base checksum/secure/key rejoin [
				uppercase form method "&" url-encode form lookup "&"
				url-encode replace/all to-webform params "+" "%20"
			] rejoin [
				settings/consumer-secret "&" any [persona/secret ""]
			] 64
			
			to-webform/prefix params
		]

		send: func [
			method [word!] params [block! object!]
			/to lookup [file! url!] /auth
		][
			case/all [
				none? lookup [lookup: %rest/]
				file? lookup [lookup: flickr/:lookup]
			]

			params: switch method [
				get [
					params: make default params
					method: params/method
					lookup: join lookup sign 'get lookup params
					copy [get ""]
				]
				post [
					params: make default params
					method: params/method
					compose/deep [post (sign 'post lookup params)]
				]
				auth [
					method: "oauth"
					lookup: join lookup sign 'get lookup params
					copy [get ""]
				]
			]

			lookup: make port! lookup
			append/only params compose [Host: (lookup/host)]

			if error? try [open/custom lookup params][
				raise ["Method " any [method "'unnamed'"] " Failed: " get-http-response lookup]
			]

			method: copy lookup
			close lookup
			method
		]
	]

	engage: func [lookup /post][
		post: either post ['post]['get]
		all [
			; lookup: sign lookup
			lookup: send post lookup
			lookup: load-json lookup

			switch lookup/stat [
				"ok" [lookup]
				"fail" [raise lookup/message]
			]
		]
	]

	register: use [request-broker access-broker verification-page][
		request-broker: http://www.flickr.com/services/oauth/request_token
		verification-page: http://www.flickr.com/services/oauth/authorize?oauth_token=
		access-broker: http://www.flickr.com/services/oauth/access_token

		func [
			[catch]
			/requester request [function!]
			/local response verifier
		][
			request: any [:request :ask]
			set persona none

			response: load-webform send/to 'auth [oauth_callback: "oob"] request-broker

			persona/token: response/oauth_token
			persona/secret: response/oauth_token_secret

			browse join verification-page response/oauth_token 
			trim/all verifier: request "Enter your PIN from Flickr: "

			response: load-webform send/to 'auth [oauth_verifier: verifier] access-broker

			persona/id: to-email response/user_nsid
			persona/name: response/username
			persona/fullname: response/fullname
			persona/token: response/oauth_token
			persona/secret: response/oauth_token_secret

			persona
		]
	]
]