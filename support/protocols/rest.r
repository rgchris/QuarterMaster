Rebol [
	Title: "REST-Friendly HTTP Protocol"
	Date: 30-Oct-2012
	Author: "Christopher Ross-Gill"
	Type: 'module
	Version: 0.1.3
	File: %rest-curl.r
	Purpose: {
		An elementary HTTP protocol allowing more versatility when developing Web
		Services clients.
	}
	Note: {To Do: Multipart and Multipart OAuth; Better Header Support}
	Usage: http://www.ross-gill.com/page/REST_Protocol
	History: [
		15-Aug-2006 0.0.1 "Original REST Version"
	]
]

require %markup/webform.r
require %shell/curl.r

unless in system/schemes 'rest [
	system/schemes: make system/schemes [REST: none]
]

system/schemes/rest: make system/standard/port [
	scheme: 'rest
	port-id: 80
	passive: none
	cache-size: 5
	proxy: make object! [host: port-id: user: pass: type: bypass: none]
]

system/schemes/rest/handler: use [prepare transcribe execute][
	prepare: use [
		request-prototype header-prototype
		oauth-credentials oauth-prototype
		sign
	][
		request-prototype: context [
			version: 1.1
			action: "GET"
			headers: none
			query: none
			oauth: target: content: length: timeout: none
			type: 'application/x-www-form-urlencoded
		]

		header-prototype: context [
			Accept: "*/*"
			Connection: "close"
			User-Agent: rejoin ["REBOL/" system/product " " system/version]
			Content-Length: Content-Type: Authorization: Range: none
		]

		oauth-credentials: context [
			consumer-key: consumer-secret:
			oauth-token: oauth-token-secret: ""
		]

		oauth-prototype: context [
			oauth_callback: none
			oauth_consumer_key: none
			oauth_token: oauth_nonce: none
			oauth_signature_method: "HMAC-SHA1"
			oauth_timestamp: none
			oauth_version: 1.0
			oauth_verifier: oauth_signature: none
		]

		sign: func [request [object!] /local header params timestamp out][
			out: copy ""
			timestamp: now/precise

			header: make oauth-prototype [
				oauth_consumer_key: request/oauth/consumer-key
				oauth_token: request/oauth/oauth-token
				oauth_nonce: enbase/base checksum/secure join timestamp oauth_consumer_key 64
				oauth_timestamp: form any [
					attempt [to-integer difference timestamp 1-Jan-1970/0:0:0]
					timestamp - 1-Jan-1970/0:0:0 * 86400.0
				]
				clear find/last oauth_timestamp "."
			]

			params: make header any [request/content []]
			params: sort/skip third params 2

			header/oauth_signature: enbase/base checksum/secure/key rejoin [
				uppercase form request/action "&" url-encode form request/url "&"
				url-encode replace/all to-webform params "+" "%20"
			] rejoin [
				request/oauth/consumer-secret "&" any [request/oauth/oauth-token-secret ""]
			] 64

			foreach [name value] third header [
				if value [
					repend out [", " form name {="} url-encode form value {"}]
				]
			]

			switch request/action [
				"GET" [
					if request/content [
						request/url: join request/url to-webform/prefix request/content
						request/content: none
					]
				]

				"POST" "PUT" [
					request/content: to-webform request/content
					request/headers/Content-Type: "application/x-www-form-urlencoded"
				]
			]

			request/headers/Authorization: join "OAuth" next out
		]

		prepare: func [port [port!] /local request][
			port/locals/request: request: make request-prototype port/locals/request
			request/action: uppercase form request/action
			request/headers: make header-prototype any [request/headers []]
			request/content: any [port/state/custom request/content]

			either request/oauth [sign request][
				request/headers/Authorization: any [
					request/headers/authorization
					if all [port/user port/pass][
						join "Basic " enbase join port/user [#":" port/pass]
					]
				]
			]

			if port/state/index > 0 [
				request/version: 1.1
				request/headers/Range: rejoin ["bytes=" port/state/index "-"]
			]

			case/all [
				block? request/content [
					request/content: to-webform request/content
					request/headers/Content-Type: "application/x-www-form-urlencoded"
				]

				string? request/content [
					request/length: length? request/content
					request/headers/Content-Length: length? request/content
					request/headers/Content-Type: request/type
				]
			]

			port
		]
	]

	execute: func [port [port!]][
		curl/full/method/header/with/timeout/into ; url action headers content response
		port/locals/request/url
		port/locals/request/action
		port/locals/request/headers
		port/locals/request/content
		port/locals/request/timeout
		port/locals/response
	]

	transcribe: use [
		response-code header-name header-part header-feed
		response-prototype header-prototype
	][
		response-code: use [digit][
			digit: charset "0123456789"
			[3 digit]
		]

		header-part: use [chars][
			chars: complement charset [#"^(00)" - #"^(1F)"]
			[some chars]
		]

		header-name: use [chars][
			chars: charset ["_-0123456789" #"a" - #"z" #"A" - #"Z"]
			[some chars]
		]

		header-feed: [newline | crlf]

		space: use [space][
			space: charset " ^-"
			[some space]
		]

		response-prototype: context [
			status: message: http-headers: headers: content: binary: type: length: none
		]

		header-prototype: context [
			Date: Server: Last-Modified: Accept-Ranges: Content-Encoding: Content-Type:
			Content-Length: Location: Expires: Referer: Connection: Authorization: none
		]

		transcribe: func [port [port!] /local response name value pos][
			port/locals/response: response: make response-prototype [
				unless parse/all port/locals/response [
					"HTTP/1." ["0" | "1"] space
					copy status response-code (message: "")
					opt [space copy message header-part] header-feed
					(net-utils/net-log reform ["HTTP Response:" status message])
					(
						status: load status
						headers: make block! []
					)
					some [
						copy name header-name ":" any " "
						copy value header-part header-feed
						(repend headers [to-set-word name value])
					]
					header-feed content: to end (
						content: to string! binary: to binary! content
					)
				][
					net-utils/net-log pos
					make error! "Could Not Parse Response"
				]

				headers: make header-prototype http-headers: headers

				type: all [
					path? type: attempt [load headers/Content-Type]
					type
				]

				length: any [attempt [headers/Content-Length: to-integer headers/Content-Length] 0]
			]
		]
	]

	context [
		port-flags: system/standard/port-flags/pass-thru

		init: func [port [port!] spec [url! block!] /local url][
			port/locals: context [
				request: case/all [
					url? spec [
						spec: compose [url: (to-url replace form spec rest:// http://)]
					]
					block? spec [
						if all [
							url? url: pick find/tail spec [url:] 1
							parse/all url ["http" opt "s" "://" to end]
						][
							spec
						]
					]
				]

				response: make string! ""
			]
		]

		open: func [port [port!]][
			port/state/flags: port/state/flags or port-flags
			execute prepare port
		]

		copy: :transcribe

		close: does []
	]
]