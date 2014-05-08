REBOL [
	Title: "Clean"
	Date: 14-Aug-2013
	Purpose: "Trims and Converts Errant CP-1252 to UTF-8"
	Author: "Christopher Ross-Gill"
	Version: 0.1.1
	Type: 'module
	Exports: [clean encode-utf8]
]

encode-utf8: func [ ; does this function belong in QM?
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

		true [[]]
	]
]

clean: use [codepoints][
	codepoints: [
		; for 8-bit encodings other than CP-1252, change these codepoints
		128 8364  130 8218  131 402  132 8222  133 8230  134 8224
		135 8225  136 710  137 8240  138 352  139 8249  140 338  142 381
		145 8216  146 8217  147 8220  148 8221  149 8226  150 8211
		151 8212  152 732  153 8482  154 353  155 8250  156 339 158 382
	]

	clean: func [
		"Convert Windows 1252 characters in a string to UTF-8"
		string [string! binary!] "String to convert"
		/local here char
	][
		string: here: as-binary replace/all trim/auto string crlf newline

		while [here: invalid-utf? here][
			change/part here case [
				158 < char: to integer! here/1 [
					encode-utf8 char
				]
				char: select codepoints char [
					encode-utf8 char
				]
				true [""]
			] 1
		]

		as-string string
	]
]