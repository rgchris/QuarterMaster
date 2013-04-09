REBOL [
	Title: "Wordify Text"
	Date: 18-Oct-2011
	Author: "Christopher Ross-Gill"
	Exports: [wordify unwordify]
	Purpose: "To simplify a string to a base alphanumeric format"
]

{
	Áá	Àà	Ăă	Ắắ	Ằằ	Ẵẵ	Ẳẳ	Ââ	Ấấ	Ầầ	Ẫẫ	Ẩẩ	Ǎǎ	Åå	Ǻǻ	Ää	Ǟǟ	Ãã	Ȧȧ	Ǡǡ	Ąą	Āā	Ảả	Ȁȁ	Ȃȃ	Ạạ
	Ặặ	Ậậ	Ḁḁ	Ⱥⱥ	ᶏ	Ɐɐ	Ɑɑ
	Āā	Ēē	Ḡḡ	Ī ī Ōō	Ūū	Ȳȳ	Ǣǣ Testing UTF-8: Iñtërnâtiônàlizætiøn°
}

wordify: use [out emit alphanum punct extras extras-loose extras-tight codes chars clean-up][
	alphanum: charset [#"0" - #"9" #"a" - #"z" #"A" - #"Z"]
	punct: charset "!()" ; "*!',()"

	codes: [
		  copy chars some alphanum (emit chars)
		| [" " | "-" | "/" | "_" | "‑" | "–" | "—" | "⁻" | "₋" | " "] (emit "-")
		| ["⁰" | "₀"] (emit "0")
		| ["¹" | "₁"] (emit "1")
		| ["²" | "₂"] (emit "2")
		| ["³" | "₃"] (emit "3")
		| ["⁴" | "₄"] (emit "4")
		| ["⁵" | "₅"] (emit "5")
		| ["⁶" | "₆"] (emit "6")
		| ["⁷" | "₇"] (emit "7")
		| ["⁸" | "₈"] (emit "8")
		| ["⁹" | "₉"] (emit "9")
		| ["Ä"] (emit "Ae")
		| ["À" | "Á" | "Â" | "Ã" | "Å" | "Ā"] (emit "A")
		| ["ä"] (emit "ae")
		| ["ₐ" | "ª" | "à" | "á" | "â" | "ã" | "å" | "ā"] (emit "a")
		| ["Ç" | "Ć"] (emit "C")
		| ["ç" | "ć"] (emit "c")
		| ["Ð"] (emit "D")
		| ["đ"] (emit "d")
		| ["È" | "É" | "Ê" | "Ë"] (emit "E")
		| ["ₑ" | "è" | "é" | "ê" | "ë"] (emit "e")
		| ["ƒ"] (emit "f")
		| ["ğ"] (emit "g")
		| ["Ğ"] (emit "G")
		| ["I" | "Ì" | "Í" | "Î"] (emit "I")
		| ["ì" | "í" | "î" | "ī" | "ı"] (emit "i")
		| ["Ï"] (emit "Ii")
		| ["ï"] (emit "ii")
		| ["Ñ"] (emit "N")
		| ["ⁿ" | "ñ"] (emit "n")
		| ["№"] (emit "Nr")
		| ["Ö" | "Œ"] (emit "Oe")
		| ["Ò" | "Ó" | "Ô" | "Õ" | "Ø"] (emit "O")
		| ["ö" | "œ"] (emit "oe")
		| ["ₒ" | "ð" | "ò" | "ó" | "ô" | "õ" | "ø"] (emit "o")
		| ["Ş" | "Š"] (emit "S")
		| ["ş" | "š"] (emit "s")
		| ["ß"] (emit "ss")
		| ["Ú" | "Ù" | "Û"] (emit "U")
		| ["ù" | "ú" | "û"] (emit "u")
		| ["Ü"] (emit "Ue")
		| ["ü"] (emit "ue")
		| ["×"] (emit "x")
		| ["Ý" | "Ÿ"] (emit "Y")
		| ["ý" | "ÿ"] (emit "y")
		| ["Ž"] (emit "Z")
		| ["ž"] (emit "z")
		| extras
		| skip
	]

	extras-loose: [
		copy chars some punct (emit chars)
		| "." (emit "_")
		| ["⁰" | "₀"] (emit "(0)")
		| ["¹" | "₁"] (emit "(1)")
		| ["²" | "₂"] (emit "(2)")
		| ["³" | "₃"] (emit "(3)")
		| ["⁴" | "₄"] (emit "(4)")
		| ["⁵" | "₅"] (emit "(5)")
		| ["⁶" | "₆"] (emit "(6)")
		| ["⁷" | "₇"] (emit "(7)")
		| ["⁸" | "₈"] (emit "(8)")
		| ["⁹" | "₉"] (emit "(9)")
		| "¼" (emit "(1-4)")
		| "½" (emit "(1-2)")
		| "¾" (emit "(3-4)")
		| "Æ" (emit "(AE)")
		| "æ" (emit "(ae)")
		| "©" (emit "(c)")
		| "°" (emit "(deg)")
		| "º" (emit "(o)")
		| "®" (emit "(r)")
		| "™" (emit "(tm)")
	]

	extras-tight: [
		  ["⁰" | "₀"] (emit "0")
		| ["¹" | "₁"] (emit "1")
		| ["²" | "₂"] (emit "2")
		| ["³" | "₃"] (emit "3")
		| ["⁴" | "₄"] (emit "4")
		| ["⁵" | "₅"] (emit "5")
		| ["⁶" | "₆"] (emit "6")
		| ["⁷" | "₇"] (emit "7")
		| ["⁸" | "₈"] (emit "8")
		| ["⁹" | "₉"] (emit "9")
		| "¼" (emit "1-4")
		| "½" (emit "1-2")
		| "¾" (emit "3-4")
		| "Æ" (emit "AE")
		| "æ" (emit "ae")
		| "©" (emit "c")
		| "º" (emit "o")
		| "°" (emit "deg")
		| "®" (emit "r")
		| "™" (emit "tm")
	]

	clean-up: use [mk ex tm][
		[
			(tm: [end skip])
			mk: any "-" ex: (remove/part mk ex) :mk
			some [
				  mk: some "-" end (clear mk) :mk
				| "-" mk: some "-" ex: (remove/part mk ex) :mk
				| skip tm:
			]
			:tm end
		]
	]

	out: ""
	emit: func [data][append out data]

	func [text [string!] /case /loose][
		out: copy ""
		extras: either loose [:extras-loose][:extras-tight]

		all [
			parse/all/case text [any codes]
			parse/all out [clean-up]
			either case [out][lowercase out]
		]
	]
]

unwordify: use [alpha digit punct codes char in-word][
	digit: charset [#"0" - #"9"]
	alpha: charset [#"a" - #"z" #"A" - #"Z"]
	punct: charset ["*!',()"]

	codes: [
		copy char some alpha (
			emit either in-word [char][uppercase/part char 1]
			in-word: true
		)
		| copy char some digit (emit char in-word: true)
		| (in-word: true) "(1)" (emit "¹")
		| "(2)" (emit "²")
		| "(3)" (emit "³")
		| "(1-4)" (emit "¼")
		| "(1-2)" (emit "½")
		| "(3-4)" (emit "¾")
		| "(AE)" (emit "Æ")
		| "(ae)" (emit "æ")
		| "(c)" (emit "©")
		| "(o)" (emit "º")
		| "(r)" (emit "®")
		| "(tm)" (emit "™")
		| (in-word: false) "_" (emit ".")
		| "-" (emit " ")
		| copy char some punct (emit char)
		| skip
	]

	out: ""
	emit: func [data][append out data]

	func [word [string!]][
		out: copy ""
		in-word: false

		parse/all/case word [any codes]
		out
	]
]
