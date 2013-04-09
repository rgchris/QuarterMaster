REBOL [
	Title: "Extract URLs"
	File: %extract-urls.r
	Version: 2.0.0
	Home: http://www.ross-gill.com/page/Beyond_Regular_Expressions
	Date: 31-Jul-2010
	Purpose: "To identify and extract URIs from plain text"
	Author: "Christopher Ross-Gill"
	Type: 'module
	Exports: [extract-urls]
	Reference: http://daringfireball.net/2010/07/improved_regex_for_matching_urls
]

extract-urls: use [out text uri letter digit word space punct chars paren][
	letter: charset [#"a" - #"z"]
	digit: charset [#"0" - #"9"]
	word: charset [#"_" #"0" - #"9" #"A" - #"Z" #"a" - #"z"] ; per regex
	space: charset "^/^- ()<>^"'" ; for curly quotes, need unicode (R3)
	punct: charset "!'#$%&`*+,-./:;=?@[/]^^{|}~" ; regex 'punct without ()<>
	chars: complement union space punct
	paren: ["(" some [chars | punct | "(" some [chars | punct] ")"]")"]

	uri: [
		[
			  letter some [word | "-"] ":" [1 3 "/" | letter | digit | "%"]
			| "www" 0 3 digit "."
			| some [letter | digit] "." 2 4 letter
		]
		some [opt [some punct] some [chars | paren] opt "/"]
	]

	text: use [emit-link emit-text link mk ex][
		emit-link: [(append out to-url link)]
		emit-text: [(unless mk = ex [append out copy/part mk ex])]

		[
			mk: any [
				ex: copy link uri emit-text emit-link mk:
				| some [chars | punct] some space ; non-uri words, line not required
				| skip
			]
			ex: emit-text
		]
	]

	func [
		"Separates URLs from plain text"
		txt [string!] "Text to be split"
	][
		out: copy []
		if parse/all txt text [out]
	]
]