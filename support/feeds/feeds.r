REBOL [
	Title: "Build Feeds"
	Date: 4-Sep-2012
	Author: "Christopher Ross-Gill"
	Type: 'module
	Exports: [build-feed]
	Folder: wrt://site/feed/
	Version: 1.0.0
]

build-feed: use [header!][
	header!: context [
		title: "My Site Title"
		subtitle: "My Site Subtitle"
		id: base: http://mysite.me
		link: %/
		target: %/news.feed
		icon: logo: %/icon.png
		updated: entries: none
	]

	build-feed: func [
		header [block! object!]
		entries [block!]
		/custom template [file! string!]
	][
		template: any [template %feeds/atom.feed.rsp]
		header: make header! header
		render/with :template [header entries]
	]
]
