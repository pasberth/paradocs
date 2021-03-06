.SECONDEXPANSION:

GHCJS=ghcjs
PARADOCS=paradocs

gh-pages: gh-pages/static \
			gh-pages/doc/cheatsheet.html \
			gh-pages/doc/cheatsheet/hello.pdoc \
			gh-pages/doc/lib/index.html \
			gh-pages/try-paradocs \
			$(patsubst lib/%.pdoc, gh-pages/doc/lib/%.html, $(shell find lib -not -type d))

gh-pages/static: gh-pages/static/highlightjs/paradocs.js

gh-pages/static/highlightjs/paradocs.js: misc/paradocs.js
	mkdir -p gh-pages/static/highlightjs
	cp $^ $@

gh-pages/try-paradocs: gh-pages/try-paradocs/index.html gh-pages/try-paradocs/try-paradocs.jsexe gh-pages/try-paradocs/lib.json gh-pages/try-paradocs/static

gh-pages/try-paradocs/index.html: try-paradocs/index.pdoc
	mkdir -p gh-pages/try-paradocs
	$(PARADOCS) try-paradocs/index.pdoc > gh-pages/try-paradocs/index.html

gh-pages/try-paradocs/try-paradocs.jsexe: try-paradocs/TryParadocs.hs
	$(GHCJS) -o gh-pages/try-paradocs/try-paradocs.jsexe try-paradocs/TryParadocs.hs

gh-pages/try-paradocs/lib.json: try-paradocs/mklibjson.rb $(shell git ls-files lib)
	ruby try-paradocs/mklibjson.rb > gh-pages/try-paradocs/lib.json

gh-pages/try-paradocs/static: gh-pages/try-paradocs/static/pure-min.css \
								gh-pages/try-paradocs/static/codemirror.js \
								gh-pages/try-paradocs/static/codemirror.css \
								gh-pages/try-paradocs/static/paradocs-mode.js \
								gh-pages/try-paradocs/static/jquery-2.1.0.min.js \
	 							gh-pages/try-paradocs/static/nprogress.js \
	 							gh-pages/try-paradocs/static/nprogress.css

gh-pages/try-paradocs/static/pure-min.css:
	mkdir -p gh-pages/try-paradocs/static
	cd gh-pages/try-paradocs/static && wget http://yui.yahooapis.com/pure/0.4.2/pure-min.css

/tmp/codemirror-4.1:
	mkdir -p gh-pages/try-paradocs/static
	cd /tmp && wget http://codemirror.net/codemirror.zip && unzip codemirror.zip

gh-pages/try-paradocs/static/codemirror.js: /tmp/codemirror-4.1
	mkdir -p gh-pages/try-paradocs/static
	cp /tmp/codemirror-4.1/lib/codemirror.js gh-pages/try-paradocs/static/codemirror.js

gh-pages/try-paradocs/static/codemirror.css: /tmp/codemirror-4.1
	mkdir -p gh-pages/try-paradocs/static
	cp /tmp/codemirror-4.1/lib/codemirror.css gh-pages/try-paradocs/static/codemirror.css

gh-pages/try-paradocs/static/paradocs-mode.js: misc/paradocs-mode.js
	cp $^ $@

gh-pages/try-paradocs/static/jquery-2.1.0.min.js:
	mkdir -p gh-pages/try-paradocs/static
	cd gh-pages/try-paradocs/static && wget http://code.jquery.com/jquery-2.1.0.min.js

gh-pages/try-paradocs/static/nprogress.js:
	mkdir -p gh-pages/try-paradocs/static
	cd gh-pages/try-paradocs/static && wget http://ricostacruz.com/nprogress/nprogress.js

gh-pages/try-paradocs/static/nprogress.css:
	mkdir -p gh-pages/try-paradocs/static
	cd gh-pages/try-paradocs/static && wget http://ricostacruz.com/nprogress/nprogress.css

gh-pages/doc/cheatsheet.html: doc/cheatsheet.pdoc
	$(PARADOCS) $^ > $@

gh-pages/doc/cheatsheet/hello.pdoc: doc/cheatsheet/hello.pdoc
	mkdir -p $(shell dirname $@)
	cp $^ $@

gh-pages/doc/lib/index.html: doc/lib/index.pdoc
	$(PARADOCS) $^ > $@

gh-pages/doc/lib/%.html: lib/%.pdoc
	mkdir -p $(shell dirname $@)
	echo '%include lib/paradocs/html.pdoc' > libdoc.pdoc
	echo '=' $^ >> libdoc.pdoc
	echo >> libdoc.pdoc
	echo '%pre%paradocs%read' $^ >> libdoc.pdoc
	$(PARADOCS) libdoc.pdoc > $@