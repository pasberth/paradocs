GHCJS=ghcjs
PARADOCS=paradocs

gh-pages: gh-pages/try-paradocs

gh-pages/try-paradocs: gh-pages/try-paradocs/index.html gh-pages/try-paradocs/try-paradocs.jsexe gh-pages/try-paradocs/lib.json gh-pages/try-paradocs/static

gh-pages/try-paradocs/index.html: try-paradocs/index.pdoc
	mkdir -p gh-pages/try-paradocs
	$(PARADOCS) try-paradocs/index.pdoc > gh-pages/try-paradocs/index.html

gh-pages/try-paradocs/try-paradocs.jsexe: try-paradocs/TryParadocs.hs
	$(GHCJS) -o gh-pages/try-paradocs/try-paradocs.jsexe try-paradocs/TryParadocs.hs

gh-pages/try-paradocs/lib.json: try-paradocs/mklibjson.rb $(shell git ls-files lib)
	ruby try-paradocs/mklibjson.rb > gh-pages/try-paradocs/lib.json

gh-pages/try-paradocs/static: gh-pages/try-paradocs/static/jquery-2.1.0.min.js \
	 							gh-pages/try-paradocs/static/nprogress.js \
	 							gh-pages/try-paradocs/static/nprogress.css
	mkdir -p gh-pages/try-paradocs/static/purecss
	cd gh-pages/try-paradocs/static/purecss && wget http://yui.yahooapis.com/pure/0.4.2/pure-min.css
	mkdir -p gh-pages/try-paradocs/static
	cd gh-pages/try-paradocs/static && wget http://codemirror.net/codemirror.zip && unzip codemirror.zip

gh-pages/try-paradocs/static/jquery-2.1.0.min.js:
	cd gh-pages/try-paradocs/static && wget http://code.jquery.com/jquery-2.1.0.min.js

gh-pages/try-paradocs/static/nprogress.js:
	cd gh-pages/try-paradocs/static && wget http://ricostacruz.com/nprogress/nprogress.js

gh-pages/try-paradocs/static/nprogress.css:
	cd gh-pages/try-paradocs/static && wget http://ricostacruz.com/nprogress/nprogress.css