target_pdf = __book/thesis.pdf

pdf:
	R -e 'require("segmentr"); bookdown::render_book(".", bookdown::pdf_book(template="ime.tex", citation_package="natbib"))'
.PHONY: pdf

web:
	R -e 'require("segmentr"); bookdown::render_book(".", bookdown::gitbook(template="ime.tex", citation_package="natbib"))'
.PHONE: web

clean:
	rm -rf _bookdown_files/ _book/
.PHONY: clean
