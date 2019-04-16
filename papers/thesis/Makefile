target_pdf = __book/thesis.pdf

pdf:
	R -e 'bookdown::render_book(".", bookdown::pdf_book(template="ime.tex", citation_package="natbib"))'
.PHONY: pdf

clean:
	rm -rf _bookdown_files/ _book/
.PHONY: clean
