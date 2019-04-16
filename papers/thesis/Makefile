target_pdf = __book/thesis.pdf

pdf: __book/thesis.pdf

$(target_pdf):
	R -e 'bookdown::render_book(".", bookdown::pdf_book(template="ime.tex", citation_package="natbib"))'

clean:
	rm -rf _bookdown_files/ _book/
