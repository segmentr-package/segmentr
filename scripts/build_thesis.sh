#!/bin/sh

cd ./papers/thesis
R -e 'bookdown::render_book("index.Rmd", bookdown::html_document2())'
R -e 'bookdown::render_book("index.Rmd", bookdown::pdf_document2())'
