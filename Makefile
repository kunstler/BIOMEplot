PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')

all:
	make -C src

document: roxygen

roxygen:
	@mkdir -p man
	Rscript -e "library(methods); devtools::document()"

install: roxygen
	R CMD INSTALL .

clean:
	make -C src clean

build:
	R CMD build .

check: build
	R CMD check --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

# test:
# 	make -C tests/testthat

.PHONY: attributes document roxygen install clean build check
