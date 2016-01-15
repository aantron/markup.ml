LIB := markup
VERSION := 0.6

if_package = ! ocamlfind query $(1) > /dev/null 2> /dev/null || ( $(2) )

OCAML_VERSION := \
	$(shell ocamlc -version | grep -E -o '^[0-9]+\.[0-9]+' | sed 's/\.//')

ifeq ($(shell test $(OCAML_VERSION) -ge 402 && echo true),true)
SAFE_STRING := ,-safe-string
ifeq ($(shell ocamlfind query bisect_ppx > /dev/null 2> /dev/null && \
              echo true),true)
TEST_TAGS := \
	-tag-line '<src/*>: package(bisect_ppx)' \
	-tag-line '<test/*.native>: package(bisect_ppx)'
endif
endif

ifeq ($(shell test $(OCAML_VERSION) -ge 400 && echo true),true)
BIN_ANNOT := ,-bin-annot
endif

CFLAGS := -cflags -g,-w,+A-4-9-44-45-48$(BIN_ANNOT)$(SAFE_STRING)

OCAMLBUILD := ocamlbuild -use-ocamlfind -j 0 -no-links

.PHONY : build
build :
	$(OCAMLBUILD) $(CFLAGS) $(LIB).cma $(LIB).cmxa
	$(call if_package,lwt,\
	  	$(OCAMLBUILD) $(CFLAGS) $(LIB)_lwt.cma $(LIB)_lwt.cmxa)
	$(call if_package,lwt.unix,\
	  	$(OCAMLBUILD) $(CFLAGS) $(LIB)_lwt_unix.cma $(LIB)_lwt_unix.cmxa)

.PHONY : entities
entities :
	$(OCAMLBUILD) -quiet $(CFLAGS) translate_entities.native -- \
		> src/entities.ml

COVERAGE := test/coverage
run_test = \
	$(OCAMLBUILD) $(CFLAGS) $(TEST_TAGS) $(COVERAGE_PPXOPT) $(1) && \
	ulimit -s 256 && \
	_build/test/$(1)

.PHONY : test
test :
	@rm -f bisect*.out
	@$(call run_test,test.native)
	@echo
	@$(call if_package,lwt,$(call run_test,test_lwt.native) -runner sequential)
	@which bisect-ppx-report > /dev/null 2> /dev/null && make bisect-report \
		|| true

.PHONY : bisect-report
bisect-report :
	@bisect-ppx-report -I _build -html $(COVERAGE) bisect*.out
	@echo
	@bisect-ppx-report -I _build -summary-only -text - bisect*.out | tail -n 1 \
		| sed 's/ - total/Coverage/'
	@echo "See $(COVERAGE)/index.html for coverage report"

.PHONY : performance-test
performance-test :
	@$(OCAMLBUILD) $(CFLAGS) performance_markup.native --
	@$(call if_package,netstring,\
	  $(OCAMLBUILD) $(CFLAGS) performance_nethtml.native --)
	@$(call if_package,xmlm,\
	  $(OCAMLBUILD) $(CFLAGS) performance_xmlm.native --)

.PHONY : js-test
js-test :
	$(call if_package,js_of_ocaml,$(OCAMLBUILD) $(CLFAGS) test_js_of_ocaml.byte)
	$(call if_package,js_of_ocaml,\
	  	js_of_ocaml _build/test/js_of_ocaml/test_js_of_ocaml.byte)

DEP_TEST_DIR := test/dependency
dependency_test = \
	cd $(DEP_TEST_DIR) && \
	$(OCAMLBUILD) $(1).native -- && \
	$(OCAMLBUILD) $(1).byte --

.PHONY : dependency-test
dependency-test :
	cd $(DEP_TEST_DIR) && $(OCAMLBUILD) -clean
	$(call dependency_test,dep_core)
	$(call if_package,lwt,$(call dependency_test,dep_lwt))
	$(call if_package,lwt.unix,$(call dependency_test,dep_lwt_unix))

HTML := doc/html
DOCFLAGS := -docflags -colorize-code

.PHONY : docs
docs : docs-odocl
	$(OCAMLBUILD) $(DOCFLAGS) doc/$(LIB).docdir/index.html
	rm -rf $(HTML)
	mkdir -p $(HTML)
	rsync -r _build/doc/$(LIB).docdir/* $(HTML)/
	cp doc/style.css $(HTML)/
	$(call if_package,lambdasoup,\
	  test $(OCAML_VERSION) -eq 402 \
	  || ( make docs-postprocess \
	  && rm -f $(HTML)/type_*.html $(HTML)/html.stamp $(HTML)/index*.html \
	  && _build/doc/postprocess.native ))
	@echo "\nSee $(HTML)/index.html"

.PHONY : docs-postprocess
docs-postprocess :
	$(OCAMLBUILD) postprocess.native

ODOCL := doc/markup.odocl

.PHONY : docs-odocl
docs-odocl :
	echo Markup > $(ODOCL)
	$(call if_package,lwt,echo Markup_lwt >> $(ODOCL))
	$(call if_package,lwt.unix,echo Markup_lwt_unix >> $(ODOCL))

PUBLISH := doc/publish

.PHONY : publish-docs
publish-docs : check-doc-prereqs docs
	rm -rf $(PUBLISH)
	mkdir -p $(PUBLISH)
	cd $(PUBLISH) \
		&& git init \
		&& git remote add github git@github.com:aantron/markup.ml.git \
		&& rsync -r ../html/* ./ \
		&& git add -A \
		&& git commit -m 'Markup.ml documentation.' \
		&& git push -uf github master:gh-pages

DOC_ZIP := doc/$(LIB)-$(VERSION)-doc.zip

.PHONY : package-docs
package-docs : check-doc-prereqs docs
	rm -f $(DOC_ZIP)
	zip -9 $(DOC_ZIP) $(HTML)/*

.PHONY : check-doc-prereqs
check-doc-prereqs :
	@test $(OCAML_VERSION) -ne 402 \
		|| (echo "\nocamldoc is broken in 4.02" && false)
	@ocamlfind query lwt.unix > /dev/null 2> /dev/null \
		|| (echo "\nLwt not installed" && false)
	@ocamlfind query lambdasoup > /dev/null 2> /dev/null \
		|| (echo "\nLambda Soup not installed" && false)

need_package = \
	ocamlfind query $(1) > /dev/null 2> /dev/null \
		|| echo "Missing package '$(1)' (opam install $(2))"

.PHONY : all-tests
all-tests :
	@$(call need_package,oUnit,ounit)
	@test $(OCAML_VERSION) -lt 402 || $(call need_package,bisect_ppx,bisect_ppx)
	@$(call need_package,lwt,lwt)
	@$(call need_package,lwt.unix,lwt)
	@$(call need_package,netstring,ocamlnet)
	@$(call need_package,xmlm,xmlm)
	@$(call need_package,lambdasoup,lambdasoup)
	@echo
	make uninstall install
	@echo
	make dependency-test
	@echo
	make build
	@echo
	make test
	@echo
	make js-test
	@echo
	make performance-test
	@echo
	make docs

OUTPUT := _build/src
generated = \
	$(OUTPUT)/$(1).cma $(OUTPUT)/$(1).cmxa $(OUTPUT)/$(1).a $(OUTPUT)/$(1).cmi \
	$(OUTPUT)/$(1).mli $(OUTPUT)/$(1).cmti $(OUTPUT)/$(1).cmt
INSTALL := \
	$(call generated,$(LIB)) \
	$(call generated,$(LIB)_lwt) \
	$(call generated,$(LIB)_lwt_unix)
PACKAGE := markup

.PHONY : ocamlfind-install
ocamlfind-install :
	ocamlfind install $(PACKAGE) src/META -optional $(INSTALL)

.PHONY : ocamlfind-uninstall
ocamlfind-uninstall :
	ocamlfind remove $(PACKAGE)

.PHONY : install
install :
	[ -f opam ] || ln -s src/opam
	opam pin add . -y

.PHONY : uninstall
uninstall :
	opam pin remove $(PACKAGE) -y

.PHONY : clean
clean :
	$(OCAMLBUILD) -clean
	rm -rf bisect*.out $(COVERAGE) $(HTML) $(PUBLISH) $(DOC_ZIP) opam
	cd $(DEP_TEST_DIR) && $(OCAMLBUILD) -clean
