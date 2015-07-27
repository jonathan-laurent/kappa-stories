MAIN_DIR=kappa-stories
SOURCE_DIRS=grammar dataStructures $(MAIN_DIR) $(MAIN_DIR)/dataStructures $(MAIN_DIR)/kappa
EXCLUDED_DIRS=pattern siteGraphs
TARGETS=$(MAIN_DIR)/main.native $(MAIN_DIR)/storyBooks.docdir/index.html

FLAGS=-use-ocamlfind -pkgs "mlpost unix ocamlgraph"

build :
#echo $(CMX_FILES)
	cd .. ; ocamlbuild $(FLAGS) $(TARGETS) -Is "$(SOURCE_DIRS)" -Xs "$(EXCLUDED_DIRS)"
	cp ../main.native stories
	cp -r ../storyBooks.docdir .

clean :
	-rm -rf ../_build
	-rm -f  ../main.native stories
	-rm -rf ../storyBooks.docdir

	-rm -rf storyBooks.docdir
	-rm -f  stories
	-rm -f *.log *.aux *.mps *.pdf

test :
	./stories tests/simpl.ka || true
	pdflatex main.tex
	xdg-open *.pdf


showdoc :
	firefox ./storyBooks.docdir/index.html

