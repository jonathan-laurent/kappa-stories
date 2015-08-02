MAIN_DIR=kappa-stories
SOURCE_DIRS=grammar dataStructures $(MAIN_DIR) $(MAIN_DIR)/dataStructures $(MAIN_DIR)/kappa

EXCLUDED_DIRS=pattern siteGraphs
TARGETS=$(MAIN_DIR)/main.byte 

FLAGS=-use-ocamlfind -pkgs "mlpost unix ocamlgraph" -lflag "-g"
TEST=sos

OCAMLBUILD=ocamlbuild $(FLAGS) -Is "$(SOURCE_DIRS)" -Xs "$(EXCLUDED_DIRS)"

build :
#echo $(CMX_FILES)
	cd .. ; $(OCAMLBUILD) $(TARGETS)
	cp ../main.byte stories

clean :
	-rm -rf ../_build
	-rm -f  ../main.* stories
	-rm -rf ../storyBooks.docdir

	-rm -rf out
	-rm -rf *.docdir
	-rm -f  stories renderer
	-rm -f *.log *.aux *.mps *.pdf *.txt *.png

test :
	-mkdir out
	./stories tests/$(TEST).ka -n 1
	dot -Tpng out/1.dot > story.png
	eog story.png

renderer :
	cd .. ; $(OCAMLBUILD) $(MAIN_DIR)/renderer.native
	cp ../renderer.native renderer

doc :
	cd .. ; $(OCAMLBUILD) $(MAIN_DIR)/storyBooks.docdir/index.html
	cp -r ../storyBooks.docdir .


showdoc :
	firefox ./storyBooks.docdir/index.html

