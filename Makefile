MODULES=audio game graphics main state tetromino
PKGS=sdl
SRC=src/
PATHS=$(addprefix $(SRC), $(MODULES))
OBJECTS=$(PATHS:=.cmo)
MLS=$(PATHS:=.ml)
MLIS=$(PATHS:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(SRC)$(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(SRC)$(MAIN) && ./$(MAIN)
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private
