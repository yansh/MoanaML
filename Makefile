NAME = MoanaML
OCAMLBUILD = ocamlbuild -use-ocamlfind -use-menhir -classic-display
PACKAGES = -package lwt,yojson,irmin,irmin.unix,oUnit -tag thread

all:
		$(OCAMLBUILD) $(PACKAGES) tests.byte examples/contacts.byte

clean:
		rm *.byte

test:	
		$(OCAMLBUILD) $(PACKAGES) tests.byte 

example:
		$(OCAMLBUILD) examples/contacts.byte


