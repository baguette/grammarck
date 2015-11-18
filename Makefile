OCB=ocamlbuild

all:
	cd src && $(OCB) main.native

clean:
	cd src && $(OCB) -clean

