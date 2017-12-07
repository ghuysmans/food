PKG := cmdliner,lwt.ppx,lwt.unix,cohttp.lwt,markup.lwt,lambdasoup,ppx_deriving.show,ppx_deriving_yojson,ANSITerminal,tyxml

all:
	ocamlbuild -use-ocamlfind -package ${PKG} menu.byte
clean:
	ocamlbuild -clean

.merlin: Makefile
	echo "B _build" >.merlin
	echo "PKG ${PKG}" | sed 's/,/\nPKG /g' >>.merlin
