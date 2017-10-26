PKG := cmdliner,lwt.ppx,lwt.unix,cohttp.lwt,markup.lwt,lambdasoup,ppx_deriving.std,ppx_deriving_yojson

all:
	ocamlbuild -use-ocamlfind -package ${PKG} menu.byte
clean:
	ocamlbuil -clean

.merlin: Makefile
	echo "B _build" >.merlin
	echo "PKG ${PKG}" | sed 's/,/\nPKG /g' >>.merlin
