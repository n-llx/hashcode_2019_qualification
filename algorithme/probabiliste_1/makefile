exe : ../../outline/outline.cmo probabiliste.cmo
	ocamlc -I ../../outline/ outline.cmo probabiliste.cmo -o exe

probabiliste.cmo probabiliste.cmi : probabiliste.ml
	ocamlc -I ../../outline/ -c probabiliste.ml

../../outline/outline.cmi : ../../outline/outline.mli
	ocamlc -I ../../outline/ -c ../../outline/outline.mli

../../outline/outline.cmo : ../../outline/outline.ml
	ocamlc -I ../../outline/ -c ../../outline/outline.ml
