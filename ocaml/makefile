semi_force_brute.exe : outline/outline.cmo algorithme/semi_force_brute/semi_force_brute.cmo
	ocamlc -I outline/ outline.cmo algorithme/semi_force_brute/semi_force_brute.cmo -o algorithme/semi_force_brute/semi_force_brute.exe

algorithme/semi_force_brute/semi_force_brute.cmo : algorithme/semi_force_brute/semi_force_brute.ml
	ocamlc -I outline/ -c algorithme/semi_force_brute/semi_force_brute.ml

probabiliste.exe : outline/outline.cmo algorithme/probabiliste_1/probabiliste.cmo
	ocamlc -I outline/ outline/outline.cmo algorithme/probabiliste_1/probabiliste.cmo -o algorithme/probabiliste_1/probabiliste.exe

algorithme/probabiliste_1/probabiliste.cmo : outline/outline.cmo algorithme/probabiliste_1/probabiliste.ml
	ocamlc -I outline/ -c algorithme/probabiliste_1/probabiliste.ml

outline/outline.cmo : outline/outline.ml outline/outline.cmi
	ocamlc -I outline/ -c outline/outline.ml

outline/outline.cmi : outline/outline.mli
	ocamlc -c outline/outline.mli

clean :
	find . -name "*.cm[iox]" -exec rm {} \;

clean_exe :
	find . -name "*.exe" -exec rm {} \;


