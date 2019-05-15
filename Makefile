OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep


################################################################################

INTERFACES=cal.mli
SOURCES=cal.ml test.ml

complet: $(INTERFACES) $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(SOURCES:.ml=.cmo)

################################################################################

# Règles de compilation exécutées à chaque make
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

################################################################################

# Nettoyage des fichiers temporaires
clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f run

include .depend
