MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -classic-display -quiet -no-hygiene
ifdef OBV
OB += -verbose 1
endif

BYTE_TARGETS=src/rc.byte 
NATIVE_TARGETS=src/rc.native

.PHONY: all clean runtime

.DEFAULT_GOAL: all

all: main runtime success

runtime:
	@cd runtime && make -s all && cd ..

main:
	@mkdir -p _build
	@$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS) >_build/.error || true
	@$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

success:
	@echo "=====   SUCCESS   ====="

clean:
	rm -rf _build *.log *.native *.byte Makefile~ runtime/runtime.o
