.PHONY: all
all:
	@cd lib && $(MAKE)
	@cd lib/gtk && $(MAKE) -f Makefile
	@cd test/unit && $(MAKE)
	@cd test/visual && $(MAKE)
	@cd test/gui && $(MAKE)
	@cd test/speed && $(MAKE)
	@cd examples/minimal && $(MAKE)
	@cd examples/gui_minimal && $(MAKE)
	@cd examples/gui_dynamic && $(MAKE)

.PHONY:	install
install:
	@cd lib && $(MAKE) $@
	@cd lib/gtk && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@
	@cd lib/gtk && $(MAKE) $@

.PHONY:	clean
clean:
	@cd lib && $(MAKE) $@
	@cd lib/gtk && $(MAKE) $@
	@cd test/unit && $(MAKE) $@
	@cd test/visual && $(MAKE) $@
	@cd test/gui && $(MAKE) $@
	@cd test/speed && $(MAKE) $@
	@cd examples/minimal && $(MAKE) $@
	@cd examples/gui_minimal && $(MAKE) $@
	@cd examples/gui_dynamic && $(MAKE) $@


.PHONY:	check
check:
	@cd lib && $(MAKE) $@
	@cd lib/gtk && $(MAKE) $@
	@cd test/unit && $(MAKE) $@
	@cd test/visual && $(MAKE) $@
	@cd test/gui && $(MAKE) $@
	@cd test/speed && $(MAKE) $@
