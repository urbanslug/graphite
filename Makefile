.PHONY: blank
blank:
ifeq ($(wildcard ./bin),)
	@echo "Creating ./bin"
	@mkdir ./bin
endif
	@echo "Compiling raco into ./bin/graphite"
	@raco exe -o ./bin/graphite graphite/graphite.rkt

install:
	@echo "Copying graphite binary into ~./local/usr/bin/"
	@cp ./bin/graphite ~/.local/usr/bin/

clean:
	find . -type d -name "compiled" -exec rm -rf {} +
	rm *.dot *.svg *.gfa *.gv *.gra
	rm -r bin/
