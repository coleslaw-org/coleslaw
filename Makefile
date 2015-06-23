APP_NAME   = coleslaw
VERSION_FILE := version.lisp-expr
VERSION    := $(shell cat ${VERSION_FILE})

# use either sbcl or ccl
CL	   = sbcl

# default to 4096 MB of RAM size in the image
DYNSIZE    = 4096

LISP_SRC   = $(wildcard src/*lisp)           \
             $(wildcard src/cli/*lisp)       \
             $(wildcard src/cli/utils/*lisp) \
             coleslaw-cli.asd                \
             coleslaw.asd

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
QLDIR      = $(BUILDDIR)/quicklisp
MANIFEST   = $(BUILDDIR)/manifest.ql

BUILDAPP   = $(BUILDDIR)/bin/buildapp.sbcl
CL_OPTS    = --no-sysinit --no-userinit

BUILDAPP_OPTS = --require sb-posix      \
	--require sb-bsd-sockets            \
	--require sb-rotate-byte

COLESLAW        = $(BUILDDIR)/bin/$(APP_NAME)

clean:
	rm -rf $(LIBS) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(COLESLAW)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(BUILDDIR)/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(BUILDDIR)/quicklisp.lisp                         \
	     --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
	     --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): $(QLDIR)/setup.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
	     --eval '(push "$(PWD)/" asdf:*central-registry*)'    \
	     --eval '(ql:quickload "coleslaw-cli")'               \
	     --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): $(LIBS)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
	     --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")' \
	     --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
	     --eval '(ql:quickload "buildapp")'                   \
	     --eval '(buildapp:build-buildapp "$@")'              \
	     --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(COLESLAW): $(MANIFEST) $(BUILDAPP) $(LISP_SRC)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP) --logfile /tmp/build.log         \
	     $(BUILDAPP_OPTS)                        \
	     --sbcl $(CL)                            \
	     --asdf-path .                           \
	     --manifest-file $(MANIFEST)             \
	     --asdf-tree $(QLDIR)/dists              \
	     --asdf-path .                           \
	     --load-system $(APP_NAME)               \
         --load-system coleslaw-cli              \
	     --entry coleslaw-cli:main               \
	     --output $@


coleslaw: $(COLESLAW) ;
