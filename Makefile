emacs := /Applications/Emacs.app/Contents/MacOS/Emacs

unexport EMACSLOADPATH # evil emacs!

.PHONY : ce install clean clean-emacs-backups clean-compiled-files clean-generated-dependencies

project-prefix := ce

elisp-files := $(project-prefix)-entries \
               $(project-prefix)-macros \
               $(project-prefix)-quotes \
               $(project-prefix)-spell \
               $(project-prefix)-tidy \
               $(project-prefix)-unadorned \
               $(project-prefix)-validate \
               $(project-prefix)-dash \
               $(project-prefix)-utils \
               $(project-prefix)-entities \
               $(project-prefix)-xhtml \
               $(project-prefix)-requires \
               $(project-prefix)
perl-scripts := validate

els := $(addsuffix .el,$(elisp-files))
el-deps := $(addsuffix .deps,$(elisp-files))
elcs := $(addsuffix .elc,$(elisp-files))
pls := $(addsuffix .pl,$(perl-scripts))

files := Makefile .gitignore $(els) $(pls) README.mkdn
emacs-backups := $(addsuffix ~,$(files))

empty :=
install-root = $(if $(HOME),$(HOME),$(empty))
install-dir = $(install-root)/share/emacs/site-lisp/sep

all: $(elcs)

%.deps: %.el
	@set -e; rm -f $@; \
	/bin/echo "$< : " | sed -e 's/\.el/.elc/' | tr -d '\n' > $@; \
	egrep --only-matching "^\(require .+\)" $< \
	  | tr -d "()'" \
	  | sed -e 's/^require //' \
	  | grep '^$(project-prefix)-' \
	  | sed -e 's/$$/.elc/' \
	  | tr '\n' ' ' \
	  >> $@

%.elc: %.el
	@$(emacs) --no-window-system \
                  --no-site-file \
                  --no-init-file \
                  --batch \
                  --directory '.' \
                  --funcall batch-byte-compile $*.el

install: $(elcs) $(els)
	mkdir -p $(install-dir)
	for el in $(els); do cp $$el $(install-dir); done
	for elc in $(elcs); do cp $$elc $(install-dir); done
	@echo
	@echo "Don't forget to add $(install-dir) to your Emacs load path!"
	@echo "In your Emacs initialization file, add"
	@echo
	@echo "  (add-to-list 'load-path \"$(install-dir)\")"
	@echo "  (require 'ce)"
	@echo
	@echo "so that SEP copyeditor mode is loaded whenever you start Emacs."
	@echo "The functionality is located in the 'SEP' menu, which ought to be"
	@echo "visible in your menu bar if the (require 'ce) form was evaluated"
	@echo "without error."
	@echo

uninstall:
	rm -Rf $(install-dir)

clean: clean-emacs-backups clean-compiled-files clean-generated-dependencies

clean-emacs-backups:
	rm -f $(emacs-backups)

clean-compiled-files:
	rm -f $(elcs)

clean-generated-dependencies:
	rm -f $(el-deps)

-include $(el-deps)
