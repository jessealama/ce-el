install-dir := $(HOME)/share/emacs/site-lisp/ce
emacs := /Applications/Emacs.app/Contents/MacOS/Emacs

unexport EMACSLOADPATH # evil emacs!

.PHONY : ce install clean

elisp-files := ce-entries \
               ce-macros \
               ce-quotes \
               ce-spell \
               ce-tidy \
               ce-unadorned \
               ce-validate \
               ce-dash \
               ce-utils \
               ce
perl-scripts := validate

els := $(addsuffix .el,$(elisp-files))
elcs := $(addsuffix .elc,$(elisp-files))
pls := $(addsuffix .pl,$(perl-scripts))

files := Makefile $(els) $(pls)
emacs-backups := $(addsuffix ~,$(files))

all: $(elcs)

%.elc: %.el
	@$(emacs) --no-window-system \
                  --no-site-file \
                  --no-init-file \
                  --batch \
                  --directory '.' \
                  --funcall batch-byte-compile $*.el

install: $(elcs)
	install --directory $(install-dir)
	install --mode 644 --target-directory $(install-dir) $(els)
	install --mode 644 --target-directory $(install-dir) $(elcs)
	@echo
	@echo "Don't forget to add $(install-dir) to your Emacs load path!"
	@echo "In your Emacs initialization file, add"
	@echo
	@echo "  (add-to-list 'load-path \"$(install-dir)\")"
	@echo
	@echo "so that SEP copyeditor mode is loaded whenever you start Emacs."

uninstall:
	rm -Rf $(install-dir)

clean:
	rm -f $(emacs-backups)
	rm -f $(elcs)
