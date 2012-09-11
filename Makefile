CE-INSTALL-DIR=$(HOME)/share/emacs/site-lisp/ce
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

unexport EMACSLOADPATH # evil emacs!

.PHONY : ce install clean emacs-is-real

files = ce-entries ce-macros ce-quotes ce-spell ce-tidy ce-unadorned ce-validate ce
els = $(addsuffix .el,$(files))
emacs-backups = $(addsuffix ~,$(els))
elcs = $(addsuffix .elc,$(files))

all: emacs-is-real $(elcs)

emacs-is-real:
	which $(EMACS)

%.elc: %.el
	$(EMACS) --no-window-system \
                 --no-site-file \
                 --no-init-file \
                 --batch \
                 --directory '.' \
                 --funcall batch-byte-compile $*.el

install: $(elcs)
	install --directory $(CE-INSTALL-DIR)
	install --mode 644 --target-directory $(CE-INSTALL-DIR) $(els)
	install --mode 644 --target-directory $(CE-INSTALL-DIR) $(elcs)
	@echo
	@echo "Don't forget to add $(CE-INSTALL-DIR) to your Emacs load path!"
	@echo "In your Emacs initialization file, add"
	@echo
	@echo "  (add-to-list 'load-path \"$(CE-INSTALL-DIR)\")"
	@echo
	@echo "so that SEP copyeditor mode is loaded whenever you start Emacs."

clean:
	rm -f $(emacs-backups)
	rm -f $(elcs)
