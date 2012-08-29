CE-INSTALL-DIR=$(HOME)/share/emacs/site-lisp/ce
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

unexport EMACSLOADPATH # evil emacs!

all: ce-entries.el ce-macros.el ce-quotes.el ce-spell.el ce-tidy.el ce-unadorned.el ce-validate.el ce.el

%.elc: %.el
	$(EMACS) --no-window-system \
                 --no-site-file \
                 --no-init-file \
                 --batch \
                 --directory '.' \
                 --funcall batch-byte-compile $*.el

install: ce
	mkdir -p $(CE-INSTALL-DIR)
	cp *.el *.elc $(CE-INSTALL-DIR)
	@echo "Don't forget to add $(CE-INSTALL-DIR) to your Emacs load path!"
	@echo "In your Emacs initialization file, add"
	@echo
	@echo "  (add-to-list 'load-path \"$(CE-INSTALL-DIR)\")"
	@echo
	@echo "so that SEP copyeditor mode is loaded whenever you start Emacs."

clean:
	rm -f *.elc
