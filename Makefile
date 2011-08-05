all: ce

ce: ce-macros.elc ce-unadorned.elc ce-validate.elc ce.elc

%.elc:
	emacs --no-window-system \
	      --no-site-file \
	      --no-init-file \
	      --batch \
	      --directory '.' \
	      --funcall batch-byte-compile \
	    $*.el

clean:
	rm *.elc
