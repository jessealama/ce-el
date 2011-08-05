all: ce

ce: ce-macros.elc ce-unadorned.elc ce-validate.elc ce.elc

%.elc:
	emacs --no-window-system \
	      --no-site-file \
	      --no-init-file \
	      --batch \
	      --eval "(add-to-list 'load-path \".\")" \
	      --funcall batch-byte-compile \
	    $*.el