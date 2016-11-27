QL_SETUP=~/quicklisp/setup.lisp

life3d: life3d.lisp life3d.asd package.lisp systems.txt
	buildapp --manifest-file systems.txt \
		 --load-system life3d \
		 --entry life3d::main \
		 --output life3d

systems.txt: 
	sbcl --no-userinit --no-sysinit --non-interactive --load $(QL_SETUP) --eval '(ql:write-asdf-manifest-file "systems.txt")'

