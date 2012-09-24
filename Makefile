README.md: src/compile-latex
	pod2markdown $< |perl -p -e  's/(\(#[A-Z ]*\))/lc $$1/eg' > $@
