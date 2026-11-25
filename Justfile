# Run ERT with the installed Typst Tree-sitter grammar.
test:
    emacs -Q --batch -L . -l test/touchtypst-tests.el -l test/org-touchtypst-tests.el -f ert-run-tests-batch-and-exit

# Reload the core and, if already loaded, the Org adapter in the running Emacs server.
load:
    emacsclient --alternate-editor=false --eval '(progn (load-file "./touchtypst-parse.el") (load-file "./touchtypst.el") (when (featurep (quote org-touchtypst)) (load-file "./org-touchtypst.el")))'
