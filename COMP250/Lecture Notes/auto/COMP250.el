(TeX-add-style-hook
 "COMP250"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1in") ("xcolor" "dvipsnames") ("cancel" "makeroom")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "hyperref"
    "fancyhdr"
    "setspace"
    "enumerate"
    "amsmath"
    "lastpage"
    "geometry"
    "xcolor"
    "graphicx"
    "amsthm"
    "cancel")
   (TeX-add-symbols
    '("tab" 1)
    "mybox")
   (LaTeX-add-amsthm-newtheorems
    "thm"
    "defn"))
 :latex)

