(load "tidyr.asd")
(ql:quickload :tidyr)

(sb-ext:save-lisp-and-die "tidyr" :executable t :toplevel 'tidyr-main)
