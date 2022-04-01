(load "tidyr.asd")
(ql:quickload :tidyr)

(asdf:make-build :tidyr
  :type :program
  :move-here #P"./"
  :prologue-code '(require :ecl-quicklisp)
  :epilogue-code '(tidyr-main))
(quit)
