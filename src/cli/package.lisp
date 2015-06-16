(defpackage #:coleslaw-cli
  (:use #:cl)
  (:import-from #:coleslaw-cli/build
                #:build)
  (:import-from #:coleslaw-cli/clean
                #:clean)
  (:import-from #:coleslaw-cli/rebuild
                #:rebuild)
  (:import-from #:coleslaw-cli/serve
                #:serve)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:command-line-arguments
                #:handle-command-line)
  (:documentation "CLI processing tools.")
  (:export
   #:process-parameters
   #:main))
