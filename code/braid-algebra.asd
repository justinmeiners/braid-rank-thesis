(defsystem "braid-algebra"
    :version "0.0.1"
    :author "Justin Meiners <justin.meiners@gmail.com>"
    :license "GPL"
    :depends-on ("bordeaux-threads" "lparallel")
    :components (
                (:file "laurent-series")
                (:file "matrix")
                (:file "group")
                (:file "permute")
                (:file "braid" :depends-on ("permute" "group"))
                (:file "lk" :depends-on ("matrix" "permute" "braid"))
                (:file "planar-diagram" :depends-on ("braid" "laurent-series"))
                (:file "rank" :depends-on ("braid" "permute"))
                ))
