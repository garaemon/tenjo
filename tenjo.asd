(defsystem tenjo
    :depends-on (chimi)
    :components
    ((:file "tenjo")
     (:file "bench" :depends-on ("tenjo")))
    )

