(assert (= (store (store ((as const (Array Bool Int)) 0) true 0) false 1) (store (store ((as const (Array Bool Int)) 0) false 1) true 0)))
(check-sat)
(get-model)
