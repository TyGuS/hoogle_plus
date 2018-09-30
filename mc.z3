(set-logic HORN)
(declare-fun mc (Int Int) Bool)

(assert (forall ((m Int)) (=> (> m 100) (mc m (- m 10)))))
(assert (mc 10 0))
(check-sat)
(get-model)