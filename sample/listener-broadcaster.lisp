(defclass (<listener-type> (L (<listener-type> L B)) (B (<broadcaster-type> L B))))
(defclass (<broadcaster-type> (L (<listener-type> L B)) (B (<broadcaster-type> L B))))
(defclass <listener> ((<listener-type> <listener> <broadcaster>)))
(defclass <broadcaster> ((<broadcaster-type> <listener> <broadcaster>)))
