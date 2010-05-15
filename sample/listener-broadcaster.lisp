;; class ListenerType<L extends ListenerType<L, B>, B extends BroadcasterType<L, B>> {
;; }
;; class BroadcasterType<L extends ListenerType<L, B>, B extends BroadcasterType<L, B>> {
;; }
;; class Listener extends ListenerType<Listener, Broadcaster> {
;; }
;; class Broadcaster extends BroadcasterType<Listener, Broadcaster> {
;; }

(defclass (<listener-type> (<: L (<listener-type> L B)) (<: B (<broadcaster-type> L B))))
(defclass (<broadcaster-type> (<: L (<listener-type> L B)) (<: B (<broadcaster-type> L B))))
(defclass <listener> ((<listener-type> <listener> <broadcaster>)))
(defclass <broadcaster> ((<broadcaster-type> <listener> <broadcaster>)))
