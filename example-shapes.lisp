;;; Classical example of extending an existing module implementing
;;; geometric shapes with () a new datatype () a new operation () both.
;;;
;;; Note that this is for illustration of the module system only.
;;; I'm not sure anyone would actually want to program that way.

(defpackage Shape ()
  (interface
    (defclass <t>)
    (defclass <rect> <: <t>)
    (defclass <circ> <: <t>)
    (defun make-rect (w h -> <rect>))
    (defun make-circ (r -> <circ>))
    (defun draw (<t> x y))))

(defpackage Graphics ()
  (interface
    (defun draw-rect (x y w h))
    (defun draw-circ (x y r))))

(defpackage Shape-Impl ((G Graphics))
  (implementation
    (defclass <t>)
    (defclass <rect> <: <t>
      constructor: (w h)
      w h)
    (defclass <circ> <: <t>
      constructor: (r)
      r)
    (defgeneric draw (<t> x y))
    (defmethod draw (<rect .w .h> x y)
      (G::draw-rect x y w h))
    (defmethod draw (<circ .r> x y)
      (G::draw-rect x y r))))

(defpackage Add-Class-Ext ((S Shape))
  documentation: "Adds a new shape class with an existing method."
  (interface
    (include S)
    (defclass <trans> <: <t>)
    (defun make-trans (<t> dx dy))))

(defpackage Add-Class-Ext-Impl ((SI Shape-Impl))
  (implementation
    (include SI)
    (defclass <trans> <: <t>
      constructor: (<t> dx dy)
      <t> dx dy)
    (defmethod draw (<trans .t .dx .dy> x y)
      (draw t (+ x dx) (+ y dy)))))

(defpackage Make-Add-Class-Ext ((G Graphics) -> Add-Class-Ext)
  (Add-Class-Ext-Impl (Shape-Impl G)))

(defpackage My-Use-Add-Class-Ext ((G Graphics))
  (implementation
    (use S = (Make-Add-Class-Ext G))
    (let* rect = (S::make-rect 10 10)
          trans = (S::make-trans rect 25 25)
      (S::draw trans 0 0))))

(defpackage Add-Method-Ext ((S Shape))
  documentation: "Adds a new method to existing shapes."
  (interface
    (include S)
    (defun area (<t>))))

(defpackage Add-Method-Ext-Impl ((SI Shape-Impl))
  (implementation
    (include SI)
    (defgeneric area (<t>))
    (defmethod area (<rect .w .w>) (* w w))
    (defmethod area (<circ .r>) (* 3.14 r r))))

(defpackage Make-Add-Method-Ext ((G Graphics) -> Add-Method-Ext)
  (Add-Method-Ext-Impl (Shape-Impl G)))

(defpackage My-Use-Add-Method-Ext ((G Graphics))
  (implementation
    (use S = (Make-Add-Method-Ext G))
    (let rect = (S::make-rect 10 10)
      (S::area rect))))

(defpackage Add-Class-And-Method-Ext ((S Shape))
  (interface
    (include (Add-Class-Ext S))
    (include (Add-Method-Ext S))))
  
(defpackage Add-Class-And-Method-Ext-Impl ((SI Shape-Impl))
  (implementation
    (include (Add-Class-Ext-Impl SI))
    (include (Add-Method-Ext-Impl SI))
    (defmethod area (<trans .t>) 
      (area t))))

(defpackage Make-Class-And-Method-Ext ((G Graphics) -> Add-Class-And-Method-Ext)
  (Add-Class-And-Method-Ext-Impl (Shape-Impl G)))

(defpackage My-Use-Extended-Class-And-Method ((G Graphics))
  (implementation
    (use S = (Make-Add-Class-And-Method-Ext G))
    (let rect = (S::make-rect 10 10)
         trans = (S::make-trans rect 25 25)
      (S::area trans))))
