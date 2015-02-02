(defpackage :sjakk3
  (:use :cl)
  (:export :computer-move :human-move))

(defsystem sjakkmotor
  :name "sjakkmotor"
  :version "0.0.2"
  :maintainer "hgjersdal"
  :description "A simple chess engine."
  :components ((:file "building-blocks")
	       (:file "moves" :depends-on ("building-blocks"))
	       (:file "evaluate" :depends-on ("moves"))
	       (:file "search" :depends-on ("evaluate"))
	       (:file "ui" :depends-on ("search"))))

