;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:sjakkui-asd
  (:use :cl :asdf))

(in-package :sjakkui-asd)

(defsystem sjakkui
    :name "sjakkui"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "sjakkui"
    :depends-on (:weblocks :sjakkmotor)
    :components ((:file "sjakkui")
                 (:module conf
                  :components ((:file "stores"))
                  :depends-on ("sjakkui"))
                 (:module src
                  :components ((:file "init-session"))
                  :depends-on ("sjakkui" conf))))

