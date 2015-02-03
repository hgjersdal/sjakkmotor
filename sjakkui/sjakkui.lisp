
(defpackage #:sjakkui
  (:use :cl :weblocks
        :f-underscore :anaphora
	:sjakk3)
  (:import-from :hunchentoot #:header-in
                #:set-cookie #:set-cookie* #:cookie-in
                #:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :sjakkui)

(export '(start-sjakkui stop-sjakkui))

;; A macro that generates a class or this webapp

(defwebapp sjakkui
    :prefix "/sjakkui/"
    :description "sjakkui: A new application"
    :init-user-session 'sjakkui::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )

;; Top level start & stop scripts

(defun start-sjakkui (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'sjakkui))

(defun stop-sjakkui ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'sjakkui)
  (stop-weblocks))

(start-weblocks :port 8901)
(start-webapp 'sjakkui)
;;(ql:quickload 'sjakkui)
;;(start-sjakkui)
;;(stop-sjakkui)
