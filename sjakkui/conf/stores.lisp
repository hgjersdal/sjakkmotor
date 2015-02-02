
(in-package :sjakkui)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *sjakkui-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
                   (asdf-system-directory :sjakkui)))

