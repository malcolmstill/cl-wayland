;;;; cl-wayland.asd

(asdf:defsystem #:cl-wayland
  :description "libwayland bindings for Common Lisp"
  :author "Malcolm Still"
  :license "BSD3"
  :depends-on (#:cffi)
  :serial t
  :components (;;(:file "package")
               ;;(:file "cl-wayland")
	       (:file "wayland-server-core")
	       (:file "wayland-server-protocol")
	       (:file "xdg-shell-server-protocol")
	       ))

