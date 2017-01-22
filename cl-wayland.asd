;;;; cl-wayland.asd

(asdf:defsystem #:cl-wayland
  :description "libwayland bindings for Common Lisp"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi #:closer-mop)
  :serial t
  :components ((:file "wayland-util")
	       (:file "wayland-server-core")
	       (:file "wayland-server-protocol")
	       (:file "xdg-shell-server-protocol")
	       (:file "zxdg-shell-v6-server-protocol")
	       (:file "waylisp")
	       ))

