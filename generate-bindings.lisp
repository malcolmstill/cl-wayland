(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))

(defpackage :generate-bindings
  (:use :common-lisp :xmls :split-sequence))

(in-package :generate-bindings)

;; (generate-bindings :server "wayland-server" "/usr/share/wayland/wayland.xml" "libwayland-server")
;; (generate-bindings :server "xdg-shell-server" "xdg-shell.xml" "./lib-xdg-shell")

(defconstant nl (coerce '(#\newline) 'string))

(defvar symbols-list nil)

(defun underscore-to-hyphen (string)
  (substitute-if #\- (lambda (c) (equalp #\_ c)) string))

(defun add-to-symbols (symbol)
  (setf symbols-list (cons (underscore-to-hyphen symbol) symbols-list)))

(defun add-space (string)
  (concatenate 'string string nl))

(defun preamble (package symbols path-to-lib)
  (concatenate 'string
	       "(defpackage :" package "-protocol" nl  "(:use :common-lisp :cffi :wayland-server-core)" nl "(:export "
	       (apply #'concatenate 'string (mapcar #'add-space symbols))
	       "))" nl nl
	       "(in-package :" package "-protocol)" nl nl

	       (if path-to-lib
		   (concatenate 'string
				"(define-foreign-library " package nl
				"(t (:default \"" path-to-lib "\")))" nl nl
				  
				"(use-foreign-library " package ")" nl
				)
		   "")))

(defun read-wayland-xml (path)
  (with-open-file (s path)
    (xmls:parse s)))

(defmacro of-type (x type)
  `(and (listp ,x) (stringp (first ,x)) (string= (first ,x) ,type)))

(defun requests-of (interface)
  (remove-if (lambda (x)
	       (not (of-type x "request")))
	     interface))

(defun events-of (interface)
  (remove-if (lambda (x)
	       (not (of-type x "event")))
	     interface))

(defun enums-of (interface)
  (remove-if (lambda (x)
	       (not (of-type x "enum")))
	     interface))

(defun name-of (object)
  (second (find-if (lambda (x)
		     (and (listp x) (stringp (first x)) (string= (first x) "name")))
		   (xmls:node-attrs object))))

(defun name-of-w/space (object)
  (concatenate 'string (name-of object) " "))

(defun type-of-arg (object)
  (second (find-if (lambda (x)
		     (and (listp x) (stringp (first x)) (string= (first x) "type")))
		   (xmls:node-attrs object))))

;; The variable arguments' types are:a
;; - type=uint:	uint32_t
;; - type=int:		int32_t
;; - type=fixed:	wl_fixed_t
;; - type=string:	(const char *) to a nil-terminated string
;; - type=array:	(struct wl_array *)
;; - type=fd:		int, that is an open file descriptor
;; - type=new_id:	(struct wl_object *) or (struct wl_resource *)
;; - type=object:	(struct wl_object *) or (struct wl_resource *)

(defun type-lookup (type)
  (cond
    ((string= type "int") "int32")
    ((string= type "uint") "uint32")
    ((string= type "fixed") "int32") ; need to convert between int32 and fixed
    ((string= type "string") "string")
    ((string= type "array") "pointer")
    ((string= type "fd") "int32")
    ((string= type "new_id") "pointer")
    ((string= type "object") "pointer")))

(defun generate-enum (enum-sxml)
  )

(defun generate-arg (arg-sxml)
  (let ((name (underscore-to-hyphen (name-of arg-sxml))))
    (concatenate 'string  name " ")))

(defun generate-arg-w/type (arg-sxml)
  (let ((name (underscore-to-hyphen (name-of arg-sxml)))
	(type (type-lookup (type-of-arg arg-sxml))))
    (concatenate 'string  ":" type " " name " ")))

(defun generate-callback-arg (arg-sxml)
  (let ((name (underscore-to-hyphen (name-of arg-sxml)))
	(type (type-lookup (type-of-arg arg-sxml))))
    (concatenate 'string "(" name " :" type ")")))

(defun generate-callback-args (request-xml)
  (string-trim '(#\space)
	       (apply #'concatenate 'string (mapcar (lambda (x)
						      (if (of-type x "arg")
							  (generate-callback-arg x)
							  ""))
						    request-xml))))

(defun generate-args (event-xml with-type)
  (string-trim '(#\space)
	       (apply #'concatenate 'string (mapcar (lambda (x)
						      (if (of-type x "arg")
							  (if with-type
							      (generate-arg-w/type x)
							      (generate-arg x))
							  ""))
						    event-xml))))

(defun generate-event (client-or-server name event-sxml opcode)
  (let ((event-name (name-of event-sxml)))
    (add-to-symbols (concatenate 'string name "-send-" event-name))
    (concatenate 'string
		 "(defun " (underscore-to-hyphen name) "-send-" (underscore-to-hyphen event-name)  " (resource " (generate-args event-sxml nil) ")" nl
		 "(wl-resource-post-event resource " (write-to-string  opcode) " " 
		 (generate-args event-sxml t)
		 "))" nl nl)))

(defun generate-events (client-or-server name interface-sxml)
  (let* ((events (events-of interface-sxml))
	 (event-opcodes (loop :for i :from 0 :to (- (length events) 1) :collecting i)))
    (if events
	(apply #'concatenate
	       'string
	       (mapcar (lambda (sxml opcode)
			 (generate-event client-or-server name sxml opcode))
		       events event-opcodes))
	"")))

(defun generate-request (client-or-server request-sxml)
  (let ((request-name (name-of request-sxml)))
    ; Make cstruct
    (concatenate 'string
		 "(" (underscore-to-hyphen request-name) " :pointer)" nl)))

(defun generate-request-make-setfs (interface-name request-sxml)
  (let ((request-name (name-of request-sxml)))
    (concatenate 'string
		 "(setf (foreign-slot-value implementation '(:struct " interface-name "-implementation) " "'" (underscore-to-hyphen request-name) ") (if " (underscore-to-hyphen request-name) " " (underscore-to-hyphen request-name) (generate-empty-callback interface-name request-sxml)"))" nl)))

(defun generate-optional-arg (request)
  (concatenate 'string "(" (underscore-to-hyphen (name-of-w/space request)) "nil) "))

(defun generate-empty-callback (name request-sxml)
  (let ((request-name (underscore-to-hyphen (name-of request-sxml))))
    (concatenate 'string
		 "(get-callback (defcallback empty-" request-name " :void" nl
		 "((client :pointer) (resource :pointer) " (generate-callback-args request-sxml) ")" nl
		; "(format t \"Empty callback to " name "-" request-name "~%\")" nl
		 "))" nl
    )))

(defun generate-requests (client-or-server name interface-sxml)
  (let ((requests (requests-of interface-sxml)))
    (if requests
	(progn
	  (add-to-symbols (concatenate 'string name "_implementation"))
	  (add-to-symbols (concatenate 'string "implement-" name))
	  ; Make implemenation cstruct
	  (concatenate 'string
		       "(defcstruct " (underscore-to-hyphen name) "-implementation" nl
		       (apply #'concatenate
			      'string
			      (mapcar (lambda (sxml)
					(generate-request client-or-server sxml))
				      requests))
		       ")" nl nl
		       "(defun implement-" (underscore-to-hyphen name)
		       " (&key " (apply #'concatenate 'string
					(mapcar #'generate-optional-arg requests))
					;(apply #'concatenate 'string
				 ; (mapcar #'generate-empty-callback
				;	  requests))
		       ")" nl
					  "(let ((implementation (foreign-alloc '(:struct " (underscore-to-hyphen name) "-implementation))))" nl
					  (apply #'concatenate 'string
						 (mapcar (lambda (req)
							   (generate-request-make-setfs (underscore-to-hyphen name) req))
							 requests))
		       nl "implementation))" nl nl
		       ))
	"")))
  
(defun export-interface (name)
  (add-to-symbols (concatenate 'string name "-interface"))
  (concatenate 'string "(defparameter " (underscore-to-hyphen name) "-interface (foreign-symbol-pointer \"" name "_interface\"))" nl))

(defun find-description (interface-sxml)
  (find-if (lambda (x)
	     (of-type x "description"))
	   interface-sxml))

(defun write-description (client-or-server interface description)
  (apply #'concatenate
	 'string
	 (append (list nl ";; Interface " interface nl)
		 (mapcar (lambda (line)
			   (concatenate 'string ";; " line nl))
			 (split-sequence #\newline (first (last description)))))))

(defun generate-interface (client-or-server interface-sxml)
  (let ((name (second (second (second interface-sxml)))))
    (concatenate 'string
		 (write-description client-or-server name (find-description interface-sxml))
		 (export-interface name)
		 (if (equalp client-or-server :client)
		     "To do"
		     (generate-requests client-or-server name interface-sxml))
		 (if (equalp client-or-server :client)
		     "To do"
		     (generate-events client-or-server name interface-sxml)))))

(defun generate-bindings (client-or-server package xml-file &optional (path-to-lib nil))
  (setf symbols-list nil)
  (let* ((wayland.xml (read-wayland-xml xml-file))
	 (code (apply #'concatenate 'string (mapcar (lambda (x)
						      (if (of-type x "interface")
							  (generate-interface client-or-server x)
							  ""))
						    wayland.xml))))
    (with-open-file (s (concatenate 'string package "-protocol.lisp") :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "~A" (preamble package symbols-list path-to-lib))
      (format s "~A" code))))
