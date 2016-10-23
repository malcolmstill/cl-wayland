;;
;;
;; (generate-bindings nil 'wayland-server "/usr/share/wayland/wayland.xml" :path-to-lib '("libwayland-server"))
;; (generate-bindings nil 'xdg-shell-server "xdg-shell.xml" :dependencies (list :wayland-server-protocol) :generate-interfaces? t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))

(defpackage :generate-bindings
  (:use :common-lisp :xmls :split-sequence))

(in-package :generate-bindings)

(defclass wl-interface ()
  ((name :accessor name :initarg :name :initform nil)
   (version :accessor version :initarg :version :initform nil)
   (requests :accessor requests :initarg :requests :initform nil)
   (events :accessor events :initarg :events :initform nil)))

(defclass wl-arg ()
  ((name :accessor name :initarg :name :initform nil)
   (arg-type :accessor arg-type :initarg :arg-type :initform nil)
   (interface :accessor interface :initarg :interface :initform nil)
   (nullable? :accessor nullable? :initarg :nullable? :initform nil)))

(defclass wl-roe ()
  ((name :accessor name :initarg :name :initform nil)
   (args :accessor args :initarg :args :initform nil)))

;; Utility function

(defparameter *generate-interfaces* nil)

(defun flatten (list)
  (reverse (reduce (lambda (a b)
		     (cons (second b) (cons (first b) a)))
		   list
		   :initial-value nil)))

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

(defun interfaces-of (protocol)
  (remove-if (lambda (x)
	       (not (of-type x "interface")))
	     protocol))

(defun enums-of (interface)
  (remove-if (lambda (x)
	       (not (of-type x "enum")))
	     interface))

(defun name-of (object)
  (second (find-if (lambda (x)
		     (and (listp x) (stringp (first x)) (string= (first x) "name")))
		   (xmls:node-attrs object))))

(defun version-of (object)
  (parse-integer
   (second (find-if (lambda (x)
		      (and (listp x) (stringp (first x)) (string= (first x) "version")))
		    (xmls:node-attrs object)))))

(defun type-of-arg (object)
  (second (find-if (lambda (x)
		     (and (listp x) (stringp (first x)) (string= (first x) "type")))
		   (xmls:node-attrs object))))

(defun interface-of (object)
  (second (find-if (lambda (x)
		     (and (listp x) (stringp (first x)) (string= (first x) "interface")))
		   (xmls:node-attrs object))))

(defun allow-null (arg-sxml)
  (string=
   (second (find-if (lambda (x)
		      (and (listp x) (stringp (first x)) (string= (first x) "allow-null")))
		    (xmls:node-attrs arg-sxml)))
   "true"))

(defun underscore-to-hyphen (string)
  (string-upcase (substitute-if #\- (lambda (c) (equalp #\_ c)) string)))

(defun lisp-name (&rest rest)
  (intern (underscore-to-hyphen (apply #'concatenate 'string rest))))

#|
(defun export-lisp-name (&rest rest)
  (let ((symbol (intern (underscore-to-hyphen (apply #'concatenate 'string rest)))))
    (add-to-symbols symbol)
    symbol))
|#

(defun lisp-name-keyword (&rest rest)
  (intern (underscore-to-hyphen (apply #'concatenate 'string rest)) :keyword))

(defun type-lookup (type)
  (cond
    ((string= type "int") "INT32")
    ((string= type "uint") "UINT32")
    ((string= type "fixed") "INT32") ; need to convert between int32 and fixed
    ((string= type "string") "STRING")
    ((string= type "array") "POINTER")
    ((string= type "fd") "INT32")
    ((string= type "new_id") "POINTER")
    ((string= type "object") "POINTER")))

(defun signature-lookup (type)
  (cond
    ((string= type "int") "i")
    ((string= type "uint") "u")
    ((string= type "fixed") "f")
    ((string= type "string") "s")
    ((string= type "object") "o")
    ((string= type "new_id") "n")
    ((string= type "array") "a")
    ((string= type "fd") "h")))

;; Functions for reading protocol xml

(defun read-arg (arg-sxml)
  (make-instance 'wl-arg
		 :name (name-of arg-sxml)
		 :arg-type (type-of-arg arg-sxml)
		 :interface (interface-of arg-sxml)
		 :nullable? (allow-null arg-sxml)))

(defun read-args (roe-sxml)
  (remove nil
	  (mapcar (lambda (entry)
		    (when (of-type entry "arg")
		      (read-arg entry)))
		  roe-sxml)))

(defun read-roe (roe-sxml)
  (let* ((name (name-of roe-sxml))
	 (roe (make-instance 'wl-roe
			     :name name
			     :args (read-args roe-sxml))))
    roe))

(defun read-roes (type interface-sxml)
  (remove nil
	  (mapcar (lambda (entry)
		    (when (of-type entry type)
		      (read-roe entry)))
		  interface-sxml)))

(defun read-interface (interface-sxml)
  (let* ((interface-name (name-of interface-sxml))
	 (interface-version (version-of interface-sxml))
	 (interface-entries (xmls:node-children interface-sxml))
	 (interface (make-instance 'wl-interface
				   :name interface-name
				   :version interface-version)))
    (setf (requests interface) (read-roes "request" interface-entries))
    (setf (events interface) (read-roes "event" interface-entries))
    interface))

(defun read-protocol (protocol-sxml)
  (mapcar #'read-interface
	  (interfaces-of (xmls:node-children protocol-sxml))))

;; Functions for generating the lisp code

(defun generate-callback-args (args)
  (mapcar (lambda (arg)
	    (with-slots (name arg-type) arg
	      `(,(lisp-name name) ,(lisp-name-keyword (type-lookup arg-type)))))
	  args))

(defun generate-get-empty-callback (interface-name roe)
  (with-slots (name args) roe
    (let ((callback-name (lisp-name "EMPTY-" interface-name "-" name)))
      `(get-callback ',callback-name))))

(defun generate-empty-callback (interface-name roe)
  (with-slots (name args) roe
    (let ((callback-name (lisp-name "EMPTY-" interface-name "-" name)))
      `(defcallback ,callback-name :void
	 ((client :pointer) (resource :pointer)
	  ,@(generate-callback-args args))))))

(defun generate-implementation-setfs (interface-name implementation implement-name roe)
  (with-slots (name args) roe
    (let ((roe-name (lisp-name name)))
      `(setf (foreign-slot-value ,implementation
				'(:struct ,implement-name)
				',roe-name)
	    (if ,roe-name
		,roe-name
		,(generate-get-empty-callback interface-name roe))))))

(defun generate-optional-arg (roe)
  (with-slots (name) roe
    `(,(lisp-name name) nil)))

(defun generate-struct-entry (roe)
  (with-slots (name) roe
    `(,(lisp-name name) :pointer)))

(defun generate-implementation (interface roes)
  (with-slots (name) interface
    (let (;;(implementation (gensym "IMPLEMENTATION"))
	  (implement-name (lisp-name name "-IMPLEMENTATION"))
	  (implement-func (lisp-name "IMPLEMENT-" name))
	  (implement-func-args (mapcar #'generate-optional-arg roes)))
      `((defcstruct ,implement-name
	    ,@(mapcar #'generate-struct-entry roes))

	,@(mapcar (lambda (roe)
		    (generate-empty-callback name roe))
		  roes)
	
	(defun ,implement-func (&key ,@implement-func-args)
	  (let ((implementation (foreign-alloc '(:struct ,implement-name))))
	    ,@(mapcar (lambda (roe)
			(generate-implementation-setfs name
						       'implementation
						       implement-name
						       roe))
		      roes)
	    implementation))))))

(defun generate-arg (arg)
  (lisp-name (name arg)))

(defun generate-arg-w/type (arg)
  (with-slots (name arg-type) arg
    `(,(lisp-name-keyword (type-lookup arg-type)) ,(lisp-name name))))

(defun generate-args (args with-type)
  (if with-type
      (flatten (mapcar #'generate-arg-w/type args))
      (mapcar #'generate-arg args)))

(defun generate-rpe (interface roe opcode)
  (with-slots (name args) roe
    (let* ((func-name (lisp-name (name interface) "-SEND-" name))
	   (func-args (generate-args args nil))
	   (args-w/types (generate-args args t)))
    `(defun ,func-name (resource ,@func-args)
       (wl-resource-post-event resource ,opcode ,@args-w/types)))))

(defun generate-rpes (interface roes)
  (let* ((opcodes (loop :for i :from 0 :to (- (length roes) 1) :collecting i)))
    (mapcar (lambda (roe opcode)
	      (generate-rpe interface roe opcode))
	    roes opcodes)))

(defun export-interface (interface)
  (let ((interface-name (lisp-name (name interface) "-INTERFACE")))
    (if *generate-interfaces*
	`((defparameter ,interface-name nil))
	`((defparameter ,interface-name
	    (foreign-symbol-pointer ,(concatenate 'string (name interface) "_interface")))))))

(defun generate-server-side (interface)
  (append
   (export-interface interface)
   (generate-implementation interface (requests interface))
   (generate-rpes interface (events interface))))

(defun generate-client-side (interface)
  (append
   (export-interface interface)
   (generate-implementation interface (events interface))
   (generate-rpes interface (requests interface))))

(defun generate-server-protocol (interfaces)
  (apply #'append (mapcar #'generate-server-side interfaces)))

(defun generate-client-protocol (interfaces)
  (apply #'append (mapcar #'generate-client-side interfaces)))

(defun generate-setf-interface (generate-interfaces? interface)
  (with-slots (name version requests events) interface
    (let ((interface-name (lisp-name (name interface) "-INTERFACE")))
      (if generate-interfaces?
	  `(setf ,interface-name (make-wl-interface
				  ,name
				  ,version
				  ,(length requests)
				  (null-pointer)
				  ,(length events)
				  (null-pointer)))
	  `(setf ,interface-name
		 (foreign-symbol-pointer ,(concatenate 'string name "_interface")))))))

(defun replace-type (interfaces)
  (mapcar (lambda (interface)
	    (if interface
		(lisp-name interface "-INTERFACE")
		'(null-pointer)))
	  interfaces))

(defun interfaces-of-interface (interface)
  (replace-type
   (mapcar #'interface
	   (append (apply #'append (mapcar (lambda (request)
					     (args request))
					   (requests interface)))
		   (apply #'append (mapcar (lambda (request)
					     (args request))
					   (events interface)))))))

(defun arg-to-signature (arg)
  (if (nullable? arg)
      (concatenate 'string "?" (signature-lookup (arg-type arg)))
      (signature-lookup (arg-type arg))))

(defun args-to-signature (args)
  (apply #'concatenate 'string (mapcar #'arg-to-signature args)))

(defparameter *offset* 0)

(defun generate-message-entry (roe types)
  (let ((form `(list ,(name roe) ,(args-to-signature (args roe))
		     ,(if (> (length (args roe)) 0)
			  `(offset-types ,types ,*offset*)
			  `,types))))
    (incf *offset* (length (args roe)))
    form))

(defun generate-message (roes types)
  `(make-wl-message ,@(mapcar (lambda (roe)
				(generate-message-entry roe types))
			      roes)))

(defun set-requests-and-events (interface types)
  (let* ((interface-name (lisp-name (name interface) "-INTERFACE")))
    `((set-requests ,interface-name
		    ,(generate-message (requests interface) types))
      (set-events ,interface-name
		  ,(generate-message (events interface) types)))))

(defun generate-interface-init (generate-interfaces? protocol interfaces)
  (let ((name (lisp-name "INITIALIZE-" (symbol-name protocol) "-INTERFACES"))
	(types-name (lisp-name (symbol-name protocol) "-TYPES")))
    (if generate-interfaces?
	(progn
	  (setf *offset* 1)
	  `((defparameter ,types-name nil)
	    
	    (defun ,name ()
	      ,@(mapcar (lambda (interface)
			  (generate-setf-interface generate-interfaces? interface))
			interfaces)
	      
	      (setf ,types-name
		    (make-wl-types
		     (null-pointer)
		     ,@(apply #'append (mapcar #'interfaces-of-interface interfaces))))
	      
	      ,@(apply #'append
		       (mapcar (lambda (interface)
				 (set-requests-and-events interface types-name))
			       interfaces)))))
	`((defun ,name ()
	    ,@(mapcar (lambda (interface)
			(generate-setf-interface generate-interfaces? interface))
			interfaces))))))

;; 

(defun preamble (package symbols path-to-lib dependencies)
  (let ((package-keyword (intern (concatenate 'string
					      (symbol-name package)
					      "-PROTOCOL") :keyword)))
    (remove nil `((defpackage ,package-keyword
		    (:use :common-lisp :cffi :wayland-util :wayland-server-core ,@dependencies)
		    (:export
		     ,@symbols))
		  
		  (in-package ,package-keyword)
		  
		  ,@(when path-to-lib
		      `(,(if (rest path-to-lib)
			    `(define-foreign-library ,package
				(:unix (:or ,@(rest path-to-lib)))
				(t (:default ,(first path-to-lib))))
			    `(define-foreign-library ,package
				(t (:default ,(first path-to-lib)))))

			(use-foreign-library ,package)))))))

;; If we don't have a lib that exports the interface objects
;; we have to build them 

(defun generate-bindings (client? package xml-file &key (path-to-lib nil) (generate-interfaces? nil) (dependencies nil))
  (setf *generate-interfaces* generate-interfaces?)
  (when (and path-to-lib generate-interfaces?)
    (error "Can't provide path-to-lib and generate-interfaces as true"))
  (let* ((protocol.xml (read-wayland-xml xml-file))
	 (protocol (read-protocol protocol.xml))
	 (code (append (if client?
			   (generate-client-protocol protocol)
			   (generate-server-protocol protocol))
		       (generate-interface-init generate-interfaces? package protocol)))
	 (symbols (mapcar #'second code)))
    (with-open-file (s (concatenate 'string (string-downcase (symbol-name package)) "-protocol.lisp") :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop :for sexp :in (preamble package symbols path-to-lib dependencies)
	 :do (format s "~S~%~%" sexp))
      (loop :for sexp :in code
	 :do (format s "~S~%~%" sexp)))))
