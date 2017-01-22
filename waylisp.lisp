;; waylisp.lisp
;; This is the beginning of a lispy layer on top
;; of the cffi-heavy cl-wayland

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defpackage :waylisp
  (:use :common-lisp :cffi :wayland-server-core :wayland-server-protocol :xdg-shell-server-protocol :zxdg-shell-v6-server-protocol)
  (:export
   wl-resource
   with-wl-array
   defimplementation
   def-wl-callback
   def-wl-bind
   def-wl-delete
   resources
   keyboard
   pointer
   ->resource
   find-resource
   remove-resource
   get-version
   isurface
   wl-rect
   wl-region
   wl-client
   get-client
   remove-client
   activate
   deactivate
   resize
   accepts-pointer-events?
   x
   y
   width
   height
   origin-x
   origin-y
   scale-x
   scale-y
   effects
   opacity
   operation
   ->region
   rects
   ->client
   regions
   ->pointer
   ->keyboard
   find-region
   client
   ->surface
   ->buffer
   texture
   ->frame-callback
   input-region
   opaque-region
   wl-surface
   subsurfaces
   parent
   keyboard-send-leave
   keyboard-send-enter
   keyboard-send-modifiers))

(in-package :waylisp)

(defparameter *clients* nil)
(defparameter *surfaces* nil) ;; List of all Wayland surfaces

#|
Time for some macro fu. This will greatly simplify the plumbing code.
|#

(defclass wl-client ()
  ((->client :accessor ->client :initarg :->client :initform nil)
   (regions :accessor regions :initarg :regions :initform nil)
   (resources :accessor resources :initargs :resources :initform nil)
   (pointer :accessor pointer :initarg :pointer :initform nil)
   (keyboard :accessor keyboard :initarg :keyboard :initform nil)))

(defmethod print-object ((obj wl-client) out)
  (print-unreadable-object (obj out :type t)
    (format out "@ ~X" (cffi:pointer-address (->client obj)))))

(defun get-client (client-ptr)
  (let ((client (find-if (lambda (client)
			   (cffi:pointer-eq (->client client) client-ptr))
			 *clients*)))
    (if client
	client
	(let ((new-client (make-instance 'wl-client :->client client-ptr)))
	  (push new-client *clients*)
	  new-client))))

(defclass wl-resource ()
  ((->resource :accessor ->resource :initarg :->resource :initform nil)
   (id :accessor id :initarg :id :initform nil)
   (client :accessor client :initarg :client :initform nil)
   (implementation :accessor implementation :initarg :implementation :initform nil)
   (interface :accessor interface :initarg :interface :initform nil)))

(defun get-version (resource)
  (wl-resource-get-version (->resource resource)))

(defmacro def-wl-callback (name (client resource &rest args) &body body)
  (let ((client-ptr (gensym "CLIENT-PTR"))
	(resource-ptr (gensym "RESOURCE-PTR")))
    `(cffi:defcallback ,name :void ((,client-ptr :pointer) (,resource-ptr :pointer) ,@args)
       (let* ((,client (get-client ,client-ptr))
	      (,resource (find-resource ,client (wl-resource-get-user-data ,resource-ptr))))
	 ,@body))))

(defmacro def-wl-bind (name (client &rest args) &body body)
  (let ((client-ptr (gensym "CLIENT-PTR")))
    `(cffi:defcallback ,name :void ((,client-ptr :pointer) ,@args)
       (let* ((,client (get-client ,client-ptr)))
	 ,@body))))

(defmacro def-wl-delete (name (resource) &body body)
  (let ((resource-ptr (gensym "RESOURCE-PTR")))
    `(cffi:defcallback ,name :void ((,resource-ptr :pointer))
       (let* ((,resource (find-resource-all ,resource-ptr)))
	 ,@body))))

(defun find-resource (client ->resource)
  (find-if (lambda (resource)
	     (cffi:pointer-eq (->resource resource) ->resource))
	   (resources client)))

(defun remove-resource (resource)
  (with-slots (client) resource
    (with-slots (resources) client
      (setf resources (remove resource resources)))))

(defun find-resource-all (->resource)
  (loop :for client :in *clients*
     :do (let ((r (find-resource client ->resource)))
	   (when r
	     (return-from find-resource-all r)))))

(cffi:defcallback empty-delete-function :void ((resource :pointer))
    )

(defmacro defimplementation (name (&rest superclasses) (&rest impls) (&rest slots))
  (let ((impl (intern (concatenate 'string (string-upcase (symbol-name name)) "-IMPLEMENTATION")))
	(iface (intern (concatenate 'string (string-upcase (symbol-name name)) "-INTERFACE")))
	(impl-fn (intern (concatenate 'string "IMPLEMENT-" (string-upcase (symbol-name name)))))
	(set-impl-fn (intern (concatenate 'string "SET-IMPLEMENTATION-" (string-upcase (symbol-name name)))))
	(->resource (gensym "->RESOURCE"))
	(bind-fn (intern (concatenate 'string "BIND-" (string-upcase (symbol-name name)))))
	(make-fn (intern (concatenate 'string "MAKE-" (string-upcase (symbol-name name))))))
    `(progn
       (defparameter ,impl nil)
       
       (defclass ,name (wl-resource ,@superclasses)
	 (,@slots))
       
       (defmethod print-object ((obj ,name) out)
	 (print-unreadable-object (obj out :type t)
	   (format out "impl:~s, iface:~s, ~s:~s @ ~X of ~s:~X" (implementation obj) (interface obj)
		   ;;(wl-resource-get-class (->resource obj))
		   (id obj) (wl-resource-get-id (->resource obj)) (cffi:pointer-address (->resource obj)) (client obj) (cffi:pointer-address (wl-resource-get-client (->resource obj)))
		   )))
       
       (defun ,set-impl-fn ()
	 (setf ,impl (,impl-fn
			,@(apply #'concatenate 'list (mapcar (lambda (x)
							       `(,(first x) (cffi:callback ,(second x))))
							     impls)))))

       (defun ,bind-fn (client-ptr version id &key (resource (cffi:null-pointer) supplied) (delete-fn (cffi:callback empty-delete-function)) (implementation? t))
	 (let ((,->resource (wl-resource-create client-ptr;;(->client client)
						,iface
						version
						id)))
	   (when implementation?
	     (wl-resource-set-implementation ,->resource
					     ,impl
					     (if supplied
						 (->resource resource)
						 ,->resource)
					     delete-fn))
	   ,->resource))
       
       (defun ,make-fn (client version id &key (resource (cffi:null-pointer) supplied) (delete-fn (cffi:callback empty-delete-function)) (implementation? t))
	 (let ((,->resource (wl-resource-create (->client client)
						,iface
						version
						id)))
	   (when (not ,impl)
	     (setf ,impl (,impl-fn
			  ,@(apply #'concatenate 'list (mapcar (lambda (x)
								 `(,(first x) (cffi:callback ,(second x))))
							       impls)))))
	   (when implementation?
	     (wl-resource-set-implementation ,->resource
					     ,impl
					     (if supplied
						 (->resource resource)
						 ,->resource)
					     delete-fn))
	   (make-instance ',name
			  :->resource ,->resource
			  :client client
			  :id id
			  :version version
			  :implementation ',impl
			  :interface ',iface))))))

(defmethod initialize-instance :before ((resource wl-resource) &key ->resource client version id implementation interface)
  (setf (->resource resource) ->resource)
  (setf (id resource) id)
  (setf (interface resource) interface)
  (setf (implementation resource) implementation)
  (setf (client resource) client)
  (push resource (resources client)))

(defclass wl-rect ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (operation :accessor operation :initarg :operation :initform nil)))

(defclass isurface ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)
   (width :accessor width :initarg :width :initform 0.0)
   (height :accessor height :initarg :height :initform 0.0)
   (opacity :accessor opacity :initarg :opacity :initform 1.0)
   (scale-x :accessor scale-x :initarg :scale-x :initform 1.0)
   (scale-y :accessor scale-y :initarg :scale-y :initform 1.0)
   (origin-x :accessor origin-x :initarg :origin-x :initform 0.0)
   (origin-y :accessor origin-y :initarg :origin-y :initform 0.0)
   (wl-surface :accessor wl-surface :initarg :wl-surface :initform nil)
   (effects :accessor effects :initarg :effects :initform nil)
   (subsurfaces :accessor subsurfaces :initarg :subsurfaces :initform nil)))

#|
(defun remove-client (client-pointer)
  (let ((client (get-client client-pointer)))
    (loop :for resource :in (resource client)
       (r
    (setf (resources client) nil)
    (setf *clients* (remove-if (lambda (client)
				 (and (pointerp (waylisp:->client client)) (pointer-eq (waylisp:->client client) client-pointer)))
			       *clients*))))
|#

(defun find-region (->region client)
  (find-if (lambda (region)
	     (and (pointerp (->resource region))
		  (pointer-eq (->resource region) ->region)))
	   (regions client)))

#|
(defclass wl-surface ()
  ((->surface :accessor ->surface :initarg :->surface :initform nil)
   (->buffer :accessor ->buffer :initarg :->buffer :initform nil)
   (client :accessor client :initarg :client :initform nil)
   (width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (texture :accessor texture :initarg texture :initform nil)
   (->frame-callback :accessor ->frame-callback :initarg :->frame-callback :initform nil)
   (input-region :accessor input-region :initarg :input-region :initform nil)
   (opaque-region :accessor opaque-region :initarg :opaque-region :initform nil)
   (subsurfaces :accessor subsurfaces :initarg :subsurfaces :initform nil)))
|#

#|
(defclass xdg-surface (wl-surface)
  ((->xdg-surface :accessor ->xdg-surface :initarg :->xdg-surface :initform nil)))

(defun xdg-surface? (surface)
  (eql (class-of surface) (find-class 'xdg-surface)))
|#

(defmacro with-wl-array (array &body body)
  `(let ((,array (foreign-alloc '(:struct wl_array))))
     (wl-array-init ,array)
     ,@body
     (wl-array-release ,array)
     (foreign-free ,array)))

(defmethod activate ((surface (eql nil)) active-surface mods)
  (when active-surface
    (deactivate active-surface))
  surface)

(defmethod activate ((surface isurface) active-surface mods)
  (when active-surface
    (deactivate active-surface))
  (keyboard-send-enter surface)
  (keyboard-send-modifiers surface
			   (first mods)
			   (second mods)
			   (third mods)
			   (fourth mods))
  surface)

#|
(defmethod activate ((surface xdg-surface) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (xdg-surface-send-configure (->xdg-surface surface) 0 0 array time)
    (wl-array-release array)
    (foreign-free array)))
|#

(defmethod deactivate ((surface (eql nil)))
  )

(defmethod deactivate ((surface isurface))
  (keyboard-send-leave surface))

#|
(defmethod deactivate ((surface xdg-surface) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (xdg-surface-send-configure (->xdg-surface surface) 0 0 array time)
    (wl-array-release array)
    (foreign-free array)))
|#

#|
(defmethod resize ((surface xdg-surface) width height time &key (activate? t))
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 3)
    (when activate?
      (setf (mem-aref (wl-array-add array 4) :int32) 4))
    (xdg-surface-send-configure (->xdg-surface surface) (round width) (round height) array time)
    (wl-array-release array)
    (foreign-free array)))
|#

#|
(defclass xdg-popup (wl-surface)
  ((->xdg-popup :accessor ->xdg-popup :initarg :->xdg-popup :initform nil)))

;;; ZXDG

(defclass zxdg-surface (wl-surface)
  ((->zxdg-surface :accessor ->zxdg-surface :initarg :->zxdg-surface :initform nil)))

(defclass zxdg-toplevel (zxdg-surface)
  ((->zxdg-toplevel :accessor ->zxdg-toplevel :initarg :->zxdg-toplevel :initform nil)))

(defun zxdg-toplevel? (surface)
  (eql (class-of surface) (find-class 'zxdg-toplevel)))
|#

#|
(defmethod deactivate ((surface zxdg-toplevel) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)

    (wl-array-release array)
    (foreign-free array)))
|#

#|
(defmethod resize ((surface zxdg-toplevel) width height time &key (activate? t))
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 3)
    (when activate?
      (setf (mem-aref (wl-array-add array 4) :int32) 4))
    (zxdg-toplevel-v6-send-configure (->zxdg-toplevel surface) (round width) (round height) array)
    (zxdg-surface-v6-send-configure (->zxdg-surface surface) 0)
    (wl-array-release array)
    (foreign-free array)))
|#

(defmethod keyboard-send-enter ((surface isurface))
  (when (keyboard (client surface))
    (with-wl-array array
      (wl-array-init array)
      (wl-keyboard-send-enter (->resource (keyboard (client surface)))
			      0
			      (->resource (wl-surface surface)) array))))

(defmethod keyboard-send-modifiers ((surface isurface) depressed latched locked group)
  (when (and (client surface) (keyboard (client surface)))
    (wl-keyboard-send-modifiers (->resource (keyboard (client surface)))
				0
				depressed
				latched
				locked
				group)))
  

(defmethod keyboard-send-leave ((surface isurface))
  (when (and (client surface) (keyboard (client surface)))
    (wl-keyboard-send-leave (->resource (keyboard (client surface))) 0 (->resource (wl-surface surface)))))

#|
(defmethod remove-surface ((surface wl-surface))
  ;; Compositor should implement this to
  ;; decide what to do when a removing a surface
  )

;; REMOVE-SURFACE will always remove it from waylisp's list
(defmethod remove-surface :after ((surface wl-surface))
  (setf *surfaces* (remove surface *surfaces*)))
|#
