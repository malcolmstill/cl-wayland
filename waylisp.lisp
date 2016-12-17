;; waylisp.lisp
;; This is the beginning of a lispy layer on top
;; of the cffi-heavy cl-wayland

(defpackage :waylisp
  (:use :common-lisp :cffi :wayland-server-core :wayland-server-protocol :xdg-shell-server-protocol :zxdg-shell-server-protocol)
  (:export
   wl-rect
   wl-region
   wl-client
   get-client
   remove-client
   wl-surface
   wl-subsurface
   wl-cursor
   wl-cursor?
   xdg-surface
   xdg-surface?
   zxdg-surface
   zxdg-surface?
   zxdg-toplevel
   zxdg-toplevel?
   zxdg-popup
   zxdg-popup?
   activate
   deactivate
   resize
   xdg-popup
   accepts-pointer-events?
   x
   y
   width
   height
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
   subsurfaces
   ->xdg-surface
   ->xdg-toplevel
   ->xdg-popup
   ->zxdg-surface
   ->zxdg-toplevel
   parent
   hotspot-x
   hotspot-y
   keyboard-send-leave
   keyboard-send-enter
   keyboard-send-modifiers))

(in-package :waylisp)

(defparameter *clients* nil)
(defparameter *surfaces* nil) ;; List of all Wayland surfaces

(defclass wl-rect ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (operation :accessor operation :initarg :operation :initform nil)))

(defclass wl-region ()
  ((->region :accessor ->region :initarg :->region :initform nil)
   (rects :accessor rects :initarg :rects :initform nil)))

(defclass wl-client ()
  ((->client :accessor ->client :initarg :->client :initform nil)
   (regions :accessor regions :initarg :regions :initform nil)
   (->pointer :accessor ->pointer :initarg :->pointer :initform nil)
   (->keyboard :accessor ->keyboard :initarg :->keyboard :initform nil)))

(defun get-client (client-ptr)
  (let ((client (find-if (lambda (client)
			   (pointer-eq (->client client) client-ptr))
			 *clients*)))
    (if client
	client
	(let ((new-client (make-instance 'wl-client :->client client-ptr)))
	  (push new-client *clients*)
	  new-client))))

(defun remove-client (client-pointer)
  (let ((client (get-client client-pointer)))
    (setf *clients* (remove-if (lambda (client)
				 (and (pointerp (waylisp:->client client)) (pointer-eq (waylisp:->client client) client-pointer)))
			       *clients*))))

(defun find-region (->region client)
  (find-if (lambda (region)
	     (and (pointerp (->region region))
		  (pointer-eq (->region region) ->region)))
	   (regions client)))

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

(defclass xdg-surface (wl-surface)
  ((->xdg-surface :accessor ->xdg-surface :initarg :->xdg-surface :initform nil)))

(defun xdg-surface? (surface)
  (eql (class-of surface) (find-class 'xdg-surface)))

(defmethod activate ((surface xdg-surface) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (xdg-surface-send-configure (->xdg-surface surface) 0 0 array time)
    (wl-array-release array)
    (foreign-free array)))

(defmethod deactivate ((surface xdg-surface) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (xdg-surface-send-configure (->xdg-surface surface) 0 0 array time)
    (wl-array-release array)
    (foreign-free array)))

(defmethod resize ((surface xdg-surface) width height time &key (activate? t))
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 3)
    (when activate?
      (setf (mem-aref (wl-array-add array 4) :int32) 4))
    (xdg-surface-send-configure (->xdg-surface surface) (round width) (round height) array time)
    (wl-array-release array)
    (foreign-free array)))

(defclass xdg-popup (wl-surface)
  ((->xdg-popup :accessor ->xdg-popup :initarg :->xdg-popup :initform nil)))

;;; ZXDG

(defclass zxdg-surface (wl-surface)
  ((->zxdg-surface :accessor ->zxdg-surface :initarg :->zxdg-surface :initform nil)))

(defclass zxdg-toplevel (zxdg-surface)
  ((->zxdg-toplevel :accessor ->zxdg-toplevel :initarg :->zxdg-toplevel :initform nil)))

(defun zxdg-toplevel? (surface)
  (eql (class-of surface) (find-class 'zxdg-toplevel)))

(defmethod activate ((surface zxdg-toplevel) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (setf (mem-aref (wl-array-add array 4) :int32) 4)
    (zxdg-toplevel-v6-send-configure (->zxdg-toplevel surface) 0 0 array)
    (zxdg-surface-v6-send-configure (->zxdg-surface surface) 0)
    (wl-array-release array)
    (foreign-free array)))

(defmethod deactivate ((surface zxdg-toplevel) time)
  (let ((array (foreign-alloc '(:struct wl_array))))
    (wl-array-init array)
    (zxdg-toplevel-v6-send-configure (->zxdg-toplevel surface) 0 0 array)
    (zxdg-surface-v6-send-configure (->zxdg-surface surface) 0)
    (wl-array-release array)
    (foreign-free array)))

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

;; WL-SUBSURFACE

(defclass wl-subsurface (wl-surface)
  ((->subsurface :accessor ->subsurface :initarg :->subsurface :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)))

(defclass wl-cursor (wl-surface)
  ((hotspot-x :accessor hotspot-x :initarg :hotspot-x :initform nil)
   (hotspot-y :accessor hotspot-y :initarg :hotspot-y :initform nil)))

(defun wl-cursor? (surface)
  (eql (class-of surface) (find-class 'wl-cursor)))

(defun class-or-subclass-of (object class)
  (or 
   (eql (class-of object) (find-class class)) ;; class
   (find (find-class class) (closer-mop:class-direct-superclasses (class-of object)))))

(defmethod accepts-pointer-events? ((surface wl-surface))
  nil)

;(defmethod accepts-pointer-events? ((surface wl-shell))
;  (and (client surface) (->pointer (client surface))))

(defmethod accepts-pointer-events? ((surface xdg-surface))
  (and (client surface) (->pointer (client surface))))

(defmethod accepts-pointer-events? ((surface zxdg-surface))
  (and (client surface) (->pointer (client surface))))

(defmethod accepts-pointer-events? ((surface wl-cursor))
  nil)

(defmethod accepts-pointer-events? ((surface (eql nil)))
  nil)

(defmethod keyboard-send-enter ((surface wl-surface))
  (when (and (client surface) (->keyboard (client surface)))
    (let ((array (foreign-alloc '(:struct wl_array))))
      (wl-array-init array)
      (when (->keyboard (client surface))
	(wl-keyboard-send-enter (->keyboard (client surface)) 0 (->surface surface) array))
      (wl-array-release array)
      (foreign-free array))))

(defmethod keyboard-send-modifiers ((surface wl-surface) depressed latched locked group)
  (when (and (client surface) (->keyboard (client surface)))
    (wl-keyboard-send-modifiers (->keyboard (client surface))
				0
				depressed
				latched
				locked
				group)))
  

(defmethod keyboard-send-leave ((surface wl-surface))
  (when (and (client surface) (->keyboard (client surface)))
    (wl-keyboard-send-leave (->keyboard (client surface)) 0 (->surface surface))))

#|
(defmethod remove-surface ((surface wl-surface))
  ;; Compositor should implement this to
  ;; decide what to do when a removing a surface
  )

;; REMOVE-SURFACE will always remove it from waylisp's list
(defmethod remove-surface :after ((surface wl-surface))
  (setf *surfaces* (remove surface *surfaces*)))
|#
