
(defpackage :wayland-util
  (:use :common-lisp :cffi)
  (:export
   wl-message
   make-wl-message
   wl-interface
   make-wl-interface
   set-requests
   set-events
   make-wl-types
   offset-types))

(in-package :wayland-util)

(defcstruct wl-interface)

(defcstruct wl-message
  (name :string)
  (signature :string)
  (types (:pointer (:pointer (:struct wl-interface)))))

(defun make-wl-message (&rest methods)
  (let* ((count (length methods))
	 (message-array-ptr (foreign-alloc '(:struct wl-message) :count count)))
    (loop :for method :in methods
       :for index :from 0 :to (- count 1)
       :do (let ((message-ptr (mem-aptr message-array-ptr '(:struct wl-message) index)))
	     (when (not (= (length method) 3))
	       (error "wl-message takes 3 values"))
	     (setf (foreign-slot-value message-ptr '(:struct wl-message) 'name)
		   (first method))
	     (setf (foreign-slot-value message-ptr '(:struct wl-message) 'signature)
		   (second method))
	     (setf (foreign-slot-value message-ptr '(:struct wl-message) 'types)
		   (third method))))
    message-array-ptr))

(defcstruct wl-interface
  (name :string)
  (version :int)
  (method-count :int)
  (methods (:pointer (:struct wl-message)))
  (event-count :int)
  (events (:pointer (:struct wl-message))))

(defun make-wl-interface (name version method-count methods event-count events)
  (let ((interface-ptr (foreign-alloc '(:struct wl-interface))))
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'name) name)
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'version) version)
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'method-count) method-count)
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'methods) methods)
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'event-count) event-count)
    (setf (foreign-slot-value interface-ptr '(:struct wl-interface) 'events) events)
    interface-ptr))

(defun set-requests (interface requests)
  (setf (foreign-slot-value interface '(:struct wl-interface) 'methods) requests))

(defun set-events (interface events)
  (setf (foreign-slot-value interface '(:struct wl-interface) 'events) events))

(defun make-wl-types (&rest types)
  (let* ((count (length types))
	 (types-ptr (foreign-alloc '(:pointer (:struct wl-interface)) :count count)))
    (loop :for type :in types
       :for index :from 0 :to (- count 1)
       :do (setf (mem-aref types-ptr '(:pointer (:struct wl-interface)) index) (nth index types)))
    types-ptr))

(defun offset-types (types offset)
  (mem-aptr types :pointer offset))
