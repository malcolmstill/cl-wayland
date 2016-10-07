(defpackage :wayland-server-protocol
(:use :common-lisp :cffi :wayland-server-core)
(:export implement-wl-subsurface
wl-subsurface-implementation
wl-subsurface-interface
implement-wl-subcompositor
wl-subcompositor-implementation
wl-subcompositor-interface
implement-wl-region
wl-region-implementation
wl-region-interface
wl-output-send-scale
wl-output-send-done
wl-output-send-mode
wl-output-send-geometry
wl-output-interface
wl-touch-send-cancel
wl-touch-send-frame
wl-touch-send-motion
wl-touch-send-up
wl-touch-send-down
implement-wl-touch
wl-touch-implementation
wl-touch-interface
wl-keyboard-send-repeat-info
wl-keyboard-send-modifiers
wl-keyboard-send-key
wl-keyboard-send-leave
wl-keyboard-send-enter
wl-keyboard-send-keymap
implement-wl-keyboard
wl-keyboard-implementation
wl-keyboard-interface
wl-pointer-send-axis-discrete
wl-pointer-send-axis-stop
wl-pointer-send-axis-source
wl-pointer-send-frame
wl-pointer-send-axis
wl-pointer-send-button
wl-pointer-send-motion
wl-pointer-send-leave
wl-pointer-send-enter
implement-wl-pointer
wl-pointer-implementation
wl-pointer-interface
wl-seat-send-name
wl-seat-send-capabilities
implement-wl-seat
wl-seat-implementation
wl-seat-interface
wl-surface-send-leave
wl-surface-send-enter
implement-wl-surface
wl-surface-implementation
wl-surface-interface
wl-shell-surface-send-popup-done
wl-shell-surface-send-configure
wl-shell-surface-send-ping
implement-wl-shell-surface
wl-shell-surface-implementation
wl-shell-surface-interface
implement-wl-shell
wl-shell-implementation
wl-shell-interface
implement-wl-data-device-manager
wl-data-device-manager-implementation
wl-data-device-manager-interface
wl-data-device-send-selection
wl-data-device-send-drop
wl-data-device-send-motion
wl-data-device-send-leave
wl-data-device-send-enter
wl-data-device-send-data-offer
implement-wl-data-device
wl-data-device-implementation
wl-data-device-interface
wl-data-source-send-action
wl-data-source-send-dnd-finished
wl-data-source-send-dnd-drop-performed
wl-data-source-send-cancelled
wl-data-source-send-send
wl-data-source-send-target
implement-wl-data-source
wl-data-source-implementation
wl-data-source-interface
wl-data-offer-send-action
wl-data-offer-send-source-actions
wl-data-offer-send-offer
implement-wl-data-offer
wl-data-offer-implementation
wl-data-offer-interface
wl-buffer-send-release
implement-wl-buffer
wl-buffer-implementation
wl-buffer-interface
wl-shm-send-format
implement-wl-shm
wl-shm-implementation
wl-shm-interface
implement-wl-shm-pool
wl-shm-pool-implementation
wl-shm-pool-interface
implement-wl-compositor
wl-compositor-implementation
wl-compositor-interface
wl-callback-send-done
wl-callback-interface
wl-registry-send-global-remove
wl-registry-send-global
implement-wl-registry
wl-registry-implementation
wl-registry-interface
wl-display-send-delete-id
wl-display-send-error
implement-wl-display
wl-display-implementation
wl-display-interface
))

(in-package :wayland-server-protocol)

;(define-foreign-library wayland-server
;(t (:default "libwayland-server")))

(load-foreign-library 'wayland-server-core::wayland-server)

;; Interface wl_display
;; The core global object.  This is a special singleton object.  It
;;       is used for internal Wayland protocol features.
(defparameter wl-display-interface (foreign-symbol-pointer "wl_display_interface"))
(defcstruct wl-display-implementation
(sync :pointer)
(get-registry :pointer)
)

(defun implement-wl-display (&key (sync nil) (get-registry nil) )
(let ((implementation (foreign-alloc '(:struct wl-display-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-display-implementation) 'sync) (if sync sync(get-callback (defcallback empty-sync :void
((client :pointer) (resource :pointer) (callback :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-display-implementation) 'get-registry) (if get-registry get-registry(get-callback (defcallback empty-get-registry :void
((client :pointer) (resource :pointer) (registry :pointer))
))
))

implementation))

(defun wl-display-send-error (resource object-id code message)
(wl-resource-post-event resource 0 :pointer object-id :uint32 code :string message))

(defun wl-display-send-delete-id (resource id)
(wl-resource-post-event resource 1 :uint32 id))


;; Interface wl_registry
;; The global registry object.  The server has a number of global
;;       objects that are available to all clients.  These objects
;;       typically represent an actual object in the server (for example,
;;       an input device) or they are singleton objects that provide
;;       extension functionality.
;; 
;;       When a client creates a registry object, the registry object
;;       will emit a global event for each global currently in the
;;       registry.  Globals come and go as a result of device or
;;       monitor hotplugs, reconfiguration or other events, and the
;;       registry will send out global and global_remove events to
;;       keep the client up to date with the changes.  To mark the end
;;       of the initial burst of events, the client can use the
;;       wl_display.sync request immediately after calling
;;       wl_display.get_registry.
;; 
;;       A client can bind to a global object by using the bind
;;       request.  This creates a client-side handle that lets the object
;;       emit events to the client and lets the client invoke requests on
;;       the object.
(defparameter wl-registry-interface (foreign-symbol-pointer "wl_registry_interface"))
(defcstruct wl-registry-implementation
(bind :pointer)
)

(defun implement-wl-registry (&key (bind nil) )
(let ((implementation (foreign-alloc '(:struct wl-registry-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-registry-implementation) 'bind) (if bind bind(get-callback (defcallback empty-bind :void
((client :pointer) (resource :pointer) (name :uint32)(id :pointer))
))
))

implementation))

(defun wl-registry-send-global (resource name interface version)
(wl-resource-post-event resource 0 :uint32 name :string interface :uint32 version))

(defun wl-registry-send-global-remove (resource name)
(wl-resource-post-event resource 1 :uint32 name))


;; Interface wl_callback
;; Clients can handle the 'done' event to get notified when
;;       the related request is done.
(defparameter wl-callback-interface (foreign-symbol-pointer "wl_callback_interface"))
(defun wl-callback-send-done (resource callback-data)
(wl-resource-post-event resource 0 :uint32 callback-data))


;; Interface wl_compositor
;; A compositor.  This object is a singleton global.  The
;;       compositor is in charge of combining the contents of multiple
;;       surfaces into one displayable output.
(defparameter wl-compositor-interface (foreign-symbol-pointer "wl_compositor_interface"))
(defcstruct wl-compositor-implementation
(create-surface :pointer)
(create-region :pointer)
)

(defun implement-wl-compositor (&key (create-surface nil) (create-region nil) )
(let ((implementation (foreign-alloc '(:struct wl-compositor-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-compositor-implementation) 'create-surface) (if create-surface create-surface(get-callback (defcallback empty-create-surface :void
((client :pointer) (resource :pointer) (id :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-compositor-implementation) 'create-region) (if create-region create-region(get-callback (defcallback empty-create-region :void
((client :pointer) (resource :pointer) (id :pointer))
))
))

implementation))


;; Interface wl_shm_pool
;; The wl_shm_pool object encapsulates a piece of memory shared
;;       between the compositor and client.  Through the wl_shm_pool
;;       object, the client can allocate shared memory wl_buffer objects.
;;       All objects created through the same pool share the same
;;       underlying mapped memory. Reusing the mapped memory avoids the
;;       setup/teardown overhead and is useful when interactively resizing
;;       a surface or for many small buffers.
(defparameter wl-shm-pool-interface (foreign-symbol-pointer "wl_shm_pool_interface"))
(defcstruct wl-shm-pool-implementation
(create-buffer :pointer)
(destroy :pointer)
(resize :pointer)
)

(defun implement-wl-shm-pool (&key (create-buffer nil) (destroy nil) (resize nil) )
(let ((implementation (foreign-alloc '(:struct wl-shm-pool-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-shm-pool-implementation) 'create-buffer) (if create-buffer create-buffer(get-callback (defcallback empty-create-buffer :void
((client :pointer) (resource :pointer) (id :pointer)(offset :int32)(width :int32)(height :int32)(stride :int32)(format :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shm-pool-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-shm-pool-implementation) 'resize) (if resize resize(get-callback (defcallback empty-resize :void
((client :pointer) (resource :pointer) (size :int32))
))
))

implementation))


;; Interface wl_shm
;; A global singleton object that provides support for shared
;;       memory.
;; 
;;       Clients can create wl_shm_pool objects using the create_pool
;;       request.
;; 
;;       At connection setup time, the wl_shm object emits one or more
;;       format events to inform clients about the valid pixel formats
;;       that can be used for buffers.
(defparameter wl-shm-interface (foreign-symbol-pointer "wl_shm_interface"))
(defcstruct wl-shm-implementation
(create-pool :pointer)
)

(defun implement-wl-shm (&key (create-pool nil) )
(let ((implementation (foreign-alloc '(:struct wl-shm-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-shm-implementation) 'create-pool) (if create-pool create-pool(get-callback (defcallback empty-create-pool :void
((client :pointer) (resource :pointer) (id :pointer)(fd :int32)(size :int32))
))
))

implementation))

(defun wl-shm-send-format (resource format)
(wl-resource-post-event resource 0 :uint32 format))


;; Interface wl_buffer
;; A buffer provides the content for a wl_surface. Buffers are
;;       created through factory interfaces such as wl_drm, wl_shm or
;;       similar. It has a width and a height and can be attached to a
;;       wl_surface, but the mechanism by which a client provides and
;;       updates the contents is defined by the buffer factory interface.
(defparameter wl-buffer-interface (foreign-symbol-pointer "wl_buffer_interface"))
(defcstruct wl-buffer-implementation
(destroy :pointer)
)

(defun implement-wl-buffer (&key (destroy nil) )
(let ((implementation (foreign-alloc '(:struct wl-buffer-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-buffer-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-buffer-send-release (resource )
(wl-resource-post-event resource 0 ))


;; Interface wl_data_offer
;; A wl_data_offer represents a piece of data offered for transfer
;;       by another client (the source client).  It is used by the
;;       copy-and-paste and drag-and-drop mechanisms.  The offer
;;       describes the different mime types that the data can be
;;       converted to and provides the mechanism for transferring the
;;       data directly from the source client.
(defparameter wl-data-offer-interface (foreign-symbol-pointer "wl_data_offer_interface"))
(defcstruct wl-data-offer-implementation
(accept :pointer)
(receive :pointer)
(destroy :pointer)
(finish :pointer)
(set-actions :pointer)
)

(defun implement-wl-data-offer (&key (accept nil) (receive nil) (destroy nil) (finish nil) (set-actions nil) )
(let ((implementation (foreign-alloc '(:struct wl-data-offer-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-data-offer-implementation) 'accept) (if accept accept(get-callback (defcallback empty-accept :void
((client :pointer) (resource :pointer) (serial :uint32)(mime-type :string))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-offer-implementation) 'receive) (if receive receive(get-callback (defcallback empty-receive :void
((client :pointer) (resource :pointer) (mime-type :string)(fd :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-offer-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-offer-implementation) 'finish) (if finish finish(get-callback (defcallback empty-finish :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-offer-implementation) 'set-actions) (if set-actions set-actions(get-callback (defcallback empty-set-actions :void
((client :pointer) (resource :pointer) (dnd-actions :uint32)(preferred-action :uint32))
))
))

implementation))

(defun wl-data-offer-send-offer (resource mime-type)
(wl-resource-post-event resource 0 :string mime-type))

(defun wl-data-offer-send-source-actions (resource source-actions)
(wl-resource-post-event resource 1 :uint32 source-actions))

(defun wl-data-offer-send-action (resource dnd-action)
(wl-resource-post-event resource 2 :uint32 dnd-action))


;; Interface wl_data_source
;; The wl_data_source object is the source side of a wl_data_offer.
;;       It is created by the source client in a data transfer and
;;       provides a way to describe the offered data and a way to respond
;;       to requests to transfer the data.
(defparameter wl-data-source-interface (foreign-symbol-pointer "wl_data_source_interface"))
(defcstruct wl-data-source-implementation
(offer :pointer)
(destroy :pointer)
(set-actions :pointer)
)

(defun implement-wl-data-source (&key (offer nil) (destroy nil) (set-actions nil) )
(let ((implementation (foreign-alloc '(:struct wl-data-source-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-data-source-implementation) 'offer) (if offer offer(get-callback (defcallback empty-offer :void
((client :pointer) (resource :pointer) (mime-type :string))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-source-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-source-implementation) 'set-actions) (if set-actions set-actions(get-callback (defcallback empty-set-actions :void
((client :pointer) (resource :pointer) (dnd-actions :uint32))
))
))

implementation))

(defun wl-data-source-send-target (resource mime-type)
(wl-resource-post-event resource 0 :string mime-type))

(defun wl-data-source-send-send (resource mime-type fd)
(wl-resource-post-event resource 1 :string mime-type :int32 fd))

(defun wl-data-source-send-cancelled (resource )
(wl-resource-post-event resource 2 ))

(defun wl-data-source-send-dnd-drop-performed (resource )
(wl-resource-post-event resource 3 ))

(defun wl-data-source-send-dnd-finished (resource )
(wl-resource-post-event resource 4 ))

(defun wl-data-source-send-action (resource dnd-action)
(wl-resource-post-event resource 5 :uint32 dnd-action))


;; Interface wl_data_device
;; There is one wl_data_device per seat which can be obtained
;;       from the global wl_data_device_manager singleton.
;; 
;;       A wl_data_device provides access to inter-client data transfer
;;       mechanisms such as copy-and-paste and drag-and-drop.
(defparameter wl-data-device-interface (foreign-symbol-pointer "wl_data_device_interface"))
(defcstruct wl-data-device-implementation
(start-drag :pointer)
(set-selection :pointer)
(release :pointer)
)

(defun implement-wl-data-device (&key (start-drag nil) (set-selection nil) (release nil) )
(let ((implementation (foreign-alloc '(:struct wl-data-device-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-data-device-implementation) 'start-drag) (if start-drag start-drag(get-callback (defcallback empty-start-drag :void
((client :pointer) (resource :pointer) (source :pointer)(origin :pointer)(icon :pointer)(serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-device-implementation) 'set-selection) (if set-selection set-selection(get-callback (defcallback empty-set-selection :void
((client :pointer) (resource :pointer) (source :pointer)(serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-device-implementation) 'release) (if release release(get-callback (defcallback empty-release :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-data-device-send-data-offer (resource id)
(wl-resource-post-event resource 0 :pointer id))

(defun wl-data-device-send-enter (resource serial surface x y id)
(wl-resource-post-event resource 1 :uint32 serial :pointer surface :int32 x :int32 y :pointer id))

(defun wl-data-device-send-leave (resource )
(wl-resource-post-event resource 2 ))

(defun wl-data-device-send-motion (resource time x y)
(wl-resource-post-event resource 3 :uint32 time :int32 x :int32 y))

(defun wl-data-device-send-drop (resource )
(wl-resource-post-event resource 4 ))

(defun wl-data-device-send-selection (resource id)
(wl-resource-post-event resource 5 :pointer id))


;; Interface wl_data_device_manager
;; The wl_data_device_manager is a singleton global object that
;;       provides access to inter-client data transfer mechanisms such as
;;       copy-and-paste and drag-and-drop.  These mechanisms are tied to
;;       a wl_seat and this interface lets a client get a wl_data_device
;;       corresponding to a wl_seat.
;; 
;;       Depending on the version bound, the objects created from the bound
;;       wl_data_device_manager object will have different requirements for
;;       functioning properly. See wl_data_source.set_actions,
;;       wl_data_offer.accept and wl_data_offer.finish for details.
(defparameter wl-data-device-manager-interface (foreign-symbol-pointer "wl_data_device_manager_interface"))
(defcstruct wl-data-device-manager-implementation
(create-data-source :pointer)
(get-data-device :pointer)
)

(defun implement-wl-data-device-manager (&key (create-data-source nil) (get-data-device nil) )
(let ((implementation (foreign-alloc '(:struct wl-data-device-manager-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-data-device-manager-implementation) 'create-data-source) (if create-data-source create-data-source(get-callback (defcallback empty-create-data-source :void
((client :pointer) (resource :pointer) (id :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-data-device-manager-implementation) 'get-data-device) (if get-data-device get-data-device(get-callback (defcallback empty-get-data-device :void
((client :pointer) (resource :pointer) (id :pointer)(seat :pointer))
))
))

implementation))


;; Interface wl_shell
;; This interface is implemented by servers that provide
;;       desktop-style user interfaces.
;; 
;;       It allows clients to associate a wl_shell_surface with
;;       a basic surface.
(defparameter wl-shell-interface (foreign-symbol-pointer "wl_shell_interface"))
(defcstruct wl-shell-implementation
(get-shell-surface :pointer)
)

(defun implement-wl-shell (&key (get-shell-surface nil) )
(let ((implementation (foreign-alloc '(:struct wl-shell-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-shell-implementation) 'get-shell-surface) (if get-shell-surface get-shell-surface(get-callback (defcallback empty-get-shell-surface :void
((client :pointer) (resource :pointer) (id :pointer)(surface :pointer))
))
))

implementation))


;; Interface wl_shell_surface
;; An interface that may be implemented by a wl_surface, for
;;       implementations that provide a desktop-style user interface.
;; 
;;       It provides requests to treat surfaces like toplevel, fullscreen
;;       or popup windows, move, resize or maximize them, associate
;;       metadata like title and class, etc.
;; 
;;       On the server side the object is automatically destroyed when
;;       the related wl_surface is destroyed. On the client side,
;;       wl_shell_surface_destroy() must be called before destroying
;;       the wl_surface object.
(defparameter wl-shell-surface-interface (foreign-symbol-pointer "wl_shell_surface_interface"))
(defcstruct wl-shell-surface-implementation
(pong :pointer)
(move :pointer)
(resize :pointer)
(set-toplevel :pointer)
(set-transient :pointer)
(set-fullscreen :pointer)
(set-popup :pointer)
(set-maximized :pointer)
(set-title :pointer)
(set-class :pointer)
)

(defun implement-wl-shell-surface (&key (pong nil) (move nil) (resize nil) (set-toplevel nil) (set-transient nil) (set-fullscreen nil) (set-popup nil) (set-maximized nil) (set-title nil) (set-class nil) )
(let ((implementation (foreign-alloc '(:struct wl-shell-surface-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'pong) (if pong pong(get-callback (defcallback empty-pong :void
((client :pointer) (resource :pointer) (serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'move) (if move move(get-callback (defcallback empty-move :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'resize) (if resize resize(get-callback (defcallback empty-resize :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32)(edges :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-toplevel) (if set-toplevel set-toplevel(get-callback (defcallback empty-set-toplevel :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-transient) (if set-transient set-transient(get-callback (defcallback empty-set-transient :void
((client :pointer) (resource :pointer) (parent :pointer)(x :int32)(y :int32)(flags :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-fullscreen) (if set-fullscreen set-fullscreen(get-callback (defcallback empty-set-fullscreen :void
((client :pointer) (resource :pointer) (method :uint32)(framerate :uint32)(output :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-popup) (if set-popup set-popup(get-callback (defcallback empty-set-popup :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32)(parent :pointer)(x :int32)(y :int32)(flags :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-maximized) (if set-maximized set-maximized(get-callback (defcallback empty-set-maximized :void
((client :pointer) (resource :pointer) (output :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-title) (if set-title set-title(get-callback (defcallback empty-set-title :void
((client :pointer) (resource :pointer) (title :string))
))
))
(setf (foreign-slot-value implementation '(:struct wl-shell-surface-implementation) 'set-class) (if set-class set-class(get-callback (defcallback empty-set-class :void
((client :pointer) (resource :pointer) (class- :string))
))
))

implementation))

(defun wl-shell-surface-send-ping (resource serial)
(wl-resource-post-event resource 0 :uint32 serial))

(defun wl-shell-surface-send-configure (resource edges width height)
(wl-resource-post-event resource 1 :uint32 edges :int32 width :int32 height))

(defun wl-shell-surface-send-popup-done (resource )
(wl-resource-post-event resource 2 ))


;; Interface wl_surface
;; A surface is a rectangular area that is displayed on the screen.
;;       It has a location, size and pixel contents.
;; 
;;       The size of a surface (and relative positions on it) is described
;;       in surface-local coordinates, which may differ from the buffer
;;       coordinates of the pixel content, in case a buffer_transform
;;       or a buffer_scale is used.
;; 
;;       A surface without a "role" is fairly useless: a compositor does
;;       not know where, when or how to present it. The role is the
;;       purpose of a wl_surface. Examples of roles are a cursor for a
;;       pointer (as set by wl_pointer.set_cursor), a drag icon
;;       (wl_data_device.start_drag), a sub-surface
;;       (wl_subcompositor.get_subsurface), and a window as defined by a
;;       shell protocol (e.g. wl_shell.get_shell_surface).
;; 
;;       A surface can have only one role at a time. Initially a
;;       wl_surface does not have a role. Once a wl_surface is given a
;;       role, it is set permanently for the whole lifetime of the
;;       wl_surface object. Giving the current role again is allowed,
;;       unless explicitly forbidden by the relevant interface
;;       specification.
;; 
;;       Surface roles are given by requests in other interfaces such as
;;       wl_pointer.set_cursor. The request should explicitly mention
;;       that this request gives a role to a wl_surface. Often, this
;;       request also creates a new protocol object that represents the
;;       role and adds additional functionality to wl_surface. When a
;;       client wants to destroy a wl_surface, they must destroy this 'role
;;       object' before the wl_surface.
;; 
;;       Destroying the role object does not remove the role from the
;;       wl_surface, but it may stop the wl_surface from "playing the role".
;;       For instance, if a wl_subsurface object is destroyed, the wl_surface
;;       it was created for will be unmapped and forget its position and
;;       z-order. It is allowed to create a wl_subsurface for the same
;;       wl_surface again, but it is not allowed to use the wl_surface as
;;       a cursor (cursor is a different role than sub-surface, and role
;;       switching is not allowed).
(defparameter wl-surface-interface (foreign-symbol-pointer "wl_surface_interface"))
(defcstruct wl-surface-implementation
(destroy :pointer)
(attach :pointer)
(damage :pointer)
(frame :pointer)
(set-opaque-region :pointer)
(set-input-region :pointer)
(commit :pointer)
(set-buffer-transform :pointer)
(set-buffer-scale :pointer)
(damage-buffer :pointer)
)

(defun implement-wl-surface (&key (destroy nil) (attach nil) (damage nil) (frame nil) (set-opaque-region nil) (set-input-region nil) (commit nil) (set-buffer-transform nil) (set-buffer-scale nil) (damage-buffer nil) )
(let ((implementation (foreign-alloc '(:struct wl-surface-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'attach) (if attach attach(get-callback (defcallback empty-attach :void
((client :pointer) (resource :pointer) (buffer :pointer)(x :int32)(y :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'damage) (if damage damage(get-callback (defcallback empty-damage :void
((client :pointer) (resource :pointer) (x :int32)(y :int32)(width :int32)(height :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'frame) (if frame frame(get-callback (defcallback empty-frame :void
((client :pointer) (resource :pointer) (callback :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'set-opaque-region) (if set-opaque-region set-opaque-region(get-callback (defcallback empty-set-opaque-region :void
((client :pointer) (resource :pointer) (region :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'set-input-region) (if set-input-region set-input-region(get-callback (defcallback empty-set-input-region :void
((client :pointer) (resource :pointer) (region :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'commit) (if commit commit(get-callback (defcallback empty-commit :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'set-buffer-transform) (if set-buffer-transform set-buffer-transform(get-callback (defcallback empty-set-buffer-transform :void
((client :pointer) (resource :pointer) (transform :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'set-buffer-scale) (if set-buffer-scale set-buffer-scale(get-callback (defcallback empty-set-buffer-scale :void
((client :pointer) (resource :pointer) (scale :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-surface-implementation) 'damage-buffer) (if damage-buffer damage-buffer(get-callback (defcallback empty-damage-buffer :void
((client :pointer) (resource :pointer) (x :int32)(y :int32)(width :int32)(height :int32))
))
))

implementation))

(defun wl-surface-send-enter (resource output)
(wl-resource-post-event resource 0 :pointer output))

(defun wl-surface-send-leave (resource output)
(wl-resource-post-event resource 1 :pointer output))


;; Interface wl_seat
;; A seat is a group of keyboards, pointer and touch devices. This
;;       object is published as a global during start up, or when such a
;;       device is hot plugged.  A seat typically has a pointer and
;;       maintains a keyboard focus and a pointer focus.
(defparameter wl-seat-interface (foreign-symbol-pointer "wl_seat_interface"))
(defcstruct wl-seat-implementation
(get-pointer :pointer)
(get-keyboard :pointer)
(get-touch :pointer)
(release :pointer)
)

(defun implement-wl-seat (&key (get-pointer nil) (get-keyboard nil) (get-touch nil) (release nil) )
(let ((implementation (foreign-alloc '(:struct wl-seat-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-seat-implementation) 'get-pointer) (if get-pointer get-pointer(get-callback (defcallback empty-get-pointer :void
((client :pointer) (resource :pointer) (id :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-seat-implementation) 'get-keyboard) (if get-keyboard get-keyboard(get-callback (defcallback empty-get-keyboard :void
((client :pointer) (resource :pointer) (id :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-seat-implementation) 'get-touch) (if get-touch get-touch(get-callback (defcallback empty-get-touch :void
((client :pointer) (resource :pointer) (id :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-seat-implementation) 'release) (if release release(get-callback (defcallback empty-release :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-seat-send-capabilities (resource capabilities)
(wl-resource-post-event resource 0 :uint32 capabilities))

(defun wl-seat-send-name (resource name)
(wl-resource-post-event resource 1 :string name))


;; Interface wl_pointer
;; The wl_pointer interface represents one or more input devices,
;;       such as mice, which control the pointer location and pointer_focus
;;       of a seat.
;; 
;;       The wl_pointer interface generates motion, enter and leave
;;       events for the surfaces that the pointer is located over,
;;       and button and axis events for button presses, button releases
;;       and scrolling.
(defparameter wl-pointer-interface (foreign-symbol-pointer "wl_pointer_interface"))
(defcstruct wl-pointer-implementation
(set-cursor :pointer)
(release :pointer)
)

(defun implement-wl-pointer (&key (set-cursor nil) (release nil) )
(let ((implementation (foreign-alloc '(:struct wl-pointer-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-pointer-implementation) 'set-cursor) (if set-cursor set-cursor(get-callback (defcallback empty-set-cursor :void
((client :pointer) (resource :pointer) (serial :uint32)(surface :pointer)(hotspot-x :int32)(hotspot-y :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-pointer-implementation) 'release) (if release release(get-callback (defcallback empty-release :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-pointer-send-enter (resource serial surface surface-x surface-y)
(wl-resource-post-event resource 0 :uint32 serial :pointer surface :int32 surface-x :int32 surface-y))

(defun wl-pointer-send-leave (resource serial surface)
(wl-resource-post-event resource 1 :uint32 serial :pointer surface))

(defun wl-pointer-send-motion (resource time surface-x surface-y)
(wl-resource-post-event resource 2 :uint32 time :int32 surface-x :int32 surface-y))

(defun wl-pointer-send-button (resource serial time button state)
(wl-resource-post-event resource 3 :uint32 serial :uint32 time :uint32 button :uint32 state))

(defun wl-pointer-send-axis (resource time axis value)
(wl-resource-post-event resource 4 :uint32 time :uint32 axis :int32 value))

(defun wl-pointer-send-frame (resource )
(wl-resource-post-event resource 5 ))

(defun wl-pointer-send-axis-source (resource axis-source)
(wl-resource-post-event resource 6 :uint32 axis-source))

(defun wl-pointer-send-axis-stop (resource time axis)
(wl-resource-post-event resource 7 :uint32 time :uint32 axis))

(defun wl-pointer-send-axis-discrete (resource axis discrete)
(wl-resource-post-event resource 8 :uint32 axis :int32 discrete))


;; Interface wl_keyboard
;; The wl_keyboard interface represents one or more keyboards
;;       associated with a seat.
(defparameter wl-keyboard-interface (foreign-symbol-pointer "wl_keyboard_interface"))
(defcstruct wl-keyboard-implementation
(release :pointer)
)

(defun implement-wl-keyboard (&key (release nil) )
(let ((implementation (foreign-alloc '(:struct wl-keyboard-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-keyboard-implementation) 'release) (if release release(get-callback (defcallback empty-release :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-keyboard-send-keymap (resource format fd size)
(wl-resource-post-event resource 0 :uint32 format :int32 fd :uint32 size))

(defun wl-keyboard-send-enter (resource serial surface keys)
(wl-resource-post-event resource 1 :uint32 serial :pointer surface :pointer keys))

(defun wl-keyboard-send-leave (resource serial surface)
(wl-resource-post-event resource 2 :uint32 serial :pointer surface))

(defun wl-keyboard-send-key (resource serial time key state)
(wl-resource-post-event resource 3 :uint32 serial :uint32 time :uint32 key :uint32 state))

(defun wl-keyboard-send-modifiers (resource serial mods-depressed mods-latched mods-locked group)
(wl-resource-post-event resource 4 :uint32 serial :uint32 mods-depressed :uint32 mods-latched :uint32 mods-locked :uint32 group))

(defun wl-keyboard-send-repeat-info (resource rate delay)
(wl-resource-post-event resource 5 :int32 rate :int32 delay))


;; Interface wl_touch
;; The wl_touch interface represents a touchscreen
;;       associated with a seat.
;; 
;;       Touch interactions can consist of one or more contacts.
;;       For each contact, a series of events is generated, starting
;;       with a down event, followed by zero or more motion events,
;;       and ending with an up event. Events relating to the same
;;       contact point can be identified by the ID of the sequence.
(defparameter wl-touch-interface (foreign-symbol-pointer "wl_touch_interface"))
(defcstruct wl-touch-implementation
(release :pointer)
)

(defun implement-wl-touch (&key (release nil) )
(let ((implementation (foreign-alloc '(:struct wl-touch-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-touch-implementation) 'release) (if release release(get-callback (defcallback empty-release :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun wl-touch-send-down (resource serial time surface id x y)
(wl-resource-post-event resource 0 :uint32 serial :uint32 time :pointer surface :int32 id :int32 x :int32 y))

(defun wl-touch-send-up (resource serial time id)
(wl-resource-post-event resource 1 :uint32 serial :uint32 time :int32 id))

(defun wl-touch-send-motion (resource time id x y)
(wl-resource-post-event resource 2 :uint32 time :int32 id :int32 x :int32 y))

(defun wl-touch-send-frame (resource )
(wl-resource-post-event resource 3 ))

(defun wl-touch-send-cancel (resource )
(wl-resource-post-event resource 4 ))


;; Interface wl_output
;; An output describes part of the compositor geometry.  The
;;       compositor works in the 'compositor coordinate system' and an
;;       output corresponds to a rectangular area in that space that is
;;       actually visible.  This typically corresponds to a monitor that
;;       displays part of the compositor space.  This object is published
;;       as global during start up, or when a monitor is hotplugged.
(defparameter wl-output-interface (foreign-symbol-pointer "wl_output_interface"))
(defun wl-output-send-geometry (resource x y physical-width physical-height subpixel make model transform)
(wl-resource-post-event resource 0 :int32 x :int32 y :int32 physical-width :int32 physical-height :int32 subpixel :string make :string model :int32 transform))

(defun wl-output-send-mode (resource flags width height refresh)
(wl-resource-post-event resource 1 :uint32 flags :int32 width :int32 height :int32 refresh))

(defun wl-output-send-done (resource )
(wl-resource-post-event resource 2 ))

(defun wl-output-send-scale (resource factor)
(wl-resource-post-event resource 3 :int32 factor))


;; Interface wl_region
;; A region object describes an area.
;; 
;;       Region objects are used to describe the opaque and input
;;       regions of a surface.
(defparameter wl-region-interface (foreign-symbol-pointer "wl_region_interface"))
(defcstruct wl-region-implementation
(destroy :pointer)
(add :pointer)
(subtract :pointer)
)

(defun implement-wl-region (&key (destroy nil) (add nil) (subtract nil) )
(let ((implementation (foreign-alloc '(:struct wl-region-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-region-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-region-implementation) 'add) (if add add(get-callback (defcallback empty-add :void
((client :pointer) (resource :pointer) (x :int32)(y :int32)(width :int32)(height :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-region-implementation) 'subtract) (if subtract subtract(get-callback (defcallback empty-subtract :void
((client :pointer) (resource :pointer) (x :int32)(y :int32)(width :int32)(height :int32))
))
))

implementation))


;; Interface wl_subcompositor
;; The global interface exposing sub-surface compositing capabilities.
;;       A wl_surface, that has sub-surfaces associated, is called the
;;       parent surface. Sub-surfaces can be arbitrarily nested and create
;;       a tree of sub-surfaces.
;; 
;;       The root surface in a tree of sub-surfaces is the main
;;       surface. The main surface cannot be a sub-surface, because
;;       sub-surfaces must always have a parent.
;; 
;;       A main surface with its sub-surfaces forms a (compound) window.
;;       For window management purposes, this set of wl_surface objects is
;;       to be considered as a single window, and it should also behave as
;;       such.
;; 
;;       The aim of sub-surfaces is to offload some of the compositing work
;;       within a window from clients to the compositor. A prime example is
;;       a video player with decorations and video in separate wl_surface
;;       objects. This should allow the compositor to pass YUV video buffer
;;       processing to dedicated overlay hardware when possible.
(defparameter wl-subcompositor-interface (foreign-symbol-pointer "wl_subcompositor_interface"))
(defcstruct wl-subcompositor-implementation
(destroy :pointer)
(get-subsurface :pointer)
)

(defun implement-wl-subcompositor (&key (destroy nil) (get-subsurface nil) )
(let ((implementation (foreign-alloc '(:struct wl-subcompositor-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-subcompositor-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-subcompositor-implementation) 'get-subsurface) (if get-subsurface get-subsurface(get-callback (defcallback empty-get-subsurface :void
((client :pointer) (resource :pointer) (id :pointer)(surface :pointer)(parent :pointer))
))
))

implementation))


;; Interface wl_subsurface
;; An additional interface to a wl_surface object, which has been
;;       made a sub-surface. A sub-surface has one parent surface. A
;;       sub-surface's size and position are not limited to that of the parent.
;;       Particularly, a sub-surface is not automatically clipped to its
;;       parent's area.
;; 
;;       A sub-surface becomes mapped, when a non-NULL wl_buffer is applied
;;       and the parent surface is mapped. The order of which one happens
;;       first is irrelevant. A sub-surface is hidden if the parent becomes
;;       hidden, or if a NULL wl_buffer is applied. These rules apply
;;       recursively through the tree of surfaces.
;; 
;;       The behaviour of a wl_surface.commit request on a sub-surface
;;       depends on the sub-surface's mode. The possible modes are
;;       synchronized and desynchronized, see methods
;;       wl_subsurface.set_sync and wl_subsurface.set_desync. Synchronized
;;       mode caches the wl_surface state to be applied when the parent's
;;       state gets applied, and desynchronized mode applies the pending
;;       wl_surface state directly. A sub-surface is initially in the
;;       synchronized mode.
;; 
;;       Sub-surfaces have also other kind of state, which is managed by
;;       wl_subsurface requests, as opposed to wl_surface requests. This
;;       state includes the sub-surface position relative to the parent
;;       surface (wl_subsurface.set_position), and the stacking order of
;;       the parent and its sub-surfaces (wl_subsurface.place_above and
;;       .place_below). This state is applied when the parent surface's
;;       wl_surface state is applied, regardless of the sub-surface's mode.
;;       As the exception, set_sync and set_desync are effective immediately.
;; 
;;       The main surface can be thought to be always in desynchronized mode,
;;       since it does not have a parent in the sub-surfaces sense.
;; 
;;       Even if a sub-surface is in desynchronized mode, it will behave as
;;       in synchronized mode, if its parent surface behaves as in
;;       synchronized mode. This rule is applied recursively throughout the
;;       tree of surfaces. This means, that one can set a sub-surface into
;;       synchronized mode, and then assume that all its child and grand-child
;;       sub-surfaces are synchronized, too, without explicitly setting them.
;; 
;;       If the wl_surface associated with the wl_subsurface is destroyed, the
;;       wl_subsurface object becomes inert. Note, that destroying either object
;;       takes effect immediately. If you need to synchronize the removal
;;       of a sub-surface to the parent surface update, unmap the sub-surface
;;       first by attaching a NULL wl_buffer, update parent, and then destroy
;;       the sub-surface.
;; 
;;       If the parent wl_surface object is destroyed, the sub-surface is
;;       unmapped.
(defparameter wl-subsurface-interface (foreign-symbol-pointer "wl_subsurface_interface"))
(defcstruct wl-subsurface-implementation
(destroy :pointer)
(set-position :pointer)
(place-above :pointer)
(place-below :pointer)
(set-sync :pointer)
(set-desync :pointer)
)

(defun implement-wl-subsurface (&key (destroy nil) (set-position nil) (place-above nil) (place-below nil) (set-sync nil) (set-desync nil) )
(let ((implementation (foreign-alloc '(:struct wl-subsurface-implementation))))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'set-position) (if set-position set-position(get-callback (defcallback empty-set-position :void
((client :pointer) (resource :pointer) (x :int32)(y :int32))
))
))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'place-above) (if place-above place-above(get-callback (defcallback empty-place-above :void
((client :pointer) (resource :pointer) (sibling :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'place-below) (if place-below place-below(get-callback (defcallback empty-place-below :void
((client :pointer) (resource :pointer) (sibling :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'set-sync) (if set-sync set-sync(get-callback (defcallback empty-set-sync :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct wl-subsurface-implementation) 'set-desync) (if set-desync set-desync(get-callback (defcallback empty-set-desync :void
((client :pointer) (resource :pointer) )
))
))

implementation))

