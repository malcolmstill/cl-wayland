(defpackage :xdg-shell-server-protocol
(:use :common-lisp :cffi :wayland-server-core)
(:export xdg-popup-send-popup-done
implement-xdg-popup
xdg-popup-implementation
xdg-popup-interface
xdg-surface-send-close
xdg-surface-send-configure
implement-xdg-surface
xdg-surface-implementation
xdg-surface-interface
xdg-shell-send-ping
implement-xdg-shell
xdg-shell-implementation
xdg-shell-interface
))

(in-package :xdg-shell-server-protocol)

(define-foreign-library xdg-shell-server
  (:unix (:or "/usr/lib64/lib-xdg-shell.so"))
  (t (:default "./lib-xdg-shell")))

(use-foreign-library xdg-shell-server)

;; Interface xdg_shell
;; xdg_shell allows clients to turn a wl_surface into a "real window"
;;       which can be dragged, resized, stacked, and moved around by the
;;       user. Everything about this interface is suited towards traditional
;;       desktop environments.
(defparameter xdg-shell-interface (foreign-symbol-pointer "xdg_shell_interface"))
(defcstruct xdg-shell-implementation
(destroy :pointer)
(use-unstable-version :pointer)
(get-xdg-surface :pointer)
(get-xdg-popup :pointer)
(pong :pointer)
)

(defun implement-xdg-shell (&key (destroy nil) (use-unstable-version nil) (get-xdg-surface nil) (get-xdg-popup nil) (pong nil) )
(let ((implementation (foreign-alloc '(:struct xdg-shell-implementation))))
(setf (foreign-slot-value implementation '(:struct xdg-shell-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct xdg-shell-implementation) 'use-unstable-version) (if use-unstable-version use-unstable-version(get-callback (defcallback empty-use-unstable-version :void
((client :pointer) (resource :pointer) (version :int32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-shell-implementation) 'get-xdg-surface) (if get-xdg-surface get-xdg-surface(get-callback (defcallback empty-get-xdg-surface :void
((client :pointer) (resource :pointer) (id :pointer)(surface :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-shell-implementation) 'get-xdg-popup) (if get-xdg-popup get-xdg-popup(get-callback (defcallback empty-get-xdg-popup :void
((client :pointer) (resource :pointer) (id :pointer)(surface :pointer)(parent :pointer)(seat :pointer)(serial :uint32)(x :int32)(y :int32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-shell-implementation) 'pong) (if pong pong(get-callback (defcallback empty-pong :void
((client :pointer) (resource :pointer) (serial :uint32))
))
))

implementation))

(defun xdg-shell-send-ping (resource serial)
(wl-resource-post-event resource 0 :uint32 serial))


;; Interface xdg_surface
;; An interface that may be implemented by a wl_surface, for
;;       implementations that provide a desktop-style user interface.
;; 
;;       It provides requests to treat surfaces like windows, allowing to set
;;       properties like maximized, fullscreen, minimized, and to move and resize
;;       them, and associate metadata like title and app id.
;; 
;;       The client must call wl_surface.commit on the corresponding wl_surface
;;       for the xdg_surface state to take effect. Prior to committing the new
;;       state, it can set up initial configuration, such as maximizing or setting
;;       a window geometry.
;; 
;;       Even without attaching a buffer the compositor must respond to initial
;;       committed configuration, for instance sending a configure event with
;;       expected window geometry if the client maximized its surface during
;;       initialization.
;; 
;;       For a surface to be mapped by the compositor the client must have
;;       committed both an xdg_surface state and a buffer.
(defparameter xdg-surface-interface (foreign-symbol-pointer "xdg_surface_interface"))
(defcstruct xdg-surface-implementation
(destroy :pointer)
(set-parent :pointer)
(set-title :pointer)
(set-app-id :pointer)
(show-window-menu :pointer)
(move :pointer)
(resize :pointer)
(ack-configure :pointer)
(set-window-geometry :pointer)
(set-maximized :pointer)
(unset-maximized :pointer)
(set-fullscreen :pointer)
(unset-fullscreen :pointer)
(set-minimized :pointer)
)

(defun implement-xdg-surface (&key (destroy nil) (set-parent nil) (set-title nil) (set-app-id nil) (show-window-menu nil) (move nil) (resize nil) (ack-configure nil) (set-window-geometry nil) (set-maximized nil) (unset-maximized nil) (set-fullscreen nil) (unset-fullscreen nil) (set-minimized nil) )
(let ((implementation (foreign-alloc '(:struct xdg-surface-implementation))))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-parent) (if set-parent set-parent(get-callback (defcallback empty-set-parent :void
((client :pointer) (resource :pointer) (parent :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-title) (if set-title set-title(get-callback (defcallback empty-set-title :void
((client :pointer) (resource :pointer) (title :string))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-app-id) (if set-app-id set-app-id(get-callback (defcallback empty-set-app-id :void
((client :pointer) (resource :pointer) (app-id :string))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'show-window-menu) (if show-window-menu show-window-menu(get-callback (defcallback empty-show-window-menu :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32)(x :int32)(y :int32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'move) (if move move(get-callback (defcallback empty-move :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'resize) (if resize resize(get-callback (defcallback empty-resize :void
((client :pointer) (resource :pointer) (seat :pointer)(serial :uint32)(edges :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'ack-configure) (if ack-configure ack-configure(get-callback (defcallback empty-ack-configure :void
((client :pointer) (resource :pointer) (serial :uint32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-window-geometry) (if set-window-geometry set-window-geometry(get-callback (defcallback empty-set-window-geometry :void
((client :pointer) (resource :pointer) (x :int32)(y :int32)(width :int32)(height :int32))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-maximized) (if set-maximized set-maximized(get-callback (defcallback empty-set-maximized :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'unset-maximized) (if unset-maximized unset-maximized(get-callback (defcallback empty-unset-maximized :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-fullscreen) (if set-fullscreen set-fullscreen(get-callback (defcallback empty-set-fullscreen :void
((client :pointer) (resource :pointer) (output :pointer))
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'unset-fullscreen) (if unset-fullscreen unset-fullscreen(get-callback (defcallback empty-unset-fullscreen :void
((client :pointer) (resource :pointer) )
))
))
(setf (foreign-slot-value implementation '(:struct xdg-surface-implementation) 'set-minimized) (if set-minimized set-minimized(get-callback (defcallback empty-set-minimized :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun xdg-surface-send-configure (resource width height states serial)
(wl-resource-post-event resource 0 :int32 width :int32 height :pointer states :uint32 serial))

(defun xdg-surface-send-close (resource )
(wl-resource-post-event resource 1 ))


;; Interface xdg_popup
;; A popup surface is a short-lived, temporary surface that can be
;;       used to implement menus. It takes an explicit grab on the surface
;;       that will be dismissed when the user dismisses the popup. This can
;;       be done by the user clicking outside the surface, using the keyboard,
;;       or even locking the screen through closing the lid or a timeout.
;; 
;;       When the popup is dismissed, a popup_done event will be sent out,
;;       and at the same time the surface will be unmapped. The xdg_popup
;;       object is now inert and cannot be reactivated, so clients should
;;       destroy it. Explicitly destroying the xdg_popup object will also
;;       dismiss the popup and unmap the surface.
;; 
;;       Clients will receive events for all their surfaces during this
;;       grab (which is an "owner-events" grab in X11 parlance). This is
;;       done so that users can navigate through submenus and other
;;       "nested" popup windows without having to dismiss the topmost
;;       popup.
;; 
;;       Clients that want to dismiss the popup when another surface of
;;       their own is clicked should dismiss the popup using the destroy
;;       request.
;; 
;;       The parent surface must have either an xdg_surface or xdg_popup
;;       role.
;; 
;;       Specifying an xdg_popup for the parent means that the popups are
;;       nested, with this popup now being the topmost popup. Nested
;;       popups must be destroyed in the reverse order they were created
;;       in, e.g. the only popup you are allowed to destroy at all times
;;       is the topmost one.
;; 
;;       If there is an existing popup when creating a new popup, the
;;       parent must be the current topmost popup.
;; 
;;       A parent surface must be mapped before the new popup is mapped.
;; 
;;       When compositors choose to dismiss a popup, they will likely
;;       dismiss every nested popup as well. When a compositor dismisses
;;       popups, it will follow the same dismissing order as required
;;       from the client.
;; 
;;       The x and y arguments passed when creating the popup object specify
;;       where the top left of the popup should be placed, relative to the
;;       local surface coordinates of the parent surface. See
;;       xdg_shell.get_xdg_popup.
;; 
;;       The client must call wl_surface.commit on the corresponding wl_surface
;;       for the xdg_popup state to take effect.
;; 
;;       For a surface to be mapped by the compositor the client must have
;;       committed both the xdg_popup state and a buffer.
(defparameter xdg-popup-interface (foreign-symbol-pointer "xdg_popup_interface"))
(defcstruct xdg-popup-implementation
(destroy :pointer)
)

(defun implement-xdg-popup (&key (destroy nil) )
(let ((implementation (foreign-alloc '(:struct xdg-popup-implementation))))
(setf (foreign-slot-value implementation '(:struct xdg-popup-implementation) 'destroy) (if destroy destroy(get-callback (defcallback empty-destroy :void
((client :pointer) (resource :pointer) )
))
))

implementation))

(defun xdg-popup-send-popup-done (resource )
(wl-resource-post-event resource 0 ))

