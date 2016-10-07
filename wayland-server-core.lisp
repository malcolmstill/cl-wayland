#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi))
|#

(defpackage :wayland-server-core
  (:use :common-lisp :cffi)
  (:export
   wl-array-add
   wl-array-copy
   wl-array-init
   wl-array-release
   wl-client-add-destroy-listener
   wl-client-create
   wl-client-destroy
   wl-client-flush
   wl-client-get-credentials
   wl-client-get-destroy-listener
   wl-client-get-fd
   wl-client-get-object
   wl-client-post-no-memory
   wl-display-add-destroy-listener
   wl-display-add-shm-format
   wl-display-add-socket
   wl-display-add-socket-auto
   wl-display-add-socket-fd
   wl-display-create
   wl-display-destroy
   wl-display-flush-clients
   wl-display-get-destroy-listener
   wl-display-get-event-loop
   wl-display-get-serial
   wl-display-init-shm
   wl-display-next-serial
   wl-display-run
   wl-display-terminate
   wl-event-loop-add-destroy-listener
   wl-event-loop-add-fd
   wl-event-loop-add-idle
   wl-event-loop-add-signal
   wl-event-loop-add-timer
   wl-event-loop-create
   wl-event-loop-destroy
   wl-event-loop-dispatch
   wl-event-loop-dispatch-idle
   wl-event-loop-get-destroy-listener
   wl-event-loop-get-fd
   wl-event-source-check
   wl-event-source-fd-update
   wl-event-source-remove
   wl-event-source-timer-update
   wl-global-create
   wl-global-destroy
   wl-list-empty
   wl-list-init
   wl-list-insert
   wl-list-insert-list
   wl-list-length
   wl-list-remove
   wl-resource-create
   wl-resource-set-implementation
   wl-resource-set-dispatcher
   wl-resource-destroy
   wl-resource-post-event
   wl-resource-get-user-data
   wl-resource-get-version
   wl-shm-buffer-begin-access
   wl-shm-buffer-end-access
   wl-shm-buffer-get
   wl-shm-buffer-get-data
   wl-shm-buffer-get-stride
   wl-shm-buffer-get-format
   wl-shm-buffer-get-width
   wl-shm-buffer-get-height
   wl-shm-buffer-ref-pool
   wl-shm-pool-unref
   wl_argument
   wl_array
   wl_array-tclass
   wl_compositor_interface
   wl_compositor_interface-tclass
   wl_interface
   wl_interface-tclass
   wl_list
   wl_list-tclass
   wl_listener
   wl_listener-tclass
   wl_message
   wl_message-tclass
   wl_shell_interface

   ))


(in-package :wayland-server-core)

(define-foreign-library wayland-server
  (t (:default "libwayland-server")))

(use-foreign-library wayland-server)

(defcstruct wl_message
  (name :string)
  (signature :string)

  (types (:pointer)))

(defcstruct wl_interface
  (name :string)
  (version :int32)
  (method-count :int32)
  (methods (:pointer (:struct wl_message)))
  (event_count :int32)
  (events (:pointer (:struct wl_message))))

(defun make-interface ()
  (foreign-alloc '(:struct wl_interface)))

(defcfun "wl_event_loop_create" :pointer)

(defcfun "wl_event_loop_destroy" :void
  (loop :pointer))

(defcfun "wl_event_loop_add_fd" :pointer
  (loop :pointer)
  (fd :int32)
  (mask :uint32)
  (func :pointer) ; type of wl_event_loop_fd_func_t
  (data :pointer))

(defcfun "wl_event_source_fd_update" :int32
  (source :pointer)
  (mask :uint32))

(defcfun "wl_event_loop_add_timer" :pointer
  (loop :pointer)
  (func :pointer)
  (data :pointer))

(defcfun "wl_event_loop_add_signal" :pointer
  (loop :pointer)
  (signal-number :int32)
  (func :pointer)
  (data :pointer))

(defcfun "wl_event_source_timer_update" :int32
  (source :pointer)
  (ms-delay :int32))

(defcfun "wl_event_source_remove" :int32
  (source :pointer))

(defcfun "wl_event_source_check" :void
  (source :pointer))

(defcfun "wl_event_loop_dispatch" :int32
  (loop :pointer)
  (timeout :int32))

(defcfun "wl_event_loop_dispatch_idle" :void
  (loop :pointer))

(defcfun "wl_event_loop_add_idle" :pointer
  (loop :pointer)
  (func :pointer)
  (data :pointer))

(defcfun "wl_event_loop_get_fd" :int32
  (loop :pointer))

(defcfun "wl_event_loop_add_destroy_listener" :void
  (loop :pointer)
  (listener :pointer))

(defcfun "wl_event_loop_get_destroy_listener" :pointer
  (loop :pointer)
  (func :pointer))


;; wl_display
(defcfun "wl_display_create" :pointer)

(defcfun "wl_display_destroy" :void
  (display :pointer))

(defcfun "wl_display_get_event_loop" :pointer
  (display :pointer))

(defcfun "wl_display_add_socket" :int32
  (display :pointer)
  (name :string))

(defcfun "wl_display_add_socket_auto" :string
  (display :pointer))

(defcfun "wl_display_add_socket_fd" :int32
  (display :pointer)
  (sock-fd :int32))

(defcfun "wl_display_terminate" :void
  (display :pointer))

(defcfun "wl_display_run" :void
  (display :pointer))

(defcfun "wl_display_flush_clients" :void
  (display :pointer))

(defcfun "wl_display_get_serial" :uint32
  (display :pointer))

(defcfun "wl_display_next_serial" :uint32
  (display :pointer))

(defcfun "wl_display_add_destroy_listener" :void
  (display :pointer)
  (listener :pointer))

(defcfun "wl_display_get_destroy_listener" :pointer
  (display :pointer)
  (notify :pointer))

(defcfun "wl_global_create" :pointer ; struct wl_global *
  (display :pointer)
  (interface :pointer)
  (version :int32)
  (data :pointer)
  (func :pointer))

(defcfun "wl_global_destroy" :void
  (display :pointer))

;; wl_client
(defcfun "wl_client_create" :pointer ; struct wl_client *
  (display :pointer)
  (fd :int32))

(defcfun "wl_client_destroy" :void
  (client :pointer))

(defcfun "wl_client_flush" :void
  (client :pointer))

(defcfun "wl_client_get_credentials" :void
  (client :pointer)
  (pid :pointer)
  (uid :pointer)
  (gid :pointer))

(defcfun "wl_client_get_fd" :int32
  (client :pointer))

(defcfun "wl_client_add_destroy_listener" :void
  (client :pointer)
  (listener :pointer))

(defcfun "wl_client_get_destroy_listener" :pointer
  (client :pointer)
  (func :pointer))

(defcfun "wl_client_get_object" :pointer
  (client :pointer)
  (id :uint32))

(defcfun "wl_client_post_no_memory" :void
  (client :pointer))

(defcstruct wl_list
  (prev :pointer)
  (next :pointer))

(defcstruct wl_listener
  "wl_listener struct"
  (link (:struct wl_list))
  (notify :pointer))

(defcfun "wl_list_init" :void
  (list :pointer))

(defcfun "wl_list_insert" :void
  (list :pointer)
  (elm :pointer))

(defcfun "wl_list_remove" :void
  (elm :pointer))

(defcfun "wl_list_length" :int32
  (list :pointer))

(defcfun "wl_list_empty" :int32
  (list :pointer))

(defcfun "wl_list_insert_list" :void
  (list :pointer)
  (other :pointer))

(defcstruct wl_array
  (size :uint32) ; size_t
  (alloc :uint32) ; size_t
  (data :pointer))

(defcfun "wl_array_init" :void
  (array :pointer))

(defcfun "wl_array_release" :void
  (array :pointer))

(defcfun "wl_array_add" :pointer
  (array :pointer)
  (size :uint32)) ; size_t

(defcfun "wl_array_copy" :int32
  (array :pointer)
  (source :pointer))

;(defcfun "wl_fixed_to_double" :double
					;  (f :int32))

(defcunion wl_argument
  (i :int32)
  (u :uint32)
  (f :int32)
  (s :string)
  (o :pointer) ; struct wl_object *
  (n :uint32)
  (a :pointer) ; struct wl_array *
  (h :int32))

(defcfun "wl_resource_post_event" :void
  (resource :pointer)
  (opcode :uint32)
  &rest)

(defcfun "wl_resource_create" :pointer
  (client :pointer)
  (interface :pointer)
  (version :int32)
  (id :uint32))

(defcfun "wl_resource_set_implementation" :void
  (resource :pointer)
  (implementation :pointer)
  (data :pointer)
  (destroy :pointer))

(defcfun "wl_resource_set_dispatcher" :void
  (resource :pointer)
  (implementation :pointer)
  (data :pointer)
  (destroy :pointer))

(defcfun "wl_resource_destroy" :void
  (resource :pointer))

(defcfun "wl_resource_get_user_data" :pointer
  (resource :pointer))

(defcfun "wl_resource_get_version" :int
  (resource :pointer))

;; Shared memory (SHM) functions

(defcfun "wl_shm_buffer_begin_access" :void
  (buffer :pointer))

(defcfun "wl_shm_buffer_end_access" :void
  (buffer :pointer))

(defcfun "wl_shm_buffer_get" :pointer
  (resource :pointer))

(defcfun "wl_shm_buffer_get_data" :pointer
  (buffer :pointer))

(defcfun "wl_shm_buffer_get_stride" :int32
  (buffer :pointer))

(defcfun "wl_shm_buffer_get_format" :uint32
  (buffer :pointer))

(defcfun "wl_shm_buffer_get_width" :int32
  (buffer :pointer))

(defcfun "wl_shm_buffer_get_height" :int32
  (buffer :pointer))

(defcfun "wl_shm_buffer_ref_pool" :pointer
  (buffer :pointer))

(defcfun "wl_shm_pool_unref" :void
  (pool :pointer))

(defcfun "wl_display_init_shm" :int32
  (display :pointer))

(defcfun "wl_display_add_shm_format" :pointer
  (display :pointer)
  (format :uint32))
