;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-ext)

;; these came from RE-INITIALIZE-KEY-EVENTS:
(define-clx-modifier (xlib:make-state-mask :shift) "Shift")
(define-clx-modifier (xlib:make-state-mask :mod-1) "Meta")
(define-clx-modifier (xlib:make-state-mask :control) "Control")
(define-clx-modifier (xlib:make-state-mask :lock) "Lock")

;;; TRANSLATE-KEY-EVENT -- Public.
;;;
(defun translate-key-event (display scan-code bits)
  "Translates the X scan-code and X bits to a key-event.  First this maps
   scan-code to an X keysym using XLIB:KEYCODE->KEYSYM looking at bits and
   supplying index as 1 if the X shift bit is on, 0 otherwise.

   If the resulting keysym is undefined, and it is not a modifier keysym, then
   this signals an error.  If the keysym is a modifier key, then this returns
   nil.

   If the following conditions are satisfied
      the keysym is defined
      the X shift bit is off
      the X lock bit is on
      the X keysym represents a lowercase letter
   then this maps the scan-code again supplying index as 1 this time, treating
   the X lock bit as a caps-lock bit.  If this results in an undefined keysym,
   this signals an error.  Otherwise, this makes a key-event with the keysym
   and bits formed by mapping the X bits to key-event bits.

   If any state bit is set that has no suitable modifier translation, it is
   passed to XLIB:DEFAULT-KEYSYM-INDEX in order to handle Mode_Switch keys
   appropriately.

   Otherwise, this makes a key-event with the keysym and bits formed by mapping
   the X bits to key-event bits."
  (let ((new-bits 0)
        shiftp lockp)
    (dolist (map *modifier-translations*)
      (unless (zerop (logand (car map) bits))
        ;; ignore the bits of the mapping for the determination of a key index
        (setq bits (logxor bits (car map)))
        (cond
         ((string-equal (cdr map) "Shift")
          (setf shiftp t))
         ((string-equal (cdr map) "Lock")
          (setf lockp t))
         (t (setf new-bits
                  (logior new-bits (key-event-modifier-mask (cdr map))))))))
    ;; here pass any remaining modifier bits to clx
    (let* ((index  (and (not (zerop bits))
                        (xlib:default-keysym-index display scan-code bits)))
           (keysym (xlib:keycode->keysym display scan-code (or index (if shiftp 1 0)))))
      (cond ((null (keysym-names keysym))
             nil)
            ((and (not shiftp) lockp (<= 97 keysym 122)) ; small-alpha-char-p
             (let ((keysym (xlib:keycode->keysym display scan-code 1)))
               (if (keysym-names keysym)
                   (make-key-event keysym new-bits)
                   nil)))
            (t
             (make-key-event keysym new-bits))))))

(defun call-with-clx-event-handling (fun display handler)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  (unwind-protect
      (progn
        (enable-clx-event-handling display handler)
        (funcall fun))
    (disable-clx-event-handling display)))

;;;; Object set event handling.

;;; This is bound by OBJECT-SET-EVENT-HANDLER, so DISPATCH-EVENT can clear
;;; events on the display before signalling any errors.  This is necessary
;;; since reading on certain CMU Common Lisp streams involves SERVER, and
;;; getting an error while trying to handle an event causes repeated attempts
;;; to handle the same event.
;;;
(defvar *process-clx-event-display* nil)

(defvar *object-set-event-handler-print* nil)


(defun bitmap-dispatch (event-key event-window &rest args &key display  &allow-other-keys)
  (multiple-value-bind (object object-set)
      (lisp--map-xwindow event-window)
    (unless object
      (cond ((not (typep event-window 'xlib:window))
             ;;(xlib:discard-current-event display)
             (warn "Discarding ~S event on non-window ~S."
                   event-key event-window)
             (return-from bitmap-dispatch t))
            (t
             (flush-display-events display)
             (error "~S not a known X window.~%~
                                   Received event ~S."
                    event-window event-key))))
    (handler-bind ((error #'(lambda (condx)
                              (declare (ignore condx))
                              (flush-display-events display))))
      (when *object-set-event-handler-print*
        (print event-key) (force-output))
      (apply (gethash event-key
                      (object-set-table object-set)
                      (object-set-default-handler
                       object-set))
             object event-key
             args))
    t))


(defun object-set-event-handler (display &optional (timeout 0))
  "This display event handler uses object sets to map event windows
   cross event types to handlers.  it uses xlib:event-case to bind all
   the slots of each event, calling the handlers on all these values
   in addition to the event key and send-event-p.
   Describe EXT:SERVE-MUMBLE, where MUMBLE is an event keyword name
   for the exact order of arguments. :MAPPING-NOTIFY AND
   :KEYMAP-NOTIFY events are ignored since they do not occur on any
   particular window. After calling a handler, each branch returns T
   to discard the event. Wwhile the handler is executing, all errors
   go through a handler that flushes all the display's events and
   returns.  This prevents infinite errors since the debug and
   terminal streams loop over SYSTEM:SERVE-EVENT. This function
   returns T if there were some event to handle, NIL otherwise. It
   returns immediately if there is no event to handle."
  (macrolet ((bitmap-dispatch-snippet
                 (event-key event-window &key child hint-p root root-x root-y
                                           same-screen-p send-event-p state
                                           time x y)
               `(progn
                  (bitmap-dispatch
                   ,event-key ,event-window
                   :root ,root
                   :child ,child
                   :same-screen-p ,same-screen-p
                   :x ,x
                   :y ,y
                   :root-x ,root-x
                   :root-y ,root-y
                   :state ,state
                   :time ,time
                   :hint-p ,hint-p
                   :send-event-p ,send-event-p)
                  (setf result t)))
             ;; (dispatch (event-key &rest args)
             ;;   `(multiple-value-bind (object object-set)
             ;;        (lisp--map-xwindow event-window)
             ;;      (unless object
             ;;        (cond ((not (typep event-window 'xlib:window))
             ;;               ;;(xlib:discard-current-event display)
             ;;               (warn "discarding ~s event on non-window ~s."
             ;;                     ,event-key event-window)
             ;;               (return-from object-set-event-handler nil)
             ;;               )
             ;;              (t
             ;;               (flush-display-events display)
             ;;               (error "~s not a known x window.~%~
             ;;                       received event ~s."
             ;;                      event-window ,event-key))))
             ;;      (handler-bind ((error #'(lambda (condx)
             ;;                                (declare (ignore condx))
             ;;                                (flush-display-events display))))
             ;;        (when *object-set-event-handler-print*
             ;;          (print ,event-key) (force-output))
             ;;        (funcall (gethash ,event-key
             ;;                          (object-set-table object-set)
             ;;                          (object-set-default-handler
             ;;                           object-set))
             ;;                 object ,event-key
             ;;                 ,@args))
             ;;      (setf result t)))
             )
    (let* ((*process-clx-event-display* display)
           (result nil))
      (xlib:event-case (display :timeout timeout)
        ((:key-press :key-release :button-press :button-release)
         (event-key event-window root child same-screen-p x y root-x
                    root-y state time #+nil code send-event-p)
         (bitmap-dispatch-snippet event-key event-window :child child
                                                         :root root
                                                         :root-x root-x
                                                         :root-y root-y
                                                         :same-screen-p same-screen-p
                                                         :send-event-p send-event-p
                                                         :state state
                                                         :time time
                                                         :x x
                                                         :y y))
        (:motion-notify
         (event-window root child same-screen-p x y root-x root-y state
                       time hint-p send-event-p)
         (bitmap-dispatch-snippet :motion-notify event-window :child child :hint-p hint-p :root root :root-x root-x :root-y root-y
                                  :same-screen-p same-screen-p :send-event-p send-event-p :state state
                                  :time time
                                  :x x
                                  :y y))
        (:enter-notify
         (event-window root child same-screen-p x y root-x root-y state
                       time #+nil mode #+nil kind send-event-p)
         (bitmap-dispatch-snippet :enter-notify event-window :child child :root root :root-x root-x :root-y root-y :same-screen-p same-screen-p
                                  :send-event-p send-event-p :state state
                                  :time time
                                  :x x
                                  :y y))
        (:leave-notify
         (event-window root child same-screen-p x y root-x root-y state
                       time #+nil mode #+nil kind send-event-p)
         (bitmap-dispatch-snippet :leave-notify event-window :child child :root root :root-x root-x :root-y root-y :same-screen-p same-screen-p
                                  :send-event-p send-event-p :state state :time time
                                  :x x
                                  :y y))
        (:exposure
         (event-window x y #+nil width #+nil height #+nil count send-event-p)
         (bitmap-dispatch-snippet :exposure event-window :send-event-p send-event-p
                                  :x x
                                  :y y))
        (:graphics-exposure
         (event-window x y #+nil width #+nil height #+nil count #+nil major #+nil minor send-event-p)
         (bitmap-dispatch-snippet :graphics-exposure event-window :send-event-p send-event-p
                                  :x x
                                  :y y))
        (:no-exposure
         (event-window #+nil major #+nil minor send-event-p)
         (bitmap-dispatch-snippet :no-exposure event-window :send-event-p send-event-p))
        (:focus-in
         (event-window #+nil mode #+nil kind send-event-p)
         (bitmap-dispatch-snippet :focus-in event-window :send-event-p send-event-p))
        (:focus-out
         (event-window #+nil mode #+nil kind send-event-p)
         (bitmap-dispatch-snippet :focus-out  event-window :send-event-p send-event-p))
        (:keymap-notify
         ()
         (warn "ignoring keymap notify event.")
         (when *object-set-event-handler-print*
           (print :keymap-notify) (force-output))
         (setf result t))
        (:visibility-notify
         (event-window state send-event-p)
         (bitmap-dispatch-snippet :visibility-notify event-window :send-event-p send-event-p :state state))
        (:create-notify
         (event-window #+nil window x y #+nil width #+nil height #+nil border-width #+nil override-redirect-p send-event-p)
         (bitmap-dispatch-snippet :create-notify event-window :send-event-p send-event-p
                                  :x x
                                  :y y))
        (:destroy-notify
         (event-window #+nil window send-event-p)
         (bitmap-dispatch-snippet :destroy-notify event-window :send-event-p send-event-p))
        (:unmap-notify (event-window #+nil window #+nil configure-p send-event-p)
                       (bitmap-dispatch-snippet :unmap-notify event-window :send-event-p send-event-p))
        (:map-notify (event-window #+nil window #+nil override-redirect-p send-event-p)
                     (bitmap-dispatch-snippet :map-notify event-window :send-event-p send-event-p))
        (:map-request (event-window #+nil window send-event-p)
                      (bitmap-dispatch-snippet :map-request event-window :send-event-p send-event-p))
        (:reparent-notify (event-window #+nil window #+nil parent x y #+nil override-redirect-p
                                                                      send-event-p)
                          (bitmap-dispatch-snippet :reparent-notify event-window :send-event-p send-event-p
                                                   :x x
                                                   :y y))
        (:configure-notify (event-window #+nil window x y #+nil width #+nil height #+nil border-width
                                                          #+nil above-sibling #+nil override-redirect-p send-event-p)
                           (bitmap-dispatch-snippet :configure-notify event-window :send-event-p send-event-p
                                                    :x x
                                                    :y y))
        (:gravity-notify (event-window #+nil window x y send-event-p)
                         (bitmap-dispatch-snippet :gravity-notify event-window :send-event-p send-event-p
                                                  :x x
                                                  :y y))
        (:resize-request (event-window #+nil width #+nil height send-event-p)
                         (bitmap-dispatch-snippet :resize-request event-window :send-event-p send-event-p))
        (:configure-request (event-window #+nil window x y #+nil width #+nil height #+nil border-width
                                                           #+nil stack-mode #+nil above-sibling #+nil value-mask send-event-p)
                            (bitmap-dispatch-snippet :configure-request event-window :send-event-p send-event-p
                                                     :x x
                                                     :y y))
        (:circulate-notify (event-window #+nil window #+nil place send-event-p)
                           (bitmap-dispatch-snippet :circulate-notify event-window :send-event-p send-event-p))
        (:circulate-request (event-window #+nil window #+nil place send-event-p)
                            (bitmap-dispatch-snippet :circulate-request event-window :send-event-p send-event-p))
        (:property-notify
         (event-window #+nil atom state time send-event-p)
         (bitmap-dispatch-snippet :property-notify event-window :send-event-p send-event-p :state state :time time))
        (:selection-clear (event-window #+nil selection time send-event-p)
                          (bitmap-dispatch-snippet :selection-notify event-window :send-event-p send-event-p :time time))
        (:selection-request (event-window #+nil requestor #+nil selection #+nil target #+nil property
                                          time send-event-p)
                            (bitmap-dispatch-snippet :selection-request event-window :send-event-p send-event-p :time time))
        (:selection-notify (event-window #+nil selection #+nil target #+nil property time
                                         send-event-p)
                           (bitmap-dispatch-snippet :selection-notify event-window :send-event-p send-event-p :time time))
        (:colormap-notify (event-window #+nil colormap #+nil new-p #+nil installed-p send-event-p)
                          (bitmap-dispatch-snippet :colormap-notify event-window :send-event-p send-event-p))
        (:mapping-notify (request)
                         (warn "ignoring mapping notify event -- ~s." request)
                         (when *object-set-event-handler-print*
                           (print :mapping-notify) (force-output))
                         (setf result t))
        (:client-message (event-window #+nil format #+nil data send-event-p)
                         (bitmap-dispatch-snippet :client-message event-window :send-event-p send-event-p)))
      result)))

(defun default-clx-event-handler (object event-key event-window &rest ignore)
  (declare (ignore ignore))
  (flush-display-events *process-clx-event-display*)
  (error "no handler for event type ~s on ~s in ~s."
         event-key object (lisp--map-xwindow event-window)))

(defun flush-display-events (display)
  "dumps all the events in display's event queue including the current one
   in case this is called from within xlib:event-case, etc."
  (xlib:discard-current-event display)
  (xlib:event-case (display :discard-p t :timeout 0)
    (t () nil)))

(defvar *display-event-handlers* nil)

;;; enable-clx-event-handling associates the display with the handler in
;;; *display-event-handlers*.  it also uses system:add-fd-handler to have
;;; system:serve-event call call-display-event-handler whenever anything shows
;;; up from the display. since call-display-event-handler is called on a
;;; file descriptor, the file descriptor is also mapped to the display in
;;; *clx-fds-to-displays*, so the user's handler can be called on the display.
;;;
(defvar *clx-fds-to-displays* (make-hash-table))
(defun enable-clx-event-handling (display handler)
  "after calling this, when system:serve-event notices input on display's
   connection to the x11 server, handler is called on the display.  handler
   is invoked in a dynamic context with an error handler bound that will
   flush all events from the display and return.  by returning, it declines
   to handle the error, but it will have cleared all events; thus, entering
   the debugger will not result in infinite errors due to streams that wait
   via system:serve-event for input.  calling this repeatedly on the same
   display establishes handler as a new handler, replacing any previous one
   for display."
  (check-type display xlib:display)
  (let ((change-handler (assoc display *display-event-handlers*)))
    (if change-handler
        (setf (cdr change-handler) handler)
        (let ((fd (hi::stream-fd (xlib::display-input-stream display))))
          (iolib:set-io-handler
           hi::*event-base* fd :read #'call-display-event-handler)
          (setf (gethash fd *clx-fds-to-displays*) display)
          (push (cons display handler) *display-event-handlers*)))))

;;; call-display-event-handler maps the file descriptor to its display and maps
;;; the display to its handler.  if we can't find the display, we remove the
;;; file descriptor using system:invalidate-descriptor and try to remove the
;;; display from *display-event-handlers*.  this is necessary to try to keep
;;; system:serve-event from repeatedly trying to handle the same event over and
;;; over.  this is possible since many cmu common lisp streams loop over
;;; system:serve-event, so when the debugger is entered, infinite errors are
;;; possible.
;;;
(defun call-display-event-handler (file-descriptor event error)
  (declare (ignore event))
  (if (eq error :error)
      (iolib:remove-fd-handlers hi::*event-base* file-descriptor :read t)
      (let ((display (gethash file-descriptor *clx-fds-to-displays*)))
        (unless display
          (iolib.multiplex:remove-fd-handlers hi::*event-base* file-descriptor)
          (setf *display-event-handlers*
                (delete file-descriptor *display-event-handlers*
                        :key #'(lambda (d/h)
                                 (hi::stream-fd
                                  (xlib::display-input-stream
                                   (car d/h))))))
          (error "file descriptor ~s not associated with any clx display.~%~
                  it has been removed from system:serve-event's knowledge."
                 file-descriptor))
        (let ((handler (cdr (assoc display *display-event-handlers*))))
          (unless handler
            (flush-display-events display)
            (error "display ~s not associated with any event handler."
                   display))
          (handler-bind ((error #'(lambda (condx)
                                    (declare (ignore condx))
                                    (flush-display-events display))))
            (funcall handler display))))))

(defun disable-clx-event-handling (display)
  "undoes the effect of ext:enable-clx-event-handling."
  (setf *display-event-handlers*
        (delete display *display-event-handlers* :key #'car))
  (let ((fd (hi::stream-fd (xlib::display-input-stream display))))
    (remhash fd *clx-fds-to-displays*)
    (iolib:remove-fd-handlers hi::*event-base* fd :read t)))

;;; (defun serve-event (&optional timeout)
;;;   (let ((dps))
;;;     (maphash (lambda (win value)
;;;                (declare (ignore value))
;;;                (pushnew (xlib:window-display win) dps))
;;;              *xwindow-hash*)
;;;     (when dps
;;;       (object-set-event-handler (car dps) timeout))))
