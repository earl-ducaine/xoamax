



;; Routines to perform the tedious task of setting up the CLX
;; envirnoment to the point where we can draw a window.

(defpackage :xomax-clx-basics
  (:use cl xlib))

(in-package xomax-clx-basics)


(defvar *display*)
(defvar *screen*)
(defvar *font*)
(defvar *root-window*)
(defvar *a-window*)

;; Create a convincing test for with-desplay.

(defparameter *display-tests*
  '((setf *display* (xlib:open-default-display))
    (display-authorization-data *display*)
    (display-authorization-name *display*)
    (display-bitmap-format *display*)
    (display-byte-order *display*)
    (display-display *display*)
    (display-error-handler *display*)
    (display-image-lsb-first-p *display*)
    (display-keycode-range *display*)
    (display-max-keycode *display*)
    (display-max-request-length *display*)
    (display-min-keycode *display*)
    (display-motion-buffer-size *display*)
    (display-p *display*)
    (display-pixmap-formats *display*)
    (display-plist *display*)
    (display-protocol-major-version *display*)
    (display-protocol-minor-version *display*)
    (display-protocol-version *display*)
    (display-resource-id-base *display*)
    (display-resource-id-mask *display*)
    (display-roots *display*)
    (display-vendor *display*)
    (display-vendor-name *display*)
    (display-xid *display*)
    (setf (display-after-function *display*) (lambda () (display-force-output *display*)))
    (display-force-output *display*)
    (close-display  *display*)))

(defparameter *screen-tests*
  '((screen-backing-stores *screen*)
    (screen-black-pixel *screen*)
    (screen-default-colormap *screen*)
    (screen-depths *screen*)
    (screen-event-mask-at-open *screen*)
    (screen-height *screen*)
    (screen-height-in-millimeters *screen*)
    (screen-max-installed-maps *screen*)
    (screen-min-installed-maps *screen*)
    (screen-p *screen*)
    (screen-plist *screen*)
    (screen-root *screen*)
    (screen-root-depth *screen*)
    (screen-save-unders-p *screen*)
    (screen-white-pixel *screen*)
    (screen-width *screen*)
    (screen-width-in-millimeters *screen*)))




(defun do-test-type (test-type)
  ;; Question: is open-default-display able to read the DISPLAY
  ;; envronment variable if it's left blank CLX manual doesn't specify.
  (dolist (test test-type)
    (format t "~s: ~s~%" test (eval test))))



(defun init-xlib ()
  (do-test-type *screen-tests*))
