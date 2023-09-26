;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                                                                      @vlad-km
            /     \                   2013, Original github.com/koroshiya/HTML5-canvas-matrix-digital-rain
            )     (                   Copyright © 2018,2022  @vlad-km
           /       \                  Redesign for Electron >= electron@21.2.2
           \       /                               JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

;;; moren screen lock for render process
;;; BrowserWindow: frameless, alwaysOnTop, inherited render screen-bounds, titleBarOverlay
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "MATRIX")
    (make-package :matrix :use (list 'cl))))

(in-package :matrix-rain)

(defconstant +rain-tic+ 200)
(defparameter *rCount*  90)
(defvar *x* (make-array 0 :fill-pointer 0))
(defvar *y* (make-array 0 :fill-pointer 0))
(defvar *drop-y* (make-array 0 :fill-pointer 0))
(defvar *font-size* (make-array 0 :fill-pointer 0))
(defvar *colors* (vector "#cefbe4" "#81ec72" "#5cd646" "#54d13c" "#4ccc32" "#43c728"))
(defvar canvas)
(defvar context)
(defvar timer)
(defvar e-current-width)
(defvar e-current-height)

(defvar *chars* "abcdefghijklmnopqrstuvwxyz")
(defvar *text*
  ((jscl::oget (jscl::lisp-to-js *chars*) "split") ""))
(defparameter *text-length* (length *text*))


(defun do-firstly (&optional (iteration *rCount*))
  (flet ((init (i s &optional a)
           (let ((v (#j:Math:floor (* (#j:Math:random) s))))
             (if a (+ v a)
                 v))))
    (dotimes (idx iteration)
      (vector-push-extend (init idx 1256) *x* )
      (vector-push-extend (init idx 24 12) *font-size*)
      (vector-push-extend (init idx 7 3) *drop-y*)
      (vector-push-extend -100 *y*))))

(defun inject-canvas (&rest attrs)
  (let ((e (#j:window:document:createElement "canvas")))
    (loop for (a v) on  attrs by (function cddr) do
      ((jscl::oget e "setAttribute") a v))
    (setq context ((jscl::oget e "getContext") "2d"))
    ((jscl::oget #j:document:body "appendChild")  e)
    e))

(defvar *color-style*
  (list (cons 0 (aref *colors* 0))
        (cons 1 (aref *colors* 1))
        (cons 3 (aref *colors* 2))
        (cons 7 (aref *colors* 3))
        (cons 13 (aref *colors* 4))
        (cons 17 (aref *colors* 5))))

(defun get-Color (idx)
  (let ((c (car (member idx '(0 1 3 7 13 17)))))
    (if c
        (aref *colors* c)
        (aref *colors* (random 5)))))

(defun get-Color (idx)
  (let ((c (assoc idx *color-style*)))
    (if c c
        (aref *colors* (random 6)))))

(defun random-char-idx ()
  (#j:Math:floor (* (#j:Math:random) *text-length*)))

(defun get-letter ()
  (aref *text* (random-char-idx)))

(defun draw-falling-rain (x y)
  (dotimes (i 21)
    (setf (jscl::oget context "fillStyle") (get-color i))
    ((jscl::oget context "fillText") (get-letter) x y)
    (setq y (- y (aref *font-size* i)))))

(defun do-rain ()
  (setq e-current-width (jscl::oget canvas "width")
        e-current-height (jscl::oget canvas "height"))
  (labels ((font (idx)
             (jscl::concat (aref *font-size* idx) "px MatrixCode"))
           (context-up (idx)
             (setf (jscl::oget context "font") (font idx)
                   (jscl::oget context "textBaseline") "top"
                   (jscl::oget context "textAlign") "center"))
           (u ()
             (#j:Math:floor (* (#j:Math:random) e-current-width)))
           (u2 (x y)
             (+ (#j:Math:floor (* (#j:Math:random) x)) y)))
    ((jscl::oget context "clearRect") 0 0 e-current-width e-current-height)
    (setf (jscl::oget context "shadowOffsetX") 0
          (jscl::oget context "shadowOffsetY") 0
          (jscl::oget context "shadowBlur") 8
          (jscl::oget context "shadowColor") "#94f475")
    (dotimes (i *rCount*)
      (context-up i)
      (cond ((> (aref *y* i) 1358)
             (setf (aref *x* i) (u)
                   (aref *y* i) -100
                   (aref *drop-y*i) (u2 7 3)
                   (aref *font-size* i) (u2 24 12))
             (draw-falling-rain (aref *x* i) (aref *y* i)))
            (t (draw-falling-rain (aref *x* i) (aref *y* i))))
      (setf (aref *y* i) (+ (aref *y* i) (aref *drop-y*i))))))

;;; (:hook awake cl-user)
(defun main ()
  (do-firstly)
  (setq canvas (inject-canvas "width" #j:window:innerWidth
                              "height" #j:window:innerHeight))
  (setq timer
        (#j:setInterval
         (lambda () (do-rain))
         +rain-tic+)))

(defun awake ()
  (#j:window:addEventListener "load"
                              (lambda (&rest args) (main))))


(in-package :cl-user)
  
;;; EOF
