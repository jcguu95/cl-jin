;;;; My CAS
;;;; Author: Jin-Cheng Guu <jcguu95@gmail.com>
;;;;
;;;; This is based on my design note <2021-12-28 15:15>.

(defpackage my-cas (:use #:cl))
(in-package :my-cas)

(defclass set-- ()
  ((name :initform nil :accessor name
         :documentation "Name of the instance.")
   (desc :initform nil :accessor desc
         :documentation "Description of the instance.")
   (element-data-predicate
    :initarg :element-data-predicate
    :accessor element-data-predicate)
   (morphism-data-predicate
    :initarg :morphism-data-predicate
    :accessor morphism-data-predicate)
   (element-examples
    :initarg :element-examples
    :accessor element-examples))
  (:documentation "Sets, but not in the sense of the
  mathematicians. I wonder how to define it.."))

(defclass +-magma (set--)
  ((+.operation :initarg :+.operation :accessor +.operation))
  (:documentation "A magma is a set-- with a binary operation.
  Here, the operation is usually denoted by '+. The operation
  does not satisfy any rule."))

(defclass *-magma (set--)
  ((*.operation :initarg :*.operation :accessor *.operation))
  (:documentation "A magma is a set-- with a binary operation.
  Here, the operation is usually denoted by '*. The operation
  does not satisfy any rule."))

(defclass loose-rg (+-magma *-magma) ()
  (:documentation "A rg is almost a ring but without
  multiplicative identity and additive inverse. A loose-rg is
  almost a rg but with both operations (denoted usually by '+ and
  '*) satisfying no rules, including the associative and
  distributive law."))

(defclass set--.element ()
  ((name :initform nil :accessor name
         :documentation "Name of the instance.")
   (desc :initform nil :accessor desc
         :documentation "Description of the instance.")
   (data :initarg :data :accessor data
         :documentation "The data that describe the instance. It
         must pass the predicate specified by the slot
         ELEMENT-DATA-PREDICATE of the AMBIENT-SPACE of the
         instance.")
   ;; TODO implement method that enforces the requirement
   ;; specified in the doc, for each type: set--, +-magma,
   ;; *-magma, loose-rg.
   (ambient-space
    :initarg :ambient-space :accessor ambient-space
    :documentation "The ambient space of the element in the sense
    of mathematics. Do not confuse this with the parent in the
    sense of OOP.")))

(defclass +-magma.element (set--.element) ())
(defclass *-magma.element (set--.element) ())
(defclass loose-rg.element (set--.element) ())

(defmethod prod ((x *-magma.element) (y *-magma.element)))
(defmethod sum ((x +-magma.element) (y +-magma.element)))

;; TODO e.g. Implement symbolic-loose-rg, as an instance of
;; loose-rg.
