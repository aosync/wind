(ql:quickload "tokbuf")

(defpackage #:wind
  (:use #:cl))

(defpackage #:wind.frontend
  (:use #:cl #:tokbuf))
