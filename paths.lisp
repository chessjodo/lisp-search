;;;; paths.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun path-saver (successors cost-fn cost-left-fn)
  (lambda (old-path)
    (let ((old-state (path-state old-path)))
      (mapcar (lambda (new-state)
                (let ((old-cost (+ (path-cost-so-far old-path)
                                   (funcall cost-fn old-state new-state))))
                  (make-path :state new-state
                             :previous old-path
                             :cost-so-far old-cost
                             :total-cost
                             (+ old-cost (funcall cost-left-fn new-state)))))
              (funcall successors old-state)))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (city-name (path-state path)) (path-total-cost path)))

(defun show-city-path (path &optional (stream t))
  "Show the length of a path, and the cities along it."
  (when path
    (format stream "~&#<Path ~,1f km: ~{~:(~a~)~^ - ~}>"
            (path-total-cost path)
            (reverse (pathmap #'city-name path)))))

(defun pathmap (fn path)
  "Call fn on each state in the path, collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
            (pathmap fn (path-previous path)))))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  (lambda (path)
    (funcall test value (funcall key path))))

(defun trip-path (start dest &key (beam-width 1) (range 1000.0))
  "Search for the best path from the start to dest."
  (beam-search (make-path :state (city start))
               (is (city dest) :key #'path-state)
               (path-saver (lambda (city) (cities-within-range city range))
                           #'great-circle-distance
                           (lambda (c) (great-circle-distance c (city dest))))
               #'path-total-cost
               beam-width))

(defun iter-wide-search (start goal-p successors cost-fn
                         &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
        (iter-wide-search start goal-p successors cost-fn
                          :width (+ width 1) :max max))))
