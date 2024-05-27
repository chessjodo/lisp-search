;;;; utils.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

;;; Utility functions

(defun binary-tree (n)
  (list (* 2 n)
        (+  1 (* 2 n))))

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
  with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(defun is (value)
  #'(lambda (x)
      (eql x value)))

(defun prepend (new old)
  "Prepend y to start of x"
  (append old new))

(defun diff (num)
  "Return the function that finds the difference from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun price-is-right (price)
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  #'(lambda (x)
      (if (> x price)
          most-positive-fixnum
          (- price x))))

(defun next2 (n)
  (list (+ n 1) (+ n 2)))
