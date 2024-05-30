;;;; packages.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage :search-assignment
  (:use :cl :iterate :alexandria)
  (:export :make-state :state :get-left :get-right :state-left :state-right
           :state-p :get-left :get-right :fail :tree-search
           :depth-limited-tree-search :depth-limited-depth-first-search
           :depth-first-search :breadth-first-search :best-first-search
           :beam-search))
