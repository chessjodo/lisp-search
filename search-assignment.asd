;;;; search-assignment.asd
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defsystem :search-assignment
  :description "Assignment on searching algorithms in Lisp"
  :author "Breanndán Ó Nualláin"
  :mailto "<o@uva.nl>"
  :license "GNU Affero General Public License v3.0 or later"
  :serial t
  :components ((:file "packages")
               (:file "debugging")
               (:file "utils")
               (:file "tree-search")
               (:file "cities")
               (:file "paths")
               (:file "graphs")
               (:file "grid")))
