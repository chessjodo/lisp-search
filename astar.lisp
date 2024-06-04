;;;; graphs.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

(defun generate-grid-neighbours (walls &key (grid-size 100) (wall-length 10))
  "Generate a grid neighbours function. The generated function takes X and Y
coordinates and returns a list of neighbours at the four compass points,
omitting those that are blocked by a wall. Generate-grid-neighbours takes a
number of walls and an optional wall length."
  (iter outer
        (for wall below walls)
        (for direction = (random 2))
        (for (startx starty) = (list (1+ (random (- grid-size wall-length)))
                                     (1+ (random (- grid-size wall-length)))))
        (iter (for brick below wall-length)
              (in outer (adjoining (if (= direction 1)
                                       (list (+ startx brick) starty)
                                       (list startx (+ starty brick)))
                                   test #'equal
                                   into inaccessible)))
        (finally (return-from outer
                   (lambda (x y)
                     (set-difference
                      (let (compass-points)
                        (when (< x grid-size)
                          (push (list (1+ x) y) compass-points))
                        (when (> x 0)
                          (push (list (1- x) y) compass-points))
                        (when (< y grid-size)
                          (push (list x (1+ y)) compass-points))
                        (when (> y 0)
                          (push (list x (1- y)) compass-points))
                        compass-points)
                      inaccessible
                      :test #'equal))))))

(defun run-a* (walls)
  (let (generate-grid (generate-grid-neighbours walls))
    (a*-search (list (make-path :state '(99 99)))
               (lambda (p)
                 (and (= 0 (first p))
                      (= 0 (second p))))
               (lambda (p)
                 (generate-grid (first p) (second p))
                 )
               (constantly 1)
               (lambda (p)
                 (+ (first p) (second p)))
               :state= #'equal))
  )
