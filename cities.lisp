;;;; cities.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  (let ((city-data (multiple-value-bind (foundp found-system
                                         pathname
                                         previous previous-time)
                       (asdf:locate-system :search-assignment)
                     (declare (ignore foundp found-system
                                      previous previous-time))
                     (merge-pathnames "cities.dat" pathname))))
    (with-open-file (in city-data)
      (iter (for name in-stream in)
            (for long in-stream in)
            (for lat in-stream in)
            (for pop in-stream in)
            (collect (make-city :name name :long long :lat lat))))))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities* :test #'string-equal))

(defconstant earth-radius (/ 20000 pi)
  "Radius of planet earth in kilometers.")

(defun great-circle-distance (city1 city2)
  "The great circle distance between two cities (structures). (For cities that
are very close together, the cosine of the central angle can be greater than
one, leading to a complex central angle. To prevent this we max the cosine at
1."
  (let* ((long1 (rad<-deg (city-long city1)))
         (long2 (rad<-deg (city-long city2)))
         (lat1  (rad<-deg (city-lat city1)))
         (lat2  (rad<-deg (city-lat city2)))

         (delta-long (abs (- long1 long2)))

         (central-angle (acos (min 1
                                   (+ (* (sin lat1)
                                         (sin lat2))
                                      (* (cos lat1)
                                         (cos lat2)
                                         (cos delta-long)))))))
    (* earth-radius central-angle)))

(defun rad<-deg (deg)
  "Convert degrees to radians."
  (* 2 pi (/ deg 360)))

(defun cities-within-range (city &optional (range 1000.0))
  "Find all cities within RANGE kilometers."
  (remove-if-not #'(lambda (c)
                     (and (not (eq c city))
                          (< (great-circle-distance c city) range)))
                 *cities*))

(defun trip (start dest)
  "Search for a way from the start to dest."
  (beam-search (city start)
               (is (city dest))
               #'cities-within-range
               (lambda (city) (great-circle-distance city (city dest)))
               1))

(defun trip-range (start dest range)
  "Search for a way from the start to dest with a given range limit."
  (beam-search (city start)
               (is (city dest))
               (lambda (city) (cities-within-range city range))
               (lambda (city) (great-circle-distance city (city dest)))
               1))
