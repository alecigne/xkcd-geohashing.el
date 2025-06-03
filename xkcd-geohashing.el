;;; xkcd-geohashing.el ---  An implementation of XKCD's geohashing algorithm for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2025 Anthony Le Cigne

;; Author: Anthony Le Cigne <dev@lecigne.net>
;; Maintainer: Anthony Le Cigne <dev@lecigne.net>
;; Created: 2025-06-01
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/alecigne/xkcd-geohashing.el
;; Keywords: games

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; xkcd-geohashing.el is an implementation of XKCD's geohashing algorithm for
;; Emacs.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

;;; Variables

(defgroup xkcd-geohashing nil
  "Settings for xkcd-geohashing.el."
  :group 'applications
  :prefix "xkcd-gh-")

(defvar xkcd-gh--djia-cache (make-hash-table :test #'equal)
  "A date-based in-memory cache for DJIA openings.
Values are stored as floating point numbers.")

(defcustom xkcd-gh-use-djia-cache t
  "If non-nil, use the DJIA cache for lookups and storage."
  :type 'boolean
  :group 'xkcd-geohashing)

(defcustom xkcd-gh-djia-cache-file
  (expand-file-name "xkcd-geohashing-cache.el" user-emacs-directory)
  "The target file for persisting the DJIA cache."
  :type 'file
  :group 'xkcd-geohashing)

(defconst xkcd-gh--djia-url "http://geo.crox.net/djia/%s"
  "URL used to retrieve the DJIA opening for a given day, as a format string.")

(defconst xkcd-gh--30w-rule-cutoff (date-to-time "2008-05-27")
  "The cutoff date for the 30W rule.")

(defconst xkcd-gh--earth-radius 6371.0
  "The average Earth radius, used to compute the distance between coordinates.")

;;; Lisp helpers

(defmacro xkcd-gh--validate (&rest checklist)
  "Run CHECKLIST, with each check being of the form (TEST ERROR-MESSAGE).
Collect all ERROR-MESSAGEs where TEST is false.
Signal a single error if any checks fail."
  (let ((errors (cl-gensym "errors")))
    `(let ((,errors nil))
       ,@(mapcar
          (lambda (check)
            (let ((test (nth 0 check)) (msg (nth 1 check)))
              `(unless ,test (push ,msg ,errors))))
          checklist)
       (when ,errors
         (error (mapconcat #'identity (nreverse ,errors) "; "))))))

;;; Data structures and related functions

;;;; Coordinates
;; Coordinates are structs containing a latitude LAT and a longitude LON as
;; floating point numbers. Geohashes and globalhashes are special instances of
;; coordinates.

(cl-defstruct (xkcd-gh--coords
               (:constructor xkcd-gh--make-coords (lat lon)))
  lat
  lon)

(defun xkcd-gh--coords (lat lon)
  "Make coordinates from LAT and LON, validating bounds."
  (xkcd-gh--validate
   ((and (numberp lat) (<= -90 lat 90)) "Latitude must be a number in [-90, 90]")
   ((and (numberp lon) (<= -180 lon 180)) "Longitude must be a number in [-180, 180]"))
  (xkcd-gh--make-coords lat lon))

(defun xkcd-gh--haversine (from to)
  "Return great-circle distance (in km) between coordinates FROM and TO."
  (cl-labels ((deg->rad (deg) (* deg (/ float-pi 180.0)))
              (sin2 (x) (let ((s (sin x))) (* s s))))
    (let* ((lat1 (deg->rad (xkcd-gh--coords-lat from)))
           (lon1 (deg->rad (xkcd-gh--coords-lon from)))
           (lat2 (deg->rad (xkcd-gh--coords-lat to)))
           (lon2 (deg->rad (xkcd-gh--coords-lon to)))
           (dlat (/ (- lat2 lat1) 2.0))
           (dlon (/ (- lon2 lon1) 2.0)))
      (* 2 xkcd-gh--earth-radius (asin (sqrt (+ (sin2 dlat)
                                                (* (cos lat1) (cos lat2) (sin2 dlon)))))))))

(defun xkcd-gh--distance-from-home (coords)
  "Compute the distance between coordinates COORDS and the user's coordinates.
Return NIL if user's coordinates are not available."
  (when (and (boundp 'calendar-latitude) (numberp calendar-latitude)
             (boundp 'calendar-longitude) (numberp calendar-longitude))
    (let ((home (xkcd-gh--coords calendar-latitude calendar-longitude)))
      (xkcd-gh--haversine coords home))))

(defun xkcd-gh--coords-message (coords)
  "Display coordinates COORDS to the user in a friendly manner."
  (let ((dist (xkcd-gh--distance-from-home coords)))
    (message "Coordinates are at: %.5f, %.5f%s"
             (xkcd-gh--coords-lat coords)
             (xkcd-gh--coords-lon coords)
             (if dist (format " (%.1f km from home)" dist) ""))))

;;;; Graticules
;; Graticules are structs made of the following components: a latitude sign, a
;; latitude in degrees, a longitude sign, and a longitude in degrees. This data
;; structure accommodates the graticules with a `-0' component, which cannot be
;; represented as a number.
;;
;; Graticule strings have the following format: [LAT,LON]. LAT and LON can be
;; prefixed with `+' or `-'. The `+' sign can be omitted for positive numbers.

(defconst xkcd-gh--graticule-regex
  "\\`\\[\\([+-]?\\)\\([0-9]+\\),\\([+-]?\\)\\([0-9]+\\)\\]\\'"
  "Regex for parsing graticule strings.")

(cl-defstruct (xkcd-gh--graticule
               (:constructor xkcd-gh--make-graticule (lat-sign lat-deg lon-sign lon-deg)))
  lat-sign
  lat-deg
  lon-sign
  lon-deg)

(defun xkcd-gh--graticule (lat-sign lat-deg lon-sign lon-deg)
  "Make a graticule from its individual components, with validation.
LAT-SIGN: latitude sign; LAT-DEG: latitude in degrees; LON-SIGN:
longitude sign; LON-DEG: longitude in degrees."
  (xkcd-gh--validate
   ((and (numberp lat-deg) (<= 0 lat-deg 90)) "Absolute latitude must be a number in [0, 90]")
   ((and (numberp lon-deg) (<= 0 lon-deg 180)) "Absolute longitude must be a number in [0, 180]"))
  (xkcd-gh--make-graticule lat-sign lat-deg lon-sign lon-deg))

(defun xkcd-gh--parse-graticule (graticule-string)
  "Parse GRATICULE-STRING into a graticule.
An error will be thrown if GRATICULE-STRING's format is incorrect."
  (unless (string-match xkcd-gh--graticule-regex graticule-string)
    (error "Invalid graticule string: %s" graticule-string))
  (let* ((lat-sign (if (string= (match-string 1 graticule-string) "-") '- '+))
         (lat-deg  (string-to-number (match-string 2 graticule-string)))
         (lon-sign (if (string= (match-string 3 graticule-string) "-") '- '+))
         (lon-deg  (string-to-number (match-string 4 graticule-string))))
    (xkcd-gh--graticule lat-sign lat-deg lon-sign lon-deg)))

(defun xkcd-gh--signed-offset (sign deg &optional offset)
  "Compute DEG's signed offset.
OFFSET is added to DEG and the overall SIGN is applied to the result."
  (funcall sign (+ deg (or offset 0))))

(defun xkcd-gh--graticule-to-coords (graticule coords)
  "Convert graticule GRATICULE to coordinates.
Since a graticule coordinates don't have decimals, an offset COORDS is
provided in the form of coordinates."
  (let ((lat-sign   (xkcd-gh--graticule-lat-sign graticule))
        (lat-deg    (xkcd-gh--graticule-lat-deg graticule))
        (lon-sign   (xkcd-gh--graticule-lon-sign graticule))
        (lon-deg    (xkcd-gh--graticule-lon-deg graticule))
        (lat-offset (xkcd-gh--coords-lat coords))
        (lon-offset (xkcd-gh--coords-lon coords)))
    (xkcd-gh--coords (xkcd-gh--signed-offset lat-sign lat-deg lat-offset)
                     (xkcd-gh--signed-offset lon-sign lon-deg lon-offset))))

(defun xkcd-gh--read-graticule ()
  "Prompt for a graticule string and return a graticule."
  (xkcd-gh--parse-graticule (read-string "Enter graticule [lat,lon]: ")))

;;;; Dates
;; Dates are Emacs Lisp timestamps and are serialized to an ISO string.

(defconst xkcd-gh--date-regex "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
  "Regex for parsing date strings.")

(defun xkcd-gh--yesterday (date)
  "Get yesterday's date from DATE."
  (time-subtract date (days-to-time 1)))

(defun xkcd-gh--on-or-after (date target)
  "Check if DATE is on or after TARGET."
  (not (time-less-p date target)))

(defun xkcd-gh--format-date (date)
  "Get a string in ISO format (YYYY-MM-DD) from DATE."
  (format-time-string "%Y-%m-%d" date))

;; TODO Unused function from previous version of the code, might be useful
;; later.
(defun xkcd-gh--parse-date (date-string)
  "Parse a date string DATE-STRING to its internal value.
An error will be thrown if DATE-STRING's format is incorrect."
  (unless (string-match xkcd-gh--date-regex date-string)
    (error "Invalid date string: %s" date-string))
  (date-to-time date-string))

(defun xkcd-gh--read-date ()
  "Prompt for a date string and return a date."
  (org-read-date nil t))

;;;; DJIA openings
;; DJIA openings are simply floating point numbers like 41525.7 or
;; 12542.9. Their serialized version contains two decimals.

(defun xkcd-gh--format-djia (djia)
  "Format a DJIA from a floating point number to a string."
  (format "%.2f" djia))

;;; Main algorithm

(defun xkcd-gh--make-gh-string (date djia)
  "Make the geohashing source string from DATE and DJIA."
  (concat (xkcd-gh--format-date date) "-" (xkcd-gh--format-djia djia)))

(defun xkcd-gh--hex-frac (hex16)
  "Return a float in range [0.0, 1.0) corresponding to 0.<HEX16> in base 16."
  (/ (string-to-number hex16 16) (float (expt 16 16))))

(defun xkcd-gh--fractional-coords (date djia)
  "Return fractional latitude and longitude derived from DATE and DJIA opening."
  (let* ((hash (md5 (xkcd-gh--make-gh-string date djia)))
         (lat-frac (xkcd-gh--hex-frac (substring hash 0 16)))
         (lon-frac (xkcd-gh--hex-frac (substring hash 16 32))))
    (xkcd-gh--coords lat-frac lon-frac)))

(defun xkcd-gh--geohash (date djia graticule)
  "Return full coordinates for GRATICULE using DATE and DJIA opening."
  (xkcd-gh--graticule-to-coords graticule (xkcd-gh--fractional-coords date djia)))

(defun xkcd-gh--globalhash (date djia)
  "Return globalhash using DATE and DJIA opening."
  (let ((coords-frac (xkcd-gh--fractional-coords date djia)))
    (xkcd-gh--coords (- (* (xkcd-gh--coords-lat coords-frac) 180) 90)
                     (- (* (xkcd-gh--coords-lon coords-frac) 360) 180))))

(defun xkcd-gh--30w-rule-p (date graticule)
  "Check that GRATICULE is eligible for the 30W rule on DATE."
  (and
   (xkcd-gh--on-or-after date xkcd-gh--30w-rule-cutoff)
   (let ((lon (xkcd-gh--signed-offset (xkcd-gh--graticule-lon-sign graticule)
                                      (xkcd-gh--graticule-lon-deg graticule))))
     (> lon -30))))

;;; Fetching DJIA openings

(defun xkcd-gh--empty-cache-p ()
  "Check if DJIA cache is empty."
  (zerop (hash-table-count xkcd-gh--djia-cache)))

(defun xkcd-gh--empty-cache ()
  "Empty the in-memory DJIA cache."
  (unless (xkcd-gh--empty-cache-p)
    (clrhash xkcd-gh--djia-cache)))

(defun xkcd-gh--cache-insert (k v)
  "Associate key K to value V in DJIA cache."
  (puthash k v xkcd-gh--djia-cache))

(defun xkcd-gh--load-cache ()
  "Load the DJIA openings cache into memory."
  (when (file-exists-p xkcd-gh-djia-cache-file)
    (with-temp-buffer
      (insert-file-contents xkcd-gh-djia-cache-file)
      (let ((data (read (current-buffer))))
        (xkcd-gh--empty-cache)
        (dolist (pair data)
          (xkcd-gh--cache-insert (car pair) (cdr pair)))))))

(defun xkcd-gh--save-cache ()
  "Persist the in-memory DJIA cache to disk."
  (with-temp-file xkcd-gh-djia-cache-file
    (let (acc)
      (maphash (lambda (k v) (push (cons k v) acc)) xkcd-gh--djia-cache)
      (prin1 acc (current-buffer)))))

(defun xkcd-gh--fetch-djia (date)
  "Retrieve the DJIA opening for DATE."
  (let* ((iso-date (xkcd-gh--format-date date))
         (url (format xkcd-gh--djia-url iso-date))
         (buf (url-retrieve-synchronously url t t 10)))
    (unless buf (error "Failed to fetch DJIA value for %s" iso-date))
    (with-current-buffer buf
      (goto-char (point-min))
      (unless (re-search-forward "\n\n" nil t)
        (kill-buffer buf)
        (error "Malformed HTTP response from %s" url))
      (let ((result (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (kill-buffer buf)
        (if (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" result)
            (string-to-number result)
          (error "Invalid DJIA response for %s: %s" iso-date result))))))

(defun xkcd-gh--get-djia (date)
  "Return the DJIA opening for DATE, using the cache or fetching if necessary."
  (let ((iso-date (xkcd-gh--format-date date)))
    (if xkcd-gh-use-djia-cache
        (progn
          (when (xkcd-gh--empty-cache-p) (xkcd-gh--load-cache))
          (or (gethash iso-date xkcd-gh--djia-cache)
              (let ((djia (xkcd-gh--fetch-djia date)))
                (xkcd-gh--cache-insert iso-date djia)
                (xkcd-gh--save-cache)
                djia)))
      (xkcd-gh--fetch-djia date))))

;;; Wrapping up

(defun xkcd-gh--compute-geohash (date graticule)
  "Compute the geohashing for a given DATE and GRATICULE."
  (let* ((adjusted-date (if (xkcd-gh--30w-rule-p date graticule)
                            (xkcd-gh--yesterday date)
                          date))
         (djia (xkcd-gh--get-djia adjusted-date)))
    (xkcd-gh--geohash date djia graticule)))

;;;###autoload
(defun xkcd-gh-show-geohash ()
  "Prompt for a date and graticule, compute the geohash, and display the result."
  (interactive)
  (let* ((date (xkcd-gh--read-date))
         (graticule (xkcd-gh--read-graticule))
         (coords (xkcd-gh--compute-geohash date graticule)))
    (xkcd-gh--coords-message coords)))

(defun xkcd-gh--compute-globalhash (date)
  "Compute the globalhash for a given DATE."
  (let* ((adjusted-date (xkcd-gh--yesterday date))
         (djia (xkcd-gh--get-djia adjusted-date)))
    (xkcd-gh--globalhash date djia)))

;;;###autoload
(defun xkcd-gh-show-globalhash ()
  "Prompt for the date, compute the globalhash, and display the result."
  (interactive)
  (let* ((date (xkcd-gh--read-date))
         (coords (xkcd-gh--compute-globalhash date)))
    (xkcd-gh--coords-message coords)))

(provide 'xkcd-geohashing)

;;; xkcd-geohashing.el ends here
