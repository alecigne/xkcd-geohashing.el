;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

;;; Commentary:
;; Tests for xkcd-geohashing.el, an implementation of XKCD's geohashing
;; algorithm for Emacs.

;;; Code:

(require 'xkcd-geohashing)

(describe "The validate macro"

  (it "should not throw if no error"
    (expect (xkcd-gh--validate
             ((numberp 2) "Not a number")
             ((zerop 0) "Not zero"))
            :not :to-throw))

  (it "should check a condition and produce an error"
    (expect (xkcd-gh--validate ((numberp "a") "Not a number"))
            :to-throw 'error '("Not a number")))

  (it "should check multiple conditions and produce an error"
    (expect (xkcd-gh--validate
             ((numberp "a") "Not a number")
             ((zerop 2) "Not zero"))
            :to-throw 'error '("Not a number; Not zero"))
    (expect (xkcd-gh--validate
             ((numberp 2) "Not a number")
             ((zerop 2) "Not zero"))
            :to-throw 'error '("Not zero"))))

(describe "The coordinates logic"

  (it "should build coordinates from latitude and longitude"
    (dolist (test '((28.57 -12.10)
                    (-45.0 120.0)
                    (0.0 0.0)
                    (-0.0 -0.0)
                    (90 180)
                    (-90 -180)))
      (cl-destructuring-bind (lat lon) test
        (let ((coord (xkcd-gh--coords lat lon)))
          (expect (xkcd-gh--coords-p coord) :to-be-truthy)
          (expect (xkcd-gh--coords-lat coord) :to-equal lat)
          (expect (xkcd-gh--coords-lon coord) :to-equal lon)))))

  (it "should control its input"
    (let* ((errlat "Latitude must be a number in [-90, 90]")
           (errlon "Longitude must be a number in [-180, 180]")
           (errboth (concat errlat "; " errlon)))
      (expect (xkcd-gh--coords -91 10) :to-throw 'error `(,errlat))
      (expect (xkcd-gh--coords 37 -181) :to-throw 'error `(,errlon))
      (expect (xkcd-gh--coords -95 192) :to-throw 'error `(,errboth))))

  (it "should extract latitude from coords"
    (expect (xkcd-gh--coords-lat (xkcd-gh--coords 12 -4)) :to-be 12)
    (expect (xkcd-gh--coords-lat (xkcd-gh--coords -12 4)) :to-be -12))

  (it "should extract longitude from coords"
    (expect (xkcd-gh--coords-lon (xkcd-gh--coords 12 -4)) :to-be -4)
    (expect (xkcd-gh--coords-lon (xkcd-gh--coords -12 4)) :to-be 4))

  (it "should compute the haversine distance between two coordinates"
    ;; Checked against this resource: https://www.vcalc.com/wiki/vcalc/haversine-distance
    (expect (xkcd-gh--haversine (xkcd-gh--coords 45 10) (xkcd-gh--coords 39 8))
            :to-be-close-to 687 0)
    (expect (xkcd-gh--haversine (xkcd-gh--coords 0 0) (xkcd-gh--coords 90 90))
            :to-be-close-to 10007 0))

  (it "should compute the distance between coords and user's home"
    (setq calendar-latitude 39 calendar-longitude 8)
    (expect (xkcd-gh--distance-from-home (xkcd-gh--coords 45 10))
            :to-be-close-to 687 0)
    ;; Reset original values
    (setq calendar-latitude nil calendar-longitude nil)))

(describe "The graticules logic"

  (defun expect-graticule (g lat-sign lat-deg lon-sign lon-deg)
    "Validate graticule G against its components.
The components are LAT-SIGN, LAT-DEG, LON-SIGN and LON-DEG."
    (expect (xkcd-gh--graticule-p g) :to-be-truthy)
    (expect (xkcd-gh--graticule-lat-sign g) :to-equal lat-sign)
    (expect (xkcd-gh--graticule-lat-deg g) :to-equal lat-deg)
    (expect (xkcd-gh--graticule-lon-sign g) :to-equal lon-sign)
    (expect (xkcd-gh--graticule-lon-deg g) :to-equal lon-deg))

  (describe "when building a graticule"

    (it "should build graticule from its components"
      (expect-graticule (xkcd-gh--graticule '+ 37 '- 122) '+ 37 '- 122)
      (expect-graticule (xkcd-gh--graticule '- 0 '- 122) '- 0 '- 122))

    (it "should validate input"
      (let* ((errlat "Absolute latitude must be a number in [0, 90]")
             (errlon "Absolute longitude must be a number in [0, 180]")
             (errboth (concat errlat "; " errlon)))
        (expect (xkcd-gh--graticule '+ 91 '- 122) :to-throw 'error `(,errlat))
        (expect (xkcd-gh--graticule '+ 45 '- 185) :to-throw 'error `(,errlon))
        (expect (xkcd-gh--graticule '+ 91 '- 185) :to-throw 'error `(,errboth)))))

  (describe "when parsing a graticule string"

    (it "should build a correct graticule"
      (expect-graticule (xkcd-gh--parse-graticule "[37,-122]") '+ 37 '- 122)
      (expect-graticule (xkcd-gh--parse-graticule "[+37,-122]") '+ 37 '- 122))

    (it "should validate its input"
      (expect (xkcd-gh--parse-graticule "[40,a]")
              :to-throw 'error '("Invalid graticule string: [40,a]"))
      ;; Small integration test: error is thrown by xkcd-gh--graticule
      (expect (xkcd-gh--parse-graticule "[91,90]")
              :to-throw 'error '("Absolute latitude must be a number in [0, 90]"))))

  (it "should convert a graticule to coords"
    (let ((coord (xkcd-gh--graticule-to-coords (xkcd-gh--graticule '+ 37 '- 122)
                                               (xkcd-gh--coords 0.5 0.5))))
      (expect (xkcd-gh--coords-p coord) :to-be-truthy)
      (expect (xkcd-gh--coords-lat coord) :to-equal 37.5)
      (expect (xkcd-gh--coords-lon coord) :to-equal -122.5))))

(describe "The dates logic"

  (it "should return yesterday's date"
    (expect (time-equal-p
             (xkcd-gh--yesterday (date-to-time "2025-05-29"))
             (date-to-time "2025-05-28"))
            :to-be-truthy))

  (it "should format a date to an ISO string"
    (expect (xkcd-gh--format-date (encode-time '(0 0 0 29 5 2025)))
            :to-equal "2025-05-29"))

  (describe "when parsing a date string"

    (it "should build a correct Lisp timestamp"
      (expect (xkcd-gh--parse-date "2025-05-29")
              :to-equal (encode-time '(0 0 0 29 5 2025))))

    (it "should validate input"
      (expect (xkcd-gh--parse-date "2025-05--29")
              :to-throw 'error '("Invalid date string: 2025-05--29")))))

(describe "The DJIA logic"
  (it "should format a DJIA opening"
    (expect (xkcd-gh--format-djia 12000) :to-equal "12000.00")
    (expect (xkcd-gh--format-djia 12000.1) :to-equal "12000.10")
    (expect (xkcd-gh--format-djia 12000.15) :to-equal "12000.15")))

(describe "The geohashing algorithm"
  (it "should compute a correct geohash"
    ;; https://xkcd.com/426/
    (let ((geohash (xkcd-gh--geohash (date-to-time "2005-05-26")
                                     10458.68
                                     (xkcd-gh--parse-graticule "[37,-122]"))))
      (expect (xkcd-gh--coords-lat geohash) :to-be-close-to 37.857713 6)
      (expect (xkcd-gh--coords-lon geohash) :to-be-close-to -122.544543 6))))

(describe "The globalhash algorithm"
  (it "should compute a correct globalhash"
    ;; https://geohashing.site/geohashing/30W_Time_Zone_Rule
    (let ((globalhash (xkcd-gh--globalhash (date-to-time "2008-05-30") 12593.87)))
      (expect (xkcd-gh--coords-lat globalhash) :to-be-close-to -31.91030 5)
      (expect (xkcd-gh--coords-lon globalhash) :to-be-close-to 73.65004 5))))

(describe "The 30W algorithm"
  ;; https://geohashing.site/geohashing/30W_Time_Zone_Rule
  (let ((g-without-30w-rule (xkcd-gh--graticule '+ 68 '- 30))
        (g-with-30w-rule (xkcd-gh--graticule '+ 68 '- 29)))

    (it "should not be detected before cutoff date"
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-26") g-without-30w-rule)
       :to-be nil)
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-26") g-with-30w-rule)
       :to-be nil))

    (it "should be detected on cutoff date depending on graticule"
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-27") g-without-30w-rule)
       :to-be nil)
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-27") g-with-30w-rule)
       :to-be t))

    (it "should be detected after cutoff date depending on graticule"
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-28") g-without-30w-rule)
       :to-be nil)
      (expect
       (xkcd-gh--30w-rule-p (date-to-time "2008-05-28") g-with-30w-rule)
       :to-be t))))

(describe "The DJIA retrieval functionality"
  (let ((http-header (concat "HTTP/1.1 200 OK\n"
                             "Date: Thu, 29 May 2025 21:31:27 GMT\n"
                             "Server: Apache/2.4.62 (Debian)\n"
                             "Vary: Accept-Encoding\n"
                             "Content-Length: 28\n"
                             "Keep-Alive: timeout=5, max=100\n"
                             "Connection: Keep-Alive\n"
                             "Content-Type: text/plain;charset=UTF-8\n"
                             "\n")))

    (it "should get DJIA opening and parse it to floating number"
      (let ((buf (generate-new-buffer "*mock-http-response*")))
        (with-current-buffer buf
          (insert (concat http-header "12345.67"))
          (goto-char (point-min)))
        (spy-on 'url-retrieve-synchronously :and-return-value buf)
        (expect (xkcd-gh--fetch-djia (date-to-time "2025-05-29")) :to-equal 12345.67)
        (kill-buffer buf)))

    (it "should throw when weird value"
      (let ((buf (generate-new-buffer "*mock-http-response*")))
        (with-current-buffer buf
          (insert (concat http-header "hello"))
          (goto-char (point-min)))
        (spy-on 'url-retrieve-synchronously :and-return-value buf)
        (expect (xkcd-gh--fetch-djia (date-to-time "2025-05-29"))
                :to-throw 'error '("Invalid DJIA response for 2025-05-29: hello"))
        (kill-buffer buf)))

    (it "should throw when malformed response"
      (let ((buf (generate-new-buffer "*mock-http-response*")))
        (with-current-buffer buf
          (insert "foobar")
          (goto-char (point-min)))
        (spy-on 'url-retrieve-synchronously :and-return-value buf)
        (expect (xkcd-gh--fetch-djia (date-to-time "2025-05-29"))
                :to-throw 'error '("Malformed HTTP response from http://geo.crox.net/djia/2025-05-29"))
        (kill-buffer buf)))))

;;; xkcd-geohashing-tests.el ends here
