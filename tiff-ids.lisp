(in-package #:cl-tiff)

;;;
;;;  Exif/Tiff basic types
;;; 

(define-enumeration ifd-tag
    (gps-version-id                     0)
  (gps-latitude-ref                     1)
  (gps-latitude				2)
  (gps-longitude-ref			3)
  (gps-longitude			4)
  (gps-altitude-ref			5)
  (gps-altitude				6)
  (gps-timestamp			7)
  (gps-satellites			8)
  (gps-status				9)
  (gps-measure-mode			10)
  (gps-degree-of-precision		11)
  (gps-speed-ref			12)
  (gps-speed				13)
  (gps-track-ref			14)
  (gps-track				15)
  (gps-img-direction-ref		16)
  (gps-img-direction			17)
  (gps-map-datum			18)
  (gps-dest-latitude-ref		19)
  (gps-dest-latitude			20)
  (gps-dest-longitude-ref		21)
  (gps-dest-longitude			22)
  (gps-dest-bearing-ref			23)
  (gps-dest-bearing			24)
  (gps-dest-distance-ref		25)
  (gps-dest-distance			26)
  (gps-processing-method		27)
  (gps-area-information			28)
  (gps-datestamp			29)
  (gps-differential			30)
  (subfile-type                         254)
  (image-width				256)
  (image-length				257)
  (bits-per-sample                      258)
  (compression				259)
  ;; (resolution-unit			259)
  (photometric-interpretation		262)
  (make					271)
  (model				272)
  (strip-offsets			273)
  (orientation				274)
  (samples-per-pixel                    277)
  (rows-per-strip                       278)
  (strip-byte-counts                    279)
  (min-sample-value			280)
  (max-sample-value			281)
  (x-resolution				282)
  (y-resolution				283)
  (planar-config                        284)
  (resolution-unit                      296)
  (date-time				306)
  (predictor                            317)
  (color-map                            320)
  (tile-width                           322)
  (tile-length                          323)
  (tile-offsets                         324)
  (tile-byte-count                      325)
  (sub-ifd                              330) ;rev 6 tech note 1
  ;; TODO per tech notes 2002, should ignore sub-ifd with bit depth other than 8
  (ink-set                              332)
  (ink-names                            333)
  (number-of-inks                       334)
  (dot-range                            336)
  (extra-samples                        338)
  (clip-path                            343) ;rev 6 tech note 2
  (x-clip-path-units                    344)
  (y-clip-path-units                    345)
  (indexed                              346)  ;rev 6 tech note 3
  (opi-proxy                            351)
  (JPEG-proc                            512)
  (JPEG-Interchange-Format		513)
  (JPEG-Interchange-Format-Length	514)
  (JPEG-restart-interval                515)
  (JPEG-lossless-predictors             517)
  (JPEG-point-transforms                518)
  (JPEG-q-tables                        519)
  (JPEG-DC-tables                       520)
  (JPEG-AC-tables                       521)
  (YCbCr-Positioning			531)
  (image-id                             32781) ;opi related tags
  (it8-raster-padding                   34019)
  (it8-color-table                      34022)
  (private-exif-ifd			34665)
  (gps-info				34853)
  (exposure-time			33434)
  (f-number				33437)
  (exposure-program			34850)
  (spectral-sensitivity			34852)
  (iso-speed-ratings			34855)
  (oecf					34856)
  (exif-version				36864)
  (date-time-original			36867)
  (date-time-digitized			36868)
  (components-configuration		37121)
  (compressed-bits-per-pixel		37122)
  (shutter-speed-value			37377)
  (aperture-value			37378)
  (brightness-value			37379)
  (exposure-bias-value			37380)
  (max-aperture-value			37381)
  (subject-distance			37382)
  (metering-mode			37383)
  (light-source				37384)
  (flash				37385)
  (focal-length				37386)
  (subject-area				37396)
  (maker-note				37500)
  (user-comment				37510)
  (subsec-time				37520)
  (subsec-time-original			37521)
  (subsec-time-digitized		37522)
  (image-source-data                    37724)
  (flashpix-version			40960)
  (color-space				40961)
  (pixel-x-dimension			40962)
  (pixel-y-dimension			40963)
  (related-sound-file			40964)
  (interoperability-ifd                 40965)
  (flash-energy				41483)
  (spatial-frequency-response		41484)
  (focal-plane-x-resolution		41486)
  (focal-plane-y-resolution		41487)
  (focal-plane-resolution-unit		41488)
  (subject-location			41492)
  (exposure-index			41493)
  (sensing-method			41495)
  (file-source				41728)
  (scene-type				41729)
  (cfa-pattern				41730)
  (custom-rendered			41985)
  (exposure-mode			41986)
  (white-balance			41987)
  (digital-zoom-ratio			41988)
  (focal-length-in-35mm-film            41989)
  (scene-capture-type			41990)
  (gain-control				41991)
  (contrast				41992)
  (saturation				41993)
  (sharpness				41994)
  (device-setting-description		41995)
  (subject-distance-range		41996)
  (image-unique-id			42016))

(defparameter special-ifd-tags '(private-exif-ifd
                                 gps-info
                                 interoperability-ifd))

