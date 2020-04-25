;;;; rayngine.lisp

(in-package #:rayngine)

(defvar *ambient-intensity* (v3:vec 0.1 0.1 0.1))

(defun ray-point (ray dist)
  (v3:+ (ray-origin ray) (v3:scale (ray-direction ray) dist)))

(defvar *scene* (list (make-thing
                       :shape (make-sphere :radius 2f0
                                           :center (v3:vec -2 2 2))
                       :material (make-material :diffuse (v3:vec 0.7)
                                                :specular (v3:vec 0.4)
                                                :fresnel (v3:vec 0.2)
                                                :shininess 5f0))
                      (make-thing
                       :shape (make-sphere :radius 1.5f0
                                           :center (v3:vec 1.5 1.5 2))
                       :material (make-material :ambient (v3:vec 0.6 0.2 0.2)
                                                :diffuse (v3:vec 0.7 0.2 0.2)
                                                :specular (v3:vec 0.4 0.2 0.2)
                                                :fresnel (v3:vec 0.2 0.1 0.1)
                                                :shininess 5f0))
                      (make-thing
                       :shape (make-sphere :radius 1f0
                                           :center (v3:vec 1.5 1 -1))
                       :material (make-material :ambient (v3:vec 0.2 0.6 0.2)
                                                :diffuse (v3:vec 0.2 0.7 0.2)
                                                :specular (v3:vec 0.2 0.4 0.2)
                                                :fresnel (v3:vec 0.1 0.2 0.1)
                                                :shininess 5f0))
                      (make-thing
                       :shape (make-sphere :radius 0.5f0
                                           :center (v3:vec -2 0.5 -1))
                       :material (make-material :ambient (v3:vec 0.2 0.2 0.6)
                                                :diffuse (v3:vec 0.2 0.2 0.7)
                                                :specular (v3:vec 0.2 0.2 0.4)
                                                :fresnel (v3:vec 0.1 0.1 0.2)
                                                :shininess 5f0))
                      (make-thing
                       :shape (make-plane :normal (v3:vec 0 1 0))
                       :material (make-material :ambient (v3:vec 0.1)
                                                :diffuse (v3:vec 0.1)
                                                :specular (v3:vec 0.9)
                                                :fresnel (v3:vec 0.9)
                                                :shininess 16f0))
                      #+(or)(make-thing
                       :shape (make-plane :point (v3:vec 0 0 6)
                                          :normal (v3:vec 0 0 -1))
                       :material (make-material :ambient (v3:vec 0.1)
                                                :diffuse (v3:vec 0.45)
                                                :specular (v3:vec 0.45)
                                                :fresnel (v3:vec 0.2)
                                                :shininess 4f0))))

(defun ambient (out material)
  (declare (optimize speed))
  (v3:*! out (material-ambient material) *ambient-intensity*))

(defun diffuse (out material normal light-dir)
  (declare (optimize speed))
  (v3:scale! out (material-diffuse material) (v3:dot normal light-dir)))

(defun specular (out material viewer-dir light-dir normal)
  (let ((halfway (v3:normalize! out (v3:+! out viewer-dir light-dir))))
    (v3:expt! halfway (v3:scale (material-specular material)
                                (max (v3:dot halfway normal) 0f0))
              (material-shininess material))))

(defun shadow-p (origin light-dir light-dist)
  (declare (optimize speed)
           (v3:vec origin light-dir)
           (single-float light-dist))
  (let ((ix (smarter-intersections (make-ray :origin origin
                                             :direction light-dir)
                                   *scene*)))
    (if ix
        (if (< (the single-float (car ix)) light-dist)
            t
            nil)
        nil)))

(defun pointer (origin target)
  (declare (optimize speed))
  (v3:normalize (v3:- target origin)))
(defun pointer! (out origin target)
  (declare (optimize speed))
  (v3:normalize! out (v3:-! out target origin)))

(defun reflection (normal incoming)
  (declare (optimize speed))
  (v3:- incoming (v3:scale normal (* 2 (v3:dot normal incoming)))))
(defun reflection! (out normal incoming)
  (declare (optimize speed))
  (v3:-! out incoming (v3:scale! out normal (* 2 (v3:dot normal incoming)))))

(defun quadratic (A B C)
  (declare (optimize speed)
           (single-float A B C))
  (let ((foo (- (* B B) (* 4 A C)))
        (bar (* 2 A)))
    (cond
      ((minusp foo)
       nil)
      ((plusp foo)
       (let ((tmp (sqrt foo)))
         (cons (/ (- tmp B) bar)
               (/ (- 0 tmp B) bar))))
      ((zerop foo)
       (/ (- B) bar)))))

(defun intersect (ray object cache)
  (declare (optimize speed)
           (ray ray) (v3:vec cache))
  (etypecase object
    (sphere
     (v3:-! cache (ray-origin ray) (sphere-center object))
     (let* ((dir (ray-direction ray))
            (A (v3:dot dir dir))
            (B (* 2 (v3:dot dir cache)))
            (C (- (v3:dot cache cache) (expt (sphere-radius object) 2))))
       (quadratic A B C)))
    (plane
     (let* ((n (plane-normal object))
            (denom (v3:dot (ray-direction ray) n)))
       (if (< (abs denom) 5f-5)
           nil
           (/ (v3:dot (v3:-! cache (plane-point object) (ray-origin ray)) n)
              denom))))))


(defun smarter-intersections (ray scene cache)
  (declare (optimize speed))
  (loop :for thing :in scene
        :for obj := (thing-shape thing)
        :for intrsct := (intersect ray obj cache)
        :with foo := nil
        :do (if foo
                (typecase intrsct
                  (cons (let* ((fi (car intrsct))
                               (fo (cdr intrsct))
                               (objt (if (< fi 3f-4)
                                         (if (< fo 3f-4)
                                             nil
                                             fo)
                                         (if (< fo 3f-4)
                                             fi
                                             (min fi fo)))))
                          (declare (single-float fi fo))
                          (when (and objt (< objt (the single-float (car foo))))
                            (setf (car foo) objt (cdr foo) thing))))
                  (single-float (when (and (> intrsct 3f-4)
                                           (< intrsct
                                              (the single-float (car foo))))
                                  (setf (car foo) intrsct (cdr foo) thing))))
                (typecase intrsct
                  (cons (let* ((fi (car intrsct))
                               (fo (cdr intrsct))
                               (objt (locally (declare (single-float fi fo))
                                       (if (< (the single-float fi) 3f-4)
                                           (if (< (the single-float fo) 3f-4)
                                               nil
                                               fo)
                                           (if (< (the single-float fo) 3f-4)
                                               fi
                                               (min (the single-float fi)
                                                    fo))))))
                          (when objt (setf foo (cons objt thing)))))
                  (single-float (when (> intrsct 3f-4)
                                  (setf foo (cons intrsct thing))))))
        :finally (when foo
                   (setf (cdr foo) (cons (cdr foo) nil))
                   (return foo))))


(defun find-normal! (out object point)
  (declare (optimize speed))
  (etypecase object
    (sphere (v3:normalize! out (v3:-! out point (sphere-center object))))
    (plane (v3:copy! out (plane-normal object)))))

(defun find-uv (object point normal)
  (declare (optimize speed)
           (v3:vec point normal))
  (declare (ignore point)) ; remove if needed later
  (etypecase object
    (sphere
     (v2:vec (* 0.5 (1+ (* (atan (the (single-float -1.0 1.0) (s:.z normal))
                                 (s:.x normal))
                           #.(/ o:pi))))
             (* (acos (the (single-float -1.0 1.0) (s:.y normal)))
                #.(/ o:pi))))
    (plane
     (v2:vec))))

(defun intersection (ray x normal)
  (declare (optimize speed))
  (let* ((dist (car x))
         (thing (cadr x))
         (point (ray-point ray dist))
         (shape (thing-shape thing)))
    (find-normal! normal shape point)
    (make-intersection :point point
                       :normal normal
                       :uv (find-uv shape point normal)
                       :in-dir (ray-direction ray))))


(defparameter *width* 900)
(defparameter *height* 600)


(defun camspace-rotator (camera)
  (q:to-mat3 (camera-rotation camera)))

(defun camcr (x y camera)
  (v3:vec (* (1- (* 2 (/ (+ x 0.5f0) *width*)))
             (tan (/ (camera-fov-x camera) 2)))
          (* (- 1 (* 2 (/ (+ y 0.0f0) *height*)))
             (tan (/ (camera-fov-y camera) 2)))
          1))

(defun eyedir (x y camera)
  (declare (optimize speed))
  (v3:normalize (m3:*v3 (camera-rotation-matrix camera)
                        (camcr x y camera))))

(declaim (ftype (function (v3:vec) (values (unsigned-byte 8)
                                           (unsigned-byte 8)
                                           (unsigned-byte 8)))
                rgb8))
(defun rgb8 (pixel)
  (declare (optimize speed) (v3:vec pixel))
  (flet ((srgb (f)
           (declare ((single-float 0f0) f))
           (max 0 (min 255
                       (the fixnum
                            (floor (* 256 (the (single-float 0f0)
                                               (expt f (/ 2.2f0))))))))))
    (declare (inline srgb))
    (v3:with-components ((p pixel))
      (values (srgb px)
              (srgb py)
              (srgb pz)))))

(defun doit ()
  (setf (camera-rotation-matrix *camera*) (camspace-rotator *camera*))
  (dotimes (y *height*)
    (dotimes (x *width*)
      (rgb8 (trace-pixel x y)))))

(defun render-to-png ()
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :width *width* :height *height*)))
    (with-open-file (stream "test.png"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (setf (camera-rotation-matrix *camera*) (camspace-rotator *camera*))
      (loop :for y :below *height*
            :with arr := (make-array 3 :element-type '(unsigned-byte 8)
                                       :adjustable nil)
            :do (loop :for x :below *width*
                      :do (multiple-value-bind (r g b) (rgb8 (trace-pixel x y))
                            (setf (aref arr 0) r (aref arr 1) g (aref arr 2) b)
                            (zpng:write-pixel arr png))))
      (zpng:finish-png png))))

(defun render-to-array (array)
  )

(defvar *max-depth* 3)

(defun raytrace (ray state &optional (depth 0))
  (declare (optimize debug))
  (with-temp-vars (cache normal) (state-index state) (state-stack state)
    (let ((hit (smarter-intersections ray (state-scene state) cache)))
      (if (null hit)
          (state-result state)          ;(v3:vec 0.8 0.8 1.7)
          (illumination (cadr hit) (intersection ray hit normal) state depth)))))

(defun illumination (thing intersection state &optional (depth 0))
  (with-accessors ((max-depth state-max-depth) (light state-light)
                   (result state-result) (temp state-temp)
                   (diff state-diffuse) (spec state-specular)
                   (ambi state-ambient) (stack state-stack)
                   (index state-index))
      state
    (with-accessors ((shape thing-shape) (mat thing-material)) thing
      (with-accessors ((point intersection-point)
                       (normal intersection-normal)
                       (uv intersection-uv)
                       (in-dir intersection-in-dir))
          intersection
        (with-temp-vars (light-dir viewer-dir reflection-dir)
                        index stack
          (pointer! light-dir point (light-position light))
          (v3:negate! viewer-dir in-dir)
          (reflection! reflection-dir normal in-dir)
          (let* ((reflp (< depth max-depth))
                 (lightp (> (v3:dot light-dir normal) 0)))
            (v3:copy! result v3:+zero+)
            (v3:copy! temp v3:+zero+)
            (when reflp
              (setf result (raytrace (make-ray :origin point
                                               :direction reflection-dir)
                                     state
                                     (1+ depth)))
              (v3:*! result result (material-fresnel mat)))
            (when lightp
              (let ((light-distance (v3:length (v3:-! temp (light-position light)
                                                      point))))
                (let ((shadow (softshadow point light-dir light light-distance state)))
                  (when (> shadow 0f0)
                    (v3:copy! temp (specular spec mat viewer-dir light-dir normal))
                    (v3:+! temp temp (diffuse diff mat normal light-dir))
                    (v3:*! temp temp (light-color light))
                    (v3:scale! temp temp (* shadow (/ light-distance)))
                    (v3:+! result result temp)))))
            (v3:+! result result (ambient ambi mat))))))))

(defvar *camera* (make-camera :rotation (q:from-axis-angle (v3:vec 1 0 0)
                                                           (/ o:pi 5f0))
                              :position (v3:vec 0 7 -7)
                              :fov-y o:pi/4))

(defvar *state* (make-state :scene *scene*))

(defun trace-pixel (x y)
  (declare (optimize speed))
  (let ((ray (make-ray :origin (camera-position *camera*)
                       :direction (eyedir x y *camera*))))
    (raytrace ray *state*)))

(defun foo ()
  (dolist (x *scene*)
    (let ((y (thing-shape x))
          (m (thing-material x)))
      (typecase y
        (sphere (format t "~
sphere: c = ~a, r = ~a
   mat: ka = ~a, kd = ~a,
        ks = ~a, kr = ~a, shine = ~a~%"
                        (sphere-center y)
                        (sphere-radius y)
                        (material-ambient m)
                        (material-diffuse m)
                        (material-specular m)
                        (material-fresnel m)
                        (material-shininess m)))
        (plane (format t "~
plane: p = ~a, n = ~a
  mat: ka = ~a, kd = ~a,
       ks = ~a, kr = ~a, shine = ~a~%"
                       (plane-point y)
                       (plane-normal y)
                       (material-ambient m)
                       (material-diffuse m)
                       (material-specular m)
                       (material-fresnel m)
                       (material-shininess m)))))))

(defmacro with-profiling (&body body)
  (let ((packages (remove-if-not
                   (lambda (x)
                     (or (string= x "RAYNGINE") (search "ORIGIN" x)))
                   (mapcar #'package-name (list-all-packages)))))
    `(unwind-protect
          (progn
            (sb-profile:unprofile)
            (sb-profile:profile ,@packages)
            ,@body)
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset))))
