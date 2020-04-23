;;;; rayngine.lisp

(in-package #:rayngine)

(defvar *ambient-intensity* (v3:vec 0.1 0.1 0.1))

(defstruct light
  (position (v3:vec -6 4 0) :type v3:vec)
  (color (v3:vec 1) :type v3:vec))

(defstruct material
  (ambient (v3:vec 0.5) :type v3:vec)
  (diffuse (v3:vec 0.5) :type v3:vec)
  (specular (v3:vec 0.5) :type v3:vec)
  (fresnel (v3:vec 0.5) :type v3:vec)
  (shininess 1f0 :type single-float))

(defstruct ray
  (origin (v3:vec) :type v3:vec)
  (direction (v3:vec) :type v3:vec))

(defun ray-point (ray dist)
  (v3:+ (ray-origin ray) (v3:scale (ray-direction ray) dist)))

(defstruct sphere
  (radius 1f0 :type single-float)
  (center (v3:vec) :type v3:vec))

(defstruct plane
  (point (v3:vec) :type v3:vec)
  (normal (v3:vec) :type v3:vec))

(defstruct thing
  shape
  (material (make-material) :type material))

(defstruct intersection
  (point (v3:vec) :type v3:vec)
  (normal (v3:vec) :type v3:vec)
  (uv (v2:vec) :type v2:vec)
  (in-dir (v3:vec) :type v3:vec))

(defstruct (camera (:constructor %make-camera))
  (position (v3:vec) :type v3:vec)
  (rotation (q:quat 1) :type q:quat)
  (rotation-matrix (m3:mat 1) :type m3:mat)
  (fov-x o:pi/4 :type single-float)
  (fov-y o:pi/4 :type single-float))

(defvar *light* (make-light))
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
                      (make-thing
                       :shape (make-plane :point (v3:vec 0 0 6)
                                          :normal (v3:vec 0 0 -1))
                       :material (make-material :ambient (v3:vec 0.1)
                                                :diffuse (v3:vec 0.45)
                                                :specular (v3:vec 0.45)
                                                :fresnel (v3:vec 0.2)
                                                :shininess 4f0))))

(defun ambient (material)
  (declare (optimize speed))
  (v3:* (material-ambient material) *ambient-intensity*))

(defun diffuse (material normal light-dir)
  (declare (optimize speed))
  (v3:scale (material-diffuse material) (v3:dot normal light-dir)))

(defun specular (material viewer-dir light-dir normal)
  (let ((halfway (v3:normalize (v3:+ viewer-dir light-dir))))
    (v3:expt! halfway (v3:scale (material-specular material)
                                (max (v3:dot halfway normal) 0f0))
              (material-shininess material))))

(defun shadow-multiplier (light-dir point)
  (declare (optimize speed))
  (if (smarter-intersections (make-ray :origin point
                                       :direction light-dir)
                             *scene*)
      0f0
      1f0))

(defun pointer (origin target)
  (declare (optimize speed))
  (v3:normalize (v3:- target origin)))

(defun reflection (normal incoming)
  (declare (optimize speed))
  (v3:- incoming (v3:scale normal (* 2 (v3:dot normal incoming)))))

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

(defun intersect (ray object o-c)
  (declare (optimize speed))
  (etypecase object
    (sphere
     (v3:-! o-c (ray-origin ray) (sphere-center object))
     (let* ((dir (ray-direction ray))
            (A (v3:dot dir dir))
            (B (* 2 (v3:dot dir o-c)))
            (C (- (v3:dot o-c o-c) (expt (sphere-radius object) 2))))
       (quadratic A B C)))
    (plane
     (let* ((n (plane-normal object))
            (denom (v3:dot (ray-direction ray) n)))
       (if (< (abs denom) 5f-5)
           nil
           (/ (v3:dot (v3:- (plane-point object) (ray-origin ray)) n)
              denom))))))

(defun find-normal (object point)
  (declare (optimize speed))
  (etypecase object
    (sphere (v3:normalize (v3:- point (sphere-center object))))
    (plane (plane-normal object))))

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

(defun smarter-intersections (ray scene)
  (declare (optimize speed))
  (loop :for thing :in scene
        :for obj := (thing-shape thing)
        :with cache := (v3:vec)
        :for intrsct := (intersect ray obj cache)
        :with foo := nil
        :do (if foo
                (typecase intrsct
                  (cons (let* ((fi (car intrsct))
                               (fo (cdr intrsct))
                               (objt (if (< fi 5f-5)
                                         (if (< fo 5f-5)
                                             nil
                                             fo)
                                         (if (< fo 5f-5)
                                             fi
                                             (min fi fo)))))
                          (declare (single-float fi fo))
                          (when (and objt (< objt (the single-float (car foo))))
                            (setf (car foo) objt (cdr foo) thing))))
                  (single-float (when (and (> intrsct 5f-5)
                                           (< intrsct
                                              (the single-float (car foo))))
                                  (setf (car foo) intrsct (cdr foo) thing))))
                (typecase intrsct
                  (cons (let* ((fi (car intrsct))
                               (fo (cdr intrsct))
                               (objt (locally (declare (single-float fi fo))
                                       (if (< (the single-float fi) 5f-5)
                                           (if (< (the single-float fo) 5f-5)
                                               nil
                                               fo)
                                           (if (< (the single-float fo) 5f-5)
                                               fi
                                               (min (the single-float fi)
                                                    fo))))))
                          (when objt (setf foo (cons objt thing)))))
                  (single-float (when (> intrsct 5f-5)
                                  (setf foo (cons intrsct thing))))))
        :finally (when foo
                   (setf (cdr foo) (cons (cdr foo) nil))
                   (return foo))))

(defun intersection (ray x)
  (declare (optimize speed))
  (let* ((dist (car x))
         (thing (cadr x))
         (point (ray-point ray dist))
         (shape (thing-shape thing))
         (normal (find-normal shape point)))
    (make-intersection :point point
                       :normal normal
                       :uv (find-uv shape point normal)
                       :in-dir (ray-direction ray))))


(defparameter *width* 900)
(defparameter *height* 600)

(defun make-camera (&key rotation position fov-y)
  (let ((camera (%make-camera :rotation rotation
                              :position position
                              :fov-y fov-y)))
    (setf (camera-fov-x camera) (* (/ *width* *height*)
                                   (camera-fov-y camera)))
    camera))

(defvar *camera* (make-camera :rotation (q:from-axis-angle (v3:vec 1 0 0)
                                                           (/ o:pi 5f0))
                              :position (v3:vec 0 7 -7)
                              :fov-y o:pi/4))

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

(defun rgb8 (pixel)
  (map 'vector (lambda (x)
                 (max 0 (min 255 (floor (* 255 x)))))
       (v3:expt! pixel pixel (/ 2.2f0))))

(defun doit ()
  (setf (camera-rotation-matrix *camera*) (camspace-rotator *camera*))
  (dotimes (y *height*)
    (dotimes (x *width*)
      (rgb8 (trace-pixel x y)))))

(defun render-to-png ()
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :width *width* :height *height*)))
    (with-open-file (stream "/tmp/test.png"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (setf (camera-rotation-matrix *camera*) (camspace-rotator *camera*))
      (loop :for y :below *height*
            :do (loop :for x :below *width*
                      :do (zpng:write-pixel
                           (rgb8 (trace-pixel x y))
                           png)))
      (zpng:finish-png png))))

(defun render-to-array (array)
  )

(defun doit ()
  (setf (camera-rotation-matrix *camera*) (camspace-rotator *camera*))
  (dotimes (y *height*)
    (dotimes (x *width*)
      (rgb8 (trace-pixel x y)))))

(defvar *max-depth* 3)

(defun raytrace (ray &optional (depth 0))
  (declare (optimize speed))
  (let ((hit (smarter-intersections ray *scene*)))
    (if (null hit)
        (v3:vec)
        (illumination (cadr hit) (intersection ray hit) depth))))

(defun illumination (thing intersection &optional (depth 0))
  (with-accessors ((shape thing-shape) (mat thing-material)) thing
    (with-accessors ((point intersection-point)
                     (normal intersection-normal)
                     (uv intersection-uv)
                     (in-dir intersection-in-dir))
        intersection
      (let* ((light-dir (pointer point (light-position *light*)))
             (viewer-dir (v3:negate in-dir))
             (reflection-dir (reflection normal in-dir))
             (result)
             (temp)
             (reflp (< depth *max-depth*))
             (lightp (> (v3:dot light-dir normal) 0)))
        (when reflp
          (setf result (raytrace (make-ray :origin point
                                           :direction reflection-dir)
                                 (1+ depth)))
          (v3:*! result result (material-fresnel mat)))
        (when lightp
          (setf temp (specular mat viewer-dir light-dir normal))
          (v3:+! temp temp (diffuse mat normal light-dir))
          (v3:*! temp temp (light-color *light*))
          ;;(v3:scale! temp temp (shadow-multiplier light-dir point))
          (if reflp
              (v3:+! result result temp)
              (setf result temp)))
        (if result
            (v3:+! result result (ambient mat))
            (ambient mat))))))

(defun trace-pixel (x y)
  (declare (optimize speed))
  (let ((ray (make-ray :origin (camera-position *camera*)
                       :direction (eyedir x y *camera*))))
    (raytrace ray)))

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
