;;;; rayngine.lisp

(in-package #:rayngine)

(defvar *ambient-intensity* (v3:vec 0.1 0.1 0.1))

(defclass light ()
  ((position :accessor pos :initarg :pos) ;vec3
   (color :accessor color :initarg :color))) ;ditto

(defclass material ()
  ((ambient-coefficient :accessor ka :initarg :ka) ;vec3 color
   (diffuse-coefficient :accessor kd :initarg :kd) ;vec3 color
   (specular-coefficient :accessor ks :initarg :ks)
   (fresnel-reflection-coefficient :accessor kr :initarg :kr)
   (shininess :accessor shine :initarg :shine)))

(defclass ray ()
  ((origin :accessor origin :initarg :origin)
   (direction :accessor dir :initarg :dir)))
(defun ray (origin dir)
  (make-instance 'ray :origin origin :dir dir))

(defun ray-point (ray dist)
  (v3:+ (origin ray) (v3:scale (dir ray) dist)))

(defclass sphere ()
  ((radius :accessor radius :initarg :radius)
   (center :accessor center :initarg :center)
   (cam-origin-center :accessor cam-origin-center :initarg :o-c)))
(defclass plane ()
  ((point :accessor point :initarg :point)
   (normal :accessor normal :initarg :normal)))

(defclass thing ()
  ((shape :accessor shape :initarg :shape)
   (material :accessor mat :initarg :mat)))
(defclass intersection ()
  ((point :accessor point :initarg :point)
   (normal :accessor normal :initarg :normal)
   (uv :accessor uv :initarg :uv)
   (in-dir :accessor in-dir :initarg :in-dir)))
(defclass camera ()
  ((position :accessor pos :initarg :pos)
   (rotation :accessor rot :initarg :rot) ; quat
   (rotmat :accessor rotmat :initarg :rotmat)
   (fovy :accessor fovy :initarg :fovy)
   (fovx :accessor fovx :initarg :fovx)))

(defvar *light* (make-instance 'light :pos (v3:vec -6 4 0) :color (v3:vec 1 1 1)))
(defvar *scene* (list (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 2.0f0
                                       :center (v3:vec -2 2 2))
                       :mat (make-instance 'material :ka (v3:vec 0.5 0.5 0.5)
                                                     :kd (v3:vec 0.7 0.7 0.7)
                                                     :ks (v3:vec 0.4 0.4 0.4)
                                                     :kr (v3:vec 0.2 0.2 0.2)
                                                     :shine 5f0))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 1.5f0
                                       :center (v3:vec 1.5 1.5 2))
                       :mat (make-instance 'material :ka (v3:vec 0.6 0.2 0.2)
                                                     :kd (v3:vec 0.7 0.2 0.2)
                                                     :ks (v3:vec 0.4 0.2 0.2)
                                                     :kr (v3:vec 0.2 0.1 0.1)
                                                     :shine 5f0))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 1.0f0
                                       :center (v3:vec 1.5 1 -1))
                       :mat (make-instance 'material :ka (v3:vec 0.2 0.6 0.2)
                                                     :kd (v3:vec 0.2 0.7 0.2)
                                                     :ks (v3:vec 0.2 0.4 0.2)
                                                     :kr (v3:vec 0.1 0.2 0.1)
                                                     :shine 5f0))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 0.5f0
                                       :center (v3:vec -2 0.5 -1))
                       :mat (make-instance 'material :ka (v3:vec 0.2 0.2 0.6)
                                                     :kd (v3:vec 0.2 0.2 0.7)
                                                     :ks (v3:vec 0.2 0.2 0.4)
                                                     :kr (v3:vec 0.1 0.1 0.2)
                                                     :shine 5f0))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'plane :point (v3:vec 0 0 0) :normal (v3:vec 0 1 0))
                       :mat (make-instance 'material :ka (v3:vec 0.1 0.1 0.1)
                                                     :kd (v3:vec 0.1 0.1 0.1)
                                                     :ks (v3:vec 0.9 0.9 0.9)
                                                     :kr (v3:vec 0.9 0.9 0.9)
                                                     :shine 16f0))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'plane :point (v3:vec 0 0 6) :normal (v3:vec 0 0 -1))
                       :mat (make-instance 'material :ka (v3:vec 0.1 0.1 0.1)
                                                     :kd (v3:vec 0.45 0.45 0.45)
                                                     :ks (v3:vec 0.45 0.45 0.45)
                                                     :kr (v3:vec 0.2 0.2 0.2)
                                                     :shine 4f0))))

(defun ambient (material)
  (v3:* (ka material) *ambient-intensity*))
(defun diffuse (material normal light-dir)
  (v3:scale (kd material) (v3:dot normal light-dir)))
(defun specular (material viewer-dir light-dir normal)
  (let ((halfway (v3:normalize (v3:+ viewer-dir light-dir))))
    (v3:expt! halfway (v3:scale (ks material) (max (v3:dot halfway normal) 0f0))
              (shine material))))
(defun shadow-multiplier (light-dir point)
  (if (smarter-intersections (ray point light-dir) *scene*)
      0f0
      1f0))
(defun pointer (origin target)
  (v3:normalize (v3:- target origin)))
(defun reflection (normal incoming)
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
  (etypecase object
    (sphere
     (v3:-! o-c (origin ray) (center object))
     (let* ((dir (dir ray))
            (A (v3:dot dir dir))
            (B (* 2 (v3:dot dir o-c)))
            (C (- (v3:dot o-c o-c) (expt (radius object) 2))))
       (quadratic A B C)))
    (plane
     (let* ((n (normal object))
            (denom (v3:dot (dir ray) n)))
       (if (< (abs denom) 5f-5)
           nil
           (/ (v3:dot (v3:- (point object) (origin ray)) n)
              denom))))))

(defgeneric find-normal (object point))
(defmethod find-normal ((sph sphere) point)
  (v3:normalize (v3:- point (center sph))))
(defmethod find-normal ((foo plane) point)
  (normal foo))

;(defun find-normal )

(defgeneric find-uv (object point normal))
(defmethod find-uv ((o t) point normal)
  (declare (ignore o point))
  nil)
(defmethod find-uv ((sph sphere) point normal)
  (v2:vec (* 0.5 (+ 1.0f0 (/ (atan (s:.z normal) (s:.x normal)) pi)))
          (/ (acos (s:.y normal)) pi)))

(defun smarter-intersections (ray scene)
  (loop :for thing :in scene
        :for obj := (shape thing)
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
                          (when (and objt (< objt (car foo)))
                            (setf (car foo) objt (cdr foo) thing))))
                  (single-float (when (and (> intrsct 5f-5)
                                           (< intrsct (car foo)))
                                  (setf (car foo) intrsct (cdr foo) thing))))
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
                          (when objt (setf foo (cons objt thing)))))
                  (single-float (when (> intrsct 5f-5)
                                  (setf foo (cons intrsct thing))))))
        :finally (when foo
                   (setf (cdr foo) (cons (cdr foo) nil))
                   (return foo))))

(defun intersection (ray x)
  (let* ((dist (car x))
         (thing (cadr x))
         (point (ray-point ray dist))
         (shape (shape thing))
         (normal (find-normal shape point)))
    ;(format t "intersection: in-dir: ~a, normal: ~a~%  norm2: ~a~%" (dir ray) normal
    ;        (v3:normalize normal))
    (make-instance 'intersection :point point
                                 :normal normal
                                 :in-dir (dir ray)
                                 :uv (find-uv shape point normal))))


(defvar *height* 600)
(defvar *width* 900)

(defmethod (setf fovy) :after (fovy (cam camera))
  (setf (slot-value cam 'fovx) (* (/ *width* *height*) fovy)))
(defmethod (setf fovx) :after (fovx (cam camera))
  (setf (slot-value cam 'fovy) (* (/ *height* *width*) fovx)))
(defmethod initialize-instance :after ((foo camera) &key fovy &allow-other-keys)
  (setf (fovy foo) fovy))

(defvar *camera* (make-instance 'camera :rot (q:from-axis-angle (v3:vec 1 0 0)
                                                                (/ o:pi 5f0))
                                        :pos (v3:vec 0 7 -7)
                                        :fovy (/ o:pi 4)))

(defun camspace-rotator (camera)
  (q:to-mat3 (rot camera)))

#+(or)(defun camcr (x y camera)
  (v3:vec (* (/ (- (* 2 x) *width*) *width*) (tan (/ (fovx camera) 2)))
          (- (* (/ (- (* 2 y) *height*) *height*) (tan (/ (fovy camera) 2))))
          1.0))
;;; not sure which is better
(defun camcr (x y camera)
  (v3:vec (* (1- (* 2 (/ (+ x 0.5f0) *width*))) (tan (/ (fovx camera) 2)))
          (* (- 1 (* 2 (/ (+ y 0.0f0) *height*))) (tan (/ (fovy camera) 2)))
          1))
(defun eyedir (x y camera)
  (v3:normalize (m3:*v3 (rotmat camera)
                        (camcr x y camera))))


(defun rgb8 (pixel)
  (map 'vector (lambda (x)
                 (max 0 (min 255 (floor (* 255 x)))))
       (v3:expt! pixel pixel (/ 2.2f0))))

(defun render-to-png ()
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :width *width* :height *height*)))
    (with-open-file (stream "test.png"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (setf (rotmat *camera*) (camspace-rotator *camera*))
      #+(or)(dolist (var *scene*)
        (when (typep var 'sphere)
          (setf (cam-origin-center var)
                (v3:- (pos *camera*) (center var)))))
      (loop :for y :below *height*
            :do (loop :for x :below *width*
                      :do (zpng:write-pixel
                           (rgb8 (trace-pixel x y))
                           png)))
      (zpng:finish-png png))))

(defun render-to-array (array)
  )

(defun doit ()
  (setf (rotmat *camera*) (camspace-rotator *camera*))
  (dotimes (y *height*)
    (dotimes (x *width*)
      (rgb8 (trace-pixel x y)))))

(defvar *max-depth* 3)

(defun raytrace (ray &optional (depth 0))
  (let ((hit (smarter-intersections ray *scene*)))
    (if (null hit)
        (v3:vec)
        (illumination (cadr hit) (intersection ray hit) depth))))

(defun illumination (thing intersection &optional (depth 0))
  (with-accessors ((shape shape) (mat mat)) thing
    (with-accessors ((point point) (normal normal) (uv uv) (in-dir in-dir))
        intersection
      (let* ((light-dir (pointer point (pos *light*)))
             (viewer-dir (v3:negate in-dir))
             (reflection-dir (reflection normal in-dir))
             (result)
             (temp)
             (reflp (< depth *max-depth*))
             (lightp (> (v3:dot light-dir normal) 0)))
        (when reflp
          (setf result (raytrace (make-instance
                                   'ray :origin point
                                   :dir reflection-dir)
                                  (1+ depth)))
          (v3:*! result result (kr mat)))
        (when lightp
          (setf temp (specular mat viewer-dir light-dir normal))
          (v3:+! temp temp (diffuse mat normal light-dir))
          (v3:*! temp temp (color *light*))
          ;;(v3:scale! temp temp (shadow-multiplier light-dir point))
          (if reflp
              (v3:+! result result temp)
              (setf result temp)))
        (if result
            (v3:+! result result (ambient mat))
            (ambient mat))))))

(defun trace-pixel (x y)
  (let ((ray (ray (pos *camera*) (eyedir x y *camera*))))
    (raytrace ray)))

(defun foo ()
  (dolist (x *scene*)
    (let ((y (shape x))
          (m (mat x)))
    (typecase y
      (sphere (format t "~
sphere: c = ~a, r = ~a
   mat: ka = ~a, kd = ~a,
        ks = ~a, kr = ~a, shine = ~a~%"
                      (center y) (radius y)
                      (ka m) (kd m) (ks m) (kr m) (shine m)))
      (plane (format t "~
plane: p = ~a, n = ~a
  mat: ka = ~a, kd = ~a,
       ks = ~a, kr = ~a, shine = ~a~%"
                      (point y) (normal y)
                      (ka m) (kd m) (ks m) (kr m) (shine m)))))))

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
