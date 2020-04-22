;;;; rayngine.lisp

(in-package #:rayngine)

(defvar *ambient-intensity* (v3:vec 0.2 0.2 0.2))

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
   (center :accessor center :initarg :center)))
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
   (fovy :accessor fovy :initarg :fovy)
   (fovx :accessor fovx :initarg :fovx)))

(defvar *scene* (list (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 1
                                       :center (v3:vec 0 0 4))
                       :mat (make-instance 'material :ka (v3:vec 1 0 0)
                                                     :kd (v3:vec 0.6 0.1 0.1)
                                                     :ks (v3:vec 0.3 0.1 0.1)))
                      (make-instance
                       'thing
                       :shape (make-instance
                               'sphere :radius 0.5
                                       :center (v3:vec 1 1 3.5))
                       :mat (make-instance 'material :ka (v3:vec 0.7 0.7 0.7)
                                                     :kd (v3:vec 0.8 0.8 0.8)
                                                     :ks (v3:vec 0.1 0.1 0.1)))))

(defun ambient (material)
  (v3:* (ka material) *ambient-intensity*))
(defun diffuse (material normal light-dir)
  (v3:scale (kd material) (v3:dot normal light-dir)))
(defun specular (material viewer-dir light-dir normal)
  (let ((halfway (v3:normalize (v3:+ viewer-dir light-dir))))
    (v3:expt (v3:scale (ks material) (max (v3:dot halfway normal) 0f0))
             (shine material))))
(defun pointer (origin target)
  (v3:normalize (v3:- target origin)))
(defun reflection (normal incoming)
  (v3:- incoming (v3:scale normal (* 2 (v3:dot normal incoming)))))
(defun illumination (material intersection camera light)
  (with-accessors ((point point) (normal normal)) intersection
    (let* ((light-dir (pointer point (pos light)))
           (viewer-dir (pointer point (pos camera))))
      (v3:+ (ambient material)
            (if (<= (v3:dot light-dir normal) 0)
                (v3:vec)
                (v3:* (color light)
                      (v3:+ (diffuse material normal light-dir)
                            (specular material viewer-dir light-dir normal)
                            )))))))



(defun quadratic (A B C)
  (let ((foo (- (* B B) (* 4 A C))))
    (cond ((> foo 0) (list (/ (- (sqrt foo) B)
                              (* 2 A))
                           (/ (- 0 (sqrt foo) B)
                              (* 2 A))))
          ((= foo 0) (list (/ (- B)
                            (* 2 A))))
          ((< foo 0) (list)))))


(defgeneric intersect (a b))
(defmethod intersect ((ray ray) (sph sphere))
  (let* ((A (v3:dot (dir ray) (dir ray)))
         (o-c (v3:- (origin ray) (center sph)))
         (B (* 2 (v3:dot (dir ray) o-c)))
         (C (- (v3:dot o-c o-c) (* (radius sph) (radius sph)))))
    (quadratic A B C)))
(defmethod intersect ((ray ray) (plane plane))
  (let ((denom (v3:dot (dir ray) (normal plane))))
    (if (< (abs denom) 5f-5)
        nil
        (list (/ (v3:dot (v3:- (point plane) (origin ray))
                         (normal plane))
                 denom)))))

(defgeneric find-normal (object point))
(defmethod find-normal ((sph sphere) point)
  (v3:normalize (v3:- point (center sph))))
(defmethod find-normal ((foo plane) point)
  (normal foo))

(defgeneric find-uv (object point normal))
(defmethod find-uv ((o t) point normal)
  (declare (ignore o point))
  nil)
(defmethod find-uv ((sph sphere) point normal)
  (v2:vec (* 0.5 (+ 1.0f0 (/ (atan (s:.z normal) (s:.x normal)) pi)))
          (/ (acos (s:.y normal)) pi)))

(defun find-thing-intersections (ray thing)
  (let* ((shape (shape thing))
         (i (intersect ray shape)))
    (mapcar (lambda (x) (list x thing)) i)))

(defun find-scene-intersections (ray scene)
  (loop :for x :in scene :append (find-thing-intersections ray x)))

(defun closest-intersection (intersection-list)
  (loop :for y :in intersection-list
        :for x := (if (and (> (car y) 5f-5)
                           (or (null x)
                               (> (car x) (car y))))
                      y
                      x)
        :finally (return x)))

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


(defvar *height* 100)
(defvar *width* 200)
(defvar *fovx* (/ pi 4))
(defvar *fovy* (* (/ *height* *width*) *fovx*))

(defmethod (setf fovy) :after (fovy (cam camera))
  (setf (slot-value cam 'fovx) (* (/ *width* *height*) fovy)))
(defmethod (setf fovx) :after (fovx (cam camera))
  (setf (slot-value cam 'fovy) (* (/ *height* *width*) fovx)))
(defmethod initialize-instance :after ((foo camera) &key fovy &allow-other-keys)
  (setf (fovy foo) fovy))

(defvar *camera* (make-instance 'camera :rot (q:quat 1)
                                        :pos (v3:vec)
                                        :fovy (/ pi 8)))

(defun camspace-rotator (camera)
  (q:to-mat3 (rot camera)))

(defun camcr (x y camera)
  (v3:vec (* (/ (- (* 2 x) *width*) *width*) (tan (fovx camera)))
          (- (* (/ (- (* 2 y) *height*) *height*) (tan (fovy camera))))
          1.0))
(defun eyedir (x y camera)
  (v3:normalize (m3:*v3 (camspace-rotator camera)
                        (camcr x y camera))))

(defun trace-pixel (camera scene x y)
  (let* ((eyedir (eyedir x y camera))
         (eyeray (make-instance 'ray :dir eyedir :origin (pos camera)))
         (intrsctns (find-scene-intersections eyeray scene))
         (closest (closest-intersection intrsctns)))
    (if closest
        (let* ((interx (intersection eyeray closest))
               (material (mat (cadr closest))))
          (illumination material interx camera *light*))
        (v3:vec))))
(defun rgb8 (pixel)
  (map 'vector (lambda (x)
                 (max 0 (min 255 (floor (* 255 x)))))
       pixel))

(defun render-to-png ()
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :width *width* :height *height*)))
    (with-open-file (stream "test.png"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop :for y :below *height*
            :do (loop :for x :below *width*
                      :do (zpng:write-pixel
                           (rgb8 (trace-pixel2 x y))
                           png)))
      (zpng:finish-png png))))

(defvar *max-depth* 3)
(defvar *depth* 0)

(defun raytrace (ray &optional (depth 0))
  (let ((hit (closest-intersection (find-scene-intersections ray *scene*))))
    (if (null hit)
        (v3:vec)
        (illumination2 (cadr hit) (intersection ray hit) depth))))

(defun reflect (normal dir)
  (v3:- (v3:scale normal (* 2 (v3:dot normal dir)))
        dir))
(defun illumination2 (thing intersection &optional (depth 0))
  (with-accessors ((shape shape) (mat mat)) thing
    (with-accessors ((point point) (normal normal) (uv uv) (in-dir in-dir))
        intersection
      (let* ((light-dir (pointer point (pos *light*)))
             (viewer-dir (v3:negate in-dir))
             (reflection-dir (reflection normal in-dir)))
        ;(format t "illu: origin: ~a, dir: ~a~%" point reflection-dir)
        (v3:+ (v3:+ (ambient mat)
                    (if (<= (v3:dot light-dir normal) 0)
                        (v3:vec)
                        (v3:* (color *light*)
                              (v3:+ (diffuse mat normal light-dir)
                                    (specular mat viewer-dir light-dir normal)))))
              (if (< depth *max-depth*)
                  (v3:* (raytrace (make-instance 'ray :origin point
                                                      :dir reflection-dir)
                                  (1+ depth))
                        (kr mat))
                  (v3:vec)))))))

(defun trace-pixel2 (x y)
  (let ((ray (ray (pos *camera*) (eyedir x y *camera*))))
    ;(format t "cam: origin: ~a, dir: ~a~%" (origin ray) (dir ray))
    (raytrace ray)))
