(in-package #:rayngine)


(defstruct light
  (position (v3:vec -12 8 -4) :type v3:vec)
  (color (v3:vec 14) :type v3:vec)
  (width 6f-1 :type single-float))

(defstruct material
  (ambient (v3:vec 0.5) :type v3:vec)
  (diffuse (v3:vec 0.5) :type v3:vec)
  (specular (v3:vec 0.5) :type v3:vec)
  (shininess 1f0 :type single-float)
  (fresnel (v3:vec 0.5) :type v3:vec))

(defstruct ray
  (origin (v3:vec) :type v3:vec)
  (direction (v3:vec) :type v3:vec))

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
(defun make-camera (&key rotation position fov-y)
  (let ((camera (%make-camera :rotation rotation
                              :position position
                              :fov-y fov-y)))
    (setf (camera-fov-x camera) (* (/ *width* *height*)
                                   (camera-fov-y camera)))
    camera))

(defstruct (state (:constructor %make-state))
  (max-depth 3 :type fixnum)
  (light (make-light) :type light)
  (cache (v3:vec) :type v3:vec)
  (result (v3:vec) :type v3:vec)
  (temp (v3:vec) :type v3:vec)
  (ambient (v3:vec) :type v3:vec)
  (specular (v3:vec) :type v3:vec)
  (diffuse (v3:vec) :type v3:vec)
  (index 0 :type fixnum)
  (stack (make-array 1000 :adjustable nil) :type (simple-array v3:vec (*)))
  (scene))
(defun make-state (&key (max-depth 3) scene)
  (let ((state (%make-state :max-depth max-depth :scene scene)))
    (loop :with arr := (state-stack state)
          :for i :below (length arr)
          :do (setf (aref arr i) (v3:vec)))
    state))
