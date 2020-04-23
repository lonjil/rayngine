;;;; package.lisp

(defpackage #:rayngine
  (:use #:cl)
  (:local-nicknames (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:q #:origin.quat)
                    (#:m2 #:origin.mat2)
                    (#:m3 #:origin.mat3)
                    (#:m4 #:origin.mat4)
                    (#:s #:origin.swizzle)
                    (#:o #:origin))
  (:shadow #:intersection))
