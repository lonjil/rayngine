(in-package #:rayngine)

(defmacro with-temp-vars ((&rest variables) index-place stack &body body)
  (let ((index (gensym "INDEX"))
        (stacc (gensym "STACK")))
    `(let ((,index ,index-place)
           (,stacc ,stack))
       (unwind-protect
            (symbol-macrolet
                ,(loop :for v :in variables
                       :for i :from 0
                       :collect `(,v (aref ,stacc (+ ,index ,i))))
              (incf ,index-place ,(length variables))
              ,@body)
         (setf ,index-place ,index)))))
