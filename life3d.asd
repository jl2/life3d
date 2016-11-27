;;;; life3d.asd

(asdf:defsystem #:life3d
  :serial t
  :description "Describe life3d here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com"
  :license "Specify license here"
  :depends-on (#:sdl2
               #:cl-opengl
               #:cl-glu
               #:trivial-main-thread)
  :components ((:file "package")
               (:file "life3d")))

