;;;; life3d.lisp

(in-package #:life3d)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 0)))

(defun clamp (x min max)
  (if (< x min )
      min
      (if (> x max )
          max
          x)))

(defstruct transition
  (low 0.3)
  (high 0.75)
  (death -0.3)
  (life 0.05))

(defun trans (low high death life)
  (make-transition :low low :high high
                   :death death :life life))
  
(defun init-board (grid &key (num 1000))
  "Randomly set cells to t and nil."
  (dotimes (i num)
    (let ((i (random (1- (array-dimension grid 0))))
          (j (random (1- (array-dimension grid 1))))
          (k (random (1- (array-dimension grid 2)))))
      (setf (aref grid i j k) (random 1.0)))))


(defun neighbor-weight (grid i j k)
  "Count the neighbors of grid location i,j"
  (let ((ip (if (> 0 (- i 1)) (- (array-dimension grid 0) 1) (- i 1)))
        (jp (if (> 0 (- j 1)) (- (array-dimension grid 1) 1) (- j 1)))
        (kp (if (> 0 (- k 1)) (- (array-dimension grid 2) 1) (- k 1)))
        (in (if (>= i (- (array-dimension grid 0) 1)) 0 (+ i 1)))
        (jn (if (>= j (- (array-dimension grid 1) 1)) 0 (+ j 1)))
        (kn (if (>= k (- (array-dimension grid 2) 1)) 0 (+ k 1)))
        (count 0.0))

    (incf count (aref grid ip jp kp))
    (incf count (aref grid i jp kp))
    (incf count (aref grid in jp kp))
    (incf count (aref grid ip j kp))
    (incf count (aref grid i j kp))
    (incf count (aref grid in j kp))
    (incf count (aref grid ip jn kp))
    (incf count (aref grid i jn kp))
    (incf count (aref grid in jn kp))

    (incf count (aref grid ip jp k))
    (incf count (aref grid i jp k))
    (incf count (aref grid in jp k))
    (incf count (aref grid ip j k))
    (incf count (aref grid i j k))
    (incf count (aref grid in j k))
    (incf count (aref grid ip jn k))
    (incf count (aref grid i jn k))
    (incf count (aref grid in jn k))
          
    (incf count (aref grid ip jp kn))
    (incf count (aref grid i jp kn))
    (incf count (aref grid in jp kn))
    (incf count (aref grid ip j kn))
    (incf count (aref grid i j kn))
    (incf count (aref grid in j kn))
    (incf count (aref grid ip jn kn))
    (incf count (aref grid i jn kn))
    (incf count (aref grid in jn kn))

    count))

(defun update-board (old-grid new-grid &key (transition (make-transition)))
  "Update old-grid based on neighbor counts, placing the results in new-grid."
  (let ((low-life (transition-low transition))
        (high-life (transition-high transition))
        (death-amount (transition-death transition))
        (life-amount (transition-life transition)))
  (loop for i from 0 below (array-dimension old-grid 0)
     do
       (loop for j from 0 below (array-dimension old-grid 1)
          do
            (loop for k from 0 below (array-dimension old-grid 2)
               do
                 (let ((nw (neighbor-weight old-grid i j k)))
                   (if (< low-life nw high-life)
                       (setf (aref new-grid i j k) (clamp (+ (aref old-grid i j k) life-amount) 0.0 1.0))
                       (setf (aref new-grid i j k) (clamp (+ (aref old-grid i j k) death-amount) 0.0 1.0)))))))))

(defun draw-cube (verts faces)
  (labels ((set-normal (n)
             (gl:normal (aref n 0) (aref n 1) (aref n 2)))
           (set-vertex (index)
             (let ((v (aref verts index)))
               (gl:vertex (aref v 0) (aref v 1) (aref v 2))))
           (draw-face (vertex-indices normal)
             (set-normal normal)
             (gl:begin :quads)
             (map 'nil #'set-vertex vertex-indices)
             (gl:end)))
 
    (map 'nil #'(lambda (x) (draw-face (first x) (second x))) faces)))

(defun cube-at (cx cy cz dx dy dz)
  (let ((cube-verts #(#(0 0 0)
                      #(0 1 0)
                      #(1 1 0)
                      #(1 0 0)
                      #(0 0 1)
                      #(0 1 1)
                      #(1 1 1)
                      #(1 0 1)))
 
        (cube-norms '((#(4 7 6 5) #(0 0 1))
                      (#(5 6 2 1) #(0 1 0))
                      (#(1 2 3 0) #(0 0 -1))
                      (#(0 3 7 4) #(0 -1 0))
                      (#(4 5 1 0) #(-1 0 0))
                      (#(3 2 6 7) #(1 0 0)))))
    (gl:push-matrix)
    (gl:translate cx cy cz)
    (gl:scale dx dy dz)
    (draw-cube cube-verts cube-norms)
    (gl:pop-matrix)))

(defun draw-board (grid win-width win-height &key (multiplier 1.5))
  "Used OpenGL to display the grid."
  (gl:push-matrix)
  (gl:scale 0.25 0.25 0.25)
  (let ((dx (/ win-width (array-dimension grid 0)))
        (dy (/ win-height (array-dimension grid 1)))
        (dz (/ win-height (array-dimension grid 2)))
        (cx 0)
        (cy 0)
        (cz 0))
    (loop for i from 0 below (array-dimension grid 0)
       do
         (setf cy 0)
         (loop for j from 0 below (array-dimension grid 1)
            do
              (setf cz 0)
              (loop for k from 0 below (array-dimension grid 2)
                   do
                   (let ((perc (aref grid i j k)))
                     (if (< 0.3 perc 0.7)
                         (progn
                           (gl:material :front :diffuse `#(0.1
                                                          ,(clamp (* multiplier perc) 0.0 1.0)
                                                          1.0
                                                          0.1
                                                          ,(clamp (* multiplier perc) 0.0 1.0)))
                           (cube-at cx cy cz dx dy dz))))
                   (incf cz dz))
              (incf cy dy))
         (incf cx dx))
    (gl:pop-matrix)))

(defun start-life (&key
                     (board-width 50) (board-height 50) (board-depth 50)
                     (win-width 800) (win-height 800)
                     (num (truncate (* board-width board-height board-depth 0.25)))
                     (multiplier 1.5)
                     (fps 30)
                     (delay 20)
                     (transition (make-transition)))
  "Run the game of life in an SDL window."


  (let
      ((theta 0.0)
       (boards (list
                (make-array `(,board-width ,board-height ,board-depth) :element-type 'real :initial-element 0.0)
                (make-array `(,board-width ,board-height ,board-depth) :element-type 'real :initial-element 0.0)))
       ;; boards is a cons cell pointing to two 2-d arrays of booleans
       (prev-tick 0) ;; prev-tick is the previous value of sdl-system-ticks when the board was updated
       (paused nil) ;; paused is t when paused, nil when not
       (trans (if (listp transition) (apply #'trans transition) transition)))
    (init-board (car boards) :num num )
    (sdl:with-init
        ()
      ;; Setup the window and view
      (sdl:window win-width win-height
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   16)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) fps)
      
      (gl:viewport 0 0 win-width win-height)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 50 (/ win-height win-width) 1.0 50000.0)
      (glu:look-at 200 200 600 
                    50 50 50
                    0 1 0)

      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:clear-color 0 0 0 0)
      (gl:shade-model :smooth)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)

      (gl:material :front :ambient-and-diffuse #(0.2 0.2 0.2 0.2))
      (gl:light :light0 :position #(200 200 300 0))
      (gl:light :light0 :diffuse #(1 1 1 1))
      (gl:light :light1 :position #(-100 200 -50 0))
      (gl:light :light1 :diffuse #(1 1 1 1))
      (gl:enable :cull-face :depth-test
                 :lighting :light0 :light1)

      (gl:clear :color-buffer :depth-buffer)
      
      ;; Draw the initial board
      (draw-board (car boards) win-width win-height :multiplier multiplier)
      (sdl:update-display)

      ;; Handle events
      (sdl:with-events ()
        (:quit-event () t)
        
        (:key-down-event
         ()
         ;; quit
         (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
           (sdl:push-quit-event))

         ;; Reset to a random state
         (when (sdl:key-down-p :sdl-key-r)
           (init-board (car boards)))

         ;; Pause/unpause
         (when (sdl:key-down-p :sdl-key-p)
           (if paused
               (setf paused nil)
               (setf paused t))))

        (:video-expose-event () (sdl:update-display))

        (:idle
         ;; Check if it's time to update
         (if (> (- (sdl:system-ticks) prev-tick) delay)
             (progn
               (setf prev-tick (sdl:system-ticks))

               ;; Only update the board while not paused
               (when (not paused)
                 (update-board (car boards) (cadr boards) :transition trans)
                 (setf boards (list (cadr boards) (car boards))))

               ;; Clear the screen and redraw
               (gl:clear :color-buffer :depth-buffer)
               (gl:push-matrix)
               (gl:rotate theta 0.0 1.0 0.0)
               (incf theta (/ pi 20.0))
               (gl:translate (/ board-width -2.0) (/ board-height -2.0) (/ board-depth -2.0))
               (draw-board (car boards) win-width win-height :multiplier multiplier)
               (gl:pop-matrix)
               (sdl:update-display))))))))

