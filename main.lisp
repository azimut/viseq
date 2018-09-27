(in-package :viseq)

;; TODO:
;; - Order might matter more than I though, so might be a compiler
;;   of some sort would help, so I can define the order...

(defvar *video-queue* NIL)
(defvar *text-queue* NIL)
(defvar *rotation-matrix* (cv:create-mat 2 3 5))

(defclass cvideo ()
  ((name        :initarg :name)
   (is-visible  :initarg :is-visible)
   (is-negative :initarg :is-negative)
   (capture     :initarg :capture)
   (hsv         :initarg :hsv)
   (erode       :initarg :erode)
   (glitch      :initarg :glitch)
   (repeat      :initarg :repeat)
   (rotation    :initarg :rotation)
   (xpos        :initarg :xpos)
   (ypos        :initarg :ypos)
   (flip        :initarg :flip)
   (pos         :initarg :pos)
   (scale       :initarg :scale))
  (:default-initargs
   :name (error "NAME required")
    :is-visible T
    :is-negative NIL
    :capture NIL
    :hsv NIL
    :pos 0
    :erode NIL
    :glitch NIL
    :repeat NIL
    :rotation NIL
    :xpos NIL
    :flip NIL
    :scale NIL))

(defclass ctext ()
  ((name      :initarg :name)
   (text      :initarg :text)
   (xpos      :initarg :xpos :initform 0)
   (ypos      :initarg :ypos :initform 0))
  (:default-initargs
   :name (error "NAME required")))

(defun queue-any-visible-p ()
  (member-if (lambda (x) (slot-value x 'is-visible)) *video-queue*))

(defun queue-find (name &optional (queue *video-queue*))
  (find name queue
        :test (lambda (x y) (eq x (slot-value y 'name)))))

(defun queue-skip-to (name secs)
  (declare (keyword name) (alexandria:non-negative-integer secs))
  (let ((obj (queue-find name)))
    (when obj
      (with-slots (capture) obj
        (skip-to capture secs)))))

(defun queue-delete (name)
  (declare (keyword name))
  (let ((obj (queue-find name)))
    (when obj
      (with-slots (is-visible) obj
        (setf is-visible NIL)))))

(defun make-ctext (name text &optional (xpos 0) (ypos 0))
  (declare (keyword name) (string text)
           (unsigned-byte xpos ypos))
  (let ((obj (queue-find name *text-queue*)))
    (if obj
        (setf (slot-value obj 'text) text
              (slot-value obj 'xpos) xpos
              (slot-value obj 'ypos) ypos)
        (push (make-instance 'ctext
                             :name name
                             :text text
                             :xpos xpos
                             :ypos ypos)
              *text-queue*))))

(defun make-cvideo
    (name file
     &key
       hsv glitch (flip -2 flip-p) erode repeat
       is-negative
       (pos 0)
       (rotation 0f0)
       (scale 1f0) (ypos 0) (xpos 0))
  (declare (keyword name) (string file)
           (type single-float rotation scale)
           (type (or unsigned-byte null) repeat)
           (type (integer -2 1) flip)
           (unsigned-byte xpos ypos)
           (boolean glitch hsv is-negative))
  (assert (uiop:file-exists-p file))
  ;; NOT add if already if queue
  (let ((obj (queue-find name)))
    (if obj
        (progn
          (setf (slot-value obj 'is-visible) T
                (slot-value obj 'hsv) hsv
                (slot-value obj 'glitch) glitch
                (slot-value obj 'erode) erode
                (slot-value obj 'rotation) rotation
                (slot-value obj 'scale) scale
                (slot-value obj 'xpos) xpos
                (slot-value obj 'ypos) ypos
                (slot-value obj 'flip) flip
                (slot-value obj 'is-negative) is-negative
                (slot-value obj 'pos) pos
                (slot-value obj 'repeat) repeat)) 
        (let ((capture
               (make-instance
                'cvideo
                :name name
                :capture (cv:create-file-capture file)
                :hsv hsv
                :erode erode
                :glitch glitch
                :is-negative is-negative
                :repeat repeat
                :flip flip                
                :rotation rotation
                :scale scale
                :xpos xpos
                :ypos ypos)))
          (push capture *video-queue*)
          T))))

(defun initialize ()
  (setf *text-queue* NIL)
  ;; Clear queue
  (when *video-queue*
    (loop :for video :in *video-queue* :do
       (with-slots (capture) video
         (cv:release-capture capture)))
    (setf *video-queue* NIL)))

;;(defun event-loop ())
(defun event-loop ()
  (loop :for video :in *video-queue* :do
     (when (eq (slot-value video 'name) :bunny)
       (with-slots (xpos y pos capture
                    hsv glitch rotation scale)
           video
         (setf rotation (sinr 20 10 5))))))

(defun render ()
  (declare (optimize (speed 3)))
  (if (= (the integer (cv:wait-key 30)) 27)
      'done
      (let ((size 200)
            (videos))
        (cv:with-ipl-images
            ((buf (cv:size size size) cv:+ipl-depth-8u+ 3)
             (fin (cv:size size size) cv:+ipl-depth-8u+ 3)
             (emp (cv:size size size) cv:+ipl-depth-8u+ 1))
          (loop
             :for video :in *video-queue*
             :do
             (with-slots
                   (is-visible
                    capture
                    erode
                    repeat
                    flip
                    is-negative
                    xpos ypos
                    hsv glitch
                    pos
                    rotation scale)
                 video
               (when is-visible
                 (when capture
                   (if repeat
                       (cv:with-ipl-images
                           ((small (cv:size 50 50) cv:+ipl-depth-8u+ 3))
                         (cv:resize (get-frame capture 0 pos) small)
                         (cv:repeat small buf))
                       (cv:resize (get-frame capture 0 pos) buf))
                   (if (not (= (the integer pos) 0))
                       (setf pos 0)))
                 ;; rotation, scale, move
                 (when (or (> (the single-float rotation) 0f0)
                           (not (= (the single-float scale) 1f0)))
                   (2d-rotate *rotation-matrix* xpos ypos rotation scale)
                   (cv:warp-affine buf buf *rotation-matrix*))
                 ;; hls - bgr555 - bgr565;; NOT
                 ;; rgb - lab - luv - xyz - hsv - ycrcb ;; YES
                 (when (> (the (integer -2 1) flip) -2)
                   (cv:flip buf buf flip))
                 (when erode
                   (cv:erode buf buf (cffi-sys:null-pointer) erode))
                 (when is-negative
                   (cv:not buf buf))
                 (when hsv
                   (cv:cvt-color buf buf cv:+bgr-2-hsv+))
                 (if (or glitch videos)
                     (cv:add-weighted buf .3 fin .3 .9 fin)
                     (cv:copy buf fin))
                 (setf videos T))))
          
          (loop :for text-obj :in *text-queue* :do
             (with-slots (text xpos ypos) text-obj
               (make-text fin text xpos ypos
                          :red 240 :green 240 :blue 240)))
                 
          (if videos
              (cv:show-image "multi" fin)
              (cv:show-image "multi" emp))))))

(defun show-videos ()
  "Show the video in FILENAME in a window."
  (cv:with-named-window
      ("multi" (+ +window-freeratio+ +window-gui-normal+))
    (loop
       (update-swank)
       (event-loop)
       (when (eq 'done (render))
         (return)))))

;; --------------------------------------------------

