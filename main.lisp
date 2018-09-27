(in-package :viseq)

(defvar *video-queue* NIL)
(defvar *text-queue* NIL)
(defvar *rotation-matrix* (cv:create-mat 2 3 5))

(defstruct cvideo
  (name NIL :type keyword)
  (is-visible T :type boolean)
  (is-negative NIL :type boolean)
  (hsv NIL :type boolean)
  (glitch NIL :type boolean)
  (capture NIL :type cffi:foreign-pointer)
  (erode 0 :type unsigned-byte)
  (repeat 1 :type unsigned-byte)
  (rotation 0f0 :type single-float)
  (scale 1f0 :type single-float)
  (xpos 0 :type unsigned-byte)
  (ypos 0 :type unsigned-byte)
  (flip -2 :type integer)
  (pos 0 :type integer))

(defstruct ctext
  (name NIL :type keyword)
  (text "" :type string)
  (xpos 0 :type unsigned-byte)
  (ypos 0 :type unsigned-byte))

(defun queue-any-visible-p ()
  (member-if (lambda (x) (cvideo-is-visible x)) *video-queue*))

(defun queue-find (name &optional (queue *video-queue*))
  (find name queue
        :test (lambda (x y) (eq x (cvideo-name y)))))

(defun queue-skip-to (name secs)
  (declare (keyword name) (alexandria:non-negative-integer secs))
  (let ((obj (queue-find name)))
    (when obj
      (skip-to (cvideo-capture obj) secs))))

(defun queue-delete (name)
  (declare (keyword name))
  (let ((obj (queue-find name)))
    (when obj
      (setf (cvideo-is-visible obj) NIL))))

(defun push-ctext (name text &optional (xpos 0) (ypos 0))
  (declare (keyword name) (string text)
           (unsigned-byte xpos ypos))
  (let ((obj (find name *text-queue*
                   :test (lambda (x y) (eq x (ctext-name y))))))
    (if obj
        (setf (ctext-text obj) text
              (ctext-xpos obj) xpos
              (ctext-ypos obj) ypos)
        (push (make-ctext :name name :text text
                          :xpos xpos :ypos ypos)
              *text-queue*))))

(defun push-cvideo
    (name file
     &key
       hsv glitch (flip -2 flip-p) (erode 0) (repeat 1)
       is-negative
       (pos 0)
       (rotation 0f0)
       (scale 1f0) (ypos 0) (xpos 0))
  (declare (keyword name) (string file)
           (type single-float rotation scale)
           (type unsigned-byte repeat erode xpos ypos)
           (type (integer -2 1) flip)
           (boolean glitch hsv is-negative))
  (assert (uiop:file-exists-p file))
  ;; NOT add if already if queue
  (let ((obj (queue-find name)))
    (if obj
        (progn
          (setf (cvideo-is-visible obj) T
                (cvideo-hsv obj) hsv
                (cvideo-glitch obj) glitch
                (cvideo-erode obj) erode
                (cvideo-rotation obj) rotation
                (cvideo-scale obj) scale
                (cvideo-xpos obj) xpos
                (cvideo-ypos obj) ypos
                (cvideo-flip obj) flip
                (cvideo-is-negative obj) is-negative
                (cvideo-pos obj) pos
                (cvideo-repeat obj) repeat)) 
        (let ((cvideo
               (make-cvideo
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
          (push cvideo *video-queue*)
          T))))

(defun initialize ()
  (setf *text-queue* NIL)
  ;; Clear queue
  (when *video-queue*
    (loop :for video :in *video-queue* :do
       (cv:release-capture (cvideo-capture video)))
    (setf *video-queue* NIL)))

(defun event-loop ())
;; (defun event-loop ()
;;   (loop :for video :in *video-queue* :do
;;      (when (eq (slot-value video 'name) :bunny)
;;        (with-slots (xpos y pos capture
;;                     hsv glitch rotation scale)
;;            video
;;          (setf rotation (sinr 20 10 5))))))

(defun render ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (= (the integer (cv:wait-key 30)) 27)
      'done
      (let ((size 100)
            (videos))
        (cv:with-ipl-images
            ((buf (cv:size size size) cv:+ipl-depth-8u+ 3)
             (fin (cv:size size size) cv:+ipl-depth-8u+ 3)
             (emp (cv:size size size) cv:+ipl-depth-8u+ 1))
          (loop
             :for video :in *video-queue*
             :do
             (let ((is-visible (cvideo-is-visible video))
                   (capture (cvideo-capture video))
                   (erode (cvideo-erode video))
                   (repeat (cvideo-repeat video))
                   (flip (cvideo-flip video))
                   (is-negative (cvideo-is-negative video))
                   (xpos (cvideo-xpos video))
                   (ypos (cvideo-ypos video))
                   (hsv (cvideo-hsv video))
                   (glitch (cvideo-glitch video))
                   (pos (cvideo-pos video))
                   (rotation (cvideo-rotation video))
                   (scale (cvideo-scale video)))
               (when is-visible
                 (if (not (= repeat 1))
                     (cv:with-ipl-images
                         ((small (cv:size 50 50) cv:+ipl-depth-8u+ 3))
                       (cv:resize (get-frame capture 0 pos) small)
                       (cv:repeat small buf))
                     (cv:resize (get-frame capture 0 pos) buf))
                 (if (not (= pos 0))
                     (setf pos 0))
                 ;; rotation, scale, move
                 (when (or (> rotation 0f0)
                           (not (= scale 1f0)))
                   (2d-rotate *rotation-matrix* xpos ypos rotation scale)
                   (cv:warp-affine buf buf *rotation-matrix*))
                 ;; ;; hls - bgr555 - bgr565;; NOT
                 ;; ;; rgb - lab - luv - xyz - hsv - ycrcb ;; YES
                 (when (> (the (integer -2 1) flip) -2)
                   (cv:flip buf buf flip))
                 ;; (when (not (= erode 0))
                 ;;   (cv:erode buf buf (cffi-sys:null-pointer) erode))
                 (when is-negative
                   (cv:not buf buf))
                 (when hsv
                   (cv:cvt-color buf buf cv:+bgr-2-hsv+))
                 (if (or glitch videos)
                     (cv:add-weighted buf .3 fin .3 .9 fin)
                     (cv:copy buf fin))
                 (setf videos T))))
          
          (loop :for text-obj :in *text-queue* :do
             (make-text fin
                        (ctext-text text-obj)
                        (ctext-xpos text-obj)
                        (ctext-ypos text-obj)
                        :red 240 :green 240 :blue 240))
                 
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

