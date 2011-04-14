;; Extremely simply OpenGL demo.
;; Draw a white rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require (planet stephanh/rgl:1:0/rgl)
         ffi/vector)


(define frame 
  (new frame% 
       [label "Example"]
       [width 300]
       [height 300]))

(define (print-info)
  ; print available extensions
  (for ((ext (in-set (gl-extensions))))
       (printf "Extension: ~a~%" ext))

  ; print version, as a nice parseable Racket list
  (printf "~s~%" (gl-version))

  ; demo to check for a certain extension
  (if (gl-has-extension? 'GL_EXT_texture_object)
    (printf "Yep, we have GL_EXT_texture_object~%")
    (printf "Sorry no GL_EXT_texture_object~%"))

  ; demo to query some array-based state
  (let ((v (glGetIntegerv GL_VERTEX_ARRAY)))
    (printf "glGet on GL_VERTEX_ARRAY = ~s~%" (s32vector-ref v 0))))
 

(define first-call #t)

(define (draw)
  ; print some info on the first call
  (when first-call
    (set! first-call #f)
    (print-info))

  ; draw our exciting WHITE RECTANGLE!!!
  (glClear GL_COLOR_BUFFER_BIT)
  (glBegin GL_QUADS)
  (glVertex2dv (f64vector -0.5 -0.5)) ; wow, we can even specify a vector!
  (glVertex2d 0.5 -0.5)
  (glVertex2d 0.5 0.5)
  (glVertex2d -0.5 0.5)
  (glEnd))


(define gl-canvas% 
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers)

    (define/override (on-paint)     
      (with-gl-context (lambda () (draw)))
      (swap-gl-buffers))))


(define c
  (new gl-canvas%
     [style '(gl)]  ; if you don't specify style gl it won't work
     [parent frame]))

(send c set-canvas-background (send the-color-database find-color "black"))

(send frame show #t)
