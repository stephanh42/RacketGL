#lang racket/gui

(require "rgl.rkt")
(require ffi/vector)

(define frame 
  (new frame% 
       [label "Example"]
       [width 300]
       [height 300]))

(define (draw)
  (for ((ext (in-set (gl-extensions))))
       (printf "~a~%" ext))
  (printf "~s~%" (gl-version))
  (when (gl-has-extension? 'GL_EXT_texture_object)
    (printf "Yep, we have GL_EXT_texture_object~%"))
  (let ((v (glGetIntegerv GL_VERTEX_ARRAY)))
    (printf "result = ~s~%" (s32vector-ref v 0)))
  (glClear GL_COLOR_BUFFER_BIT)
  (glBegin GL_QUADS)
  (let ((v (list->f64vector '(-0.5 -0.5))))
    (glVertex2dv v))
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
     [style '(gl)]  
     [parent frame]))
(send c set-canvas-background (send the-color-database find-color "black"))

(send frame show #t)
