#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 1)))

(provide view)

(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers refresh)

    (init-field draw)
    (init-field (setup void))

    (define setup-called #f)

    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (glViewport 0 0 width height)
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (if (< width height)
           (let ((h (/ height width)))
             (glFrustum -1.0 1.0 (- h) h 8.0 12.0))
           (let ((h (/ width height)))
             (glFrustum (- h) h -1.0 1.0 8.0 12.0)))
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (glTranslated 0.0 0.0 -10.0))))

    (define x-rotation 0)
    (define y-rotation 0)
 
    (define/override (on-paint)
      (with-gl-context               
        (lambda ()
          (unless setup-called
            (setup)
            (set! setup-called #t))
          (glClearColor 0.0 0.0 0.3 0.0) ; darkish blue
          (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
          (glPushMatrix)
          (glRotated y-rotation 1 0 0)
          (glRotated x-rotation 0 1 0)
          (draw)
          (glPopMatrix)))
      (swap-gl-buffers))

    (define handle-motion void)

    (define/override (on-event event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (case (send event get-event-type)
          ((left-down)
           (set! handle-motion
             (let ((old-x x) (old-y y))
               (lambda (new-x new-y)
                 (set! x-rotation (+ x-rotation (- new-x old-x)))
                 (set! y-rotation (+ y-rotation (- new-y old-y)))
                 (set! old-x new-x)
                 (set! old-y new-y)
                 (refresh)))))
          ((left-up)
           (set! handle-motion void))
          ((motion) (handle-motion x y)))))))






(define (view draw (setup void))
  (define frame 
    (new frame% 
         [label "OpenGL viewer"]
         [width 300]
         [height 300]))

  (define c
    (new gl-viewer% 
         (style '(gl no-autoclear)) 
         (parent frame) 
         (draw draw) (setup setup)))

  (send frame show #t))
