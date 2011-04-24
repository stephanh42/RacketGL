;; Extremely simply OpenGL demo.
;; Draw a shaded rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 1)))
(require ffi/vector)
(require "viewer.rkt")


(define logo-bm (read-bitmap "plt-logo-red-gradient.png"))
(define texture #f)

;; Convert argb -> rgba, and convert to pre-multiplied alpha.
;; (Non-premultiplied alpha gives blending artifacts and is evil.)
;; Modern wisdom is not to convert to rgba but rather use 
;; GL_BGRA with UNSIGNED_INT_8_8_8_8_REV. But that turns out not
;; to work on some implementations, even ones which advertise
;; OpenGL 1.2 support. Great.
(define (argb->rgba! pixels)
  (for ((i (in-range (/ (bytes-length pixels) 4))))
       (let* ((offset (* 4 i))
              (alpha (bytes-ref pixels offset))
              (red (bytes-ref pixels (+ 1 offset)))
              (green (bytes-ref pixels (+ 2 offset)))
              (blue (bytes-ref pixels (+ 3 offset))))
         (bytes-set! pixels offset (quotient (* alpha red) 255))
         (bytes-set! pixels (+ 1 offset) (quotient (* alpha green) 255))
         (bytes-set! pixels (+ 2 offset) (quotient (* alpha blue) 255))
         (bytes-set! pixels (+ 3 offset) alpha))))

(define (setup)
  (let* ((w (send logo-bm get-width))
         (h (send logo-bm get-height))
         (pixels (make-bytes (* w h 4))))
    (set! texture (u32vector-ref (glGenTextures 1) 0))
    (send logo-bm get-argb-pixels 0 0 w h pixels)
    ;; massage data.
    (argb->rgba! pixels)
    
    (glBindTexture GL_TEXTURE_2D texture)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
    (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_RGBA GL_UNSIGNED_BYTE pixels)))

(define (draw)
  ; the coordinates
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))

  (define texcoord-array
    (s16vector 0 1
               1 1
               1 0
               0 0))

  (glBindTexture GL_TEXTURE_2D texture)
  (glEnable GL_TEXTURE_2D)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (glVertexPointer 2 (gl-vector->type vertex-array) 0 (f64vector->cpointer vertex-array))
  (glTexCoordPointer 2 (gl-vector->type texcoord-array) 0 (s16vector->cpointer texcoord-array))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY))



(view draw setup)
