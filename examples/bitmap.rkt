;; Example library which loads a bitmap into a texture.
#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 2)))
(require ffi/vector)

(provide bitmap->texture load-texture)


;; Convert argb -> rgba, and convert to pre-multiplied alpha.
;; (Non-premultiplied alpha gives blending artifacts and is evil.)
;; Modern wisdom is not to convert to rgba but rather use 
;; GL_BGRA with GL_UNSIGNED_INT_8_8_8_8_REV. But that turns out not
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


;; Convert a Racket bitmap into an OpenGL texture.
;; (with lots of default settings.)
(define (bitmap->texture bm) 
  (let* ((w (send bm get-width))
         (h (send bm get-height))
         (pixels (make-bytes (* w h 4)))
         (texture (u32vector-ref (glGenTextures 1) 0)))
    (send bm get-argb-pixels 0 0 w h pixels)
    ;; massage data.
    (argb->rgba! pixels)
    
    (glBindTexture GL_TEXTURE_2D texture)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
    (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_RGBA GL_UNSIGNED_BYTE pixels)
    texture))

;; Directly load a file from disk as texture.
(define (load-texture filename)
  (bitmap->texture (read-bitmap filename)))
