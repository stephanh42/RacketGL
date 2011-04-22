;; Extremely simply OpenGL demo.
;; Draw a shaded rectangle on the screen.
;; Also query some information on the OpenGL implementation.

#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 1)))
(require ffi/vector)
(require "viewer.rkt")

(define (check-gl-error where)
  (let ((err (glGetError)))
    (unless (= err GL_NO_ERROR)
      (error "GL error:" where err))))


(define (load-program-source shader port)
  (let* ((lines (for/vector ((line (in-lines port))) line))
         (sizes (for/list ((line (in-vector lines))) (string-length line)))
         (sizes (list->s32vector sizes)))
   (glShaderSource shader (vector-length lines) lines sizes)
   (check-gl-error 'glShaderSource)))

(define (load-program port)
  (let ((program (glCreateProgram))
        (shader (glCreateShader GL_FRAGMENT_SHADER)))
    (load-program-source shader port)
    (glCompileShader shader)
    (check-gl-error 'glCompileShader)
    (glAttachShader program shader)
    (check-gl-error 'glAttachShader)
    (glLinkProgram program)
    (check-gl-error 'glLinkProgram)
    program))

(define program #f)

(define (setup)
  (set! program (call-with-input-file "test.glsl" load-program)))

(define (draw)
  ; the coordinates
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))

  (define texcoord-array
    (f64vector 0 0
               0.5 0
               0.5 0.5
               0 0.5))


  (glUseProgram program)
  (check-gl-error 'glUseProgram)

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (glVertexPointer 2 (gl-vector->type vertex-array) 0 (f64vector->cpointer vertex-array))
  (glTexCoordPointer 2 (gl-vector->type texcoord-array) 0 (f64vector->cpointer texcoord-array))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (glUseProgram 0))



(view draw setup)
