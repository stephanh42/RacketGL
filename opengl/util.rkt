#lang racket/base
(require opengl
         racket/class
         racket/draw
         (rename-in racket/contract (-> ->>))
         ffi/vector)

;;; Utility functions for dealing with textures
(provide/contract 
  (bitmap->texture 
    (->* ((is-a?/c bitmap%))
         (#:mipmap any/c
          #:repeat (one-of/c 'none 'x 'y 'both))
         exact-nonnegative-integer?))

  (load-texture 
    (->* ((or/c path-string? input-port?))
         (#:mipmap any/c
          #:repeat (one-of/c 'none 'x 'y 'both))
         exact-nonnegative-integer?)))

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
(define (bitmap->texture bm #:mipmap (mipmap #t) #:repeat (repeat-mode 'none))
  (let* ((w (send bm get-width))
         (h (send bm get-height))
         (pixels (make-bytes (* w h 4)))
         (texture (u32vector-ref (glGenTextures 1) 0)))

    (define (load-texture-data)
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_RGBA GL_UNSIGNED_BYTE pixels))

    (send bm get-argb-pixels 0 0 w h pixels)
    ;; massage data.
    (argb->rgba! pixels)
    
    (glBindTexture GL_TEXTURE_2D texture)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S 
                     (case repeat-mode ((x both) GL_REPEAT) (else GL_CLAMP)))
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T 
                     (case repeat-mode ((y both) GL_REPEAT) (else GL_CLAMP)))
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (cond 
      ;; modern mipmap generation method
      ((and mipmap (gl-version-at-least? '(3 0)))
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
       (load-texture-data)
       (glGenerateMipmap GL_TEXTURE_2D))

      ;; old mipmap generation method
      ((and mipmap (gl-version-at-least? '(1 4)))
       (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
       (glTexParameteri GL_TEXTURE_2D GL_GENERATE_MIPMAP GL_TRUE)
       (load-texture-data))

      (else
        ; fallback to not using mipmaps
        ; this seems more useful than erroring
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
        (load-texture-data)))

    texture))

;; Directly load a file from disk as texture.
(define (load-texture filename #:mipmap (mipmap #t) #:repeat (repeat-mode 'none))
  (bitmap->texture (read-bitmap filename) #:mipmap mipmap #:repeat repeat-mode))


;;; Utility functions for dealing with shaders
(provide/contract 
  (load-shader
    (->> (or/c path-string? input-port?) exact-nonnegative-integer? exact-nonnegative-integer?))

  (create-program
    (->* () () #:rest (listof exact-nonnegative-integer?) exact-nonnegative-integer?)))

(define (get-shader-parameter shader pname)
  (glGetShaderiv shader pname))

(define (get-shader-info-log shader)
  (let ((log-length (get-shader-parameter shader GL_INFO_LOG_LENGTH)))
    (let-values (((actual-length info-log) (glGetShaderInfoLog shader log-length)))
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

(define (get-program-parameter program pname)
  (glGetProgramiv program pname))

(define (get-program-info-log program)
  (let ((log-length (get-program-parameter program GL_INFO_LOG_LENGTH)))
    (let-values (((actual-length info-log) (glGetProgramInfoLog program log-length)))
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

(define (load-shader-source shader port)
  (let* ((lines (for/vector ((line (in-lines port))) (string-append line "\n")))
         (sizes (for/list ((line (in-vector lines))) (string-length line)))
         (sizes (list->s32vector sizes)))
   (glShaderSource shader (vector-length lines) lines sizes)))

(define (load-shader port-or-path shader-type)
  (let ((shader (glCreateShader shader-type)))
    (if (input-port? port-or-path) 
      (load-shader-source shader port-or-path)
      (call-with-input-file port-or-path (λ (p) (load-shader-source shader p)) #:mode 'text))
    (glCompileShader shader)
    (unless (= (get-shader-parameter shader GL_COMPILE_STATUS) GL_TRUE)
      (error 'load-shader "Error compiling shader ~a: ~a" port-or-path (get-shader-info-log shader)))
    shader))

(define (create-program . shaders)
  (let ((program (glCreateProgram)))
    (for-each (λ (sh) (glAttachShader program sh)) shaders)
    (glLinkProgram program)
    (unless (= (get-program-parameter program GL_LINK_STATUS) GL_TRUE)
      (error 'create-program "Error linking program: ~a" (get-program-info-log program)))
    program))
