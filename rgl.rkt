#lang racket

(require ffi/unsafe ffi/vector ffi/cvector)

(define stype (system-type))

(define gl-lib 
  (delay 
    (case stype
      [(windows) (ffi-lib "opengl32")]
      [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")]
      [else (ffi-lib "libGL" '("1" ""))])))

(define windows? (eq? 'windows stype))

(define win32?
  (and windows?
       (equal? "win32\\i386" (path->string (system-library-subpath #f)))))

(define-syntax _fun*
  (syntax-rules ()
    [(_fun* x ...)
     (if win32? (_fun #:abi 'stdcall x ...) (_fun x ...))]))


;; The getProcAddress procedure dynamically loads a GL procedure.
(define getProcAddress 
  (delay 
    (if windows?
      (get-ffi-obj "wglGetProcAddress" (force gl-lib) (_fun* _string -> _pointer)
                   (lambda () (lambda (x) #f)))
      ; According to the Linux ABI, the correct entry point is glXGetProcAddressARB.
      ; But let's try the non-ARB version too.
      (get-ffi-obj "glXGetProcAddressARB" (force gl-lib) (_fun _string -> _pointer)
                   (lambda ()
                     (get-ffi-obj "glXGetProcAddress" (force gl-lib) (_fun _string -> _pointer)
                                  (lambda () (lambda (x) #f))))))))

(define (make-undefined-procedure name)
;  (printf "OpenGL procedure not available: ~a~%" name)
  (lambda args
    (error "OpenGL procedure not available:" name)))

(define (lookup-gl-procedure name type)
  (get-ffi-obj name (force gl-lib) type
               (lambda ()
                 (let ((ptr ((force getProcAddress) (symbol->string name))))
                   (if ptr
                     (begin
;                       (printf "Loaded dynamically: ~a~%" name)
                       (function-ptr ptr type))
                     (make-undefined-procedure name))))))


; For Windows, lazy MUST be #t since the actual lookup with getProcAddress
; can only be done once a GL context is bound.
; For X, it can be #f if you are so inclined.
(define lazy #t)

(define (create-gl-procedure name type)
  (if lazy
    (let ((proc (delay (lookup-gl-procedure name type))))
      (lambda args (apply (force proc) args)))
    (lookup-gl-procedure name type)))

(define-syntax define-gl
  (syntax-rules ()
   ((_ name (type ...))
    (begin
      (define name (create-gl-procedure 'name (_fun* type ...)))
      (provide name)))))

(include "gl_specs.rkt")

(define (split-spaces str)
  (regexp-split #px"\\s+" str))

;; Query information in a convenient way.
(provide gl-version gl-extensions gl-has-extension?)

(define gl-version
  (let ((version 
          (delay 
            (map string->number
                 (regexp-split #px"\\."
                               (car (split-spaces (glGetString GL_VERSION))))))))
    (lambda () (force version))))

(define gl-extensions
  (let ((extensions (delay
                      (for/seteq ((ext (in-list (split-spaces (glGetString GL_EXTENSIONS)))))
                                 (string->symbol ext)))))
    (lambda () (force extensions))))

(define (gl-has-extension? ext)
  (set-member? (gl-extensions) ext))
