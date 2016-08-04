#lang racket/base

(require 
  racket/promise racket/future racket/include racket/set racket/match
  (rename-in racket/contract (-> ->>))
  ffi/unsafe ffi/vector)

(define stype (system-type))

(define win32?
  (and (eq? 'windows stype)
       (equal? "win32\\i386" (path->string (system-library-subpath #f)))))

(define-syntax _fun*
  (syntax-rules ()
    [(_fun* x ...)
     (if win32? (_fun #:abi 'stdcall x ...) (_fun x ...))]))

(define (load-get-proc-address gl-lib names)
  (if (null? names)
    (λ (x) (ffi-obj-ref x gl-lib (λ () #f)))
    (get-ffi-obj (car names) gl-lib (_fun* _string -> _pointer)
                 (λ () (load-get-proc-address gl-lib (cdr names))))))


;; The default-gl-procedure-loader procedure dynamically loads a GL procedure.
(define default-gl-procedure-loader
  (let ((get-proc-address 
          (delay 
            (case stype
              [(windows)
               (load-get-proc-address (ffi-lib "opengl32") 
                                      '("wglGetProcAddress"))]
              [(macosx)
               (load-get-proc-address (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")
                                      '())]
              [else ;boldly assume everybody else uses X11
                (load-get-proc-address (ffi-lib "libGL" '("1" ""))
                                       '("glXGetProcAddressARB" "glXGetProcAddress"))]))))
    (λ (name) ((force get-proc-address) name))))

(define gl-procedure-loader default-gl-procedure-loader)
(define (set-gl-procedure-loader! new-loader) (set! gl-procedure-loader new-loader))

(provide/contract
 (load-get-proc-address
  (->> ffi-lib? (listof string?)
       (->> string? (or/c cpointer? #f))))
 (default-gl-procedure-loader (->> string? (or/c cpointer? procedure? #f))) 
 (set-gl-procedure-loader! (->> (->> string? (or/c cpointer? procedure? #f)) any)))


(define (make-undefined-procedure name)
;  (printf "OpenGL procedure not available: ~a~%" name)
  (lambda args
    (error "OpenGL procedure not available:" name)))


(define (lookup-gl-procedure name type)
  (define ptr (gl-procedure-loader (symbol->string name)))
  (cond
    [ptr
     (function-ptr ptr type)]
    [else
     (make-undefined-procedure name)]))

; Load everything lazily.
; This speeds things up in general and is essential on Windows
; where GL procedures can only be looked up once a GL context is bound.
(define (create-gl-procedure name arity type checker)
  (let ((proc (delay (lookup-gl-procedure name type))))
    (procedure-rename
      (procedure-reduce-arity
       (lambda args
         (begin0
             (apply (force proc) args)
           (checker name)))
        arity)
      name)))

(define-syntax define-gl
  (syntax-rules ()
   ((_ name arity (type ...) contract checker)
    (begin
      (define name (create-gl-procedure 'name arity (_fun* type ...) checker))
      (provide/contract (name contract))))))

(define-syntax define-enum
  (syntax-rules ()
   ((_ name (constants ...))
    (begin
      (define name (let ((s (seteqv constants ...)))
                     (lambda (v) (set-member? s v))))
      (provide/contract (name (->> any/c boolean?)))))))

(define-syntax define-bitfield
  (syntax-rules ()
   ((_ name (constants ...))
    (begin
      (define name (let ((m (bitwise-ior constants ...)))
                     (lambda (v) (= v (bitwise-and v m)))))
      (provide/contract (name (->> any/c boolean?)))))))

; Check GL result
(define (check-gl-error name)
  (unless between-begin-end
    (let ((err (glGetError)))
      (unless (zero? err)
        (let ((msg (hash-ref error-messages err
                             (lambda () (format "Error code ~s." err)))))
          (error (format "OpenGL error in procedure ~a: ~a" name msg)))))))

(define (check-gl-error-begin name)
  (set! between-begin-end #t))

(define (check-gl-error-end name)
  (set! between-begin-end #f)
  (check-gl-error name))

(provide/contract
  (GLsync? (->> any/c boolean?)))

(define-cpointer-type _GLsync)

; A "gl-vector" is any homogenous vector of a type which is used with the OpenGL API.
(provide/contract 
  (gl-vector? (->> any/c boolean?))
  (gl-pointer? (->> any/c boolean?))
  (gl-type? (->> any/c boolean?)))

(define (gl-vector? obj)
  (or (bytes? obj) (s8vector? obj) (u16vector? obj) (s16vector? obj)
      (u32vector? obj) (s32vector? obj) (f32vector? obj) (f64vector? obj)))

(define (gl-pointer? obj)
  (or (cpointer? obj) (exact-nonnegative-integer? obj) (gl-vector? obj)))

(define (gl-type? obj)
  (hash-has-key? gl-types obj))

; Some functions take a pointer parameter, but when a VBO is bound the pointer
; is really interpreted as an integer offset.
; On the Racket side, we want the function to simply accept either a pointer or an integer in that case.
; So here is a conversion function to be used as a pre:-code sequence.
(define (convert-vbo-pointer v)
  (cond
    ((cpointer? v) v)
    ((exact-integer? v) (ptr-add #f v))
    (else (gl-vector->cpointer v))))

(define-fun-syntax _pointer/intptr
  (syntax-id-rules (_pointer/intptr)
    [_pointer/intptr (type: _pointer pre: (x => (convert-vbo-pointer x)))]))

(include "generated/gl_specs.inc")

(define (split-spaces str)
  (regexp-split #px"\\s+" str))

;; Query information in a convenient way.
;(provide gl-version gl-extensions gl-has-extension?)
(provide/contract 
  (gl-version (->> (listof exact-integer?)))
  (gl-extensions (->> set-eq?))
  (gl-has-extension? (->> symbol? boolean?))
  (gl-version-at-least? (->> (listof exact-integer?) boolean?)))

(define gl-version
  (let ()
    (define version 
      (delay 
        (let* ([vers (glGetString GL_VERSION)]
               [ws (split-spaces vers)]
               [nums
                (match ws
                  [(list* "OpenGL" "ES" nums _) nums]
                  [_ (car ws)])])          
          (map string->number
               (regexp-split #px"\\." nums)))))
    (lambda () (force version))))

(define gl-extensions
  (let ((extensions (delay
                      (for/seteq ((ext (in-list (split-spaces (glGetString GL_EXTENSIONS)))))
                                 (string->symbol ext)))))
    (lambda () (force extensions))))

(define (gl-has-extension? ext)
  (set-member? (gl-extensions) ext))

(define (version>= v1 v2)
  (cond
    ((null? v2) #t)
    ((null? v1) #f)
    (else
      (let ((n1 (car v1))
            (n2 (car v2)))
        (cond 
          ((= n1 n2)
           (version>= (cdr v1) (cdr v2)))
          (else (> n1 n2)))))))

(define (gl-version-at-least? version)
  (version>= (gl-version) version))

;; Get the appropriate type enum for a Racket vector.
;; Useful for glVertexPointer and friends.
;; Also get length and cpointer in one operation.
(provide/contract 
  (gl-vector->type (->> gl-vector? gl-type?))
  (gl-vector->cpointer (->> gl-vector? cpointer?))
  (gl-vector->length (->> gl-vector? gl-type?))
  (gl-vector->type/cpointer (->> gl-vector? (values gl-type? cpointer?)))
  (gl-vector->type/cpointer/length (->> gl-vector? (values gl-type? cpointer? exact-nonnegative-integer?)))
  (gl-vector-sizeof (->> gl-vector? exact-nonnegative-integer?))
  (gl-vector-alignof (->> gl-vector? exact-nonnegative-integer?))
  (gl-type->ctype (->> gl-type? ctype?))
  (gl-type-sizeof (->> gl-type? exact-nonnegative-integer?))
  (gl-type-alignof (->> gl-type? exact-nonnegative-integer?)))

(define (gl-vector->info vec)
  (cond
    ((bytes? vec) (values GL_UNSIGNED_BYTE values bytes-length))
    ((s8vector? vec) (values GL_BYTE s8vector->cpointer s8vector-length))
    ((u16vector? vec) (values GL_UNSIGNED_SHORT u16vector->cpointer u16vector-length))
    ((s16vector? vec) (values GL_SHORT s16vector->cpointer s16vector-length))
    ((u32vector? vec) (values GL_UNSIGNED_INT u32vector->cpointer u32vector-length))
    ((s32vector? vec) (values GL_INT s32vector->cpointer s32vector-length))
    ((f32vector? vec) (values GL_FLOAT f32vector->cpointer f32vector-length))
    ((f64vector? vec) (values GL_DOUBLE f64vector->cpointer f64vector-length))))


(define (gl-vector->type vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              type))

(define (gl-vector->cpointer vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              (->cpointer vec)))

(define (gl-vector->length vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              (length vec)))

(define (gl-vector->type/cpointer vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              (values type (->cpointer vec))))

(define (gl-vector->type/cpointer/length vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
              (values type (->cpointer vec) (length vec))))

(define gl-types (hasheqv 
  GL_UNSIGNED_BYTE _uint8
  GL_BYTE _sint8
  GL_UNSIGNED_SHORT _uint16
  GL_SHORT _sint16
  GL_UNSIGNED_INT _uint32
  GL_INT _sint32
  GL_FLOAT _float
  GL_DOUBLE _double))

(define (gl-type->ctype type)
  (hash-ref gl-types type))

(define (gl-type-sizeof type)
  (ctype-sizeof (gl-type->ctype type)))

(define (gl-type-alignof type)
  (ctype-alignof (gl-type->ctype type)))

(define (gl-vector-sizeof vec)
  (let-values (((type ->cpointer length) (gl-vector->info vec)))
    (* (length vec) (gl-type-sizeof type))))

(define (gl-vector-alignof vec)
  (gl-type-alignof (gl-vector->type vec)))
   
(define error-messages (hasheqv
        GL_NO_ERROR "No error has been recorded."
        GL_INVALID_ENUM "An unacceptable value is specified for an enumerated argument."
        GL_INVALID_VALUE "A numeric  argument is out of range."
        GL_INVALID_OPERATION "The specified operation is not allowed in the current state."
        GL_STACK_OVERFLOW "This command would cause a stack overflow."
        GL_STACK_UNDERFLOW "This command would cause a stack underflow."
        GL_OUT_OF_MEMORY "There is not enough memory left to execute the command."))

(define between-begin-end #f)

