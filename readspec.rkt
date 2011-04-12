#lang racket

(define-struct param-spec
               (name type mode shape size))

(define-struct function-spec
               (name
                (params #:mutable)
                (return #:mutable)))

(define-struct mode-dependent-type (in out))

(define type-to-vector
  (make-hash
    '(
      (_int8 . _s8vector)
      (_uint16 . _u16vector)
      (_int16 . _s16vector)
      (_uint32 . _u32vector)
      (_int32 . _s32vector)
      (_uint64 . _u64vector)
      (_int64 . _s64vector)
      (_float . _f32vector)
      (_double* . _f64vector))))

(define (pointer-to type . args)
  (if (and (equal? args '(1)) (not (eq? type '_void)))
    (mode-dependent-type
      `(_ptr i ,type) `(_ptr o ,type))
    (case type
      ((_void) '_pointer)
      ((_byte _uint8) (mode-dependent-type 
                        '_string*/utf-8 
                        (if (null? args)
                          '_bytes
                          `(_bytes o ,@ args))))
      (else
        (let ((vt (hash-ref type-to-vector type #f)))
          (if vt
            (mode-dependent-type
              `(,vt i)
              (if (null? args) 
                vt
                `(,vt o ,@ args))) 
            (mode-dependent-type
              `(_vector i ,type)
              (if (null? args) 
                '_pointer
                `(_vector o ,type ,@ args)))))))))


(define basic-type-map
  (make-hash
    (list
      (cons "GLshort" '_int16)
      (cons "GLvoid" '_void)
      (cons "const GLubyte *" (pointer-to '_uint8))
      ;    (cons "GLsync" _GLsync)
      ;    (cons "GLhandleARB" _GLhandleARB)
      (cons "GLboolean" '_bool)
      (cons "struct _cl_event *" '_pointer)
      (cons "GLint64EXT" '_int64)
      (cons "GLsizeiptrARB" (pointer-to '_int32))
      ;    (cons "GLDEBUGPROCARB" _GLDEBUGPROCARB)
      (cons "GLenum" '_int32)
      (cons "GLint" '_int32)
      (cons "GLclampd" '_double*)
      (cons "GLvoid*" (pointer-to '_void))
      (cons "GLhalfNV"'_uint16) ; A 16-bit floating point number. You get the bits, good luck. ;-)
      ;    (cons "_GLfuncptr" __GLfuncptr)
      (cons "GLubyte" '_uint8)
      ;    (cons "GLvdpauSurfaceNV" _GLvdpauSurfaceNV)
      (cons "GLcharARB*" (pointer-to '_byte))
      (cons "GLdouble*" (pointer-to '_double*))
      (cons "struct _cl_context *" '_pointer)
      (cons "GLcharARB" '_byte)
      (cons "GLfloat" '_float)
      (cons "GLuint64" '_uint64)
      (cons "GLbyte" '_int8)
      (cons "GLbitfield" '_uint32)
      (cons "GLuint64EXT" '_uint64)
      (cons "GLchar*" (pointer-to '_byte))
      (cons "GLsizeiptr" (pointer-to '_int32))
      (cons "GLchar" '_byte)
      ;    (cons "GLUquadric*" _GLUquadric*)
      (cons "GLdouble" '_double*)
      (cons "GLintptr" '_intptr)
      ;    (cons "GLUtesselator*" _GLUtesselator*)
      (cons "GLsizei" '_int32)
      (cons "GLvoid* const" (pointer-to '_void))
      ;    (cons "GLDEBUGPROCAMD" _GLDEBUGPROCAMD)
      (cons "GLboolean*" (pointer-to '_bool))
      (cons "GLint64" '_int64)
      (cons "GLintptrARB" '_intptr)
      ;    (cons "GLUnurbs*" _GLUnurbs*)
      (cons "GLuint" '_uint32)
      (cons "GLclampf" '_float)
      (cons "GLushort" '_uint16)
      (cons "GLfloat*" (pointer-to '_float)))))

(define (read-type-map input-port)
  (let ((result '()))

    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^#" l) => void)
           (else
             (let ((lst (regexp-split #px"\\s*,\\s*" l)))
               (set! result
                 (cons (cons (list-ref lst 0) (list-ref lst 3))
                       result))))))

    result))

(define (read-enums input-port)

  (let ((visited (set))
        (pname-map (make-hasheqv)))

    (define (parse-pname enum line)
      (cond
        ((regexp-match #px"#\\s+([0-9]+)\\s+" line)
         => (lambda (m) (hash-set! pname-map (string->number enum) 
                                   (string->number (list-ref m 1)))))))

    (define (define-enum name value line)
      (unless (set-member? visited name)
        (printf "(define GL_~a ~a)~%" name value)
        (printf "(provide GL_~a)~%" name)
        (set! visited (set-add visited name))
        (parse-pname value line)))


    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^\\s+([a-zA-Z0-9_]+)\\s*=\\s*([0-9]+)\\s*(#.*)?$" l)
            =>
            (lambda (m)
              (define-enum (list-ref m 1) (list-ref m 2) l)))

           ((regexp-match #px"^\\s+([a-zA-Z0-9_]+)\\s*=\\s*0[xX]([0-9a-fA-F]+)\\s*(#.*)?$" l)
            =>
            (lambda (m)
              (define-enum (list-ref m 1) (string-append "#x" (list-ref m 2)) l)))))

    (printf "~s~%"
            `(define pname-map (make-immutable-hasheqv
                                 ',(for/list (((k v) (in-hash pname-map))) `(,k . ,v)))))))



(define (read-function-specs input-port)
  (let ((result '())
        (current-function-spec #f))

    (define (new-function m)
      (let* ((name (list-ref m 1))
             (spec (function-spec name '() #f)))
        (set! result (cons spec result))
        (set! current-function-spec spec)))

    (define (handle-param m)
      (let ((spec (apply param-spec (cdr m))))
        (set-function-spec-params! current-function-spec
                                   (append
                                     (function-spec-params current-function-spec)
                                     (list spec)))))

    (define (handle-return m)
      (let ((return-type (list-ref m 1)))
        (set-function-spec-return! current-function-spec return-type)))

    (for ((l (in-lines input-port)))
         (cond
           ((regexp-match #px"^([a-zA-Z0-9_]+)\\(.*\\)" l) => new-function)
           ((regexp-match #px"^\\s+return\\s+(\\S+)" l) => handle-return)
           ((regexp-match #px"^\\s+param\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)(?:\\s+\\[(.*)\\])?" l) => handle-param)))

    result))

(define type-map (make-hash (call-with-input-file "gl.tm" read-type-map)))

(hash-set! type-map "void" "GLvoid")

(define (base-to-ffi-type type)
  (cond
    ((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 t2)))
    (else type)))

(define (parse-param-spec-size str)
  (if str
    (begin
      (cond
        ((regexp-match #px"^[0-9]+$" str) (list (string->number str)))
        ((regexp-match #px"^[a-zA-Z]+$" str) (list (string->symbol str)))
        ((regexp-match #px"^([a-zA-Z]+)\\*([0-9]+)$" str) 
         =>
         (lambda (m) (list '* (string->symbol (list-ref m 1)) (string->number (list-ref m 2)))))
        ((equal? str "COMPSIZE(pname)") '((hash-ref pname-map pname)))
        (else 
          (printf "; Unparseable array size expression: ~a~%" str)
          '())))
    '()))

(define (to-ffi-type param-spec)

  (let ((t (base-to-ffi-type (param-spec-type param-spec)))
        (shape (param-spec-shape param-spec))
        (size-args (parse-param-spec-size (param-spec-size param-spec))))

    (let ((result
            (cond
              ((equal? shape "value") t)
              ((equal? shape "array")
               (if t (apply pointer-to t size-args) (pointer-to '_void)))
              ((equal? shape "reference")
               (if (and t (not (eq? t '_void))) 
                 (mode-dependent-type `(_ptr i ,t) `(_ptr o ,t))
                 (pointer-to '_void)))
              (else t))))
      (select-mode result (param-spec-mode param-spec)))))

(define (return-to-ffi-type rettype)
  (cond
    ((equal? rettype "String")
     '_string*/utf-8)
    (else (select-mode (base-to-ffi-type rettype) "out"))))

(define (output-type? t)
  (and (list? t) (memq 'o t)))

(define (sanitize name)
  (if (eq? name 'values) 'the-values name))

(define (to-ffi-fun funspec)
  (let* ((params (map 
                   (lambda (p)
                     (list (sanitize (string->symbol (param-spec-name p)))
                           ': (to-ffi-type p)))
                   (function-spec-params funspec)))
         (output-params (map car (filter (lambda (p) (output-type? (list-ref p 2))) params)))
         (rettype (return-to-ffi-type (function-spec-return funspec))))
    (cond
      ((null? output-params)
       `(,@params -> ,rettype))
      ((eq? rettype '_void)
       (if (= 1 (length output-params))
         `(,@params -> ,rettype -> ,(car output-params))
         `(,@params -> ,rettype -> (values ,@ output-params))))
      (else
       `(,@params -> (result : ,rettype) -> (values result ,@ output-params))))))


(define (select-mode t mode)
  (cond
    ((mode-dependent-type? t)
     (cond
       ((equal? mode "in")
        (select-mode (mode-dependent-type-in t) mode))
       ((equal? mode "out")
        (select-mode (mode-dependent-type-out t) mode))
       (else (error "Unknown mode:" mode))))
    ((pair? t)
     (cons (select-mode (car t) mode)
           (select-mode (cdr t) mode)))
    (else t)))

(define (contains-string? expr)
  (cond
    ((string? expr) #t)
    ((pair? expr)
     (or (contains-string? (car expr)) (contains-string? (cdr expr))))
    (else #f)))

(call-with-input-file "enum.spec" read-enums)

(for ((spec (in-list (call-with-input-file "gl.spec" read-function-specs))))
     (let ((fun-t (to-ffi-fun spec)))
       (when (contains-string? fun-t) (display "; "))
       (printf "(define-gl gl~a ~s)~%" (function-spec-name spec) fun-t)))
