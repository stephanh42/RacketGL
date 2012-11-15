#lang scribble/manual
@title{glO...}
@defproc[(glObjectPurgeableAPPLE (objectType exact-integer?) (name exact-nonnegative-integer?) (option exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glObjectUnpurgeableAPPLE (objectType exact-integer?) (name exact-nonnegative-integer?) (option exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glOrtho (left real?) (right real?) (bottom real?) (top real?) (zNear real?) (zFar real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml"]{glOrtho manpage}.
}
