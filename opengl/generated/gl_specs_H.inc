#lang scribble/manual
@title{glH...}
@defproc[(glHint (target exact-integer?) (mode exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glHint.xml"]{glHint manpage}.
}
@defproc[(glHintPGI (target exact-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/PGI/misc_hints.txt"]{@racket[GL_PGI_misc_hints]}.
}
@defproc[(glHistogram (target exact-integer?) (width exact-integer?) (internalformat exact-integer?) (sink boolean?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glHistogram.xml"]{glHistogram manpage}.
}
@defproc[(glHistogramEXT (target exact-integer?) (width exact-integer?) (internalformat exact-integer?) (sink boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.

Alias of @racket[glHistogram].
}
