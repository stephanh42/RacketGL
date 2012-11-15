#lang scribble/manual

@(require planet/scribble)

@title{The RacketGL library}

@(defmodule/this-package rgl)

@(require (for-label racket/gui/base))
@(require (for-label ffi/vector))

The RacketGL library is an automatically-generated wrapper
around the OpenGL library.
Being automatically-generated means that it is quite complete.
It also means that the wrapping may not be as nice as a manual
wrapping.

Please note that these procedures are all very much @bold{unsafe};
in particular, if you call any of them while no OpenGL context
is active, you are almost certain to crash Racket.
An OpenGL context is typically establised by using the
@(xmethod canvas% with-gl-context) method.
If you are running from within DrRacket, I would recommend
to get used to saving your OpenGL program before running it.

This document contains, for reference, the calling conventions
for all the non-extension OpenGL procedures.
This information is also automatically generated.
This is only intended to easily look up type and number
of parameters and return values.
There is no information on what the various 
calls actually do; for that, please refer to the 
@hyperlink["http://www.opengl.org/documentation/"]{documentation of the OpenGL C API}.

The wrapper procedures automatically check for OpenGL errors after any call,
except between @racket[glBegin] and @racket[glEnd] where this is disallowed.
You don't have to call @racket[glGetError] yourself.

@include-section["generated/gl_specs1.0.scrbl"]
@include-section["generated/gl_specs1.1.scrbl"]
@include-section["generated/gl_specs1.2.scrbl"]
@include-section["generated/gl_specs1.3.scrbl"]
@include-section["generated/gl_specs1.4.scrbl"]
@include-section["generated/gl_specs1.5.scrbl"]
@include-section["generated/gl_specs2.0.scrbl"]
@include-section["generated/gl_specs2.1.scrbl"]
@include-section["generated/gl_specs3.0.scrbl"]
@include-section["generated/gl_specs3.1.scrbl"]
@include-section["generated/gl_specs3.2.scrbl"]
@include-section["generated/gl_specs4.1.scrbl"]

@section{Utility functions for homogenous vectors}

These functions are not part of the OpenGL API but are provided to make
working with @racket[glVertexPointer] and similar procedures easier.

@defproc[(gl-vector? (v any/c)) boolean?]{
  Returns @racket[#t] if @racket[v] belongs to one of
  the homogenous vector types which can be used with OpenGL, @racket[#f] otherwise.
  These vector types are:
  @racket[u8vector], @racket[s8vector], @racket[u16vector], @racket[s16vector],
  @racket[u32vector], @racket[s32vector], @racket[f32vector] and @racket[f64vector].
}

@defproc[(gl-vector->type (v gl-vector?)) exact-integer?]{
  Determine the OpenGL type of @racket[v].
  This returns a numerical value such as @racket[GL_SHORT], @racket[GL_FLOAT], etc., which
  can be passed into @racket[glVertexPointer] and similar procedures.
}

@defproc[(gl-vector->cpointer (v gl-vector?)) cpointer?]{
  Get a C pointer to @racket[v].
}

@defproc[(gl-vector->length (v gl-vector?)) exact-nonnegative-integer?]{
  Get the length of @racket[v].
}

@defproc[(gl-vector->type/cpointer (v gl-vector?)) (values exact-integer? cpointer?)]{
  Get the OpenGL type and C pointer of @racket[v].
  This is slightly more efficient than getting them each individually.
}

@defproc[(gl-vector->type/cpointer/length (v gl-vector?)) (values exact-integer? cpointer? exact-nonnegative-integer?)]{
  Get the OpenGL type, C pointer and length of @racket[v].
  This is slightly more efficient than getting them each individually.
}


@section{Additional utility procedures}

These procedures can be used to check the OpenGL version and supported extensions.
Note that these, too, should only be called when an OpenGL context is active!

@defproc[(gl-version) (listof exact-integer?)]{
Returns the OpenGL version, as a list of exact integers.
For example, version 3.1.2 would return a list (3 1 2).
}

@defproc[(gl-extensions) set-eq?]{
  Returns the supported OpenGL extensions, as a set of symbols.
}

@defproc[(gl-has-extension? (extension symbol?)) boolean?]{
  Checks whether the given extension is supported.
}

@defproc[(gl-version-at-least? (version (listof exact-integer?))) boolean?]{
  Checks if the OpenGL version is at least the given version.
}
