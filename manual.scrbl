#lang scribble/manual

@title{The RacketGL library}

@defmodule[(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 0)))]

The RacketGL library is an automatically-generated wrapper
around the OpenGL library.
Being automatically-generated means that it is quite complete.
It also means that the wrapping may not be as nice as a manual
wrapping.

This document contains, for reference, the calling conventions
for all the non-extension OpenGL procedures.
This information is also automatically generated.
This is only to be intended to easily look up type and number
of parameters and return values.
There is no information on what the various 
calls actually do; for that, please refer to the documentation
of the OpenGL C API.

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
