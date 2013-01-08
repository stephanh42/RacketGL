#lang scribble/manual
@title{glW...}
@defproc[(glWaitSync (sync GLsync?) (flags exact-nonnegative-integer?) (timeout exact-nonnegative-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glWaitSync.xml"]{glWaitSync manpage}.
}
@defproc[(glWeightPathsNV (resultPath exact-nonnegative-integer?) (numPaths exact-integer?) (paths u32vector?) (weights f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glWeightPointerARB (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightbvARB (size exact-integer?) (weights s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightdvARB (size exact-integer?) (weights f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightfvARB (size exact-integer?) (weights f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightivARB (size exact-integer?) (weights s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightsvARB (size exact-integer?) (weights s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightubvARB (size exact-integer?) (weights (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightuivARB (size exact-integer?) (weights u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWeightusvARB (size exact-integer?) (weights u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glWindowPos2d (x real?) (y real?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2dARB (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2d].
}
@defproc[(glWindowPos2dMESA (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2dARB].
}
@defproc[(glWindowPos2dv (v f64vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2dvARB (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2dv].
}
@defproc[(glWindowPos2dvMESA (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2dvARB].
}
@defproc[(glWindowPos2f (x flonum?) (y flonum?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2fARB (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2f].
}
@defproc[(glWindowPos2fMESA (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2fARB].
}
@defproc[(glWindowPos2fv (v f32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2fvARB (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2fv].
}
@defproc[(glWindowPos2fvMESA (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2fvARB].
}
@defproc[(glWindowPos2i (x exact-integer?) (y exact-integer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2iARB (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2i].
}
@defproc[(glWindowPos2iMESA (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2iARB].
}
@defproc[(glWindowPos2iv (v s32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2ivARB (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2iv].
}
@defproc[(glWindowPos2ivMESA (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2ivARB].
}
@defproc[(glWindowPos2s (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2sARB (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2s].
}
@defproc[(glWindowPos2sMESA (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2sARB].
}
@defproc[(glWindowPos2sv (v s16vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos2svARB (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos2sv].
}
@defproc[(glWindowPos2svMESA (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos2svARB].
}
@defproc[(glWindowPos3d (x real?) (y real?) (z real?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3dARB (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3d].
}
@defproc[(glWindowPos3dMESA (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3dARB].
}
@defproc[(glWindowPos3dv (v f64vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3dvARB (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3dv].
}
@defproc[(glWindowPos3dvMESA (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3dvARB].
}
@defproc[(glWindowPos3f (x flonum?) (y flonum?) (z flonum?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3fARB (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3f].
}
@defproc[(glWindowPos3fMESA (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3fARB].
}
@defproc[(glWindowPos3fv (v f32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3fvARB (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3fv].
}
@defproc[(glWindowPos3fvMESA (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3fvARB].
}
@defproc[(glWindowPos3i (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3iARB (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3i].
}
@defproc[(glWindowPos3iMESA (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3iARB].
}
@defproc[(glWindowPos3iv (v s32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3ivARB (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3iv].
}
@defproc[(glWindowPos3ivMESA (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3ivARB].
}
@defproc[(glWindowPos3s (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3sARB (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3s].
}
@defproc[(glWindowPos3sMESA (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3sARB].
}
@defproc[(glWindowPos3sv (v s16vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml"]{glWindowPos manpage}.
}
@defproc[(glWindowPos3svARB (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/window_pos.txt"]{@racket[GL_ARB_window_pos]}.

Alias of @racket[glWindowPos3sv].
}
@defproc[(glWindowPos3svMESA (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.

Alias of @racket[glWindowPos3svARB].
}
@defproc[(glWindowPos4dMESA (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4dvMESA (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4fMESA (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4fvMESA (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4iMESA (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4ivMESA (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4sMESA (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWindowPos4svMESA (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/MESA/window_pos.txt"]{@racket[GL_MESA_window_pos]}.
}
@defproc[(glWriteMaskEXT (res exact-nonnegative-integer?) (in exact-nonnegative-integer?) (outX exact-integer?) (outY exact-integer?) (outZ exact-integer?) (outW exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
