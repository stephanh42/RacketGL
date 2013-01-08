#lang scribble/manual
@title{glU...}
@defproc[(glUniform1d (location exact-integer?) (x real?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1dv (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1f (location exact-integer?) (v0 flonum?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1fARB (location exact-integer?) (v0 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1f].
}
@defproc[(glUniform1fv (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1fvARB (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1fv].
}
@defproc[(glUniform1i (location exact-integer?) (v0 exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1i64NV (location exact-integer?) (x exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1i64vNV (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1iARB (location exact-integer?) (v0 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1i].
}
@defproc[(glUniform1iv (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1ivARB (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform1iv].
}
@defproc[(glUniform1ui (location exact-integer?) (v0 exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1ui64NV (location exact-integer?) (x exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1ui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform1uiEXT (location exact-integer?) (v0 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform1ui].
}
@defproc[(glUniform1uiv (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform1uivEXT (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform1uiv].
}
@defproc[(glUniform2d (location exact-integer?) (x real?) (y real?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2dv (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2f (location exact-integer?) (v0 flonum?) (v1 flonum?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2fARB (location exact-integer?) (v0 flonum?) (v1 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2f].
}
@defproc[(glUniform2fv (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2fvARB (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2fv].
}
@defproc[(glUniform2i (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2i64NV (location exact-integer?) (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2i64vNV (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2iARB (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2i].
}
@defproc[(glUniform2iv (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2ivARB (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform2iv].
}
@defproc[(glUniform2ui (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2ui64NV (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2ui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform2uiEXT (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform2ui].
}
@defproc[(glUniform2uiv (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform2uivEXT (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform2uiv].
}
@defproc[(glUniform3d (location exact-integer?) (x real?) (y real?) (z real?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3dv (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3f (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3fARB (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3f].
}
@defproc[(glUniform3fv (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3fvARB (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3fv].
}
@defproc[(glUniform3i (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3i64NV (location exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3i64vNV (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3iARB (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3i].
}
@defproc[(glUniform3iv (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3ivARB (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform3iv].
}
@defproc[(glUniform3ui (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3ui64NV (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3ui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform3uiEXT (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform3ui].
}
@defproc[(glUniform3uiv (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform3uivEXT (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform3uiv].
}
@defproc[(glUniform4d (location exact-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4dv (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4f (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?) (v3 flonum?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4fARB (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?) (v3 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4f].
}
@defproc[(glUniform4fv (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4fvARB (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4fv].
}
@defproc[(glUniform4i (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?) (v3 exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4i64NV (location exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4i64vNV (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4iARB (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?) (v3 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4i].
}
@defproc[(glUniform4iv (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4ivARB (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniform4iv].
}
@defproc[(glUniform4ui (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?) (v3 exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4ui64NV (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4ui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glUniform4uiEXT (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?) (v3 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform4ui].
}
@defproc[(glUniform4uiv (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniform.xml"]{glUniform manpage}.
}
@defproc[(glUniform4uivEXT (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glUniform4uiv].
}
@defproc[(glUniformBlockBinding (program exact-nonnegative-integer?) (uniformBlockIndex exact-nonnegative-integer?) (uniformBlockBinding exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniformBlockBinding.xml"]{glUniformBlockBinding manpage}.
}
@defproc[(glUniformBufferEXT (program exact-nonnegative-integer?) (location exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glUniformHandleui64NV (location exact-integer?) (value exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glUniformHandleui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glUniformMatrix2dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix2fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.0.
}
@defproc[(glUniformMatrix2fvARB (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix2fv].
}
@defproc[(glUniformMatrix2x3dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix2x3fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformMatrix2x4dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix2x4fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformMatrix3dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix3fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.0.
}
@defproc[(glUniformMatrix3fvARB (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix3fv].
}
@defproc[(glUniformMatrix3x2dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix3x2fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformMatrix3x4dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix3x4fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformMatrix4dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix4fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.0.
}
@defproc[(glUniformMatrix4fvARB (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUniformMatrix4fv].
}
@defproc[(glUniformMatrix4x2dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix4x2fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformMatrix4x3dv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 1.2.
}
@defproc[(glUniformMatrix4x3fv (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 2.1.
}
@defproc[(glUniformSubroutinesuiv (shadertype exact-integer?) (count exact-integer?) (indices u32vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUniformSubroutines.xml"]{glUniformSubroutines manpage}.
}
@defproc[(glUniformui64NV (location exact-integer?) (value exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glUniformui64vNV (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glUnlockArraysEXT) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/compiled_vertex_array.txt"]{@racket[GL_EXT_compiled_vertex_array]}.
}
@defproc[(glUnmapBuffer (target exact-integer?)) boolean?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUnmapBuffer.xml"]{glUnmapBuffer manpage}.
}
@defproc[(glUnmapBufferARB (target exact-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glUnmapBuffer].
}
@defproc[(glUnmapNamedBufferEXT (buffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glUnmapObjectBufferATI (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/map_object_buffer.txt"]{@racket[GL_ATI_map_object_buffer]}.
}
@defproc[(glUpdateObjectBufferATI (buffer exact-nonnegative-integer?) (offset exact-nonnegative-integer?) (size exact-integer?) (pointer gl-pointer?) (preserve exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glUseProgram (program exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUseProgram.xml"]{glUseProgram manpage}.
}
@defproc[(glUseProgramObjectARB (programObj exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glUseProgram].
}
@defproc[(glUseProgramStages (pipeline exact-nonnegative-integer?) (stages exact-nonnegative-integer?) (program exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glUseProgramStages.xml"]{glUseProgramStages manpage}.
}
@defproc[(glUseShaderProgramEXT (type exact-integer?) (program exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
