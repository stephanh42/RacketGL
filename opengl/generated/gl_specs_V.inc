#lang scribble/manual
@title{glV...}
@defproc[(glVDPAUFiniNV) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vdpau_interop.txt"]{@racket[GL_NV_vdpau_interop]}.
}
@defproc[(glVDPAUInitNV (vdpDevice gl-pointer?) (getProcAddress gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vdpau_interop.txt"]{@racket[GL_NV_vdpau_interop]}.
}
@defproc[(glValidateProgram (program exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glValidateProgram.xml"]{glValidateProgram manpage}.
}
@defproc[(glValidateProgramARB (programObj exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glValidateProgram].
}
@defproc[(glValidateProgramPipeline (pipeline exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glValidateProgramPipeline.xml"]{glValidateProgramPipeline manpage}.
}
@defproc[(glVariantArrayObjectATI (id exact-nonnegative-integer?) (type exact-integer?) (stride exact-integer?) (buffer exact-nonnegative-integer?) (offset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glVariantPointerEXT (id exact-nonnegative-integer?) (type exact-integer?) (stride exact-nonnegative-integer?) (addr gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantbvEXT (id exact-nonnegative-integer?) (addr s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantdvEXT (id exact-nonnegative-integer?) (addr f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantfvEXT (id exact-nonnegative-integer?) (addr f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantivEXT (id exact-nonnegative-integer?) (addr s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantsvEXT (id exact-nonnegative-integer?) (addr s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantubvEXT (id exact-nonnegative-integer?) (addr (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantuivEXT (id exact-nonnegative-integer?) (addr u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVariantusvEXT (id exact-nonnegative-integer?) (addr u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glVertex2d (x real?) (y real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2f (x flonum?) (y flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2hNV (x (integer-in 0 65535)) (y (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex2hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex2i (x exact-integer?) (y exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2s (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex2sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3d (x real?) (y real?) (z real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3f (x flonum?) (y flonum?) (z flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3hNV (x (integer-in 0 65535)) (y (integer-in 0 65535)) (z (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex3hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex3i (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3s (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex3sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4d (x real?) (y real?) (z real?) (w real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4f (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4hNV (x (integer-in 0 65535)) (y (integer-in 0 65535)) (z (integer-in 0 65535)) (w (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex4hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertex4i (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4s (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertex4sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml"]{glVertex manpage}.
}
@defproc[(glVertexArrayBindVertexBufferEXT (vaobj exact-nonnegative-integer?) (bindingindex exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayParameteriAPPLE (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glVertexArrayRangeAPPLE (length exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glVertexArrayRangeNV (length exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_array_range.txt"]{@racket[GL_NV_vertex_array_range]}.
}
@defproc[(glVertexArrayVertexAttribBindingEXT (vaobj exact-nonnegative-integer?) (attribindex exact-nonnegative-integer?) (bindingindex exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribFormatEXT (vaobj exact-nonnegative-integer?) (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (relativeoffset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribIFormatEXT (vaobj exact-nonnegative-integer?) (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (relativeoffset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribLFormatEXT (vaobj exact-nonnegative-integer?) (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (relativeoffset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexArrayVertexAttribLOffsetEXT (vaobj exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (offset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexArrayVertexBindingDivisorEXT (vaobj exact-nonnegative-integer?) (bindingindex exact-nonnegative-integer?) (divisor exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt"]{@racket[GL_ARB_vertex_attrib_binding]}.
}
@defproc[(glVertexAttrib1d (index exact-nonnegative-integer?) (x real?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1dARB (index exact-nonnegative-integer?) (x real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1d].
}
@defproc[(glVertexAttrib1dNV (index exact-nonnegative-integer?) (x real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1d].
}
@defproc[(glVertexAttrib1dv (index exact-nonnegative-integer?) (v real?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1dvARB (index exact-nonnegative-integer?) (v real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1dv].
}
@defproc[(glVertexAttrib1dvNV (index exact-nonnegative-integer?) (v real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1dv].
}
@defproc[(glVertexAttrib1f (index exact-nonnegative-integer?) (x flonum?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1fARB (index exact-nonnegative-integer?) (x flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1f].
}
@defproc[(glVertexAttrib1fNV (index exact-nonnegative-integer?) (x flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1f].
}
@defproc[(glVertexAttrib1fv (index exact-nonnegative-integer?) (v flonum?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1fvARB (index exact-nonnegative-integer?) (v flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1fv].
}
@defproc[(glVertexAttrib1fvNV (index exact-nonnegative-integer?) (v flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1fv].
}
@defproc[(glVertexAttrib1hNV (index exact-nonnegative-integer?) (x (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib1hvNV (index exact-nonnegative-integer?) (v (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib1s (index exact-nonnegative-integer?) (x (integer-in -32768 32767))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1sARB (index exact-nonnegative-integer?) (x (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1s].
}
@defproc[(glVertexAttrib1sNV (index exact-nonnegative-integer?) (x (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1s].
}
@defproc[(glVertexAttrib1sv (index exact-nonnegative-integer?) (v (integer-in -32768 32767))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib1svARB (index exact-nonnegative-integer?) (v (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib1sv].
}
@defproc[(glVertexAttrib1svNV (index exact-nonnegative-integer?) (v (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib1sv].
}
@defproc[(glVertexAttrib2d (index exact-nonnegative-integer?) (x real?) (y real?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2dARB (index exact-nonnegative-integer?) (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2d].
}
@defproc[(glVertexAttrib2dNV (index exact-nonnegative-integer?) (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2d].
}
@defproc[(glVertexAttrib2dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2dvARB (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2dv].
}
@defproc[(glVertexAttrib2dvNV (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2dv].
}
@defproc[(glVertexAttrib2f (index exact-nonnegative-integer?) (x flonum?) (y flonum?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2fARB (index exact-nonnegative-integer?) (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2f].
}
@defproc[(glVertexAttrib2fNV (index exact-nonnegative-integer?) (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2f].
}
@defproc[(glVertexAttrib2fv (index exact-nonnegative-integer?) (v f32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2fvARB (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2fv].
}
@defproc[(glVertexAttrib2fvNV (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2fv].
}
@defproc[(glVertexAttrib2hNV (index exact-nonnegative-integer?) (x (integer-in 0 65535)) (y (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib2hvNV (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib2s (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2sARB (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2s].
}
@defproc[(glVertexAttrib2sNV (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2s].
}
@defproc[(glVertexAttrib2sv (index exact-nonnegative-integer?) (v s16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib2svARB (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib2sv].
}
@defproc[(glVertexAttrib2svNV (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib2sv].
}
@defproc[(glVertexAttrib3d (index exact-nonnegative-integer?) (x real?) (y real?) (z real?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3dARB (index exact-nonnegative-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3d].
}
@defproc[(glVertexAttrib3dNV (index exact-nonnegative-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3d].
}
@defproc[(glVertexAttrib3dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3dvARB (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3dv].
}
@defproc[(glVertexAttrib3dvNV (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3dv].
}
@defproc[(glVertexAttrib3f (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3fARB (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3f].
}
@defproc[(glVertexAttrib3fNV (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3f].
}
@defproc[(glVertexAttrib3fv (index exact-nonnegative-integer?) (v f32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3fvARB (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3fv].
}
@defproc[(glVertexAttrib3fvNV (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3fv].
}
@defproc[(glVertexAttrib3hNV (index exact-nonnegative-integer?) (x (integer-in 0 65535)) (y (integer-in 0 65535)) (z (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib3hvNV (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib3s (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3sARB (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3s].
}
@defproc[(glVertexAttrib3sNV (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3s].
}
@defproc[(glVertexAttrib3sv (index exact-nonnegative-integer?) (v s16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib3svARB (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib3sv].
}
@defproc[(glVertexAttrib3svNV (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib3sv].
}
@defproc[(glVertexAttrib4Nbv (index exact-nonnegative-integer?) (v s8vector?)) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NbvARB (index exact-nonnegative-integer?) (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nbv].
}
@defproc[(glVertexAttrib4Niv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NivARB (index exact-nonnegative-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Niv].
}
@defproc[(glVertexAttrib4Nsv (index exact-nonnegative-integer?) (v s16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NsvARB (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nsv].
}
@defproc[(glVertexAttrib4Nub (index exact-nonnegative-integer?) (x (integer-in 0 255)) (y (integer-in 0 255)) (z (integer-in 0 255)) (w (integer-in 0 255))) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NubARB (index exact-nonnegative-integer?) (x (integer-in 0 255)) (y (integer-in 0 255)) (z (integer-in 0 255)) (w (integer-in 0 255))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nub].
}
@defproc[(glVertexAttrib4Nubv (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NubvARB (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nubv].
}
@defproc[(glVertexAttrib4Nuiv (index exact-nonnegative-integer?) (v u32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NuivARB (index exact-nonnegative-integer?) (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nuiv].
}
@defproc[(glVertexAttrib4Nusv (index exact-nonnegative-integer?) (v u16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttrib4NusvARB (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4Nusv].
}
@defproc[(glVertexAttrib4bv (index exact-nonnegative-integer?) (v s8vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4bvARB (index exact-nonnegative-integer?) (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4bv].
}
@defproc[(glVertexAttrib4d (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4dARB (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4d].
}
@defproc[(glVertexAttrib4dNV (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4d].
}
@defproc[(glVertexAttrib4dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4dvARB (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4dv].
}
@defproc[(glVertexAttrib4dvNV (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4dv].
}
@defproc[(glVertexAttrib4f (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4fARB (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4f].
}
@defproc[(glVertexAttrib4fNV (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4f].
}
@defproc[(glVertexAttrib4fv (index exact-nonnegative-integer?) (v f32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4fvARB (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4fv].
}
@defproc[(glVertexAttrib4fvNV (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4fv].
}
@defproc[(glVertexAttrib4hNV (index exact-nonnegative-integer?) (x (integer-in 0 65535)) (y (integer-in 0 65535)) (z (integer-in 0 65535)) (w (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib4hvNV (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttrib4iv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4ivARB (index exact-nonnegative-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4iv].
}
@defproc[(glVertexAttrib4s (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4sARB (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4s].
}
@defproc[(glVertexAttrib4sNV (index exact-nonnegative-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4s].
}
@defproc[(glVertexAttrib4sv (index exact-nonnegative-integer?) (v s16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4svARB (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4sv].
}
@defproc[(glVertexAttrib4svNV (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4sv].
}
@defproc[(glVertexAttrib4ubNV (index exact-nonnegative-integer?) (x (integer-in 0 255)) (y (integer-in 0 255)) (z (integer-in 0 255)) (w (integer-in 0 255))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4Nub].
}
@defproc[(glVertexAttrib4ubv (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4ubvARB (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4ubv].
}
@defproc[(glVertexAttrib4ubvNV (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glVertexAttrib4Nubv].
}
@defproc[(glVertexAttrib4uiv (index exact-nonnegative-integer?) (v u32vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4uivARB (index exact-nonnegative-integer?) (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4uiv].
}
@defproc[(glVertexAttrib4usv (index exact-nonnegative-integer?) (v u16vector?)) any]{
Version 2.0.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttrib.xml"]{glVertexAttrib manpage}.
}
@defproc[(glVertexAttrib4usvARB (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttrib4usv].
}
@defproc[(glVertexAttribArrayObjectATI (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (stride exact-integer?) (buffer exact-nonnegative-integer?) (offset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glVertexAttribBinding (attribindex exact-nonnegative-integer?) (bindingindex exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glVertexAttribDivisor (index exact-nonnegative-integer?) (divisor exact-nonnegative-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttribDivisor.xml"]{glVertexAttribDivisor manpage}.
}
@defproc[(glVertexAttribDivisorARB (index exact-nonnegative-integer?) (divisor exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/instanced_arrays.txt"]{@racket[GL_ARB_instanced_arrays]}.
}
@defproc[(glVertexAttribFormat (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (relativeoffset exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glVertexAttribFormatNV (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexAttribI1i (index exact-nonnegative-integer?) (x exact-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1iEXT (index exact-nonnegative-integer?) (x exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1i].
}
@defproc[(glVertexAttribI1iv (index exact-nonnegative-integer?) (v exact-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1ivEXT (index exact-nonnegative-integer?) (v exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1iv].
}
@defproc[(glVertexAttribI1ui (index exact-nonnegative-integer?) (x exact-nonnegative-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1uiEXT (index exact-nonnegative-integer?) (x exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1ui].
}
@defproc[(glVertexAttribI1uiv (index exact-nonnegative-integer?) (v exact-nonnegative-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI1uivEXT (index exact-nonnegative-integer?) (v exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI1uiv].
}
@defproc[(glVertexAttribI2i (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2iEXT (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2i].
}
@defproc[(glVertexAttribI2iv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2ivEXT (index exact-nonnegative-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2iv].
}
@defproc[(glVertexAttribI2ui (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2uiEXT (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2ui].
}
@defproc[(glVertexAttribI2uiv (index exact-nonnegative-integer?) (v u32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI2uivEXT (index exact-nonnegative-integer?) (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI2uiv].
}
@defproc[(glVertexAttribI3i (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3iEXT (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3i].
}
@defproc[(glVertexAttribI3iv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3ivEXT (index exact-nonnegative-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3iv].
}
@defproc[(glVertexAttribI3ui (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3uiEXT (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3ui].
}
@defproc[(glVertexAttribI3uiv (index exact-nonnegative-integer?) (v u32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI3uivEXT (index exact-nonnegative-integer?) (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI3uiv].
}
@defproc[(glVertexAttribI4bv (index exact-nonnegative-integer?) (v s8vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4bvEXT (index exact-nonnegative-integer?) (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4bv].
}
@defproc[(glVertexAttribI4i (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4iEXT (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4i].
}
@defproc[(glVertexAttribI4iv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4ivEXT (index exact-nonnegative-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4iv].
}
@defproc[(glVertexAttribI4sv (index exact-nonnegative-integer?) (v s16vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4svEXT (index exact-nonnegative-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4sv].
}
@defproc[(glVertexAttribI4ubv (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4ubvEXT (index exact-nonnegative-integer?) (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4ubv].
}
@defproc[(glVertexAttribI4ui (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4uiEXT (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4ui].
}
@defproc[(glVertexAttribI4uiv (index exact-nonnegative-integer?) (v u32vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4uivEXT (index exact-nonnegative-integer?) (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4uiv].
}
@defproc[(glVertexAttribI4usv (index exact-nonnegative-integer?) (v u16vector?)) any]{
Version 3.0.
Deprecated in version 3.1.
}
@defproc[(glVertexAttribI4usvEXT (index exact-nonnegative-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribI4usv].
}
@defproc[(glVertexAttribIFormat (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (relativeoffset exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glVertexAttribIFormatNV (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexAttribIPointer (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 3.0.
}
@defproc[(glVertexAttribIPointerEXT (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glVertexAttribIPointer].
}
@defproc[(glVertexAttribL1d (index exact-nonnegative-integer?) (x real?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL1dEXT (index exact-nonnegative-integer?) (x real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL1dv (index exact-nonnegative-integer?) (v real?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL1dvEXT (index exact-nonnegative-integer?) (v real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL1i64NV (index exact-nonnegative-integer?) (x exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1i64vNV (index exact-nonnegative-integer?) (v exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1ui64NV (index exact-nonnegative-integer?) (x exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL1ui64vNV (index exact-nonnegative-integer?) (v exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2d (index exact-nonnegative-integer?) (x real?) (y real?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL2dEXT (index exact-nonnegative-integer?) (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL2dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL2dvEXT (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL2i64NV (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2i64vNV (index exact-nonnegative-integer?) (v s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2ui64NV (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL2ui64vNV (index exact-nonnegative-integer?) (v u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3d (index exact-nonnegative-integer?) (x real?) (y real?) (z real?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL3dEXT (index exact-nonnegative-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL3dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL3dvEXT (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL3i64NV (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3i64vNV (index exact-nonnegative-integer?) (v s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3ui64NV (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL3ui64vNV (index exact-nonnegative-integer?) (v u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4d (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL4dEXT (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL4dv (index exact-nonnegative-integer?) (v f64vector?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribL4dvEXT (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribL4i64NV (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4i64vNV (index exact-nonnegative-integer?) (v s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4ui64NV (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribL4ui64vNV (index exact-nonnegative-integer?) (v u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribLFormat (attribindex exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (relativeoffset exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glVertexAttribLFormatNV (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glVertexAttribLPointer (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 4.1.
}
@defproc[(glVertexAttribLPointerEXT (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glVertexAttribP1ui (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP1uiv (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP2ui (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP2uiv (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP3ui (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP3uiv (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP4ui (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribP4uiv (index exact-nonnegative-integer?) (type exact-integer?) (normalized boolean?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexAttribPointer (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glVertexAttribPointer.xml"]{glVertexAttribPointer manpage}.
}
@defproc[(glVertexAttribPointerARB (index exact-nonnegative-integer?) (size exact-integer?) (type exact-integer?) (normalized boolean?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glVertexAttribPointer].
}
@defproc[(glVertexAttribPointerNV (index exact-nonnegative-integer?) (fsize exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1dvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1fvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs1hvNV (index exact-nonnegative-integer?) (n exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs1svNV (index exact-nonnegative-integer?) (count exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2dvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2fvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs2hvNV (index exact-nonnegative-integer?) (n exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs2svNV (index exact-nonnegative-integer?) (count exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3dvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3fvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs3hvNV (index exact-nonnegative-integer?) (n exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs3svNV (index exact-nonnegative-integer?) (count exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4dvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4fvNV (index exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4hvNV (index exact-nonnegative-integer?) (n exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexAttribs4svNV (index exact-nonnegative-integer?) (count exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexAttribs4ubvNV (index exact-nonnegative-integer?) (count exact-integer?) (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glVertexBindingDivisor (bindingindex exact-nonnegative-integer?) (divisor exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glVertexBlendARB (count exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_blend.txt"]{@racket[GL_ARB_vertex_blend]}.
}
@defproc[(glVertexBlendEnvfATI (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexBlendEnviATI (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexFormatNV (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glVertexP2ui (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexP2uiv (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexP3ui (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexP3uiv (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexP4ui (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexP4uiv (type exact-integer?) (value exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glVertexPointer (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glVertexPointer.xml"]{glVertexPointer manpage}.
}
@defproc[(glVertexPointerEXT (size exact-integer?) (type exact-integer?) (stride exact-integer?) (count exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glVertexPointerListIBM (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glVertexPointervINTEL (size exact-integer?) (type exact-integer?) (pointer (vectorof gl-pointer?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glVertexStream1dATI (stream exact-integer?) (x real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1dvATI (stream exact-integer?) (coords real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1fATI (stream exact-integer?) (x flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1fvATI (stream exact-integer?) (coords flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1iATI (stream exact-integer?) (x exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1ivATI (stream exact-integer?) (coords exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1sATI (stream exact-integer?) (x (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream1svATI (stream exact-integer?) (coords (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2dATI (stream exact-integer?) (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2dvATI (stream exact-integer?) (coords f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2fATI (stream exact-integer?) (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2fvATI (stream exact-integer?) (coords f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2iATI (stream exact-integer?) (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2ivATI (stream exact-integer?) (coords s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2sATI (stream exact-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream2svATI (stream exact-integer?) (coords s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3dATI (stream exact-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3dvATI (stream exact-integer?) (coords f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3fATI (stream exact-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3fvATI (stream exact-integer?) (coords f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3iATI (stream exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3ivATI (stream exact-integer?) (coords s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3sATI (stream exact-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream3svATI (stream exact-integer?) (coords s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4dATI (stream exact-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4dvATI (stream exact-integer?) (coords f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4fATI (stream exact-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4fvATI (stream exact-integer?) (coords f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4iATI (stream exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4ivATI (stream exact-integer?) (coords s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4sATI (stream exact-integer?) (x (integer-in -32768 32767)) (y (integer-in -32768 32767)) (z (integer-in -32768 32767)) (w (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexStream4svATI (stream exact-integer?) (coords s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glVertexWeightPointerEXT (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeightfEXT (weight flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeightfvEXT (weight flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_weighting.txt"]{@racket[GL_EXT_vertex_weighting]}.
}
@defproc[(glVertexWeighthNV (weight (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVertexWeighthvNV (weight (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glVideoCaptureNV (video_capture_slot exact-nonnegative-integer?)) (values exact-integer? exact-nonnegative-integer? exact-nonnegative-integer?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterdvNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?) (params f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterfvNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glVideoCaptureStreamParameterivNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glViewport (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewport.xml"]{glViewport manpage}.
}
@defproc[(glViewportArrayv (first exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Version 4.1.
}
@defproc[(glViewportIndexedf (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (w flonum?) (h flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewportIndexed.xml"]{glViewportIndexed manpage}.
}
@defproc[(glViewportIndexedfv (index exact-nonnegative-integer?) (v f32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glViewportIndexed.xml"]{glViewportIndexed manpage}.
}
