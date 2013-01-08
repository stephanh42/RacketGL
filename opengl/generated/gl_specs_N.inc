#lang scribble/manual
@title{glN...}
@defproc[(glNamedBufferDataEXT (buffer exact-nonnegative-integer?) (size exact-integer?) (data gl-pointer?) (usage exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedBufferSubDataEXT (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedCopyBufferSubDataEXT (readBuffer exact-nonnegative-integer?) (writeBuffer exact-nonnegative-integer?) (readOffset exact-integer?) (writeOffset exact-integer?) (size exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferParameteriEXT (framebuffer exact-nonnegative-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt"]{@racket[GL_ARB_framebuffer_no_attachments]}.
}
@defproc[(glNamedFramebufferRenderbufferEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (renderbuffertarget exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture1DEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture2DEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTexture3DEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (zoffset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureFaceEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (face exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedFramebufferTextureLayerEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layer exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4dEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4dvEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (params f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4fEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameter4fvEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4iEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4ivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4uiEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameterI4uivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParameters4fvEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParametersI4ivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramLocalParametersI4uivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedProgramStringEXT (program exact-nonnegative-integer?) (target exact-integer?) (format exact-integer?) (len exact-integer?) (string gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageEXT (renderbuffer exact-nonnegative-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageMultisampleCoverageEXT (renderbuffer exact-nonnegative-integer?) (coverageSamples exact-integer?) (colorSamples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedRenderbufferStorageMultisampleEXT (renderbuffer exact-nonnegative-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glNamedStringARB (type exact-integer?) (namelen exact-integer?) (name (or/c string? bytes?)) (stringlen exact-integer?) (string (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glNewList (list exact-nonnegative-integer?) (mode exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml"]{glNewList manpage}.
}
@defproc[(glNewObjectBufferATI (size exact-integer?) (pointer gl-pointer?) (usage exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glNormal3b (nx (integer-in -128 127)) (ny (integer-in -128 127)) (nz (integer-in -128 127))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3bv (v s8vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3d (nx real?) (ny real?) (nz real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3f (nx flonum?) (ny flonum?) (nz flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3fVertex3fSUN (nx flonum?) (ny flonum?) (nz flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glNormal3fVertex3fvSUN (n f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glNormal3fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3hNV (nx (integer-in 0 65535)) (ny (integer-in 0 65535)) (nz (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glNormal3hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glNormal3i (nx exact-integer?) (ny exact-integer?) (nz exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3s (nx (integer-in -32768 32767)) (ny (integer-in -32768 32767)) (nz (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormal3sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml"]{glNormal manpage}.
}
@defproc[(glNormalFormatNV (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glNormalP3ui (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glNormalP3uiv (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glNormalPointer (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glNormalPointer.xml"]{glNormalPointer manpage}.
}
@defproc[(glNormalPointerEXT (type exact-integer?) (stride exact-integer?) (count exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glNormalPointerListIBM (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glNormalPointervINTEL (type exact-integer?) (pointer (vectorof gl-pointer?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glNormalStream3bATI (stream exact-integer?) (nx (integer-in -128 127)) (ny (integer-in -128 127)) (nz (integer-in -128 127))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3bvATI (stream exact-integer?) (coords s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3dATI (stream exact-integer?) (nx real?) (ny real?) (nz real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3dvATI (stream exact-integer?) (coords f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3fATI (stream exact-integer?) (nx flonum?) (ny flonum?) (nz flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3fvATI (stream exact-integer?) (coords f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3iATI (stream exact-integer?) (nx exact-integer?) (ny exact-integer?) (nz exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3ivATI (stream exact-integer?) (coords s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3sATI (stream exact-integer?) (nx (integer-in -32768 32767)) (ny (integer-in -32768 32767)) (nz (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glNormalStream3svATI (stream exact-integer?) (coords s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
