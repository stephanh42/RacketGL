#lang scribble/manual
@title{glC...}
@defproc[(glCallList (list exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCallList.xml"]{glCallList manpage}.
}
@defproc[(glCallLists (n exact-integer?) (type exact-integer?) (lists gl-pointer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCallLists.xml"]{glCallLists manpage}.
}
@defproc[(glCheckFramebufferStatus (target exact-integer?)) exact-integer?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCheckFramebufferStatus.xml"]{glCheckFramebufferStatus manpage}.
}
@defproc[(glCheckFramebufferStatusEXT (target exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glCheckFramebufferStatus].
}
@defproc[(glCheckNamedFramebufferStatusEXT (framebuffer exact-nonnegative-integer?) (target exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glClampColor (target exact-integer?) (clamp exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClampColor.xml"]{glClampColor manpage}.
}
@defproc[(glClampColorARB (target exact-integer?) (clamp exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/color_buffer_float.txt"]{@racket[GL_ARB_color_buffer_float]}.

Alias of @racket[glClampColor].
}
@defproc[(glClear (mask exact-nonnegative-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClear.xml"]{glClear manpage}.
}
@defproc[(glClearAccum (red flonum?) (green flonum?) (blue flonum?) (alpha flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClearAccum.xml"]{glClearAccum manpage}.
}
@defproc[(glClearBufferData (target exact-integer?) (internalformat exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Version 4.3.
}
@defproc[(glClearBufferSubData (target exact-integer?) (internalformat exact-integer?) (offset exact-integer?) (size exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Version 4.3.
}
@defproc[(glClearBufferfi (buffer exact-integer?) (drawbuffer exact-integer?) (depth flonum?) (stencil exact-integer?)) any]{
Version 3.0.
}
@defproc[(glClearBufferfv (buffer exact-integer?) (drawbuffer exact-integer?) (value f32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearBufferiv (buffer exact-integer?) (drawbuffer exact-integer?) (value s32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearBufferuiv (buffer exact-integer?) (drawbuffer exact-integer?) (value u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearBuffer.xml"]{glClearBuffer manpage}.
}
@defproc[(glClearColor (red flonum?) (green flonum?) (blue flonum?) (alpha flonum?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearColor.xml"]{glClearColor manpage}.
}
@defproc[(glClearColorIiEXT (red exact-integer?) (green exact-integer?) (blue exact-integer?) (alpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.
}
@defproc[(glClearColorIuiEXT (red exact-nonnegative-integer?) (green exact-nonnegative-integer?) (blue exact-nonnegative-integer?) (alpha exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.
}
@defproc[(glClearDepth (depth real?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearDepth.xml"]{glClearDepth manpage}.
}
@defproc[(glClearDepthdNV (depth real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/depth_buffer_float.txt"]{@racket[GL_NV_depth_buffer_float]}.
}
@defproc[(glClearDepthf (d flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearDepth.xml"]{glClearDepth manpage}.
}
@defproc[(glClearIndex (c flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClearIndex.xml"]{glClearIndex manpage}.
}
@defproc[(glClearNamedBufferDataEXT (buffer exact-nonnegative-integer?) (internalformat exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt"]{@racket[GL_ARB_clear_buffer_object]}.
}
@defproc[(glClearNamedBufferSubDataEXT (buffer exact-nonnegative-integer?) (internalformat exact-integer?) (offset exact-integer?) (size exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt"]{@racket[GL_ARB_clear_buffer_object]}.
}
@defproc[(glClearStencil (s exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClearStencil.xml"]{glClearStencil manpage}.
}
@defproc[(glClientActiveTexture (texture exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClientActiveTexture.xml"]{glClientActiveTexture manpage}.
}
@defproc[(glClientActiveTextureARB (texture exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glClientActiveTexture].
}
@defproc[(glClientActiveVertexStreamATI (stream exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_streams.txt"]{@racket[GL_ATI_vertex_streams]}.
}
@defproc[(glClientAttribDefaultEXT (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glClientWaitSync (sync GLsync?) (flags exact-nonnegative-integer?) (timeout exact-nonnegative-integer?)) exact-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glClientWaitSync.xml"]{glClientWaitSync manpage}.
}
@defproc[(glClipPlane (plane exact-integer?) (equation f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glClipPlane.xml"]{glClipPlane manpage}.
}
@defproc[(glColor3b (red (integer-in -128 127)) (green (integer-in -128 127)) (blue (integer-in -128 127))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3bv (v s8vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3d (red real?) (green real?) (blue real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3f (red flonum?) (green flonum?) (blue flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3fVertex3fSUN (r flonum?) (g flonum?) (b flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor3fVertex3fvSUN (c f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor3fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3hNV (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor3hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor3i (red exact-integer?) (green exact-integer?) (blue exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3s (red (integer-in -32768 32767)) (green (integer-in -32768 32767)) (blue (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ub (red (integer-in 0 255)) (green (integer-in 0 255)) (blue (integer-in 0 255))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ubv (v (or/c string? bytes?))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3ui (red exact-nonnegative-integer?) (green exact-nonnegative-integer?) (blue exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3uiv (v u32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3us (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor3usv (v u16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4b (red (integer-in -128 127)) (green (integer-in -128 127)) (blue (integer-in -128 127)) (alpha (integer-in -128 127))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4bv (v s8vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4d (red real?) (green real?) (blue real?) (alpha real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4f (red flonum?) (green flonum?) (blue flonum?) (alpha flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4fNormal3fVertex3fSUN (r flonum?) (g flonum?) (b flonum?) (a flonum?) (nx flonum?) (ny flonum?) (nz flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4fNormal3fVertex3fvSUN (c f32vector?) (n f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4hNV (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535)) (alpha (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor4hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glColor4i (red exact-integer?) (green exact-integer?) (blue exact-integer?) (alpha exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4s (red (integer-in -32768 32767)) (green (integer-in -32768 32767)) (blue (integer-in -32768 32767)) (alpha (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ub (red (integer-in 0 255)) (green (integer-in 0 255)) (blue (integer-in 0 255)) (alpha (integer-in 0 255))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ubVertex2fSUN (r (integer-in 0 255)) (g (integer-in 0 255)) (b (integer-in 0 255)) (a (integer-in 0 255)) (x flonum?) (y flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex2fvSUN (c (or/c string? bytes?)) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex3fSUN (r (integer-in 0 255)) (g (integer-in 0 255)) (b (integer-in 0 255)) (a (integer-in 0 255)) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubVertex3fvSUN (c (or/c string? bytes?)) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glColor4ubv (v (or/c string? bytes?))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4ui (red exact-nonnegative-integer?) (green exact-nonnegative-integer?) (blue exact-nonnegative-integer?) (alpha exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4uiv (v u32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4us (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535)) (alpha (integer-in 0 65535))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColor4usv (v u16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml"]{glColor manpage}.
}
@defproc[(glColorFormatNV (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glColorFragmentOp1ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMask exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorFragmentOp2ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMask exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?) (arg2Rep exact-nonnegative-integer?) (arg2Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorFragmentOp3ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMask exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?) (arg2Rep exact-nonnegative-integer?) (arg2Mod exact-nonnegative-integer?) (arg3 exact-nonnegative-integer?) (arg3Rep exact-nonnegative-integer?) (arg3Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glColorMask (red boolean?) (green boolean?) (blue boolean?) (alpha boolean?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glColorMask.xml"]{glColorMask manpage}.
}
@defproc[(glColorMaskIndexedEXT (index exact-nonnegative-integer?) (r boolean?) (g boolean?) (b boolean?) (a boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glColorMaski].
}
@defproc[(glColorMaski (index exact-nonnegative-integer?) (r boolean?) (g boolean?) (b boolean?) (a boolean?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glColorMask.xml"]{glColorMask manpage}.
}
@defproc[(glColorMaterial (face exact-integer?) (mode exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorMaterial.xml"]{glColorMaterial manpage}.
}
@defproc[(glColorP3ui (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glColorP3uiv (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glColorP4ui (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glColorP4uiv (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glColorPointer (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorPointer.xml"]{glColorPointer manpage}.
}
@defproc[(glColorPointerEXT (size exact-integer?) (type exact-integer?) (stride exact-integer?) (count exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glColorPointerListIBM (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glColorPointervINTEL (size exact-integer?) (type exact-integer?) (pointer (vectorof gl-pointer?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glColorSubTable (target exact-integer?) (start exact-integer?) (count exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorSubTable.xml"]{glColorSubTable manpage}.
}
@defproc[(glColorSubTableEXT (target exact-integer?) (start exact-integer?) (count exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/color_subtable.txt"]{@racket[GL_EXT_color_subtable]}.

Alias of @racket[glColorSubTable].
}
@defproc[(glColorTable (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (table gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTable.xml"]{glColorTable manpage}.
}
@defproc[(glColorTableEXT (target exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (table gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.

Alias of @racket[glColorTable].
}
@defproc[(glColorTableParameterfv (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml"]{glColorTableParameter manpage}.
}
@defproc[(glColorTableParameterfvSGI (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTableParameterfv].
}
@defproc[(glColorTableParameteriv (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml"]{glColorTableParameter manpage}.
}
@defproc[(glColorTableParameterivSGI (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTableParameteriv].
}
@defproc[(glColorTableSGI (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (table gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glColorTable].
}
@defproc[(glCombinerInputNV (stage exact-integer?) (portion exact-integer?) (variable exact-integer?) (input exact-integer?) (mapping exact-integer?) (componentUsage exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerOutputNV (stage exact-integer?) (portion exact-integer?) (abOutput exact-integer?) (cdOutput exact-integer?) (sumOutput exact-integer?) (scale exact-integer?) (bias exact-integer?) (abDotProduct boolean?) (cdDotProduct boolean?) (muxSum boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterfNV (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterfvNV (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameteriNV (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerParameterivNV (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glCombinerStageParameterfvNV (stage exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners2.txt"]{@racket[GL_NV_register_combiners2]}.
}
@defproc[(glCompileShader (shader exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompileShader.xml"]{glCompileShader manpage}.
}
@defproc[(glCompileShaderARB (shaderObj exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCompileShader].
}
@defproc[(glCompileShaderIncludeARB (shader exact-nonnegative-integer?) (count exact-integer?) (path (vectorof (or/c string? bytes?))) (length s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glCompressedMultiTexImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexImage3DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedMultiTexSubImage3DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTexImage1D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage1D.xml"]{glCompressedTexImage1D manpage}.
}
@defproc[(glCompressedTexImage1DARB (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage1D].
}
@defproc[(glCompressedTexImage2D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage2D.xml"]{glCompressedTexImage2D manpage}.
}
@defproc[(glCompressedTexImage2DARB (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage2D].
}
@defproc[(glCompressedTexImage3D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexImage3D.xml"]{glCompressedTexImage3D manpage}.
}
@defproc[(glCompressedTexImage3DARB (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexImage3D].
}
@defproc[(glCompressedTexSubImage1D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage1D.xml"]{glCompressedTexSubImage1D manpage}.
}
@defproc[(glCompressedTexSubImage1DARB (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage1D].
}
@defproc[(glCompressedTexSubImage2D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage2D.xml"]{glCompressedTexSubImage2D manpage}.
}
@defproc[(glCompressedTexSubImage2DARB (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage2D].
}
@defproc[(glCompressedTexSubImage3D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCompressedTexSubImage3D.xml"]{glCompressedTexSubImage3D manpage}.
}
@defproc[(glCompressedTexSubImage3DARB (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (imageSize exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glCompressedTexSubImage3D].
}
@defproc[(glCompressedTextureImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureImage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCompressedTextureSubImage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (imageSize exact-integer?) (bits gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glConvolutionFilter1D (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter1D.xml"]{glConvolutionFilter1D manpage}.
}
@defproc[(glConvolutionFilter1DEXT (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionFilter1D].
}
@defproc[(glConvolutionFilter2D (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter2D.xml"]{glConvolutionFilter2D manpage}.
}
@defproc[(glConvolutionFilter2DEXT (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionFilter2D].
}
@defproc[(glConvolutionParameterf (target exact-integer?) (pname exact-integer?) (params flonum?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterfEXT (target exact-integer?) (pname exact-integer?) (params flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameterf].
}
@defproc[(glConvolutionParameterfv (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterfvEXT (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameterfv].
}
@defproc[(glConvolutionParameteri (target exact-integer?) (pname exact-integer?) (params exact-integer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameteriEXT (target exact-integer?) (pname exact-integer?) (params exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameteri].
}
@defproc[(glConvolutionParameteriv (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml"]{glConvolutionParameter manpage}.
}
@defproc[(glConvolutionParameterivEXT (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glConvolutionParameteriv].
}
@defproc[(glCopyBufferSubData (readTarget exact-integer?) (writeTarget exact-integer?) (readOffset exact-integer?) (writeOffset exact-integer?) (size exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyBufferSubData.xml"]{glCopyBufferSubData manpage}.
}
@defproc[(glCopyColorSubTable (target exact-integer?) (start exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorSubTable.xml"]{glCopyColorSubTable manpage}.
}
@defproc[(glCopyColorSubTableEXT (target exact-integer?) (start exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/color_subtable.txt"]{@racket[GL_EXT_color_subtable]}.

Alias of @racket[glCopyColorSubTable].
}
@defproc[(glCopyColorTable (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorTable.xml"]{glCopyColorTable manpage}.
}
@defproc[(glCopyColorTableSGI (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.

Alias of @racket[glCopyColorTable].
}
@defproc[(glCopyConvolutionFilter1D (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter1D.xml"]{glCopyConvolutionFilter1D manpage}.
}
@defproc[(glCopyConvolutionFilter1DEXT (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glCopyConvolutionFilter1D].
}
@defproc[(glCopyConvolutionFilter2D (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter2D.xml"]{glCopyConvolutionFilter2D manpage}.
}
@defproc[(glCopyConvolutionFilter2DEXT (target exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glCopyConvolutionFilter2D].
}
@defproc[(glCopyImageSubData (srcName exact-nonnegative-integer?) (srcTarget exact-integer?) (srcLevel exact-integer?) (srcX exact-integer?) (srcY exact-integer?) (srcZ exact-integer?) (dstName exact-nonnegative-integer?) (dstTarget exact-integer?) (dstLevel exact-integer?) (dstX exact-integer?) (dstY exact-integer?) (dstZ exact-integer?) (srcWidth exact-integer?) (srcHeight exact-integer?) (srcDepth exact-integer?)) any]{
Version 4.3.
}
@defproc[(glCopyImageSubDataNV (srcName exact-nonnegative-integer?) (srcTarget exact-integer?) (srcLevel exact-integer?) (srcX exact-integer?) (srcY exact-integer?) (srcZ exact-integer?) (dstName exact-nonnegative-integer?) (dstTarget exact-integer?) (dstLevel exact-integer?) (dstX exact-integer?) (dstY exact-integer?) (dstZ exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/copy_image.txt"]{@racket[GL_NV_copy_image]}.
}
@defproc[(glCopyMultiTexImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyMultiTexSubImage3DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyPathNV (resultPath exact-nonnegative-integer?) (srcPath exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCopyPixels (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?) (type exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glCopyPixels.xml"]{glCopyPixels manpage}.
}
@defproc[(glCopyTexImage1D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (border exact-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexImage1D.xml"]{glCopyTexImage1D manpage}.
}
@defproc[(glCopyTexImage1DEXT (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexImage1D].
}
@defproc[(glCopyTexImage2D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexImage2D.xml"]{glCopyTexImage2D manpage}.
}
@defproc[(glCopyTexImage2DEXT (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexImage2D].
}
@defproc[(glCopyTexSubImage1D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage1D.xml"]{glCopyTexSubImage1D manpage}.
}
@defproc[(glCopyTexSubImage1DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage1D].
}
@defproc[(glCopyTexSubImage2D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage2D.xml"]{glCopyTexSubImage2D manpage}.
}
@defproc[(glCopyTexSubImage2DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage2D].
}
@defproc[(glCopyTexSubImage3D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCopyTexSubImage3D.xml"]{glCopyTexSubImage3D manpage}.
}
@defproc[(glCopyTexSubImage3DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/copy_texture.txt"]{@racket[GL_EXT_copy_texture]}.

Alias of @racket[glCopyTexSubImage3D].
}
@defproc[(glCopyTextureImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCopyTextureSubImage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glCoverFillPathInstancedNV (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (coverMode exact-integer?) (transformType exact-integer?) (transformValues f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverFillPathNV (path exact-nonnegative-integer?) (coverMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverStrokePathInstancedNV (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (coverMode exact-integer?) (transformType exact-integer?) (transformValues f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCoverStrokePathNV (path exact-nonnegative-integer?) (coverMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glCreateProgram) exact-nonnegative-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCreateProgram.xml"]{glCreateProgram manpage}.
}
@defproc[(glCreateProgramObjectARB) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCreateProgram].
}
@defproc[(glCreateShader (type exact-integer?)) exact-nonnegative-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCreateShader.xml"]{glCreateShader manpage}.
}
@defproc[(glCreateShaderObjectARB (shaderType exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glCreateShader].
}
@defproc[(glCreateShaderProgramEXT (type exact-integer?) (string (or/c string? bytes?))) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
@defproc[(glCreateShaderProgramv (type exact-integer?) (count exact-integer?) (strings (vectorof (or/c string? bytes?)))) exact-nonnegative-integer?]{
Version 4.1.
}
@defproc[(glCreateSyncFromCLeventARB (context cpointer?) (event cpointer?) (flags exact-nonnegative-integer?)) GLsync?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/cl_event.txt"]{@racket[GL_ARB_cl_event]}.
}
@defproc[(glCullFace (mode exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glCullFace.xml"]{glCullFace manpage}.
}
@defproc[(glCullParameterdvEXT (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/cull_vertex.txt"]{@racket[GL_EXT_cull_vertex]}.
}
@defproc[(glCullParameterfvEXT (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/cull_vertex.txt"]{@racket[GL_EXT_cull_vertex]}.
}
@defproc[(glCurrentPaletteMatrixARB (index exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
