#lang scribble/manual
@title{glM...}
@defproc[(glMakeBufferNonResidentNV (target exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeBufferResidentNV (target exact-integer?) (access exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeImageHandleNonResidentNV (handle exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeImageHandleResidentNV (handle exact-nonnegative-integer?) (access exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeNamedBufferNonResidentNV (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeNamedBufferResidentNV (buffer exact-nonnegative-integer?) (access exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glMakeTextureHandleNonResidentNV (handle exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMakeTextureHandleResidentNV (handle exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glMap1d (target exact-integer?) (u1 real?) (u2 real?) (stride exact-integer?) (order exact-integer?) (points f64vector?)) any]{
Deprecated in version 3.1.
}
@defproc[(glMap1f (target exact-integer?) (u1 flonum?) (u2 flonum?) (stride exact-integer?) (order exact-integer?) (points f32vector?)) any]{
Deprecated in version 3.1.
}
@defproc[(glMap2d (target exact-integer?) (u1 real?) (u2 real?) (ustride exact-integer?) (uorder exact-integer?) (v1 real?) (v2 real?) (vstride exact-integer?) (vorder exact-integer?) (points f64vector?)) any]{
Deprecated in version 3.1.
}
@defproc[(glMap2f (target exact-integer?) (u1 flonum?) (u2 flonum?) (ustride exact-integer?) (uorder exact-integer?) (v1 flonum?) (v2 flonum?) (vstride exact-integer?) (vorder exact-integer?) (points f32vector?)) any]{
Deprecated in version 3.1.
}
@defproc[(glMapBuffer (target exact-integer?) (access exact-integer?)) gl-pointer?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMapBuffer.xml"]{glMapBuffer manpage}.
}
@defproc[(glMapBufferARB (target exact-integer?) (access exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glMapBuffer].
}
@defproc[(glMapBufferRange (target exact-integer?) (offset exact-integer?) (length exact-integer?) (access exact-nonnegative-integer?)) gl-pointer?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMapBufferRange.xml"]{glMapBufferRange manpage}.
}
@defproc[(glMapControlPointsNV (target exact-integer?) (index exact-nonnegative-integer?) (type exact-integer?) (ustride exact-integer?) (vstride exact-integer?) (uorder exact-integer?) (vorder exact-integer?) (packed boolean?) (points gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapGrid1d (un exact-integer?) (u1 real?) (u2 real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid1f (un exact-integer?) (u1 flonum?) (u2 flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid2d (un exact-integer?) (u1 real?) (u2 real?) (vn exact-integer?) (v1 real?) (v2 real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapGrid2f (un exact-integer?) (u1 flonum?) (u2 flonum?) (vn exact-integer?) (v1 flonum?) (v2 flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml"]{glMapGrid manpage}.
}
@defproc[(glMapNamedBufferEXT (buffer exact-nonnegative-integer?) (access exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMapNamedBufferRangeEXT (buffer exact-nonnegative-integer?) (offset exact-integer?) (length exact-integer?) (access exact-nonnegative-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMapObjectBufferATI (buffer exact-nonnegative-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/map_object_buffer.txt"]{@racket[GL_ATI_map_object_buffer]}.
}
@defproc[(glMapParameterfvNV (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapParameterivNV (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glMapVertexAttrib1dAPPLE (index exact-nonnegative-integer?) (size exact-nonnegative-integer?) (u1 real?) (u2 real?) (stride exact-integer?) (order exact-integer?) (points f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib1fAPPLE (index exact-nonnegative-integer?) (size exact-nonnegative-integer?) (u1 flonum?) (u2 flonum?) (stride exact-integer?) (order exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib2dAPPLE (index exact-nonnegative-integer?) (size exact-nonnegative-integer?) (u1 real?) (u2 real?) (ustride exact-integer?) (uorder exact-integer?) (v1 real?) (v2 real?) (vstride exact-integer?) (vorder exact-integer?) (points f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMapVertexAttrib2fAPPLE (index exact-nonnegative-integer?) (size exact-nonnegative-integer?) (u1 flonum?) (u2 flonum?) (ustride exact-integer?) (uorder exact-integer?) (v1 flonum?) (v2 flonum?) (vstride exact-integer?) (vorder exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
@defproc[(glMaterialf (face exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMaterialfv (face exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMateriali (face exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMaterialiv (face exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml"]{glMaterial manpage}.
}
@defproc[(glMatrixFrustumEXT (mode exact-integer?) (left real?) (right real?) (bottom real?) (top real?) (zNear real?) (zFar real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixIndexPointerARB (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexubvARB (size exact-integer?) (indices (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexuivARB (size exact-integer?) (indices u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixIndexusvARB (size exact-integer?) (indices u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/matrix_palette.txt"]{@racket[GL_ARB_matrix_palette]}.
}
@defproc[(glMatrixLoadIdentityEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadTransposedEXT (mode exact-integer?) (m f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadTransposefEXT (mode exact-integer?) (m f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoaddEXT (mode exact-integer?) (m f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixLoadfEXT (mode exact-integer?) (m f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMode (mode exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMatrixMode.xml"]{glMatrixMode manpage}.
}
@defproc[(glMatrixMultTransposedEXT (mode exact-integer?) (m f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultTransposefEXT (mode exact-integer?) (m f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultdEXT (mode exact-integer?) (m f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixMultfEXT (mode exact-integer?) (m f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixOrthoEXT (mode exact-integer?) (left real?) (right real?) (bottom real?) (top real?) (zNear real?) (zFar real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixPopEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixPushEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixRotatedEXT (mode exact-integer?) (angle real?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixRotatefEXT (mode exact-integer?) (angle flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixScaledEXT (mode exact-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixScalefEXT (mode exact-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixTranslatedEXT (mode exact-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMatrixTranslatefEXT (mode exact-integer?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMemoryBarrier (barriers exact-nonnegative-integer?)) any]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMemoryBarrier.xml"]{glMemoryBarrier manpage}.
}
@defproc[(glMemoryBarrierEXT (barriers exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt"]{@racket[GL_EXT_shader_image_load_store]}.
}
@defproc[(glMinSampleShading (value flonum?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMinSampleShading.xml"]{glMinSampleShading manpage}.
}
@defproc[(glMinSampleShadingARB (value flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/sample_shading.txt"]{@racket[GL_ARB_sample_shading]}.

Alias of @racket[glMinSampleShading].
}
@defproc[(glMinmax (target exact-integer?) (internalformat exact-integer?) (sink boolean?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMinmax.xml"]{glMinmax manpage}.
}
@defproc[(glMinmaxEXT (target exact-integer?) (internalformat exact-integer?) (sink boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.

Alias of @racket[glMinmax].
}
@defproc[(glMultMatrixd (m f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml"]{glMultMatrix manpage}.
}
@defproc[(glMultMatrixf (m f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml"]{glMultMatrix manpage}.
}
@defproc[(glMultTransposeMatrixd (m f64vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml"]{glMultTransposeMatrix manpage}.
}
@defproc[(glMultTransposeMatrixdARB (m f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glMultTransposeMatrixd].
}
@defproc[(glMultTransposeMatrixf (m f32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml"]{glMultTransposeMatrix manpage}.
}
@defproc[(glMultTransposeMatrixfARB (m f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/transpose_matrix.txt"]{@racket[GL_ARB_transpose_matrix]}.

Alias of @racket[glMultTransposeMatrixf].
}
@defproc[(glMultiDrawArrays (mode exact-integer?) (first s32vector?) (count s32vector?) (drawcount exact-integer?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawArrays.xml"]{glMultiDrawArrays manpage}.
}
@defproc[(glMultiDrawArraysEXT (mode exact-integer?) (first s32vector?) (count s32vector?) (primcount exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt"]{@racket[GL_EXT_multi_draw_arrays]}.

Alias of @racket[glMultiDrawArrays].
}
@defproc[(glMultiDrawArraysIndirect (mode exact-integer?) (indirect gl-pointer?) (drawcount exact-integer?) (stride exact-integer?)) any]{
Version 4.3.
}
@defproc[(glMultiDrawArraysIndirectAMD (mode exact-integer?) (indirect gl-pointer?) (primcount exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt"]{@racket[GL_AMD_multi_draw_indirect]}.
}
@defproc[(glMultiDrawElementArrayAPPLE (mode exact-integer?) (first s32vector?) (count s32vector?) (primcount exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glMultiDrawElements (mode exact-integer?) (count s32vector?) (type exact-integer?) (indices (vectorof gl-pointer?)) (drawcount exact-integer?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElements.xml"]{glMultiDrawElements manpage}.
}
@defproc[(glMultiDrawElementsBaseVertex (mode exact-integer?) (count s32vector?) (type exact-integer?) (indices (vectorof gl-pointer?)) (drawcount exact-integer?) (basevertex s32vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glMultiDrawElementsBaseVertex.xml"]{glMultiDrawElementsBaseVertex manpage}.
}
@defproc[(glMultiDrawElementsEXT (mode exact-integer?) (count s32vector?) (type exact-integer?) (indices (vectorof gl-pointer?)) (primcount exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt"]{@racket[GL_EXT_multi_draw_arrays]}.

Alias of @racket[glMultiDrawElements].
}
@defproc[(glMultiDrawElementsIndirect (mode exact-integer?) (type exact-integer?) (indirect gl-pointer?) (drawcount exact-integer?) (stride exact-integer?)) any]{
Version 4.3.
}
@defproc[(glMultiDrawElementsIndirectAMD (mode exact-integer?) (type exact-integer?) (indirect gl-pointer?) (primcount exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt"]{@racket[GL_AMD_multi_draw_indirect]}.
}
@defproc[(glMultiDrawRangeElementArrayAPPLE (mode exact-integer?) (start exact-nonnegative-integer?) (end exact-nonnegative-integer?) (first s32vector?) (count s32vector?) (primcount exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/element_array.txt"]{@racket[GL_APPLE_element_array]}.
}
@defproc[(glMultiModeDrawArraysIBM (mode (vectorof exact-integer?)) (first s32vector?) (count s32vector?) (primcount exact-integer?) (modestride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt"]{@racket[GL_IBM_multimode_draw_arrays]}.
}
@defproc[(glMultiModeDrawElementsIBM (mode (vectorof exact-integer?)) (count s32vector?) (type exact-integer?) (indices (vectorof gl-pointer?)) (primcount exact-integer?) (modestride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt"]{@racket[GL_IBM_multimode_draw_arrays]}.
}
@defproc[(glMultiTexBufferEXT (texunit exact-integer?) (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexCoord1d (target exact-integer?) (s real?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1dARB (target exact-integer?) (s real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1dv (target exact-integer?) (v real?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1dvARB (target exact-integer?) (v real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1dv].
}
@defproc[(glMultiTexCoord1f (target exact-integer?) (s flonum?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1fARB (target exact-integer?) (s flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1fv (target exact-integer?) (v flonum?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1fvARB (target exact-integer?) (v flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1fv].
}
@defproc[(glMultiTexCoord1hNV (target exact-integer?) (s (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord1hvNV (target exact-integer?) (v (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord1i (target exact-integer?) (s exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1iARB (target exact-integer?) (s exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1iv (target exact-integer?) (v exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1ivARB (target exact-integer?) (v exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1iv].
}
@defproc[(glMultiTexCoord1s (target exact-integer?) (s (integer-in -32768 32767))) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1sARB (target exact-integer?) (s (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord1sv (target exact-integer?) (v (integer-in -32768 32767))) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord1svARB (target exact-integer?) (v (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord1sv].
}
@defproc[(glMultiTexCoord2d (target exact-integer?) (s real?) (t real?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2dARB (target exact-integer?) (s real?) (t real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2dv (target exact-integer?) (v f64vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2dvARB (target exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2dv].
}
@defproc[(glMultiTexCoord2f (target exact-integer?) (s flonum?) (t flonum?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2fARB (target exact-integer?) (s flonum?) (t flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2fv (target exact-integer?) (v f32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2fvARB (target exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2fv].
}
@defproc[(glMultiTexCoord2hNV (target exact-integer?) (s (integer-in 0 65535)) (t (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord2hvNV (target exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord2i (target exact-integer?) (s exact-integer?) (t exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2iARB (target exact-integer?) (s exact-integer?) (t exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2iv (target exact-integer?) (v s32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2ivARB (target exact-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2iv].
}
@defproc[(glMultiTexCoord2s (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767))) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2sARB (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord2sv (target exact-integer?) (v s16vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord2svARB (target exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord2sv].
}
@defproc[(glMultiTexCoord3d (target exact-integer?) (s real?) (t real?) (r real?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3dARB (target exact-integer?) (s real?) (t real?) (r real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3dv (target exact-integer?) (v f64vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3dvARB (target exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3dv].
}
@defproc[(glMultiTexCoord3f (target exact-integer?) (s flonum?) (t flonum?) (r flonum?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3fARB (target exact-integer?) (s flonum?) (t flonum?) (r flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3fv (target exact-integer?) (v f32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3fvARB (target exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3fv].
}
@defproc[(glMultiTexCoord3hNV (target exact-integer?) (s (integer-in 0 65535)) (t (integer-in 0 65535)) (r (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord3hvNV (target exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord3i (target exact-integer?) (s exact-integer?) (t exact-integer?) (r exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3iARB (target exact-integer?) (s exact-integer?) (t exact-integer?) (r exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3iv (target exact-integer?) (v s32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3ivARB (target exact-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3iv].
}
@defproc[(glMultiTexCoord3s (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767))) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3sARB (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord3sv (target exact-integer?) (v s16vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord3svARB (target exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord3sv].
}
@defproc[(glMultiTexCoord4d (target exact-integer?) (s real?) (t real?) (r real?) (q real?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4dARB (target exact-integer?) (s real?) (t real?) (r real?) (q real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4dv (target exact-integer?) (v f64vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4dvARB (target exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4dv].
}
@defproc[(glMultiTexCoord4f (target exact-integer?) (s flonum?) (t flonum?) (r flonum?) (q flonum?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4fARB (target exact-integer?) (s flonum?) (t flonum?) (r flonum?) (q flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4fv (target exact-integer?) (v f32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4fvARB (target exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4fv].
}
@defproc[(glMultiTexCoord4hNV (target exact-integer?) (s (integer-in 0 65535)) (t (integer-in 0 65535)) (r (integer-in 0 65535)) (q (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord4hvNV (target exact-integer?) (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glMultiTexCoord4i (target exact-integer?) (s exact-integer?) (t exact-integer?) (r exact-integer?) (q exact-integer?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4iARB (target exact-integer?) (s exact-integer?) (t exact-integer?) (r exact-integer?) (q exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4iv (target exact-integer?) (v s32vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4ivARB (target exact-integer?) (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4iv].
}
@defproc[(glMultiTexCoord4s (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767)) (q (integer-in -32768 32767))) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4sARB (target exact-integer?) (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767)) (q (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.
}
@defproc[(glMultiTexCoord4sv (target exact-integer?) (v s16vector?)) any]{
Version 1.3.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml"]{glMultiTexCoord manpage}.
}
@defproc[(glMultiTexCoord4svARB (target exact-integer?) (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glMultiTexCoord4sv].
}
@defproc[(glMultiTexCoordP1ui (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP1uiv (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP2ui (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP2uiv (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP3ui (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP3uiv (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP4ui (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordP4uiv (texture exact-integer?) (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glMultiTexCoordPointerEXT (texunit exact-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvfEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvfvEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnviEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexEnvivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGendEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (param real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGendvEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (params f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenfEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenfvEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGeniEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexGenivEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexImage3DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterIivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterIuivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterfEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterfvEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameteriEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexParameterivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexRenderbufferEXT (texunit exact-integer?) (target exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage1DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage2DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glMultiTexSubImage3DEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
