#lang scribble/manual
@title{glT...}
@defproc[(glTagSampleBufferSGIX) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/tag_sample_buffer.txt"]{@racket[GL_SGIX_tag_sample_buffer]}.
}
@defproc[(glTangent3bEXT (tx (integer-in -128 127)) (ty (integer-in -128 127)) (tz (integer-in -128 127))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3bvEXT (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3dEXT (tx real?) (ty real?) (tz real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3dvEXT (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3fEXT (tx flonum?) (ty flonum?) (tz flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3fvEXT (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3iEXT (tx exact-integer?) (ty exact-integer?) (tz exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3ivEXT (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3sEXT (tx (integer-in -32768 32767)) (ty (integer-in -32768 32767)) (tz (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangent3svEXT (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTangentPointerEXT (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glTbufferMask3DFX (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/3DFX/tbuffer.txt"]{@racket[GL_3DFX_tbuffer]}.
}
@defproc[(glTessellationFactorAMD (factor flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/vertex_shader_tesselator.txt"]{@racket[GL_AMD_vertex_shader_tesselator]}.
}
@defproc[(glTessellationModeAMD (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/vertex_shader_tesselator.txt"]{@racket[GL_AMD_vertex_shader_tesselator]}.
}
@defproc[(glTestFenceAPPLE (fence exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glTestFenceNV (fence exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glTestObjectAPPLE (object exact-integer?) (name exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glTexBuffer (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexBuffer.xml"]{glTexBuffer manpage}.
}
@defproc[(glTexBufferARB (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_buffer_object.txt"]{@racket[GL_ARB_texture_buffer_object]}.

Alias of @racket[glTexBuffer].
}
@defproc[(glTexBufferEXT (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_buffer_object.txt"]{@racket[GL_EXT_texture_buffer_object]}.

Alias of @racket[glTexBufferARB].
}
@defproc[(glTexBufferRange (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Version 4.3.
}
@defproc[(glTexBumpParameterfvATI (pname exact-integer?) (param f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glTexBumpParameterivATI (pname exact-integer?) (param s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glTexCoord1d (s real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1dv (v real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1f (s flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1fv (v flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1hNV (s (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord1hvNV (v (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord1i (s exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1iv (v exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1s (s (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord1sv (v (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2d (s real?) (t real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2f (s flonum?) (t flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2fColor3fVertex3fSUN (s flonum?) (t flonum?) (r flonum?) (g flonum?) (b flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor3fVertex3fvSUN (tc f32vector?) (c f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4fNormal3fVertex3fSUN (s flonum?) (t flonum?) (r flonum?) (g flonum?) (b flonum?) (a flonum?) (nx flonum?) (ny flonum?) (nz flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4fNormal3fVertex3fvSUN (tc f32vector?) (c f32vector?) (n f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4ubVertex3fSUN (s flonum?) (t flonum?) (r (integer-in 0 255)) (g (integer-in 0 255)) (b (integer-in 0 255)) (a (integer-in 0 255)) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fColor4ubVertex3fvSUN (tc f32vector?) (c (or/c string? bytes?)) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fNormal3fVertex3fSUN (s flonum?) (t flonum?) (nx flonum?) (ny flonum?) (nz flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fNormal3fVertex3fvSUN (tc f32vector?) (n f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fVertex3fSUN (s flonum?) (t flonum?) (x flonum?) (y flonum?) (z flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fVertex3fvSUN (tc f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord2fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2hNV (s (integer-in 0 65535)) (t (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord2hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord2i (s exact-integer?) (t exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2s (s (integer-in -32768 32767)) (t (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord2sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3d (s real?) (t real?) (r real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3f (s flonum?) (t flonum?) (r flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3hNV (s (integer-in 0 65535)) (t (integer-in 0 65535)) (r (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord3hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord3i (s exact-integer?) (t exact-integer?) (r exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3s (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord3sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4d (s real?) (t real?) (r real?) (q real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4dv (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4f (s flonum?) (t flonum?) (r flonum?) (q flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4fColor4fNormal3fVertex4fSUN (s flonum?) (t flonum?) (p flonum?) (q flonum?) (r flonum?) (g flonum?) (b flonum?) (a flonum?) (nx flonum?) (ny flonum?) (nz flonum?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fColor4fNormal3fVertex4fvSUN (tc f32vector?) (c f32vector?) (n f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fVertex4fSUN (s flonum?) (t flonum?) (p flonum?) (q flonum?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fVertex4fvSUN (tc f32vector?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/vertex.txt"]{@racket[GL_SUN_vertex]}.
}
@defproc[(glTexCoord4fv (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4hNV (s (integer-in 0 65535)) (t (integer-in 0 65535)) (r (integer-in 0 65535)) (q (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord4hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glTexCoord4i (s exact-integer?) (t exact-integer?) (r exact-integer?) (q exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4iv (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4s (s (integer-in -32768 32767)) (t (integer-in -32768 32767)) (r (integer-in -32768 32767)) (q (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoord4sv (v s16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml"]{glTexCoord manpage}.
}
@defproc[(glTexCoordFormatNV (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glTexCoordP1ui (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP1uiv (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP2ui (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP2uiv (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP3ui (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP3uiv (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP4ui (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordP4uiv (type exact-integer?) (coords exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glTexCoordPointer (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexCoordPointer.xml"]{glTexCoordPointer manpage}.
}
@defproc[(glTexCoordPointerEXT (size exact-integer?) (type exact-integer?) (stride exact-integer?) (count exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glTexCoordPointerListIBM (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glTexCoordPointervINTEL (size exact-integer?) (type exact-integer?) (pointer (vectorof gl-pointer?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt"]{@racket[GL_INTEL_parallel_arrays]}.
}
@defproc[(glTexEnvf (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnvfv (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnvi (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexEnviv (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml"]{glTexEnv manpage}.
}
@defproc[(glTexFilterFuncSGIS (target exact-integer?) (filter exact-integer?) (n exact-integer?) (weights f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_filter4.txt"]{@racket[GL_SGIS_texture_filter4]}.
}
@defproc[(glTexGend (coord exact-integer?) (pname exact-integer?) (param real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGendv (coord exact-integer?) (pname exact-integer?) (params f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGenf (coord exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGenfv (coord exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGeni (coord exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexGeniv (coord exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml"]{glTexGen manpage}.
}
@defproc[(glTexImage1D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage1D.xml"]{glTexImage1D manpage}.
}
@defproc[(glTexImage2D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage2D.xml"]{glTexImage2D manpage}.
}
@defproc[(glTexImage2DMultisample (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedsamplelocations boolean?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage2DMultisample.xml"]{glTexImage2DMultisample manpage}.
}
@defproc[(glTexImage2DMultisampleCoverageNV (target exact-integer?) (coverageSamples exact-integer?) (colorSamples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTexImage3D (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage3D.xml"]{glTexImage3D manpage}.
}
@defproc[(glTexImage3DEXT (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture3D.txt"]{@racket[GL_EXT_texture3D]}.

Alias of @racket[glTexImage3D].
}
@defproc[(glTexImage3DMultisample (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedsamplelocations boolean?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexImage3DMultisample.xml"]{glTexImage3DMultisample manpage}.
}
@defproc[(glTexImage3DMultisampleCoverageNV (target exact-integer?) (coverageSamples exact-integer?) (colorSamples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTexImage4DSGIS (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (size4d exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture4D.txt"]{@racket[GL_SGIS_texture4D]}.
}
@defproc[(glTexParameterIiv (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Version 3.0.
}
@defproc[(glTexParameterIivEXT (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glTexParameterIiv].
}
@defproc[(glTexParameterIuiv (target exact-integer?) (pname exact-integer?) (params u32vector?)) any]{
Version 3.0.
}
@defproc[(glTexParameterIuivEXT (target exact-integer?) (pname exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glTexParameterIuiv].
}
@defproc[(glTexParameterf (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameterfv (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameteri (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexParameteriv (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexParameter.xml"]{glTexParameter manpage}.
}
@defproc[(glTexRenderbufferNV (target exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glTexStorage1D (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?)) any]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage1D.xml"]{glTexStorage1D manpage}.
}
@defproc[(glTexStorage2D (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage2D.xml"]{glTexStorage2D manpage}.
}
@defproc[(glTexStorage2DMultisample (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedsamplelocations boolean?)) any]{
Version 4.3.
}
@defproc[(glTexStorage3D (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?)) any]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexStorage3D.xml"]{glTexStorage3D manpage}.
}
@defproc[(glTexStorage3DMultisample (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedsamplelocations boolean?)) any]{
Version 4.3.
}
@defproc[(glTexStorageSparseAMD (target exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (layers exact-integer?) (flags exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sparse_texture.txt"]{@racket[GL_AMD_sparse_texture]}.
}
@defproc[(glTexSubImage1D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage1D.xml"]{glTexSubImage1D manpage}.
}
@defproc[(glTexSubImage1DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/subtexture.txt"]{@racket[GL_EXT_subtexture]}.

Alias of @racket[glTexSubImage1D].
}
@defproc[(glTexSubImage2D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage2D.xml"]{glTexSubImage2D manpage}.
}
@defproc[(glTexSubImage2DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/subtexture.txt"]{@racket[GL_EXT_subtexture]}.

Alias of @racket[glTexSubImage2D].
}
@defproc[(glTexSubImage3D (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTexSubImage3D.xml"]{glTexSubImage3D manpage}.
}
@defproc[(glTexSubImage3DEXT (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture3D.txt"]{@racket[GL_EXT_texture3D]}.

Alias of @racket[glTexSubImage3D].
}
@defproc[(glTexSubImage4DSGIS (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (woffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (size4d exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture4D.txt"]{@racket[GL_SGIS_texture4D]}.
}
@defproc[(glTextureBarrierNV) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_barrier.txt"]{@racket[GL_NV_texture_barrier]}.
}
@defproc[(glTextureBufferEXT (texture exact-nonnegative-integer?) (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureBufferRangeEXT (texture exact-nonnegative-integer?) (target exact-integer?) (internalformat exact-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_buffer_range.txt"]{@racket[GL_ARB_texture_buffer_range]}.
}
@defproc[(glTextureColorMaskSGIS (red boolean?) (green boolean?) (blue boolean?) (alpha boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_color_mask.txt"]{@racket[GL_SGIS_texture_color_mask]}.
}
@defproc[(glTextureImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage2DMultisampleCoverageNV (texture exact-nonnegative-integer?) (target exact-integer?) (coverageSamples exact-integer?) (colorSamples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage2DMultisampleNV (texture exact-nonnegative-integer?) (target exact-integer?) (samples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (border exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureImage3DMultisampleCoverageNV (texture exact-nonnegative-integer?) (target exact-integer?) (coverageSamples exact-integer?) (colorSamples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureImage3DMultisampleNV (texture exact-nonnegative-integer?) (target exact-integer?) (samples exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedSampleLocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/texture_multisample.txt"]{@racket[GL_NV_texture_multisample]}.
}
@defproc[(glTextureLightEXT (pname exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glTextureMaterialEXT (face exact-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glTextureNormalEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_perturb_normal.txt"]{@racket[GL_EXT_texture_perturb_normal]}.
}
@defproc[(glTextureParameterIivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterIuivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterfEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterfvEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameteriEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureParameterivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureRangeAPPLE (target exact-integer?) (length exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/texture_range.txt"]{@racket[GL_APPLE_texture_range]}.
}
@defproc[(glTextureRenderbufferEXT (texture exact-nonnegative-integer?) (target exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureStorage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage2DMultisampleEXT (texture exact-nonnegative-integer?) (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (fixedsamplelocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt"]{@racket[GL_ARB_texture_storage_multisample]}.
}
@defproc[(glTextureStorage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (levels exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage.txt"]{@racket[GL_ARB_texture_storage]}.
}
@defproc[(glTextureStorage3DMultisampleEXT (texture exact-nonnegative-integer?) (target exact-integer?) (samples exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (fixedsamplelocations boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt"]{@racket[GL_ARB_texture_storage_multisample]}.
}
@defproc[(glTextureStorageSparseAMD (texture exact-nonnegative-integer?) (target exact-integer?) (internalFormat exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (layers exact-integer?) (flags exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sparse_texture.txt"]{@racket[GL_AMD_sparse_texture]}.
}
@defproc[(glTextureSubImage1DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (width exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureSubImage2DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureSubImage3DEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glTextureView (texture exact-nonnegative-integer?) (target exact-integer?) (origtexture exact-nonnegative-integer?) (internalformat exact-integer?) (minlevel exact-nonnegative-integer?) (numlevels exact-nonnegative-integer?) (minlayer exact-nonnegative-integer?) (numlayers exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glTrackMatrixNV (target exact-integer?) (address exact-nonnegative-integer?) (matrix exact-integer?) (transform exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glTransformFeedbackAttribsNV (count exact-nonnegative-integer?) (attribs s32vector?) (bufferMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glTransformFeedbackStreamAttribsNV (count exact-integer?) (attribs s32vector?) (nbuffers exact-integer?) (bufstreams s32vector?) (bufferMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glTransformFeedbackVaryings (program exact-nonnegative-integer?) (count exact-integer?) (varyings (vectorof (or/c string? bytes?))) (bufferMode exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glTransformFeedbackVaryings.xml"]{glTransformFeedbackVaryings manpage}.
}
@defproc[(glTransformFeedbackVaryingsEXT (program exact-nonnegative-integer?) (count exact-integer?) (varyings (vectorof (or/c string? bytes?))) (bufferMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glTransformFeedbackVaryings].
}
@defproc[(glTransformFeedbackVaryingsNV (program exact-nonnegative-integer?) (count exact-integer?) (locations s32vector?) (bufferMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glTransformFeedbackVaryings].
}
@defproc[(glTransformPathNV (resultPath exact-nonnegative-integer?) (srcPath exact-nonnegative-integer?) (transformType exact-integer?) (transformValues f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glTranslated (x real?) (y real?) (z real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml"]{glTranslate manpage}.
}
@defproc[(glTranslatef (x flonum?) (y flonum?) (z flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml"]{glTranslate manpage}.
}
