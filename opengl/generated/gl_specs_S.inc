#lang scribble/manual
@title{glS...}
@defproc[(glSampleCoverage (value flonum?) (invert boolean?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSampleCoverage.xml"]{glSampleCoverage manpage}.
}
@defproc[(glSampleCoverageARB (value flonum?) (invert boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multisample.txt"]{@racket[GL_ARB_multisample]}.

Alias of @racket[glSampleCoverage].
}
@defproc[(glSampleMapATI (dst exact-nonnegative-integer?) (interp exact-nonnegative-integer?) (swizzle exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glSampleMaskEXT (value flonum?) (invert boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multisample.txt"]{@racket[GL_EXT_multisample]}.
}
@defproc[(glSampleMaskIndexedNV (index exact-nonnegative-integer?) (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glSampleMaskSGIS (value flonum?) (invert boolean?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/multisample.txt"]{@racket[GL_SGIS_multisample]}.

Alias of @racket[glSampleMaskEXT].
}
@defproc[(glSampleMaski (index exact-nonnegative-integer?) (mask exact-nonnegative-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSampleMaski.xml"]{glSampleMaski manpage}.
}
@defproc[(glSamplePatternEXT (pattern exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/multisample.txt"]{@racket[GL_EXT_multisample]}.
}
@defproc[(glSamplePatternSGIS (pattern exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/multisample.txt"]{@racket[GL_SGIS_multisample]}.

Alias of @racket[glSamplePatternEXT].
}
@defproc[(glSamplerParameterIiv (sampler exact-nonnegative-integer?) (pname exact-integer?) (param s32vector?)) any]{
Version 1.2.
}
@defproc[(glSamplerParameterIuiv (sampler exact-nonnegative-integer?) (pname exact-integer?) (param u32vector?)) any]{
Version 1.2.
}
@defproc[(glSamplerParameterf (sampler exact-nonnegative-integer?) (pname exact-integer?) (param flonum?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameterfv (sampler exact-nonnegative-integer?) (pname exact-integer?) (param f32vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameteri (sampler exact-nonnegative-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glSamplerParameteriv (sampler exact-nonnegative-integer?) (pname exact-integer?) (param s32vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glSamplerParameter.xml"]{glSamplerParameter manpage}.
}
@defproc[(glScaled (x real?) (y real?) (z real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml"]{glScale manpage}.
}
@defproc[(glScalef (x flonum?) (y flonum?) (z flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml"]{glScale manpage}.
}
@defproc[(glScissor (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glScissor.xml"]{glScissor manpage}.
}
@defproc[(glScissorArrayv (first exact-nonnegative-integer?) (count exact-integer?) (v s32vector?)) any]{
Version 4.1.
}
@defproc[(glScissorIndexed (index exact-nonnegative-integer?) (left exact-integer?) (bottom exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glScissorIndexed.xml"]{glScissorIndexed manpage}.
}
@defproc[(glScissorIndexedv (index exact-nonnegative-integer?) (v s32vector?)) any]{
Version 4.1.
}
@defproc[(glSecondaryColor3b (red (integer-in -128 127)) (green (integer-in -128 127)) (blue (integer-in -128 127))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3bEXT (red (integer-in -128 127)) (green (integer-in -128 127)) (blue (integer-in -128 127))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3b].
}
@defproc[(glSecondaryColor3bv (v s8vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3bvEXT (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3bv].
}
@defproc[(glSecondaryColor3d (red real?) (green real?) (blue real?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3dEXT (red real?) (green real?) (blue real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3d].
}
@defproc[(glSecondaryColor3dv (v f64vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3dvEXT (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3dv].
}
@defproc[(glSecondaryColor3f (red flonum?) (green flonum?) (blue flonum?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3fEXT (red flonum?) (green flonum?) (blue flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3f].
}
@defproc[(glSecondaryColor3fv (v f32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3fvEXT (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3fv].
}
@defproc[(glSecondaryColor3hNV (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glSecondaryColor3hvNV (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glSecondaryColor3i (red exact-integer?) (green exact-integer?) (blue exact-integer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3iEXT (red exact-integer?) (green exact-integer?) (blue exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3i].
}
@defproc[(glSecondaryColor3iv (v s32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ivEXT (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3iv].
}
@defproc[(glSecondaryColor3s (red (integer-in -32768 32767)) (green (integer-in -32768 32767)) (blue (integer-in -32768 32767))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3sEXT (red (integer-in -32768 32767)) (green (integer-in -32768 32767)) (blue (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3s].
}
@defproc[(glSecondaryColor3sv (v s16vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3svEXT (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3sv].
}
@defproc[(glSecondaryColor3ub (red (integer-in 0 255)) (green (integer-in 0 255)) (blue (integer-in 0 255))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ubEXT (red (integer-in 0 255)) (green (integer-in 0 255)) (blue (integer-in 0 255))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ub].
}
@defproc[(glSecondaryColor3ubv (v (or/c string? bytes?))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3ubvEXT (v (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ubv].
}
@defproc[(glSecondaryColor3ui (red exact-nonnegative-integer?) (green exact-nonnegative-integer?) (blue exact-nonnegative-integer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3uiEXT (red exact-nonnegative-integer?) (green exact-nonnegative-integer?) (blue exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3ui].
}
@defproc[(glSecondaryColor3uiv (v u32vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3uivEXT (v u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3uiv].
}
@defproc[(glSecondaryColor3us (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535))) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3usEXT (red (integer-in 0 65535)) (green (integer-in 0 65535)) (blue (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3us].
}
@defproc[(glSecondaryColor3usv (v u16vector?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml"]{glSecondaryColor manpage}.
}
@defproc[(glSecondaryColor3usvEXT (v u16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColor3usv].
}
@defproc[(glSecondaryColorFormatNV (size exact-integer?) (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glSecondaryColorP3ui (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glSecondaryColorP3uiv (type exact-integer?) (color exact-nonnegative-integer?)) any]{
Version 1.2.
}
@defproc[(glSecondaryColorPointer (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColorPointer.xml"]{glSecondaryColorPointer manpage}.
}
@defproc[(glSecondaryColorPointerEXT (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/secondary_color.txt"]{@racket[GL_EXT_secondary_color]}.

Alias of @racket[glSecondaryColorPointer].
}
@defproc[(glSecondaryColorPointerListIBM (size exact-integer?) (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glSelectBuffer (size exact-integer?)) u32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSelectBuffer.xml"]{glSelectBuffer manpage}.
}
@defproc[(glSelectPerfMonitorCountersAMD (monitor exact-nonnegative-integer?) (enable boolean?) (group exact-nonnegative-integer?) (numCounters exact-integer?) (counterList u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glSeparableFilter2D (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (row gl-pointer?) (column gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glSeparableFilter2D.xml"]{glSeparableFilter2D manpage}.
}
@defproc[(glSeparableFilter2DEXT (target exact-integer?) (internalformat exact-integer?) (width exact-integer?) (height exact-integer?) (format exact-integer?) (type exact-integer?) (row gl-pointer?) (column gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.

Alias of @racket[glSeparableFilter2D].
}
@defproc[(glSetFenceAPPLE (fence exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glSetFenceNV (fence exact-nonnegative-integer?) (condition exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glSetFragmentShaderConstantATI (dst exact-nonnegative-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glSetInvariantEXT (id exact-nonnegative-integer?) (type exact-integer?) (addr gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glSetLocalConstantEXT (id exact-nonnegative-integer?) (type exact-integer?) (addr gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glSetMultisamplefvAMD (pname exact-integer?) (index exact-nonnegative-integer?) (val f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/sample_positions.txt"]{@racket[GL_AMD_sample_positions]}.
}
@defproc[(glShadeModel (mode exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glShadeModel.xml"]{glShadeModel manpage}.
}
@defproc[(glShaderBinary (count exact-integer?) (shaders u32vector?) (binaryformat exact-integer?) (binary gl-pointer?) (length exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glShaderBinary.xml"]{glShaderBinary manpage}.
}
@defproc[(glShaderOp1EXT (op exact-integer?) (res exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderOp2EXT (op exact-integer?) (res exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderOp3EXT (op exact-integer?) (res exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?) (arg3 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glShaderSource (shader exact-nonnegative-integer?) (count exact-integer?) (string (vectorof (or/c string? bytes?))) (length s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glShaderSource.xml"]{glShaderSource manpage}.
}
@defproc[(glShaderSourceARB (shaderObj exact-nonnegative-integer?) (count exact-integer?) (string (vectorof (or/c string? bytes?))) (length s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glShaderSource].
}
@defproc[(glShaderStorageBlockBinding (program exact-nonnegative-integer?) (storageBlockIndex exact-nonnegative-integer?) (storageBlockBinding exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glSharpenTexFuncSGIS (target exact-integer?) (n exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt"]{@racket[GL_SGIS_sharpen_texture]}.
}
@defproc[(glSpriteParameterfSGIX (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameterfvSGIX (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameteriSGIX (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glSpriteParameterivSGIX (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/sprite.txt"]{@racket[GL_SGIX_sprite]}.
}
@defproc[(glStartInstrumentsSGIX) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glStencilClearTagEXT (stencilTagBits exact-integer?) (stencilClearTag exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/stencil_clear_tag.txt"]{@racket[GL_EXT_stencil_clear_tag]}.
}
@defproc[(glStencilFillPathInstancedNV (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (fillMode exact-integer?) (mask exact-nonnegative-integer?) (transformType exact-integer?) (transformValues f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilFillPathNV (path exact-nonnegative-integer?) (fillMode exact-integer?) (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilFunc (func exact-integer?) (ref exact-integer?) (mask exact-nonnegative-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilFunc.xml"]{glStencilFunc manpage}.
}
@defproc[(glStencilFuncSeparate (face exact-integer?) (func exact-integer?) (ref exact-integer?) (mask exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilFuncSeparate.xml"]{glStencilFuncSeparate manpage}.
}
@defproc[(glStencilFuncSeparateATI (frontfunc exact-integer?) (backfunc exact-integer?) (ref exact-integer?) (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/separate_stencil.txt"]{@racket[GL_ATI_separate_stencil]}.

Alias of @racket[glStencilFuncSeparate].
}
@defproc[(glStencilMask (mask exact-nonnegative-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilMask.xml"]{glStencilMask manpage}.
}
@defproc[(glStencilMaskSeparate (face exact-integer?) (mask exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilMaskSeparate.xml"]{glStencilMaskSeparate manpage}.
}
@defproc[(glStencilOp (fail exact-integer?) (zfail exact-integer?) (zpass exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilOp.xml"]{glStencilOp manpage}.
}
@defproc[(glStencilOpSeparate (face exact-integer?) (sfail exact-integer?) (dpfail exact-integer?) (dppass exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glStencilOpSeparate.xml"]{glStencilOpSeparate manpage}.
}
@defproc[(glStencilOpSeparateATI (face exact-integer?) (sfail exact-integer?) (dpfail exact-integer?) (dppass exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/separate_stencil.txt"]{@racket[GL_ATI_separate_stencil]}.

Alias of @racket[glStencilOpSeparate].
}
@defproc[(glStencilOpValueAMD (face exact-integer?) (value exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/stencil_operation_extended.txt"]{@racket[GL_AMD_stencil_operation_extended]}.
}
@defproc[(glStencilStrokePathInstancedNV (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (reference exact-integer?) (mask exact-nonnegative-integer?) (transformType exact-integer?) (transformValues f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStencilStrokePathNV (path exact-nonnegative-integer?) (reference exact-integer?) (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glStopInstrumentsSGIX (marker exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glStringMarkerGREMEDY (len exact-integer?) (string gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/GREMEDY/string_marker.txt"]{@racket[GL_GREMEDY_string_marker]}.
}
@defproc[(glSwizzleEXT (res exact-nonnegative-integer?) (in exact-nonnegative-integer?) (outX exact-integer?) (outY exact-integer?) (outZ exact-integer?) (outW exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
