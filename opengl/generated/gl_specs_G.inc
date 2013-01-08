#lang scribble/manual
@title{glG...}
@defproc[(glGenAsyncMarkersSGIX (range exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glGenBuffers (n exact-integer?)) u32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenBuffers.xml"]{glGenBuffers manpage}.
}
@defproc[(glGenBuffersARB (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGenBuffers].
}
@defproc[(glGenFencesAPPLE (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glGenFencesNV (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glGenFragmentShadersATI (range exact-nonnegative-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glGenFramebuffers (n exact-integer?)) u32vector?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenFramebuffers.xml"]{glGenFramebuffers manpage}.
}
@defproc[(glGenFramebuffersEXT (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenFramebuffers].
}
@defproc[(glGenLists (range exact-integer?)) exact-nonnegative-integer?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGenLists.xml"]{glGenLists manpage}.
}
@defproc[(glGenNamesAMD (identifier exact-integer?) (num exact-nonnegative-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/name_gen_delete.txt"]{@racket[GL_AMD_name_gen_delete]}.
}
@defproc[(glGenOcclusionQueriesNV (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGenPathsNV (range exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGenPerfMonitorsAMD (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGenProgramPipelines (n exact-integer?)) u32vector?]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenProgramPipelines.xml"]{glGenProgramPipelines manpage}.
}
@defproc[(glGenProgramsARB (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGenProgramsNV (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGenProgramsARB].
}
@defproc[(glGenQueries (n exact-integer?)) u32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenQueries.xml"]{glGenQueries manpage}.
}
@defproc[(glGenQueriesARB (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGenQueries].
}
@defproc[(glGenRenderbuffers (n exact-integer?)) u32vector?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenRenderbuffers.xml"]{glGenRenderbuffers manpage}.
}
@defproc[(glGenRenderbuffersEXT (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenRenderbuffers].
}
@defproc[(glGenSamplers (count exact-integer?)) u32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenSamplers.xml"]{glGenSamplers manpage}.
}
@defproc[(glGenSymbolsEXT (datatype exact-integer?) (storagetype exact-integer?) (range exact-integer?) (components exact-nonnegative-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGenTextures (n exact-integer?)) u32vector?]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenTextures.xml"]{glGenTextures manpage}.
}
@defproc[(glGenTexturesEXT (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glGenTransformFeedbacks (n exact-integer?)) u32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenTransformFeedbacks.xml"]{glGenTransformFeedbacks manpage}.
}
@defproc[(glGenTransformFeedbacksNV (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glGenVertexArrays (n exact-integer?)) u32vector?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenVertexArrays.xml"]{glGenVertexArrays manpage}.
}
@defproc[(glGenVertexArraysAPPLE (n exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glGenVertexArray].
}
@defproc[(glGenVertexShadersEXT (range exact-nonnegative-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGenerateMipmap (target exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGenerateMipmap.xml"]{glGenerateMipmap manpage}.
}
@defproc[(glGenerateMipmapEXT (target exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGenerateMipmap].
}
@defproc[(glGenerateMultiTexMipmapEXT (texunit exact-integer?) (target exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGenerateTextureMipmapEXT (texture exact-nonnegative-integer?) (target exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetActiveAtomicCounterBufferiv (program exact-nonnegative-integer?) (bufferIndex exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAtomicCounterBufferiv.xml"]{glGetActiveAtomicCounterBufferiv manpage}.
}
@defproc[(glGetActiveAttrib (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveAttrib.xml"]{glGetActiveAttrib manpage}.
}
@defproc[(glGetActiveAttribARB (programObj exact-nonnegative-integer?) (index exact-nonnegative-integer?) (maxLength exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glGetActiveAttrib].
}
@defproc[(glGetActiveSubroutineName (program exact-nonnegative-integer?) (shadertype exact-integer?) (index exact-nonnegative-integer?) (bufsize exact-integer?)) (values exact-integer? bytes?)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineName.xml"]{glGetActiveSubroutineName manpage}.
}
@defproc[(glGetActiveSubroutineUniformName (program exact-nonnegative-integer?) (shadertype exact-integer?) (index exact-nonnegative-integer?) (bufsize exact-integer?)) (values exact-integer? bytes?)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineUniformName.xml"]{glGetActiveSubroutineUniformName manpage}.
}
@defproc[(glGetActiveSubroutineUniformiv (program exact-nonnegative-integer?) (shadertype exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveSubroutineUniform.xml"]{glGetActiveSubroutineUniform manpage}.
}
@defproc[(glGetActiveUniform (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniform.xml"]{glGetActiveUniform manpage}.
}
@defproc[(glGetActiveUniformARB (programObj exact-nonnegative-integer?) (index exact-nonnegative-integer?) (maxLength exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetActiveUniform].
}
@defproc[(glGetActiveUniformBlockName (program exact-nonnegative-integer?) (uniformBlockIndex exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformBlockName.xml"]{glGetActiveUniformBlockName manpage}.
}
@defproc[(glGetActiveUniformBlockiv (program exact-nonnegative-integer?) (uniformBlockIndex exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformBlock.xml"]{glGetActiveUniformBlock manpage}.
}
@defproc[(glGetActiveUniformName (program exact-nonnegative-integer?) (uniformIndex exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformName.xml"]{glGetActiveUniformName manpage}.
}
@defproc[(glGetActiveUniformsiv (program exact-nonnegative-integer?) (uniformCount exact-integer?) (uniformIndices u32vector?) (pname exact-integer?)) s32vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetActiveUniformsiv.xml"]{glGetActiveUniformsiv manpage}.
}
@defproc[(glGetActiveVaryingNV (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glGetArrayObjectfvATI (array exact-integer?) (pname exact-integer?)) flonum?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetArrayObjectivATI (array exact-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetAttachedObjectsARB (containerObj exact-nonnegative-integer?) (maxCount exact-integer?)) (values exact-integer? u32vector?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetAttachedShaders].
}
@defproc[(glGetAttachedShaders (program exact-nonnegative-integer?) (maxCount exact-integer?)) (values exact-integer? u32vector?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetAttachedShaders.xml"]{glGetAttachedShaders manpage}.
}
@defproc[(glGetAttribLocation (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetAttribLocation.xml"]{glGetAttribLocation manpage}.
}
@defproc[(glGetAttribLocationARB (programObj exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glGetAttribLocation].
}
@defproc[(glGetBooleanIndexedvEXT (target exact-integer?) (index exact-nonnegative-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glGetBooleani_v].
}
@defproc[(glGetBooleani_v (target exact-integer?) (index exact-nonnegative-integer?) (data cpointer?)) any]{
Version 3.0.
}
@defproc[(glGetBooleanv (pname exact-integer?)) (vectorof boolean?)]{
}
@defproc[(glGetBufferParameteri64v (target exact-integer?) (pname exact-integer?)) s64vector?]{
Version 3.2.
}
@defproc[(glGetBufferParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferParameteriv.xml"]{glGetBufferParameteriv manpage}.
}
@defproc[(glGetBufferParameterivARB (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferParameteriv].
}
@defproc[(glGetBufferParameterui64vNV (target exact-integer?) (pname exact-integer?)) u64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetBufferPointerv (target exact-integer?) (pname exact-integer?)) gl-pointer?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetBufferPointerv.xml"]{glGetBufferPointerv manpage}.
}
@defproc[(glGetBufferPointervARB (target exact-integer?) (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferPointerv].
}
@defproc[(glGetBufferSubData (target exact-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetBufferSubData.xml"]{glGetBufferSubData manpage}.
}
@defproc[(glGetBufferSubDataARB (target exact-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glGetBufferSubData].
}
@defproc[(glGetClipPlane (plane exact-integer?)) f64vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetClipPlane.xml"]{glGetClipPlane manpage}.
}
@defproc[(glGetColorTable (target exact-integer?) (format exact-integer?) (type exact-integer?) (table gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTable.xml"]{glGetColorTable manpage}.
}
@defproc[(glGetColorTableEXT (target exact-integer?) (format exact-integer?) (type exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterfv (target exact-integer?) (pname exact-integer?)) f32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml"]{glGetColorTableParameter manpage}.
}
@defproc[(glGetColorTableParameterfvEXT (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterfvSGI (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetColorTableParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml"]{glGetColorTableParameter manpage}.
}
@defproc[(glGetColorTableParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/paletted_texture.txt"]{@racket[GL_EXT_paletted_texture]}.
}
@defproc[(glGetColorTableParameterivSGI (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetColorTableSGI (target exact-integer?) (format exact-integer?) (type exact-integer?) (table gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGI/color_table.txt"]{@racket[GL_SGI_color_table]}.
}
@defproc[(glGetCombinerInputParameterfvNV (stage exact-integer?) (portion exact-integer?) (variable exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerInputParameterivNV (stage exact-integer?) (portion exact-integer?) (variable exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerOutputParameterfvNV (stage exact-integer?) (portion exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerOutputParameterivNV (stage exact-integer?) (portion exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetCombinerStageParameterfvNV (stage exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners2.txt"]{@racket[GL_NV_register_combiners2]}.
}
@defproc[(glGetCompressedMultiTexImageEXT (texunit exact-integer?) (target exact-integer?) (lod exact-integer?) (img gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetCompressedTexImage (target exact-integer?) (level exact-integer?) (img gl-pointer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetCompressedTexImage.xml"]{glGetCompressedTexImage manpage}.
}
@defproc[(glGetCompressedTexImageARB (target exact-integer?) (level exact-integer?) (img gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/texture_compression.txt"]{@racket[GL_ARB_texture_compression]}.

Alias of @racket[glGetCompressedTexImage].
}
@defproc[(glGetCompressedTextureImageEXT (texture exact-nonnegative-integer?) (target exact-integer?) (lod exact-integer?) (img gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetConvolutionFilter (target exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionFilter.xml"]{glGetConvolutionFilter manpage}.
}
@defproc[(glGetConvolutionFilterEXT (target exact-integer?) (format exact-integer?) (type exact-integer?) (image gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetConvolutionParameterfv (target exact-integer?) (pname exact-integer?)) f32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml"]{glGetConvolutionParameter manpage}.
}
@defproc[(glGetConvolutionParameterfvEXT (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetConvolutionParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml"]{glGetConvolutionParameter manpage}.
}
@defproc[(glGetConvolutionParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetDebugMessageLog (count exact-nonnegative-integer?) (bufsize exact-integer?) (sources s32vector?) (types s32vector?) (ids u32vector?) (severities s32vector?) (lengths s32vector?) (messageLog bytes?)) exact-nonnegative-integer?]{
Version 4.3.
}
@defproc[(glGetDebugMessageLogAMD (count exact-nonnegative-integer?) (bufsize exact-integer?)) (values exact-nonnegative-integer? s32vector? u32vector? u32vector? s32vector? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/debug_output.txt"]{@racket[GL_AMD_debug_output]}.
}
@defproc[(glGetDebugMessageLogARB (count exact-nonnegative-integer?) (bufsize exact-integer?)) (values exact-nonnegative-integer? s32vector? s32vector? u32vector? s32vector? s32vector? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/debug_output.txt"]{@racket[GL_ARB_debug_output]}.
}
@defproc[(glGetDetailTexFuncSGIS (target exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/detail_texture.txt"]{@racket[GL_SGIS_detail_texture]}.
}
@defproc[(glGetDoubleIndexedvEXT (target exact-integer?) (index exact-nonnegative-integer?) (data f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetDoublei_v (target exact-integer?) (index exact-nonnegative-integer?) (data f64vector?)) any]{
Version 4.1.
}
@defproc[(glGetDoublev (pname exact-integer?)) f64vector?]{
}
@defproc[(glGetError) exact-integer?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetError.xml"]{glGetError manpage}.
}
@defproc[(glGetFenceivNV (fence exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glGetFinalCombinerInputParameterfvNV (variable exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetFinalCombinerInputParameterivNV (variable exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glGetFloatIndexedvEXT (target exact-integer?) (index exact-nonnegative-integer?) (data f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetFloati_v (target exact-integer?) (index exact-nonnegative-integer?) (data f32vector?)) any]{
Version 4.1.
}
@defproc[(glGetFloatv (pname exact-integer?)) f32vector?]{
}
@defproc[(glGetFogFuncSGIS (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/fog_function.txt"]{@racket[GL_SGIS_fog_function]}.
}
@defproc[(glGetFragDataIndex (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFragDataIndex.xml"]{glGetFragDataIndex manpage}.
}
@defproc[(glGetFragDataLocation (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFragDataLocation.xml"]{glGetFragDataLocation manpage}.
}
@defproc[(glGetFragDataLocationEXT (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glGetFragDataLocation].
}
@defproc[(glGetFragmentLightfvSGIX (light exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentLightivSGIX (light exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentMaterialfvSGIX (face exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFragmentMaterialivSGIX (face exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glGetFramebufferAttachmentParameteriv (target exact-integer?) (attachment exact-integer?) (pname exact-integer?)) s32vector?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetFramebufferAttachmentParameter.xml"]{glGetFramebufferAttachmentParameter manpage}.
}
@defproc[(glGetFramebufferAttachmentParameterivEXT (target exact-integer?) (attachment exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGetFramebufferAttachmentParameteriv].
}
@defproc[(glGetFramebufferParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 4.3.
}
@defproc[(glGetFramebufferParameterivEXT (framebuffer exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetGraphicsResetStatusARB) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetHandleARB (pname exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetHistogram (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (the-values gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogram.xml"]{glGetHistogram manpage}.
}
@defproc[(glGetHistogramEXT (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (the-values gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetHistogramParameterfv (target exact-integer?) (pname exact-integer?)) f32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml"]{glGetHistogramParameter manpage}.
}
@defproc[(glGetHistogramParameterfvEXT (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetHistogramParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml"]{glGetHistogramParameter manpage}.
}
@defproc[(glGetHistogramParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetImageHandleNV (texture exact-nonnegative-integer?) (level exact-integer?) (layered boolean?) (layer exact-integer?) (format exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetImageTransformParameterfvHP (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glGetImageTransformParameterivHP (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glGetInfoLogARB (obj exact-nonnegative-integer?) (maxLength exact-integer?)) (values exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetInstrumentsSGIX) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glGetInteger64i_v (target exact-integer?) (index exact-nonnegative-integer?) (data s64vector?)) any]{
Version 3.2.
}
@defproc[(glGetInteger64v (pname exact-integer?)) s64vector?]{
Version 1.2.
}
@defproc[(glGetIntegerIndexedvEXT (target exact-integer?) (index exact-nonnegative-integer?) (data s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glGetIntegeri_v].
}
@defproc[(glGetIntegeri_v (target exact-integer?) (index exact-nonnegative-integer?) (data s32vector?)) any]{
Version 3.0.
}
@defproc[(glGetIntegerui64i_vNV (value exact-integer?) (index exact-nonnegative-integer?) (result u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glGetIntegerui64vNV (value exact-integer?) (result u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetIntegerv (pname exact-integer?)) s32vector?]{
}
@defproc[(glGetInternalformati64v (target exact-integer?) (internalformat exact-integer?) (pname exact-integer?) (bufSize exact-integer?)) s64vector?]{
Version 4.3.
}
@defproc[(glGetInternalformativ (target exact-integer?) (internalformat exact-integer?) (pname exact-integer?) (bufSize exact-integer?)) s32vector?]{
Version 4.2.
}
@defproc[(glGetInvariantBooleanvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetInvariantFloatvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetInvariantIntegervEXT (id exact-nonnegative-integer?) (value exact-integer?) (data s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLightfv (light exact-integer?) (pname exact-integer?)) f32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml"]{glGetLight manpage}.
}
@defproc[(glGetLightiv (light exact-integer?) (pname exact-integer?)) s32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml"]{glGetLight manpage}.
}
@defproc[(glGetListParameterfvSGIX (list exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glGetListParameterivSGIX (list exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/list_priority.txt"]{@racket[GL_SGIX_list_priority]}.
}
@defproc[(glGetLocalConstantBooleanvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLocalConstantFloatvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetLocalConstantIntegervEXT (id exact-nonnegative-integer?) (value exact-integer?) (data s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetMapAttribParameterfvNV (target exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapAttribParameterivNV (target exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapControlPointsNV (target exact-integer?) (index exact-nonnegative-integer?) (type exact-integer?) (ustride exact-integer?) (vstride exact-integer?) (packed boolean?) (points gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapParameterfvNV (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapParameterivNV (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/evaluators.txt"]{@racket[GL_NV_evaluators]}.
}
@defproc[(glGetMapdv (target exact-integer?) (query exact-integer?) (v f64vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMapfv (target exact-integer?) (query exact-integer?) (v f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMapiv (target exact-integer?) (query exact-integer?) (v s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml"]{glGetMap manpage}.
}
@defproc[(glGetMaterialfv (face exact-integer?) (pname exact-integer?)) f32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml"]{glGetMaterial manpage}.
}
@defproc[(glGetMaterialiv (face exact-integer?) (pname exact-integer?)) s32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml"]{glGetMaterial manpage}.
}
@defproc[(glGetMinmax (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (the-values gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmax.xml"]{glGetMinmax manpage}.
}
@defproc[(glGetMinmaxEXT (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (the-values gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMinmaxParameterfv (target exact-integer?) (pname exact-integer?)) f32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml"]{glGetMinmaxParameter manpage}.
}
@defproc[(glGetMinmaxParameterfvEXT (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMinmaxParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml"]{glGetMinmaxParameter manpage}.
}
@defproc[(glGetMinmaxParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/histogram.txt"]{@racket[GL_EXT_histogram]}.
}
@defproc[(glGetMultiTexEnvfvEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexEnvivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGendvEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGenfvEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexGenivEXT (texunit exact-integer?) (coord exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexImageEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexLevelParameterfvEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexLevelParameterivEXT (texunit exact-integer?) (target exact-integer?) (level exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterIivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterIuivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterfvEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultiTexParameterivEXT (texunit exact-integer?) (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetMultisamplefv (pname exact-integer?) (index exact-nonnegative-integer?)) f32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetMultisample.xml"]{glGetMultisample manpage}.
}
@defproc[(glGetMultisamplefvNV (pname exact-integer?) (index exact-nonnegative-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/explicit_multisample.txt"]{@racket[GL_NV_explicit_multisample]}.
}
@defproc[(glGetNamedBufferParameterivEXT (buffer exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedBufferParameterui64vNV (buffer exact-nonnegative-integer?) (pname exact-integer?)) u64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetNamedBufferPointervEXT (buffer exact-nonnegative-integer?) (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedBufferSubDataEXT (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedFramebufferAttachmentParameterivEXT (framebuffer exact-nonnegative-integer?) (attachment exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedFramebufferParameterivEXT (framebuffer exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt"]{@racket[GL_ARB_framebuffer_no_attachments]}.
}
@defproc[(glGetNamedProgramLocalParameterIivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterIuivEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterdvEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramLocalParameterfvEXT (program exact-nonnegative-integer?) (target exact-integer?) (index exact-nonnegative-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramStringEXT (program exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?) (string gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedProgramivEXT (program exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedRenderbufferParameterivEXT (renderbuffer exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetNamedStringARB (namelen exact-integer?) (name (or/c string? bytes?)) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glGetNamedStringivARB (namelen exact-integer?) (name (or/c string? bytes?)) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glGetObjectBufferfvATI (buffer exact-nonnegative-integer?) (pname exact-integer?)) flonum?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetObjectBufferivATI (buffer exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetObjectLabel (identifier exact-integer?) (name exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 4.3.
}
@defproc[(glGetObjectParameterfvARB (obj exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetObjectParameterivAPPLE (objectType exact-integer?) (name exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/object_purgeable.txt"]{@racket[GL_APPLE_object_purgeable]}.
}
@defproc[(glGetObjectParameterivARB (obj exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.
}
@defproc[(glGetObjectPtrLabel (ptr gl-pointer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 4.3.
}
@defproc[(glGetOcclusionQueryivNV (id exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGetOcclusionQueryuivNV (id exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glGetPathColorGenfvNV (color exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathColorGenivNV (color exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathCommandsNV (path exact-nonnegative-integer?) (commands bytes?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathCoordsNV (path exact-nonnegative-integer?) (coords f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathDashArrayNV (path exact-nonnegative-integer?) (dashArray f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathLengthNV (path exact-nonnegative-integer?) (startSegment exact-integer?) (numSegments exact-integer?)) flonum?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathMetricRangeNV (metricQueryMask exact-nonnegative-integer?) (firstPathName exact-nonnegative-integer?) (numPaths exact-integer?) (stride exact-integer?) (metrics f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathMetricsNV (metricQueryMask exact-nonnegative-integer?) (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (stride exact-integer?) (metrics f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathParameterfvNV (path exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathParameterivNV (path exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathSpacingNV (pathListMode exact-integer?) (numPaths exact-integer?) (pathNameType exact-integer?) (paths gl-pointer?) (pathBase exact-nonnegative-integer?) (advanceScale flonum?) (kerningScale flonum?) (transformType exact-integer?) (returnedSpacing f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathTexGenfvNV (texCoordSet exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPathTexGenivNV (texCoordSet exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glGetPerfMonitorCounterDataAMD (monitor exact-nonnegative-integer?) (pname exact-integer?) (dataSize exact-integer?)) (values u32vector? exact-integer?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCounterInfoAMD (group exact-nonnegative-integer?) (counter exact-nonnegative-integer?) (pname exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCounterStringAMD (group exact-nonnegative-integer?) (counter exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorCountersAMD (group exact-nonnegative-integer?) (counterSize exact-integer?)) (values exact-integer? exact-integer? u32vector?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorGroupStringAMD (group exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPerfMonitorGroupsAMD (groupsSize exact-integer?)) (values exact-integer? u32vector?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glGetPixelMapfv (map exact-integer?) (the-values f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelMapuiv (map exact-integer?) (the-values u32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelMapusv (map exact-integer?) (the-values u16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml"]{glGetPixelMap manpage}.
}
@defproc[(glGetPixelTexGenParameterfvSGIS (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glGetPixelTexGenParameterivSGIS (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glGetPixelTransformParameterfvEXT (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glGetPixelTransformParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glGetPointerIndexedvEXT (target exact-integer?) (index exact-nonnegative-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetPointerv (pname exact-integer?)) gl-pointer?]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPointerv.xml"]{glGetPointerv manpage}.
}
@defproc[(glGetPointervEXT (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.

Alias of @racket[glGetPointerv].
}
@defproc[(glGetPolygonStipple (mask bytes?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetPolygonStipple.xml"]{glGetPolygonStipple manpage}.
}
@defproc[(glGetProgramBinary (program exact-nonnegative-integer?) (bufSize exact-integer?) (binary gl-pointer?)) (values exact-integer? exact-integer?)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramBinary.xml"]{glGetProgramBinary manpage}.
}
@defproc[(glGetProgramEnvParameterIivNV (target exact-integer?) (index exact-nonnegative-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramEnvParameterIuivNV (target exact-integer?) (index exact-nonnegative-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramEnvParameterdvARB (target exact-integer?) (index exact-nonnegative-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramEnvParameterfvARB (target exact-integer?) (index exact-nonnegative-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramInfoLog (program exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramInfoLog.xml"]{glGetProgramInfoLog manpage}.
}
@defproc[(glGetProgramInterfaceiv (program exact-nonnegative-integer?) (programInterface exact-integer?) (pname exact-integer?)) s32vector?]{
Version 4.3.
}
@defproc[(glGetProgramLocalParameterIivNV (target exact-integer?) (index exact-nonnegative-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramLocalParameterIuivNV (target exact-integer?) (index exact-nonnegative-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glGetProgramLocalParameterdvARB (target exact-integer?) (index exact-nonnegative-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramLocalParameterfvARB (target exact-integer?) (index exact-nonnegative-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramNamedParameterdvNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?))) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glGetProgramNamedParameterfvNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?))) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glGetProgramParameterdvNV (target exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramParameterfvNV (target exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramPipelineInfoLog (pipeline exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramPipelineInfoLog.xml"]{glGetProgramPipelineInfoLog manpage}.
}
@defproc[(glGetProgramPipelineiv (pipeline exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramPipeline.xml"]{glGetProgramPipeline manpage}.
}
@defproc[(glGetProgramResourceIndex (program exact-nonnegative-integer?) (programInterface exact-integer?) (name (or/c string? bytes?))) exact-nonnegative-integer?]{
Version 4.3.
}
@defproc[(glGetProgramResourceLocation (program exact-nonnegative-integer?) (programInterface exact-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 4.3.
}
@defproc[(glGetProgramResourceLocationIndex (program exact-nonnegative-integer?) (programInterface exact-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 4.3.
}
@defproc[(glGetProgramResourceName (program exact-nonnegative-integer?) (programInterface exact-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 4.3.
}
@defproc[(glGetProgramResourceiv (program exact-nonnegative-integer?) (programInterface exact-integer?) (index exact-nonnegative-integer?) (propCount exact-integer?) (props s32vector?) (bufSize exact-integer?)) (values exact-integer? s32vector?)]{
Version 4.3.
}
@defproc[(glGetProgramStageiv (program exact-nonnegative-integer?) (shadertype exact-integer?) (pname exact-integer?)) exact-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgramStage.xml"]{glGetProgramStage manpage}.
}
@defproc[(glGetProgramStringARB (target exact-integer?) (pname exact-integer?) (string gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramStringNV (id exact-nonnegative-integer?) (pname exact-integer?) (program bytes?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetProgramSubroutineParameteruivNV (target exact-integer?) (index exact-nonnegative-integer?) (param u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program5.txt"]{@racket[GL_NV_gpu_program5]}.
}
@defproc[(glGetProgramiv (program exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetProgram.xml"]{glGetProgram manpage}.
}
@defproc[(glGetProgramivARB (target exact-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glGetProgramivNV (id exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetQueryIndexediv (target exact-integer?) (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryIndexed.xml"]{glGetQueryIndexed manpage}.
}
@defproc[(glGetQueryObjecti64v (id exact-nonnegative-integer?) (pname exact-integer?)) s64vector?]{
Version 1.2.
}
@defproc[(glGetQueryObjecti64vEXT (id exact-nonnegative-integer?) (pname exact-integer?)) s64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/timer_query.txt"]{@racket[GL_EXT_timer_query]}.
}
@defproc[(glGetQueryObjectiv (id exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryObject.xml"]{glGetQueryObject manpage}.
}
@defproc[(glGetQueryObjectivARB (id exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryObjectiv].
}
@defproc[(glGetQueryObjectui64v (id exact-nonnegative-integer?) (pname exact-integer?)) u64vector?]{
Version 1.2.
}
@defproc[(glGetQueryObjectui64vEXT (id exact-nonnegative-integer?) (pname exact-integer?)) u64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/timer_query.txt"]{@racket[GL_EXT_timer_query]}.
}
@defproc[(glGetQueryObjectuiv (id exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryObject.xml"]{glGetQueryObject manpage}.
}
@defproc[(glGetQueryObjectuivARB (id exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryObjectuiv].
}
@defproc[(glGetQueryiv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetQueryiv.xml"]{glGetQueryiv manpage}.
}
@defproc[(glGetQueryivARB (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glGetQueryiv].
}
@defproc[(glGetRenderbufferParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetRenderbufferParameter.xml"]{glGetRenderbufferParameter manpage}.
}
@defproc[(glGetRenderbufferParameterivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glGetRenderbufferParameteriv].
}
@defproc[(glGetSamplerParameterIiv (sampler exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.
}
@defproc[(glGetSamplerParameterIuiv (sampler exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Version 1.2.
}
@defproc[(glGetSamplerParameterfv (sampler exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSamplerParameter.xml"]{glGetSamplerParameter manpage}.
}
@defproc[(glGetSamplerParameteriv (sampler exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSamplerParameter.xml"]{glGetSamplerParameter manpage}.
}
@defproc[(glGetSeparableFilter (target exact-integer?) (format exact-integer?) (type exact-integer?) (row gl-pointer?) (column gl-pointer?) (span gl-pointer?)) any]{
Version 1.2.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetSeparableFilter.xml"]{glGetSeparableFilter manpage}.
}
@defproc[(glGetSeparableFilterEXT (target exact-integer?) (format exact-integer?) (type exact-integer?) (row gl-pointer?) (column gl-pointer?) (span gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/convolution.txt"]{@racket[GL_EXT_convolution]}.
}
@defproc[(glGetShaderInfoLog (shader exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderInfoLog.xml"]{glGetShaderInfoLog manpage}.
}
@defproc[(glGetShaderPrecisionFormat (shadertype exact-integer?) (precisiontype exact-integer?)) (values s32vector? exact-integer?)]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderPrecisionFormat.xml"]{glGetShaderPrecisionFormat manpage}.
}
@defproc[(glGetShaderSource (shader exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? bytes?)]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShaderSource.xml"]{glGetShaderSource manpage}.
}
@defproc[(glGetShaderSourceARB (obj exact-nonnegative-integer?) (maxLength exact-integer?)) (values exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetShaderSource].
}
@defproc[(glGetShaderiv (shader exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetShader.xml"]{glGetShader manpage}.
}
@defproc[(glGetSharpenTexFuncSGIS (target exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt"]{@racket[GL_SGIS_sharpen_texture]}.
}
@defproc[(glGetString (name exact-integer?)) (or/c string? bytes?)]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetString.xml"]{glGetString manpage}.
}
@defproc[(glGetStringi (name exact-integer?) (index exact-nonnegative-integer?)) (or/c string? bytes?)]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetString.xml"]{glGetString manpage}.
}
@defproc[(glGetSubroutineIndex (program exact-nonnegative-integer?) (shadertype exact-integer?) (name (or/c string? bytes?))) exact-nonnegative-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSubroutineIndex.xml"]{glGetSubroutineIndex manpage}.
}
@defproc[(glGetSubroutineUniformLocation (program exact-nonnegative-integer?) (shadertype exact-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSubroutineUniformLocation.xml"]{glGetSubroutineUniformLocation manpage}.
}
@defproc[(glGetSynciv (sync GLsync?) (pname exact-integer?) (bufSize exact-integer?)) (values exact-integer? s32vector?)]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetSync.xml"]{glGetSync manpage}.
}
@defproc[(glGetTexBumpParameterfvATI (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glGetTexBumpParameterivATI (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt"]{@racket[GL_ATI_envmap_bumpmap]}.
}
@defproc[(glGetTexEnvfv (target exact-integer?) (pname exact-integer?)) f32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml"]{glGetTexEnv manpage}.
}
@defproc[(glGetTexEnviv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml"]{glGetTexEnv manpage}.
}
@defproc[(glGetTexFilterFuncSGIS (target exact-integer?) (filter exact-integer?) (weights f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/texture_filter4.txt"]{@racket[GL_SGIS_texture_filter4]}.
}
@defproc[(glGetTexGendv (coord exact-integer?) (pname exact-integer?)) f64vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexGenfv (coord exact-integer?) (pname exact-integer?)) f32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexGeniv (coord exact-integer?) (pname exact-integer?)) s32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml"]{glGetTexGen manpage}.
}
@defproc[(glGetTexImage (target exact-integer?) (level exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexImage.xml"]{glGetTexImage manpage}.
}
@defproc[(glGetTexLevelParameterfv (target exact-integer?) (level exact-integer?) (pname exact-integer?)) f32vector?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexLevelParameter.xml"]{glGetTexLevelParameter manpage}.
}
@defproc[(glGetTexLevelParameteriv (target exact-integer?) (level exact-integer?) (pname exact-integer?)) s32vector?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexLevelParameter.xml"]{glGetTexLevelParameter manpage}.
}
@defproc[(glGetTexParameterIiv (target exact-integer?) (pname exact-integer?)) s32vector?]{
Version 3.0.
}
@defproc[(glGetTexParameterIivEXT (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glGetTexParameterIiv].
}
@defproc[(glGetTexParameterIuiv (target exact-integer?) (pname exact-integer?)) u32vector?]{
Version 3.0.
}
@defproc[(glGetTexParameterIuivEXT (target exact-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_integer.txt"]{@racket[GL_EXT_texture_integer]}.

Alias of @racket[glGetTexParameterIuiv].
}
@defproc[(glGetTexParameterPointervAPPLE (target exact-integer?) (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/texture_range.txt"]{@racket[GL_APPLE_texture_range]}.
}
@defproc[(glGetTexParameterfv (target exact-integer?) (pname exact-integer?)) f32vector?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexParameter.xml"]{glGetTexParameter manpage}.
}
@defproc[(glGetTexParameteriv (target exact-integer?) (pname exact-integer?)) s32vector?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTexParameter.xml"]{glGetTexParameter manpage}.
}
@defproc[(glGetTextureHandleNV (texture exact-nonnegative-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetTextureImageEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (format exact-integer?) (type exact-integer?) (pixels gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureLevelParameterfvEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureLevelParameterivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (level exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterIivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterIuivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterfvEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureParameterivEXT (texture exact-nonnegative-integer?) (target exact-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glGetTextureSamplerHandleNV (texture exact-nonnegative-integer?) (sampler exact-nonnegative-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glGetTrackMatrixivNV (target exact-integer?) (address exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glGetTransformFeedbackVarying (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetTransformFeedbackVarying.xml"]{glGetTransformFeedbackVarying manpage}.
}
@defproc[(glGetTransformFeedbackVaryingEXT (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (bufSize exact-integer?)) (values exact-integer? exact-integer? exact-integer? bytes?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glGetTransformFeedbackVarying].
}
@defproc[(glGetTransformFeedbackVaryingNV (program exact-nonnegative-integer?) (index exact-nonnegative-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glGetTransformFeedbackVarying].
}
@defproc[(glGetUniformBlockIndex (program exact-nonnegative-integer?) (uniformBlockName (or/c string? bytes?))) exact-nonnegative-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformBlockIndex.xml"]{glGetUniformBlockIndex manpage}.
}
@defproc[(glGetUniformBufferSizeEXT (program exact-nonnegative-integer?) (location exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glGetUniformIndices (program exact-nonnegative-integer?) (uniformCount exact-integer?) (uniformNames (vectorof (or/c string? bytes?)))) u32vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformIndices.xml"]{glGetUniformIndices manpage}.
}
@defproc[(glGetUniformLocation (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformLocation.xml"]{glGetUniformLocation manpage}.
}
@defproc[(glGetUniformLocationARB (programObj exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformLocation].
}
@defproc[(glGetUniformOffsetEXT (program exact-nonnegative-integer?) (location exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/bindable_uniform.txt"]{@racket[GL_EXT_bindable_uniform]}.
}
@defproc[(glGetUniformSubroutineuiv (shadertype exact-integer?) (location exact-integer?)) exact-nonnegative-integer?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniformSubroutine.xml"]{glGetUniformSubroutine manpage}.
}
@defproc[(glGetUniformdv (program exact-nonnegative-integer?) (location exact-integer?) (params f64vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformfv (program exact-nonnegative-integer?) (location exact-integer?) (params f32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformfvARB (programObj exact-nonnegative-integer?) (location exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformfv].
}
@defproc[(glGetUniformi64vNV (program exact-nonnegative-integer?) (location exact-integer?) (params s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glGetUniformiv (program exact-nonnegative-integer?) (location exact-integer?) (params s32vector?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformivARB (programObj exact-nonnegative-integer?) (location exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glGetUniformiv].
}
@defproc[(glGetUniformui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (params u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glGetUniformuiv (program exact-nonnegative-integer?) (location exact-integer?) (params u32vector?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetUniform.xml"]{glGetUniform manpage}.
}
@defproc[(glGetUniformuivEXT (program exact-nonnegative-integer?) (location exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glGetUniformuiv].
}
@defproc[(glGetVariantArrayObjectfvATI (id exact-nonnegative-integer?) (pname exact-integer?)) flonum?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetVariantArrayObjectivATI (id exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glGetVariantBooleanvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantFloatvEXT (id exact-nonnegative-integer?) (value exact-integer?) (data f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantIntegervEXT (id exact-nonnegative-integer?) (value exact-integer?) (data s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVariantPointervEXT (id exact-nonnegative-integer?) (value exact-integer?) (data cpointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glGetVaryingLocationNV (program exact-nonnegative-integer?) (name (or/c string? bytes?))) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glGetVertexAttribArrayObjectfvATI (index exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glGetVertexAttribArrayObjectivATI (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt"]{@racket[GL_ATI_vertex_attrib_array_object]}.
}
@defproc[(glGetVertexAttribIiv (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 3.0.
}
@defproc[(glGetVertexAttribIivEXT (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glGetVertexAttribIiv].
}
@defproc[(glGetVertexAttribIuiv (index exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Version 3.0.
}
@defproc[(glGetVertexAttribIuivEXT (index exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program4.txt"]{@racket[GL_NV_vertex_program4]}.

Alias of @racket[glGetVertexAttribIuiv].
}
@defproc[(glGetVertexAttribLdv (index exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Version 4.1.
}
@defproc[(glGetVertexAttribLdvEXT (index exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt"]{@racket[GL_EXT_vertex_attrib_64bit]}.
}
@defproc[(glGetVertexAttribLi64vNV (index exact-nonnegative-integer?) (pname exact-integer?)) s64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glGetVertexAttribLui64vNV (index exact-nonnegative-integer?) (pname exact-integer?)) u64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt"]{@racket[GL_NV_vertex_attrib_integer_64bit]}.
}
@defproc[(glGetVertexAttribPointerv (index exact-nonnegative-integer?) (pname exact-integer?)) gl-pointer?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttribPointerv.xml"]{glGetVertexAttribPointerv manpage}.
}
@defproc[(glGetVertexAttribPointervARB (index exact-nonnegative-integer?) (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribPointerv].
}
@defproc[(glGetVertexAttribPointervNV (index exact-nonnegative-integer?) (pname exact-integer?)) gl-pointer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribPointerv].
}
@defproc[(glGetVertexAttribdv (index exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribdvARB (index exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribdv].
}
@defproc[(glGetVertexAttribdvNV (index exact-nonnegative-integer?) (pname exact-integer?)) real?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribdv].
}
@defproc[(glGetVertexAttribfv (index exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribfvARB (index exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribfv].
}
@defproc[(glGetVertexAttribfvNV (index exact-nonnegative-integer?) (pname exact-integer?)) flonum?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribfv].
}
@defproc[(glGetVertexAttribiv (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glGetVertexAttrib.xml"]{glGetVertexAttrib manpage}.
}
@defproc[(glGetVertexAttribivARB (index exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glGetVertexAttribiv].
}
@defproc[(glGetVertexAttribivNV (index exact-nonnegative-integer?) (pname exact-integer?)) exact-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glGetVertexAttribiv].
}
@defproc[(glGetVideoCaptureStreamdvNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureStreamfvNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureStreamivNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoCaptureivNV (video_capture_slot exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glGetVideoi64vNV (video_slot exact-nonnegative-integer?) (pname exact-integer?)) s64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideoivNV (video_slot exact-nonnegative-integer?) (pname exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideoui64vNV (video_slot exact-nonnegative-integer?) (pname exact-integer?)) u64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetVideouivNV (video_slot exact-nonnegative-integer?) (pname exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glGetnColorTableARB (target exact-integer?) (format exact-integer?) (type exact-integer?) (bufSize exact-integer?) (table gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnCompressedTexImageARB (target exact-integer?) (lod exact-integer?) (bufSize exact-integer?) (img gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnConvolutionFilterARB (target exact-integer?) (format exact-integer?) (type exact-integer?) (bufSize exact-integer?) (image gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnHistogramARB (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (bufSize exact-integer?) (the-values gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapdvARB (target exact-integer?) (query exact-integer?) (bufSize exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapfvARB (target exact-integer?) (query exact-integer?) (bufSize exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMapivARB (target exact-integer?) (query exact-integer?) (bufSize exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnMinmaxARB (target exact-integer?) (reset boolean?) (format exact-integer?) (type exact-integer?) (bufSize exact-integer?) (the-values gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapfvARB (map exact-integer?) (bufSize exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapuivARB (map exact-integer?) (bufSize exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPixelMapusvARB (map exact-integer?) (bufSize exact-integer?)) u16vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnPolygonStippleARB (bufSize exact-integer?)) bytes?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnSeparableFilterARB (target exact-integer?) (format exact-integer?) (type exact-integer?) (rowBufSize exact-integer?) (row gl-pointer?) (columnBufSize exact-integer?) (column gl-pointer?) (span gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnTexImageARB (target exact-integer?) (level exact-integer?) (format exact-integer?) (type exact-integer?) (bufSize exact-integer?) (img gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformdvARB (program exact-nonnegative-integer?) (location exact-integer?) (bufSize exact-integer?)) f64vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformfvARB (program exact-nonnegative-integer?) (location exact-integer?) (bufSize exact-integer?)) f32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformivARB (program exact-nonnegative-integer?) (location exact-integer?) (bufSize exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGetnUniformuivARB (program exact-nonnegative-integer?) (location exact-integer?) (bufSize exact-integer?)) u32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/robustness.txt"]{@racket[GL_ARB_robustness]}.
}
@defproc[(glGlobalAlphaFactorbSUN (factor (integer-in -128 127))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactordSUN (factor real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorfSUN (factor flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactoriSUN (factor exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorsSUN (factor (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorubSUN (factor (integer-in 0 255))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactoruiSUN (factor exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
@defproc[(glGlobalAlphaFactorusSUN (factor (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUN/global_alpha.txt"]{@racket[GL_SUN_global_alpha]}.
}
