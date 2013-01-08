#lang scribble/manual
@title{glB...}
@defproc[(glBegin (mode exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml"]{glBegin manpage}.
}
@defproc[(glBeginConditionalRender (id exact-nonnegative-integer?) (mode exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginConditionalRender.xml"]{glBeginConditionalRender manpage}.
}
@defproc[(glBeginConditionalRenderNV (id exact-nonnegative-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/conditional_render.txt"]{@racket[GL_NV_conditional_render]}.

Alias of @racket[glBeginConditionalRender].
}
@defproc[(glBeginFragmentShaderATI) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glBeginOcclusionQueryNV (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glBeginPerfMonitorAMD (monitor exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/performance_monitor.txt"]{@racket[GL_AMD_performance_monitor]}.
}
@defproc[(glBeginQuery (target exact-integer?) (id exact-nonnegative-integer?)) any]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginQuery.xml"]{glBeginQuery manpage}.
}
@defproc[(glBeginQueryARB (target exact-integer?) (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glBeginQuery].
}
@defproc[(glBeginQueryIndexed (target exact-integer?) (index exact-nonnegative-integer?) (id exact-nonnegative-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginQueryIndexed.xml"]{glBeginQueryIndexed manpage}.
}
@defproc[(glBeginTransformFeedback (primitiveMode exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBeginTransformFeedback.xml"]{glBeginTransformFeedback manpage}.
}
@defproc[(glBeginTransformFeedbackEXT (primitiveMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBeginTransformFeedback].
}
@defproc[(glBeginTransformFeedbackNV (primitiveMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBeginTransformFeedback].
}
@defproc[(glBeginVertexShaderEXT) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBeginVideoCaptureNV (video_capture_slot exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBindAttribLocation (program exact-nonnegative-integer?) (index exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindAttribLocation.xml"]{glBindAttribLocation manpage}.
}
@defproc[(glBindAttribLocationARB (programObj exact-nonnegative-integer?) (index exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_shader.txt"]{@racket[GL_ARB_vertex_shader]}.

Alias of @racket[glBindAttribLocation].
}
@defproc[(glBindBuffer (target exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBuffer.xml"]{glBindBuffer manpage}.
}
@defproc[(glBindBufferARB (target exact-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBindBuffer].
}
@defproc[(glBindBufferBase (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBufferBase.xml"]{glBindBufferBase manpage}.
}
@defproc[(glBindBufferBaseEXT (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBindBufferBase].
}
@defproc[(glBindBufferBaseNV (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferBase].
}
@defproc[(glBindBufferOffsetEXT (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.
}
@defproc[(glBindBufferOffsetNV (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferOffsetEXT].
}
@defproc[(glBindBufferRange (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindBufferRange.xml"]{glBindBufferRange manpage}.
}
@defproc[(glBindBufferRangeEXT (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/transform_feedback.txt"]{@racket[GL_EXT_transform_feedback]}.

Alias of @racket[glBindBufferRange].
}
@defproc[(glBindBufferRangeNV (target exact-integer?) (index exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.

Alias of @racket[glBindBufferRange].
}
@defproc[(glBindFragDataLocation (program exact-nonnegative-integer?) (color exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFragDataLocation.xml"]{glBindFragDataLocation manpage}.
}
@defproc[(glBindFragDataLocationEXT (program exact-nonnegative-integer?) (color exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt"]{@racket[GL_EXT_gpu_shader4]}.

Alias of @racket[glBindFragDataLocation].
}
@defproc[(glBindFragDataLocationIndexed (program exact-nonnegative-integer?) (colorNumber exact-nonnegative-integer?) (index exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFragDataLocationIndexed.xml"]{glBindFragDataLocationIndexed manpage}.
}
@defproc[(glBindFragmentShaderATI (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glBindFramebuffer (target exact-integer?) (framebuffer exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindFramebuffer.xml"]{glBindFramebuffer manpage}.
}
@defproc[(glBindFramebufferEXT (target exact-integer?) (framebuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.
}
@defproc[(glBindImageTexture (unit exact-nonnegative-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layered boolean?) (layer exact-integer?) (access exact-integer?) (format exact-integer?)) any]{
Version 4.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindImageTexture.xml"]{glBindImageTexture manpage}.
}
@defproc[(glBindImageTextureEXT (index exact-nonnegative-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layered boolean?) (layer exact-integer?) (access exact-integer?) (format exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt"]{@racket[GL_EXT_shader_image_load_store]}.
}
@defproc[(glBindLightParameterEXT (light exact-integer?) (value exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindMaterialParameterEXT (face exact-integer?) (value exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindMultiTextureEXT (texunit exact-integer?) (target exact-integer?) (texture exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glBindParameterEXT (value exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindProgramARB (target exact-integer?) (program exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glBindProgramNV (target exact-integer?) (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glBindProgramARB].
}
@defproc[(glBindProgramPipeline (pipeline exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindProgramPipeline.xml"]{glBindProgramPipeline manpage}.
}
@defproc[(glBindRenderbuffer (target exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindRenderbuffer.xml"]{glBindRenderbuffer manpage}.
}
@defproc[(glBindRenderbufferEXT (target exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.
}
@defproc[(glBindSampler (unit exact-nonnegative-integer?) (sampler exact-nonnegative-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindSampler.xml"]{glBindSampler manpage}.
}
@defproc[(glBindTexGenParameterEXT (unit exact-integer?) (coord exact-integer?) (value exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindTexture (target exact-integer?) (texture exact-nonnegative-integer?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindTexture.xml"]{glBindTexture manpage}.
}
@defproc[(glBindTextureEXT (target exact-integer?) (texture exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.

Alias of @racket[glBindTexture].
}
@defproc[(glBindTextureUnitParameterEXT (unit exact-integer?) (value exact-integer?)) exact-nonnegative-integer?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindTransformFeedback (target exact-integer?) (id exact-nonnegative-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindTransformFeedback.xml"]{glBindTransformFeedback manpage}.
}
@defproc[(glBindTransformFeedbackNV (target exact-integer?) (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glBindVertexArray (array exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBindVertexArray.xml"]{glBindVertexArray manpage}.
}
@defproc[(glBindVertexArrayAPPLE (array exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glBindVertexArray].
}
@defproc[(glBindVertexBuffer (bindingindex exact-nonnegative-integer?) (buffer exact-nonnegative-integer?) (offset exact-integer?) (stride exact-integer?)) any]{
Version 4.3.
}
@defproc[(glBindVertexShaderEXT (id exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glBindVideoCaptureStreamBufferNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (frame_region exact-integer?) (offset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBindVideoCaptureStreamTextureNV (video_capture_slot exact-nonnegative-integer?) (stream exact-nonnegative-integer?) (frame_region exact-integer?) (target exact-integer?) (texture exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/video_capture.txt"]{@racket[GL_NV_video_capture]}.
}
@defproc[(glBinormal3bEXT (bx (integer-in -128 127)) (by (integer-in -128 127)) (bz (integer-in -128 127))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3bvEXT (v s8vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3dEXT (bx real?) (by real?) (bz real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3dvEXT (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3fEXT (bx flonum?) (by flonum?) (bz flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3fvEXT (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3iEXT (bx exact-integer?) (by exact-integer?) (bz exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3ivEXT (v s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3sEXT (bx (integer-in -32768 32767)) (by (integer-in -32768 32767)) (bz (integer-in -32768 32767))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormal3svEXT (v s16vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBinormalPointerEXT (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/coordinate_frame.txt"]{@racket[GL_EXT_coordinate_frame]}.
}
@defproc[(glBitmap (width exact-integer?) (height exact-integer?) (xorig flonum?) (yorig flonum?) (xmove flonum?) (ymove flonum?) (bitmap (or/c string? bytes?))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glBitmap.xml"]{glBitmap manpage}.
}
@defproc[(glBlendColor (red flonum?) (green flonum?) (blue flonum?) (alpha flonum?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendColor.xml"]{glBlendColor manpage}.
}
@defproc[(glBlendColorEXT (red flonum?) (green flonum?) (blue flonum?) (alpha flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_color.txt"]{@racket[GL_EXT_blend_color]}.

Alias of @racket[glBlendColor].
}
@defproc[(glBlendEquation (mode exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquation.xml"]{glBlendEquation manpage}.
}
@defproc[(glBlendEquationEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_minmax.txt"]{@racket[GL_EXT_blend_minmax]}.

Alias of @racket[glBlendEquation].
}
@defproc[(glBlendEquationIndexedAMD (buf exact-nonnegative-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendEquationSeparate (modeRGB exact-integer?) (modeAlpha exact-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquationSeparate.xml"]{glBlendEquationSeparate manpage}.
}
@defproc[(glBlendEquationSeparateEXT (modeRGB exact-integer?) (modeAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_equation_separate.txt"]{@racket[GL_EXT_blend_equation_separate]}.

Alias of @racket[glBlendEquationSeparate].
}
@defproc[(glBlendEquationSeparateIndexedAMD (buf exact-nonnegative-integer?) (modeRGB exact-integer?) (modeAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendEquationSeparatei (buf exact-nonnegative-integer?) (modeRGB exact-integer?) (modeAlpha exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquationSeparate.xml"]{glBlendEquationSeparate manpage}.
}
@defproc[(glBlendEquationSeparateiARB (buf exact-nonnegative-integer?) (modeRGB exact-integer?) (modeAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendEquationSeparatei].
}
@defproc[(glBlendEquationi (buf exact-nonnegative-integer?) (mode exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendEquation.xml"]{glBlendEquation manpage}.
}
@defproc[(glBlendEquationiARB (buf exact-nonnegative-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendEquationi].
}
@defproc[(glBlendFunc (sfactor exact-integer?) (dfactor exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFunc.xml"]{glBlendFunc manpage}.
}
@defproc[(glBlendFuncIndexedAMD (buf exact-nonnegative-integer?) (src exact-integer?) (dst exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendFuncSeparate (sfactorRGB exact-integer?) (dfactorRGB exact-integer?) (sfactorAlpha exact-integer?) (dfactorAlpha exact-integer?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFuncSeparate.xml"]{glBlendFuncSeparate manpage}.
}
@defproc[(glBlendFuncSeparateEXT (sfactorRGB exact-integer?) (dfactorRGB exact-integer?) (sfactorAlpha exact-integer?) (dfactorAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/blend_func_separate.txt"]{@racket[GL_EXT_blend_func_separate]}.

Alias of @racket[glBlendFuncSeparate].
}
@defproc[(glBlendFuncSeparateINGR (sfactorRGB exact-integer?) (dfactorRGB exact-integer?) (sfactorAlpha exact-integer?) (dfactorAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/INGR/blend_func_separate.txt"]{@racket[GL_INGR_blend_func_separate]}.

Alias of @racket[glBlendFuncSeparateEXT].
}
@defproc[(glBlendFuncSeparateIndexedAMD (buf exact-nonnegative-integer?) (srcRGB exact-integer?) (dstRGB exact-integer?) (srcAlpha exact-integer?) (dstAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt"]{@racket[GL_AMD_draw_buffers_blend]}.
}
@defproc[(glBlendFuncSeparatei (buf exact-nonnegative-integer?) (srcRGB exact-integer?) (dstRGB exact-integer?) (srcAlpha exact-integer?) (dstAlpha exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFuncSeparate.xml"]{glBlendFuncSeparate manpage}.
}
@defproc[(glBlendFuncSeparateiARB (buf exact-nonnegative-integer?) (srcRGB exact-integer?) (dstRGB exact-integer?) (srcAlpha exact-integer?) (dstAlpha exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendFuncSeparatei].
}
@defproc[(glBlendFunci (buf exact-nonnegative-integer?) (src exact-integer?) (dst exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlendFunc.xml"]{glBlendFunc manpage}.
}
@defproc[(glBlendFunciARB (buf exact-nonnegative-integer?) (src exact-integer?) (dst exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt"]{@racket[GL_ARB_draw_buffers_blend]}.

Alias of @racket[glBlendFunci].
}
@defproc[(glBlitFramebuffer (srcX0 exact-integer?) (srcY0 exact-integer?) (srcX1 exact-integer?) (srcY1 exact-integer?) (dstX0 exact-integer?) (dstY0 exact-integer?) (dstX1 exact-integer?) (dstY1 exact-integer?) (mask exact-nonnegative-integer?) (filter exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBlitFramebuffer.xml"]{glBlitFramebuffer manpage}.
}
@defproc[(glBlitFramebufferEXT (srcX0 exact-integer?) (srcY0 exact-integer?) (srcX1 exact-integer?) (srcY1 exact-integer?) (dstX0 exact-integer?) (dstY0 exact-integer?) (dstX1 exact-integer?) (dstY1 exact-integer?) (mask exact-nonnegative-integer?) (filter exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_blit.txt"]{@racket[GL_EXT_framebuffer_blit]}.

Alias of @racket[glBlitFramebuffer].
}
@defproc[(glBufferAddressRangeNV (pname exact-integer?) (index exact-nonnegative-integer?) (address exact-nonnegative-integer?) (length exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glBufferData (target exact-integer?) (size exact-integer?) (data gl-pointer?) (usage exact-integer?)) any]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBufferData.xml"]{glBufferData manpage}.
}
@defproc[(glBufferDataARB (target exact-integer?) (size exact-integer?) (data gl-pointer?) (usage exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBufferData].
}
@defproc[(glBufferParameteriAPPLE (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt"]{@racket[GL_APPLE_flush_buffer_range]}.
}
@defproc[(glBufferSubData (target exact-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glBufferSubData.xml"]{glBufferSubData manpage}.
}
@defproc[(glBufferSubDataARB (target exact-integer?) (offset exact-integer?) (size exact-integer?) (data gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glBufferSubData].
}
