#lang scribble/manual
@title{glF...}
@defproc[(glFeedbackBuffer (size exact-integer?) (type exact-integer?)) f32vector?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFeedbackBuffer.xml"]{glFeedbackBuffer manpage}.
}
@defproc[(glFenceSync (condition exact-integer?) (flags exact-nonnegative-integer?)) GLsync?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFenceSync.xml"]{glFenceSync manpage}.
}
@defproc[(glFinalCombinerInputNV (variable exact-integer?) (input exact-integer?) (mapping exact-integer?) (componentUsage exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/register_combiners.txt"]{@racket[GL_NV_register_combiners]}.
}
@defproc[(glFinish) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFinish.xml"]{glFinish manpage}.
}
@defproc[(glFinishAsyncSGIX) (values exact-integer? exact-nonnegative-integer?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glFinishFenceAPPLE (fence exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glFinishFenceNV (fence exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glFinishObjectAPPLE (object exact-integer?) (name exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glFinishTextureSUNX) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SUNX/constant_data.txt"]{@racket[GL_SUNX_constant_data]}.
}
@defproc[(glFlush) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFlush.xml"]{glFlush manpage}.
}
@defproc[(glFlushMappedBufferRange (target exact-integer?) (offset exact-integer?) (length exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFlushMappedBufferRange.xml"]{glFlushMappedBufferRange manpage}.
}
@defproc[(glFlushMappedBufferRangeAPPLE (target exact-integer?) (offset exact-integer?) (size exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt"]{@racket[GL_APPLE_flush_buffer_range]}.

Alias of @racket[glFlushMappedBufferRange].
}
@defproc[(glFlushMappedNamedBufferRangeEXT (buffer exact-nonnegative-integer?) (offset exact-integer?) (length exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFlushPixelDataRangeNV (target exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/pixel_data_range.txt"]{@racket[GL_NV_pixel_data_range]}.
}
@defproc[(glFlushRasterSGIX) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/flush_raster.txt"]{@racket[GL_SGIX_flush_raster]}.
}
@defproc[(glFlushVertexArrayRangeAPPLE (length exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt"]{@racket[GL_APPLE_vertex_array_range]}.
}
@defproc[(glFlushVertexArrayRangeNV) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_array_range.txt"]{@racket[GL_NV_vertex_array_range]}.
}
@defproc[(glFogCoordFormatNV (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glFogCoordPointer (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoordPointer.xml"]{glFogCoordPointer manpage}.
}
@defproc[(glFogCoordPointerEXT (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordPointer].
}
@defproc[(glFogCoordPointerListIBM (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glFogCoordd (coord real?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoorddEXT (coord real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordd].
}
@defproc[(glFogCoorddv (coord real?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoorddvEXT (coord real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoorddv].
}
@defproc[(glFogCoordf (coord flonum?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoordfEXT (coord flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordf].
}
@defproc[(glFogCoordfv (coord flonum?)) any]{
Version 1.4.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml"]{glFogCoord manpage}.
}
@defproc[(glFogCoordfvEXT (coord flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/fog_coord.txt"]{@racket[GL_EXT_fog_coord]}.

Alias of @racket[glFogCoordfv].
}
@defproc[(glFogCoordhNV (fog (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glFogCoordhvNV (fog (integer-in 0 65535))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/half_float.txt"]{@racket[GL_NV_half_float]}.
}
@defproc[(glFogFuncSGIS (n exact-integer?) (points f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/fog_function.txt"]{@racket[GL_SGIS_fog_function]}.
}
@defproc[(glFogf (pname exact-integer?) (param flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogfv (pname exact-integer?) (params f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogi (pname exact-integer?) (param exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFogiv (pname exact-integer?) (params s32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml"]{glFog manpage}.
}
@defproc[(glFragmentColorMaterialSGIX (face exact-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelfSGIX (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelfvSGIX (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModeliSGIX (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightModelivSGIX (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightfSGIX (light exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightfvSGIX (light exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightiSGIX (light exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentLightivSGIX (light exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialfSGIX (face exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialfvSGIX (face exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialiSGIX (face exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFragmentMaterialivSGIX (face exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/fragment_lighting.txt"]{@racket[GL_SGIX_fragment_lighting]}.
}
@defproc[(glFrameTerminatorGREMEDY) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/GREMEDY/frame_terminator.txt"]{@racket[GL_GREMEDY_frame_terminator]}.
}
@defproc[(glFrameZoomSGIX (factor exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/framezoom.txt"]{@racket[GL_SGIX_framezoom]}.
}
@defproc[(glFramebufferDrawBufferEXT (framebuffer exact-nonnegative-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferDrawBuffersEXT (framebuffer exact-nonnegative-integer?) (n exact-integer?) (bufs (vectorof exact-integer?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferParameteri (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Version 4.3.
}
@defproc[(glFramebufferReadBufferEXT (framebuffer exact-nonnegative-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glFramebufferRenderbuffer (target exact-integer?) (attachment exact-integer?) (renderbuffertarget exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferRenderbuffer.xml"]{glFramebufferRenderbuffer manpage}.
}
@defproc[(glFramebufferRenderbufferEXT (target exact-integer?) (attachment exact-integer?) (renderbuffertarget exact-integer?) (renderbuffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferRenderbuffer].
}
@defproc[(glFramebufferTexture (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferTexture.xml"]{glFramebufferTexture manpage}.
}
@defproc[(glFramebufferTexture1D (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Version 3.0.
}
@defproc[(glFramebufferTexture1DEXT (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture1D].
}
@defproc[(glFramebufferTexture2D (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Version 3.0.
}
@defproc[(glFramebufferTexture2DEXT (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture2D].
}
@defproc[(glFramebufferTexture3D (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (zoffset exact-integer?)) any]{
Version 3.0.
}
@defproc[(glFramebufferTexture3DEXT (target exact-integer?) (attachment exact-integer?) (textarget exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (zoffset exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glFramebufferTexture3D].
}
@defproc[(glFramebufferTextureARB (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.
}
@defproc[(glFramebufferTextureEXT (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureARB].
}
@defproc[(glFramebufferTextureFaceARB (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (face exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.
}
@defproc[(glFramebufferTextureFaceEXT (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (face exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureFaceARB].
}
@defproc[(glFramebufferTextureLayer (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layer exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFramebufferTextureLayer.xml"]{glFramebufferTextureLayer manpage}.
}
@defproc[(glFramebufferTextureLayerARB (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layer exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.

Alias of @racket[glFramebufferTextureLayer].
}
@defproc[(glFramebufferTextureLayerEXT (target exact-integer?) (attachment exact-integer?) (texture exact-nonnegative-integer?) (level exact-integer?) (layer exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.

Alias of @racket[glFramebufferTextureLayer].
}
@defproc[(glFreeObjectBufferATI (buffer exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glFrontFace (mode exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glFrontFace.xml"]{glFrontFace manpage}.
}
@defproc[(glFrustum (left real?) (right real?) (bottom real?) (top real?) (zNear real?) (zFar real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glFrustum.xml"]{glFrustum manpage}.
}
