#lang scribble/manual
@title{glP...}
@defproc[(glPNTrianglesfATI (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/pn_triangles.txt"]{@racket[GL_ATI_pn_triangles]}.
}
@defproc[(glPNTrianglesiATI (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/pn_triangles.txt"]{@racket[GL_ATI_pn_triangles]}.
}
@defproc[(glPassTexCoordATI (dst exact-nonnegative-integer?) (coord exact-nonnegative-integer?) (swizzle exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glPassThrough (token flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPassThrough.xml"]{glPassThrough manpage}.
}
@defproc[(glPatchParameterfv (pname exact-integer?) (the-values f32vector?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPatchParameter.xml"]{glPatchParameter manpage}.
}
@defproc[(glPatchParameteri (pname exact-integer?) (value exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPatchParameter.xml"]{glPatchParameter manpage}.
}
@defproc[(glPathColorGenNV (color exact-integer?) (genMode exact-integer?) (colorFormat exact-integer?) (coeffs f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCommandsNV (path exact-nonnegative-integer?) (numCommands exact-integer?) (commands (or/c string? bytes?)) (numCoords exact-integer?) (coordType exact-integer?) (coords gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCoordsNV (path exact-nonnegative-integer?) (numCoords exact-integer?) (coordType exact-integer?) (coords gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathCoverDepthFuncNV (func exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathDashArrayNV (path exact-nonnegative-integer?) (dashCount exact-integer?) (dashArray f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathFogGenNV (genMode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathGlyphRangeNV (firstPathName exact-nonnegative-integer?) (fontTarget exact-integer?) (fontName gl-pointer?) (fontStyle exact-nonnegative-integer?) (firstGlyph exact-nonnegative-integer?) (numGlyphs exact-integer?) (handleMissingGlyphs exact-integer?) (pathParameterTemplate exact-nonnegative-integer?) (emScale flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathGlyphsNV (firstPathName exact-nonnegative-integer?) (fontTarget exact-integer?) (fontName gl-pointer?) (fontStyle exact-nonnegative-integer?) (numGlyphs exact-integer?) (type exact-integer?) (charcodes gl-pointer?) (handleMissingGlyphs exact-integer?) (pathParameterTemplate exact-nonnegative-integer?) (emScale flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterfNV (path exact-nonnegative-integer?) (pname exact-integer?) (value flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterfvNV (path exact-nonnegative-integer?) (pname exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameteriNV (path exact-nonnegative-integer?) (pname exact-integer?) (value exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathParameterivNV (path exact-nonnegative-integer?) (pname exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStencilDepthOffsetNV (factor flonum?) (units flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStencilFuncNV (func exact-integer?) (ref exact-integer?) (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathStringNV (path exact-nonnegative-integer?) (format exact-integer?) (length exact-integer?) (pathString gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathSubCommandsNV (path exact-nonnegative-integer?) (commandStart exact-integer?) (commandsToDelete exact-integer?) (numCommands exact-integer?) (commands (or/c string? bytes?)) (numCoords exact-integer?) (coordType exact-integer?) (coords gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathSubCoordsNV (path exact-nonnegative-integer?) (coordStart exact-integer?) (numCoords exact-integer?) (coordType exact-integer?) (coords gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPathTexGenNV (texCoordSet exact-integer?) (genMode exact-integer?) (components exact-integer?) (coeffs f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPauseTransformFeedback) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPauseTransformFeedback.xml"]{glPauseTransformFeedback manpage}.
}
@defproc[(glPauseTransformFeedbackNV) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glPixelDataRangeNV (target exact-integer?) (length exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/pixel_data_range.txt"]{@racket[GL_NV_pixel_data_range]}.
}
@defproc[(glPixelMapfv (map exact-integer?) (mapsize exact-integer?) (the-values f32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelMapuiv (map exact-integer?) (mapsize exact-integer?) (the-values u32vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelMapusv (map exact-integer?) (mapsize exact-integer?) (the-values u16vector?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml"]{glPixelMap manpage}.
}
@defproc[(glPixelStoref (pname exact-integer?) (param flonum?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPixelStore.xml"]{glPixelStore manpage}.
}
@defproc[(glPixelStorei (pname exact-integer?) (param exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPixelStore.xml"]{glPixelStore manpage}.
}
@defproc[(glPixelTexGenParameterfSGIS (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameterfvSGIS (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameteriSGIS (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenParameterivSGIS (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/pixel_texture.txt"]{@racket[GL_SGIS_pixel_texture]}.
}
@defproc[(glPixelTexGenSGIX (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/pixel_texture.txt"]{@racket[GL_SGIX_pixel_texture]}.
}
@defproc[(glPixelTransferf (pname exact-integer?) (param flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml"]{glPixelTransfer manpage}.
}
@defproc[(glPixelTransferi (pname exact-integer?) (param exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml"]{glPixelTransfer manpage}.
}
@defproc[(glPixelTransformParameterfEXT (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameterfvEXT (target exact-integer?) (pname exact-integer?) (params flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameteriEXT (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelTransformParameterivEXT (target exact-integer?) (pname exact-integer?) (params exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/pixel_transform.txt"]{@racket[GL_EXT_pixel_transform]}.
}
@defproc[(glPixelZoom (xfactor flonum?) (yfactor flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPixelZoom.xml"]{glPixelZoom manpage}.
}
@defproc[(glPointAlongPathNV (path exact-nonnegative-integer?) (startSegment exact-integer?) (numSegments exact-integer?) (distance flonum?)) (values boolean? flonum? flonum? flonum? flonum?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glPointParameterf (pname exact-integer?) (param flonum?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterfARB (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/point_parameters.txt"]{@racket[GL_ARB_point_parameters]}.

Alias of @racket[glPointParameterf].
}
@defproc[(glPointParameterfEXT (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/point_parameters.txt"]{@racket[GL_EXT_point_parameters]}.

Alias of @racket[glPointParameterfARB].
}
@defproc[(glPointParameterfSGIS (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/point_parameters.txt"]{@racket[GL_SGIS_point_parameters]}.

Alias of @racket[glPointParameterfARB].
}
@defproc[(glPointParameterfv (pname exact-integer?) (params f32vector?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterfvARB (pname exact-integer?) (params flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/point_parameters.txt"]{@racket[GL_ARB_point_parameters]}.

Alias of @racket[glPointParameterfv].
}
@defproc[(glPointParameterfvEXT (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/point_parameters.txt"]{@racket[GL_EXT_point_parameters]}.

Alias of @racket[glPointParameterfvARB].
}
@defproc[(glPointParameterfvSGIS (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIS/point_parameters.txt"]{@racket[GL_SGIS_point_parameters]}.

Alias of @racket[glPointParameterfvARB].
}
@defproc[(glPointParameteri (pname exact-integer?) (param exact-integer?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameteriNV (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/point_sprite.txt"]{@racket[GL_NV_point_sprite]}.

Alias of @racket[glPointParameteri].
}
@defproc[(glPointParameteriv (pname exact-integer?) (params s32vector?)) any]{
Version 1.4.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointParameter.xml"]{glPointParameter manpage}.
}
@defproc[(glPointParameterivNV (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/point_sprite.txt"]{@racket[GL_NV_point_sprite]}.

Alias of @racket[glPointParameteriv].
}
@defproc[(glPointSize (size flonum?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPointSize.xml"]{glPointSize manpage}.
}
@defproc[(glPollAsyncSGIX) (values exact-integer? exact-nonnegative-integer?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glPollInstrumentsSGIX) (values exact-integer? exact-integer?)]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glPolygonMode (face exact-integer?) (mode exact-integer?)) any]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPolygonMode.xml"]{glPolygonMode manpage}.
}
@defproc[(glPolygonOffset (factor flonum?) (units flonum?)) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPolygonOffset.xml"]{glPolygonOffset manpage}.
}
@defproc[(glPolygonOffsetEXT (factor flonum?) (bias flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/polygon_offset.txt"]{@racket[GL_EXT_polygon_offset]}.
}
@defproc[(glPolygonStipple (mask (or/c string? bytes?))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPolygonStipple.xml"]{glPolygonStipple manpage}.
}
@defproc[(glPopAttrib) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopAttrib.xml"]{glPopAttrib manpage}.
}
@defproc[(glPopClientAttrib) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopClientAttrib.xml"]{glPopClientAttrib manpage}.
}
@defproc[(glPopDebugGroup) any]{
Version 4.3.
}
@defproc[(glPopMatrix) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopMatrix.xml"]{glPopMatrix manpage}.
}
@defproc[(glPopName) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPopName.xml"]{glPopName manpage}.
}
@defproc[(glPresentFrameDualFillNV (video_slot exact-nonnegative-integer?) (minPresentTime exact-nonnegative-integer?) (beginPresentTimeId exact-nonnegative-integer?) (presentDurationId exact-nonnegative-integer?) (type exact-integer?) (target0 exact-integer?) (fill0 exact-nonnegative-integer?) (target1 exact-integer?) (fill1 exact-nonnegative-integer?) (target2 exact-integer?) (fill2 exact-nonnegative-integer?) (target3 exact-integer?) (fill3 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glPresentFrameKeyedNV (video_slot exact-nonnegative-integer?) (minPresentTime exact-nonnegative-integer?) (beginPresentTimeId exact-nonnegative-integer?) (presentDurationId exact-nonnegative-integer?) (type exact-integer?) (target0 exact-integer?) (fill0 exact-nonnegative-integer?) (key0 exact-nonnegative-integer?) (target1 exact-integer?) (fill1 exact-nonnegative-integer?) (key1 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/present_video.txt"]{@racket[GL_NV_present_video]}.
}
@defproc[(glPrimitiveRestartIndex (index exact-nonnegative-integer?)) any]{
Version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glPrimitiveRestartIndex.xml"]{glPrimitiveRestartIndex manpage}.
}
@defproc[(glPrimitiveRestartIndexNV (index exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/primitive_restart.txt"]{@racket[GL_NV_primitive_restart]}.
}
@defproc[(glPrimitiveRestartNV) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/primitive_restart.txt"]{@racket[GL_NV_primitive_restart]}.
}
@defproc[(glPrioritizeTextures (n exact-integer?) (textures u32vector?) (priorities f32vector?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPrioritizeTextures.xml"]{glPrioritizeTextures manpage}.
}
@defproc[(glPrioritizeTexturesEXT (n exact-integer?) (textures u32vector?) (priorities f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.

Alias of @racket[glPrioritizeTextures].
}
@defproc[(glProgramBinary (program exact-nonnegative-integer?) (binaryFormat exact-integer?) (binary gl-pointer?) (length exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramBinary.xml"]{glProgramBinary manpage}.
}
@defproc[(glProgramBufferParametersIivNV (target exact-integer?) (buffer exact-nonnegative-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramBufferParametersIuivNV (target exact-integer?) (buffer exact-nonnegative-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramBufferParametersfvNV (target exact-integer?) (buffer exact-nonnegative-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt"]{@racket[GL_NV_parameter_buffer_object]}.
}
@defproc[(glProgramEnvParameter4dARB (target exact-integer?) (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4dvARB (target exact-integer?) (index exact-nonnegative-integer?) (params f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4fARB (target exact-integer?) (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameter4fvARB (target exact-integer?) (index exact-nonnegative-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramEnvParameterI4iNV (target exact-integer?) (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4ivNV (target exact-integer?) (index exact-nonnegative-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4uiNV (target exact-integer?) (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameterI4uivNV (target exact-integer?) (index exact-nonnegative-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParameters4fvEXT (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt"]{@racket[GL_EXT_gpu_program_parameters]}.
}
@defproc[(glProgramEnvParametersI4ivNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramEnvParametersI4uivNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameter4dARB (target exact-integer?) (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4dvARB (target exact-integer?) (index exact-nonnegative-integer?) (params f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4fARB (target exact-integer?) (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameter4fvARB (target exact-integer?) (index exact-nonnegative-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramLocalParameterI4iNV (target exact-integer?) (index exact-nonnegative-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4ivNV (target exact-integer?) (index exact-nonnegative-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4uiNV (target exact-integer?) (index exact-nonnegative-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameterI4uivNV (target exact-integer?) (index exact-nonnegative-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParameters4fvEXT (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt"]{@racket[GL_EXT_gpu_program_parameters]}.
}
@defproc[(glProgramLocalParametersI4ivNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramLocalParametersI4uivNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program4.txt"]{@racket[GL_NV_gpu_program4]}.
}
@defproc[(glProgramNamedParameter4dNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?)) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4dvNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?)) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4fNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?)) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramNamedParameter4fvNV (id exact-nonnegative-integer?) (len exact-integer?) (name (or/c string? bytes?)) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fragment_program.txt"]{@racket[GL_NV_fragment_program]}.
}
@defproc[(glProgramParameter4dNV (target exact-integer?) (index exact-nonnegative-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4dvNV (target exact-integer?) (index exact-nonnegative-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4fNV (target exact-integer?) (index exact-nonnegative-integer?) (x flonum?) (y flonum?) (z flonum?) (w flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameter4fvNV (target exact-integer?) (index exact-nonnegative-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameteri (program exact-nonnegative-integer?) (pname exact-integer?) (value exact-integer?)) any]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramParameter.xml"]{glProgramParameter manpage}.
}
@defproc[(glProgramParameteriARB (program exact-nonnegative-integer?) (pname exact-integer?) (value exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/geometry_shader4.txt"]{@racket[GL_ARB_geometry_shader4]}.

Alias of @racket[glProgramParameteri].
}
@defproc[(glProgramParameteriEXT (program exact-nonnegative-integer?) (pname exact-integer?) (value exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/geometry_shader4.txt"]{@racket[GL_EXT_geometry_shader4]}.

Alias of @racket[glProgramParameteriARB].
}
@defproc[(glProgramParameters4dvNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (v f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramParameters4fvNV (target exact-integer?) (index exact-nonnegative-integer?) (count exact-integer?) (v f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glProgramStringARB (target exact-integer?) (format exact-integer?) (len exact-integer?) (string gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.
}
@defproc[(glProgramSubroutineParametersuivNV (target exact-integer?) (count exact-integer?) (params u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_program5.txt"]{@racket[GL_NV_gpu_program5]}.
}
@defproc[(glProgramUniform1d (program exact-nonnegative-integer?) (location exact-integer?) (v0 real?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1dEXT (program exact-nonnegative-integer?) (location exact-integer?) (x real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1f (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1fEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1i (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1i64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1i64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1iEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1iv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1ivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1ui (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1ui64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1ui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform1uiEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform1uiv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform1uivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2d (program exact-nonnegative-integer?) (location exact-integer?) (v0 real?) (v1 real?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2dEXT (program exact-nonnegative-integer?) (location exact-integer?) (x real?) (y real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2f (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2fEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2i (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2i64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-integer?) (y exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2i64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2iEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2iv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2ivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2ui (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2ui64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2ui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform2uiEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform2uiv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform2uivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3d (program exact-nonnegative-integer?) (location exact-integer?) (v0 real?) (v1 real?) (v2 real?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3dEXT (program exact-nonnegative-integer?) (location exact-integer?) (x real?) (y real?) (z real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3f (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3fEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3i (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3i64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3i64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3iEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3iv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3ivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3ui (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3ui64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3ui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform3uiEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform3uiv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform3uivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4d (program exact-nonnegative-integer?) (location exact-integer?) (v0 real?) (v1 real?) (v2 real?) (v3 real?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4dEXT (program exact-nonnegative-integer?) (location exact-integer?) (x real?) (y real?) (z real?) (w real?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4f (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?) (v3 flonum?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4fEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 flonum?) (v1 flonum?) (v2 flonum?) (v3 flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4i (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?) (v3 exact-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4i64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-integer?) (y exact-integer?) (z exact-integer?) (w exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4i64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4iEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-integer?) (v1 exact-integer?) (v2 exact-integer?) (v3 exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4iv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4ivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4ui (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?) (v3 exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4ui64NV (program exact-nonnegative-integer?) (location exact-integer?) (x exact-nonnegative-integer?) (y exact-nonnegative-integer?) (z exact-nonnegative-integer?) (w exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4ui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/gpu_shader5.txt"]{@racket[GL_NV_gpu_shader5]}.
}
@defproc[(glProgramUniform4uiEXT (program exact-nonnegative-integer?) (location exact-integer?) (v0 exact-nonnegative-integer?) (v1 exact-nonnegative-integer?) (v2 exact-nonnegative-integer?) (v3 exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniform4uiv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProgramUniform.xml"]{glProgramUniform manpage}.
}
@defproc[(glProgramUniform4uivEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformHandleui64NV (program exact-nonnegative-integer?) (location exact-integer?) (value exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glProgramUniformHandleui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (the-values u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glProgramUniformMatrix2dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x3dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x3dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x3fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x3fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x4dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x4dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix2x4fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix2x4fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x2dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x2dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x2fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x2fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x4dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x4dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix3x4fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix3x4fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x2dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x2dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x2fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x2fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x3dv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x3dvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformMatrix4x3fv (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Version 4.1.
}
@defproc[(glProgramUniformMatrix4x3fvEXT (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (transpose boolean?) (value f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glProgramUniformui64NV (program exact-nonnegative-integer?) (location exact-integer?) (value exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glProgramUniformui64vNV (program exact-nonnegative-integer?) (location exact-integer?) (count exact-integer?) (value u64vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glProgramVertexLimitNV (target exact-integer?) (limit exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/geometry_program4.txt"]{@racket[GL_NV_geometry_program4]}.
}
@defproc[(glProvokingVertex (mode exact-integer?)) any]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glProvokingVertex.xml"]{glProvokingVertex manpage}.
}
@defproc[(glProvokingVertexEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/provoking_vertex.txt"]{@racket[GL_EXT_provoking_vertex]}.
}
@defproc[(glPushAttrib (mask exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushAttrib.xml"]{glPushAttrib manpage}.
}
@defproc[(glPushClientAttrib (mask exact-nonnegative-integer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushClientAttrib.xml"]{glPushClientAttrib manpage}.
}
@defproc[(glPushClientAttribDefaultEXT (mask exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/direct_state_access.txt"]{@racket[GL_EXT_direct_state_access]}.
}
@defproc[(glPushDebugGroup (source exact-integer?) (id exact-nonnegative-integer?) (length exact-integer?) (message (or/c string? bytes?))) any]{
Version 4.3.
}
@defproc[(glPushMatrix) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushMatrix.xml"]{glPushMatrix manpage}.
}
@defproc[(glPushName (name exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glPushName.xml"]{glPushName manpage}.
}
