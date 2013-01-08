#lang scribble/manual
@title{glI...}
@defproc[(glIglooInterfaceSGIX (pname exact-integer?) (params gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/igloo_interface.txt"]{@racket[GL_SGIX_igloo_interface]}.
}
@defproc[(glImageTransformParameterfHP (target exact-integer?) (pname exact-integer?) (param flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameterfvHP (target exact-integer?) (pname exact-integer?) (params f32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameteriHP (target exact-integer?) (pname exact-integer?) (param exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImageTransformParameterivHP (target exact-integer?) (pname exact-integer?) (params s32vector?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/HP/image_transform.txt"]{@racket[GL_HP_image_transform]}.
}
@defproc[(glImportSyncEXT (external_sync_type exact-integer?) (external_sync exact-integer?) (flags exact-nonnegative-integer?)) GLsync?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/x11_sync_object.txt"]{@racket[GL_EXT_x11_sync_object]}.
}
@defproc[(glIndexFormatNV (type exact-integer?) (stride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt"]{@racket[GL_NV_vertex_buffer_unified_memory]}.
}
@defproc[(glIndexFuncEXT (func exact-integer?) (ref flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/index_func.txt"]{@racket[GL_EXT_index_func]}.
}
@defproc[(glIndexMask (mask exact-nonnegative-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndexMask.xml"]{glIndexMask manpage}.
}
@defproc[(glIndexMaterialEXT (face exact-integer?) (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/index_material.txt"]{@racket[GL_EXT_index_material]}.
}
@defproc[(glIndexPointer (type exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndexPointer.xml"]{glIndexPointer manpage}.
}
@defproc[(glIndexPointerEXT (type exact-integer?) (stride exact-integer?) (count exact-integer?) (pointer gl-pointer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.
}
@defproc[(glIndexPointerListIBM (type exact-integer?) (stride exact-integer?) (pointer (vectorof gl-pointer?)) (ptrstride exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt"]{@racket[GL_IBM_vertex_array_lists]}.
}
@defproc[(glIndexd (c real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexdv (c real?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexf (c flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexfv (c flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexi (c exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexiv (c exact-integer?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexs (c (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexsv (c (integer-in -32768 32767))) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexub (c (integer-in 0 255))) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glIndexubv (c (integer-in 0 255))) any]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml"]{glIndex manpage}.
}
@defproc[(glInitNames) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glInitNames.xml"]{glInitNames manpage}.
}
@defproc[(glInsertComponentEXT (res exact-nonnegative-integer?) (src exact-nonnegative-integer?) (num exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glInstrumentsBufferSGIX (size exact-integer?)) s32vector?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/instruments.txt"]{@racket[GL_SGIX_instruments]}.
}
@defproc[(glInterleavedArrays (format exact-integer?) (stride exact-integer?) (pointer gl-pointer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glInterleavedArrays.xml"]{glInterleavedArrays manpage}.
}
@defproc[(glInterpolatePathsNV (resultPath exact-nonnegative-integer?) (pathA exact-nonnegative-integer?) (pathB exact-nonnegative-integer?) (weight flonum?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glInvalidateBufferData (buffer exact-nonnegative-integer?)) any]{
Version 4.3.
}
@defproc[(glInvalidateBufferSubData (buffer exact-nonnegative-integer?) (offset exact-integer?) (length exact-integer?)) any]{
Version 4.3.
}
@defproc[(glInvalidateFramebuffer (target exact-integer?) (numAttachments exact-integer?) (attachments s32vector?)) any]{
Version 4.3.
}
@defproc[(glInvalidateSubFramebuffer (target exact-integer?) (numAttachments exact-integer?) (attachments s32vector?) (x exact-integer?) (y exact-integer?) (width exact-integer?) (height exact-integer?)) any]{
Version 4.3.
}
@defproc[(glInvalidateTexImage (texture exact-nonnegative-integer?) (level exact-integer?)) any]{
Version 4.3.
}
@defproc[(glInvalidateTexSubImage (texture exact-nonnegative-integer?) (level exact-integer?) (xoffset exact-integer?) (yoffset exact-integer?) (zoffset exact-integer?) (width exact-integer?) (height exact-integer?) (depth exact-integer?)) any]{
Version 4.3.
}
@defproc[(glIsAsyncMarkerSGIX (marker exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glIsBuffer (buffer exact-nonnegative-integer?)) boolean?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsBuffer.xml"]{glIsBuffer manpage}.
}
@defproc[(glIsBufferARB (buffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt"]{@racket[GL_ARB_vertex_buffer_object]}.

Alias of @racket[glIsBuffer].
}
@defproc[(glIsBufferResidentNV (target exact-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glIsEnabled (cap exact-integer?)) boolean?]{

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsEnabled.xml"]{glIsEnabled manpage}.
}
@defproc[(glIsEnabledIndexedEXT (target exact-integer?) (index exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt"]{@racket[GL_EXT_draw_buffers2]}.

Alias of @racket[glIsEnabledi].
}
@defproc[(glIsEnabledi (target exact-integer?) (index exact-nonnegative-integer?)) boolean?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsEnabled.xml"]{glIsEnabled manpage}.
}
@defproc[(glIsFenceAPPLE (fence exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/fence.txt"]{@racket[GL_APPLE_fence]}.
}
@defproc[(glIsFenceNV (fence exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/fence.txt"]{@racket[GL_NV_fence]}.
}
@defproc[(glIsFramebuffer (framebuffer exact-nonnegative-integer?)) boolean?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsFramebuffer.xml"]{glIsFramebuffer manpage}.
}
@defproc[(glIsFramebufferEXT (framebuffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glIsFramebuffer].
}
@defproc[(glIsImageHandleResidentNV (handle exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glIsList (list exact-nonnegative-integer?)) boolean?]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glIsList.xml"]{glIsList manpage}.
}
@defproc[(glIsNameAMD (identifier exact-integer?) (name exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/AMD/name_gen_delete.txt"]{@racket[GL_AMD_name_gen_delete]}.
}
@defproc[(glIsNamedBufferResidentNV (buffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/shader_buffer_load.txt"]{@racket[GL_NV_shader_buffer_load]}.
}
@defproc[(glIsNamedStringARB (namelen exact-integer?) (name (or/c string? bytes?))) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shading_language_include.txt"]{@racket[GL_ARB_shading_language_include]}.
}
@defproc[(glIsObjectBufferATI (buffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glIsOcclusionQueryNV (id exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/occlusion_query.txt"]{@racket[GL_NV_occlusion_query]}.
}
@defproc[(glIsPathNV (path exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsPointInFillPathNV (path exact-nonnegative-integer?) (mask exact-nonnegative-integer?) (x flonum?) (y flonum?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsPointInStrokePathNV (path exact-nonnegative-integer?) (x flonum?) (y flonum?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/path_rendering.txt"]{@racket[GL_NV_path_rendering]}.
}
@defproc[(glIsProgram (program exact-nonnegative-integer?)) boolean?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsProgram.xml"]{glIsProgram manpage}.
}
@defproc[(glIsProgramARB (program exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/vertex_program.txt"]{@racket[GL_ARB_vertex_program]}.

Alias of @racket[glIsProgram].
}
@defproc[(glIsProgramNV (id exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.

Alias of @racket[glIsProgram].
}
@defproc[(glIsProgramPipeline (pipeline exact-nonnegative-integer?)) boolean?]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsProgramPipeline.xml"]{glIsProgramPipeline manpage}.
}
@defproc[(glIsQuery (id exact-nonnegative-integer?)) boolean?]{
Version 1.5.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsQuery.xml"]{glIsQuery manpage}.
}
@defproc[(glIsQueryARB (id exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/occlusion_query.txt"]{@racket[GL_ARB_occlusion_query]}.

Alias of @racket[glIsQuery].
}
@defproc[(glIsRenderbuffer (renderbuffer exact-nonnegative-integer?)) boolean?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsRenderbuffer.xml"]{glIsRenderbuffer manpage}.
}
@defproc[(glIsRenderbufferEXT (renderbuffer exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt"]{@racket[GL_EXT_framebuffer_object]}.

Alias of @racket[glIsRenderbuffer].
}
@defproc[(glIsSampler (sampler exact-nonnegative-integer?)) boolean?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsSampler.xml"]{glIsSampler manpage}.
}
@defproc[(glIsShader (shader exact-nonnegative-integer?)) boolean?]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsShader.xml"]{glIsShader manpage}.
}
@defproc[(glIsSync (sync GLsync?)) boolean?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsSync.xml"]{glIsSync manpage}.
}
@defproc[(glIsTexture (texture exact-nonnegative-integer?)) boolean?]{
Version 1.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsTexture.xml"]{glIsTexture manpage}.
}
@defproc[(glIsTextureEXT (texture exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glIsTextureHandleResidentNV (handle exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/bindless_texture.txt"]{@racket[GL_NV_bindless_texture]}.
}
@defproc[(glIsTransformFeedback (id exact-nonnegative-integer?)) boolean?]{
Version 1.2.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsTransformFeedback.xml"]{glIsTransformFeedback manpage}.
}
@defproc[(glIsTransformFeedbackNV (id exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback2.txt"]{@racket[GL_NV_transform_feedback2]}.
}
@defproc[(glIsVariantEnabledEXT (id exact-nonnegative-integer?) (cap exact-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_shader.txt"]{@racket[GL_EXT_vertex_shader]}.
}
@defproc[(glIsVertexArray (array exact-nonnegative-integer?)) boolean?]{
Version 3.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glIsVertexArray.xml"]{glIsVertexArray manpage}.
}
@defproc[(glIsVertexArrayAPPLE (array exact-nonnegative-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt"]{@racket[GL_APPLE_vertex_array_object]}.

Alias of @racket[glIsVertexArray].
}
@defproc[(glIsVertexAttribEnabledAPPLE (index exact-nonnegative-integer?) (pname exact-integer?)) boolean?]{
Extension @hyperlink["http://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt"]{@racket[GL_APPLE_vertex_program_evaluators]}.
}
