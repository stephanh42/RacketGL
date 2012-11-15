#lang scribble/manual
@title{glA...}
@defproc[(glAccum (op exact-integer?) (value flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAccum.xml"]{glAccum manpage}.
}
@defproc[(glActiveProgramEXT (program exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt"]{@racket[GL_EXT_separate_shader_objects]}.
}
@defproc[(glActiveShaderProgram (pipeline exact-nonnegative-integer?) (program exact-nonnegative-integer?)) any]{
Version 4.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glActiveShaderProgram.xml"]{glActiveShaderProgram manpage}.
}
@defproc[(glActiveStencilFaceEXT (face exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/stencil_two_side.txt"]{@racket[GL_EXT_stencil_two_side]}.
}
@defproc[(glActiveTexture (texture exact-integer?)) any]{
Version 1.3.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glActiveTexture.xml"]{glActiveTexture manpage}.
}
@defproc[(glActiveTextureARB (texture exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/multitexture.txt"]{@racket[GL_ARB_multitexture]}.

Alias of @racket[glActiveTexture].
}
@defproc[(glActiveVaryingNV (program exact-nonnegative-integer?) (name (or/c string? bytes?))) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/transform_feedback.txt"]{@racket[GL_NV_transform_feedback]}.
}
@defproc[(glAlphaFragmentOp1ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFragmentOp2ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?) (arg2Rep exact-nonnegative-integer?) (arg2Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFragmentOp3ATI (op exact-integer?) (dst exact-nonnegative-integer?) (dstMod exact-nonnegative-integer?) (arg1 exact-nonnegative-integer?) (arg1Rep exact-nonnegative-integer?) (arg1Mod exact-nonnegative-integer?) (arg2 exact-nonnegative-integer?) (arg2Rep exact-nonnegative-integer?) (arg2Mod exact-nonnegative-integer?) (arg3 exact-nonnegative-integer?) (arg3Rep exact-nonnegative-integer?) (arg3Mod exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/fragment_shader.txt"]{@racket[GL_ATI_fragment_shader]}.
}
@defproc[(glAlphaFunc (func exact-integer?) (ref flonum?)) any]{
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAlphaFunc.xml"]{glAlphaFunc manpage}.
}
@defproc[(glApplyTextureEXT (mode exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/light_texture.txt"]{@racket[GL_EXT_light_texture]}.
}
@defproc[(glAreProgramsResidentNV (n exact-integer?) (programs u32vector?)) (values boolean? (vectorof boolean?))]{
Extension @hyperlink["http://www.opengl.org/registry/specs/NV/vertex_program.txt"]{@racket[GL_NV_vertex_program]}.
}
@defproc[(glAreTexturesResident (n exact-integer?) (textures u32vector?)) (values boolean? (vectorof boolean?))]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glAreTexturesResident.xml"]{glAreTexturesResident manpage}.
}
@defproc[(glAreTexturesResidentEXT (n exact-integer?) (textures u32vector?)) (values boolean? (vectorof boolean?))]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/texture_object.txt"]{@racket[GL_EXT_texture_object]}.
}
@defproc[(glArrayElement (i exact-integer?)) any]{
Version 1.1.
Deprecated in version 3.1.

See the @hyperlink["http://www.opengl.org/sdk/docs/man2/xhtml/glArrayElement.xml"]{glArrayElement manpage}.
}
@defproc[(glArrayElementEXT (i exact-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/EXT/vertex_array.txt"]{@racket[GL_EXT_vertex_array]}.

Alias of @racket[glArrayElement].
}
@defproc[(glArrayObjectATI (array exact-integer?) (size exact-integer?) (type exact-integer?) (stride exact-integer?) (buffer exact-nonnegative-integer?) (offset exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ATI/vertex_array_object.txt"]{@racket[GL_ATI_vertex_array_object]}.
}
@defproc[(glAsyncMarkerSGIX (marker exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/SGIX/async.txt"]{@racket[GL_SGIX_async]}.
}
@defproc[(glAttachObjectARB (containerObj exact-nonnegative-integer?) (obj exact-nonnegative-integer?)) any]{
Extension @hyperlink["http://www.opengl.org/registry/specs/ARB/shader_objects.txt"]{@racket[GL_ARB_shader_objects]}.

Alias of @racket[glAttachShader].
}
@defproc[(glAttachShader (program exact-nonnegative-integer?) (shader exact-nonnegative-integer?)) any]{
Version 2.0.

See the @hyperlink["http://www.opengl.org/sdk/docs/man4/xhtml/glAttachShader.xml"]{glAttachShader manpage}.
}
