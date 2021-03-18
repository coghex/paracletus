#version 450
#extension GL_ARB_separate_shader_objects : enable

// see https://www.khronos.org/opengl/wiki/Interface_Block_(GLSL) for
// difference between block name and instance name
layout(binding = 0) uniform TransformationObject {
  mat4 model;
  mat4 view;
  mat4 proj;
} trans;

layout (binding = 2) buffer DynTransObject {
  mat4 move[1000];
} dyn;

layout (binding = 3) buffer DynTexTransObject {
  mat4 dynTexI[1000];
} dynTex;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inColor;
layout(location = 2) in vec3 inTexCoord;
layout(location = 3) in vec3 inMove;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out int fragTexIndex;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    int bufI = int(floor(inMove.x - 0.5));
    int dynI = int(floor(inMove.y - 0.5));

    mat4 view = trans.view;
    mat4 proj = trans.proj;
    mat4 dynV = (inMove.y > 0.0) ? (trans.model * (dyn.move[dynI])) : trans.model;
    gl_Position = proj * view * dynV * vec4(inPosition, 1.0);
    fragColor = inColor;
    int inTex = int(inTexCoord.z);
    mat4 dynTC = dynTex.dynTexI[dynI];
    int texI = int(floor(dynTC[3][2]));
    fragTexCoord = (inMove.y > 0.0) ? (vec2 (inTexCoord.x + ((dynTC[3][0])/3.0), inTexCoord.y + ((dynTC[3][1])/20.0))) : inTexCoord.xy;
    fragTexIndex = (inMove.x > 0.0) ? inTex + texI : inTex;
}
