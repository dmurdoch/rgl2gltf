
varying vec4 vCol;
attribute vec4 aCol;

attribute vec3 aPos;
#ifdef HAS_NORMALS
attribute vec3 aNorm;
#endif
#ifdef HAS_TANGENTS
attribute vec4 aTangent;
#endif
#ifdef HAS_UV
attribute vec2 aTexcoord;
#endif

uniform mat4 prMatrix;
uniform mat4 mvMatrix;
uniform mat4 normMatrix;

varying vec4 vPosition;
varying vec2 vTexcoord;

#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
varying mat3 vtbnMatrix;
#else
varying vec3 vNormal;
#endif
#endif

#ifdef HAS_JOINTS
attribute vec4 aJoint;
attribute vec4 aWeight;
uniform vec4 uJointMat[JOINTMATROWS];
#endif

void main(void) {
  // The line above is edited for animations, so don't change it

  vCol = aCol;

  #ifdef HAS_JOINTS
  mat4 skinMat = mat4(0);
  for (int i = 0; i < 4; i++) {
    skinMat[i] += aWeight.x * uJointMat[4*int(aJoint.x) + i];
    skinMat[i] += aWeight.y * uJointMat[4*int(aJoint.y) + i];
    skinMat[i] += aWeight.z * uJointMat[4*int(aJoint.z) + i];
    skinMat[i] += aWeight.w * uJointMat[4*int(aJoint.w) + i];
  }
  #endif

  #ifdef HAS_NORMALS
  #ifdef HAS_JOINTS
  mat4 skinMat2 = mat4(mat3(skinMat));
  #endif
  #ifdef HAS_TANGENTS
  #ifdef HAS_JOINTS
  vec3 normalW = normalize(vec3(normMatrix * skinMat2 * vec4(aNorm.xyz, 0.0)));
  #else
  vec3 normalW = normalize(vec3(normMatrix * vec4(aNorm.xyz, 0.0)));
  #endif
  vec3 tangentW = normalize(vec3(mvMatrix * vec4(aTangent.xyz, 0.0)));
  vec3 bitangentW = cross(normalW, tangentW) * aTangent.w;
  vtbnMatrix = mat3(tangentW, bitangentW, normalW);
  #else // HAS_TANGENTS != 1
  #ifdef HAS_JOINTS
  vNormal = normalize(vec3(mvMatrix * skinMat2 * vec4(aNorm.xyz, 0.0)));
  #else
  vNormal = normalize(vec3(mvMatrix * vec4(aNorm.xyz, 0.0)));
  #endif // HAS_JOINTS
  #endif
  #endif

  #ifdef HAS_UV
  vTexcoord = aTexcoord;
  #else
  vTexcoord = vec2(0.,0.);
  #endif

  #ifdef HAS_JOINTS
  vec4 pos = skinMat * vec4(aPos, 1.);
  gl_Position = prMatrix * mvMatrix * pos;

  #else

  vPosition = mvMatrix * vec4(aPos, 1); // needs w for proper perspective correction

  gl_Position = prMatrix * vPosition;

  #endif
}
