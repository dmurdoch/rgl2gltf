
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


void main()
{
  vCol = aCol;
  vec4 pos = mvMatrix * vec4(aPos, 1);
  vPosition = pos / pos.w;

  #ifdef HAS_NORMALS
  #ifdef HAS_TANGENTS
  vec3 normalW = normalize(vec3(normMatrix * vec4(aNorm.xyz, 0.0)));
  vec3 tangentW = normalize(vec3(mvMatrix * vec4(aTangent.xyz, 0.0)));
  vec3 bitangentW = cross(normalW, tangentW) * aTangent.w;
  vtbnMatrix = mat3(tangentW, bitangentW, normalW);
  #else // HAS_TANGENTS != 1
  vNormal = normalize(vec3(mvMatrix * vec4(aNorm.xyz, 0.0)));
  #endif
  #endif

  #ifdef HAS_UV
  vTexcoord = aTexcoord;
  #else
  vTexcoord = vec2(0.,0.);
  #endif

  gl_Position = prMatrix * mvMatrix * vec4(aPos, 1); // needs w for proper perspective correction
}
