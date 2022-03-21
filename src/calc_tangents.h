#ifndef CALC_TANGENTS_H
#define CALC_TANGENTS_H

#include "mikktspace.h"
#include <vector>
#include "gl.h" // This is just an extract

#define CALC_TANGENTS_DEBUG 0

namespace rgl2gltf {

class Mesh {

public:
  Mesh(int in_draw_mode, int in_n_vertices,
       double* in_vertices,
       double* in_normals,
       double* in_texcoords,
       double* in_tangents);
  int draw_mode, n_vertices;
  double *vertices, *normals, *texcoords, *tangents;
};

class CalcTangents {

public:
  CalcTangents();
  int calc(Mesh *mesh);

private:

  SMikkTSpaceInterface iface{};
  SMikkTSpaceContext context{};

  static int get_vertex_index(const SMikkTSpaceContext *context, int iFace, int iVert);

  static int get_num_faces(const SMikkTSpaceContext *context);
  static int get_num_vertices_of_face(const SMikkTSpaceContext *context, int iFace);
  static void get_position(const SMikkTSpaceContext *context, float outpos[],
                           int iFace, int iVert);

  static void get_normal(const SMikkTSpaceContext *context, float outnormal[],
                         int iFace, int iVert);

  static void get_tex_coords(const SMikkTSpaceContext *context, float outuv[],
                             int iFace, int iVert);

  static void set_tspace_basic(const SMikkTSpaceContext *context,
                               const float tangentu[],
                                                   float fSign, int iFace, int iVert);

};
} // namespace rgl2gltf

#endif // SPHERE_MESH_H
