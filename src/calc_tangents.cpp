#include "calc_tangents.h"
#include "R.h"
#include <array>

using namespace rgl2gltf;

Mesh::Mesh(int in_draw_mode, int in_n_vertices,
           double* in_vertices,
           double* in_normals,
           double* in_texcoords,
           double* in_tangents) {
  draw_mode = in_draw_mode;
  n_vertices = in_n_vertices;
  vertices = in_vertices;
  normals = in_normals;
  texcoords = in_texcoords;
  tangents = in_tangents;
}

CalcTangents::CalcTangents() {
  iface.m_getNumFaces = get_num_faces;
  iface.m_getNumVerticesOfFace = get_num_vertices_of_face;

  iface.m_getNormal = get_normal;
  iface.m_getPosition = get_position;
  iface.m_getTexCoord = get_tex_coords;
  iface.m_setTSpaceBasic = set_tspace_basic;

  context.m_pInterface = &iface;
}

int CalcTangents::calc(Mesh *mesh) {

  context.m_pUserData = mesh;

  if(CALC_TANGENTS_DEBUG) {
    Rprintf("[CalcTangents] with Mesh\n");
  }

  return (int)genTangSpaceDefault(&this->context);
}

int CalcTangents::get_num_faces(const SMikkTSpaceContext *context) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  int i_size = working_mesh->n_vertices;

  if(working_mesh->draw_mode == GL_TRIANGLES) {
    i_size /= 3;
  } else if (working_mesh->draw_mode == GL_QUADS) {
    i_size /= 4;
  }

  if(CALC_TANGENTS_DEBUG) {
    Rprintf("[CalcTangents] get_num_faces: %d\n", i_size);
  }

  return i_size;
}

int CalcTangents::get_num_vertices_of_face(const SMikkTSpaceContext *context,
                                           const int iFace) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  if(working_mesh->draw_mode == GL_TRIANGLES) {
    return 3;
  } else if (working_mesh->draw_mode == GL_QUADS) {
    return 4;
  }
  Rf_error("no vertices with less than 3 and more than 4 supported");
}

void CalcTangents::get_position(const SMikkTSpaceContext *context,
                                float *outpos,
                                const int iFace, const int iVert) {

  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  double* vertex = std::addressof(working_mesh->vertices[3*index]);

  if(CALC_TANGENTS_DEBUG && index < 10) {
    Rprintf("[CalcTangents] get_position(%d): %.4f %.4f %.4f\n", index,
                  vertex[0], vertex[1], vertex[2]);
  }

  outpos[0] = vertex[0];
  outpos[1] = vertex[1];
  outpos[2] = vertex[2];
}

void CalcTangents::get_normal(const SMikkTSpaceContext *context,
                              float *outnormal,
                              const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  double *normal = std::addressof(working_mesh->normals[3*index]);

  if(CALC_TANGENTS_DEBUG && index < 10) {
    Rprintf("[CalcTangents] get_normal(%d): %.4f %.4f %.4f\n", index,
                  normal[0], normal[1], normal[2]);
  }

  outnormal[0] = normal[0];
  outnormal[1] = normal[1];
  outnormal[2] = normal[2];
}

void CalcTangents::get_tex_coords(const SMikkTSpaceContext *context,
                                  float *outuv,
                                  const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  double *texcoords = std::addressof(working_mesh->texcoords[2*index]);

  if(CALC_TANGENTS_DEBUG && index < 10) {
    Rprintf("[CalcTangents] get_tex_coords(%d): %.4f %.4f\n", index,
                  texcoords[0], texcoords[1]);
  }

  outuv[0] = texcoords[0];
  outuv[1] = texcoords[1];
}

void CalcTangents::set_tspace_basic(const SMikkTSpaceContext *context,
                                    const float *tangentu,
                                    const float fSign, const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  double *tangent = std::addressof(working_mesh->tangents[4*index]);

  tangent[0] = tangentu[0];
  tangent[1] = tangentu[1];
  tangent[2] = tangentu[2];
  tangent[3] = fSign;

  if(CALC_TANGENTS_DEBUG && index < 10) {
    Rprintf("[CalcTangents] set_tspace_basic(%d) fSign: %.4f %.4f %.4f %.4f \n", index, fSign,
                  tangent[0], tangent[1], tangent[2]);
  }
}

int CalcTangents::get_vertex_index(const SMikkTSpaceContext *context, int iFace, int iVert) {

  auto face_size = get_num_vertices_of_face(context, iFace);

  auto index = (iFace * face_size) + iVert;

  return index;
}
