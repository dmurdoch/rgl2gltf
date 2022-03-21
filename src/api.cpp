// C++ source
// This file is part of rgl2gltf.
//

#include "api.h"
#include "calc_tangents.h"
#include "gl.h"
#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
#include "R.h"

using namespace rgl2gltf;

void rgl2gltf::get_tangents(int* edges,
                            int* n_vertices,
                            double* vertices,
                            double* normals,
                            double* texcoords,
                            double* tangents)
{
  int mode = *edges == 3 ? GL_TRIANGLES : GL_QUADS;
  Mesh *mesh = new Mesh(mode, *n_vertices,
                   vertices, normals, texcoords, tangents);
  CalcTangents *calculator = new CalcTangents();
  calculator->calc(mesh);

}

#ifdef __cplusplus
extern "C" {
#endif

#define C_DEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CMethodDef CEntries[] = {
  C_DEF(get_tangents, 6),
  {NULL, NULL, 0}
};

void attribute_visible R_init_rgl2gltf(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

#ifdef __cplusplus
}
#endif
