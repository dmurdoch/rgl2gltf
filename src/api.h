#ifndef RGL2GLTF_API_H
#define RGL2GLTF_API_H

#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
#include "R.h"

namespace rgl2gltf {

#ifdef __cplusplus
extern "C" {
#endif

void get_tangents(int* edges,
               int* n_vertices,
               double* vertices,
               double* normals,
               double* texcoords,
               double* tangents);

void attribute_visible R_init_rgl2gltf(DllInfo *dll);

#ifdef __cplusplus
}
#endif

} // namespace rgl2gltf

#endif /* RGL2GLTF_API_H */

