CalcTangents::CalcTangents() {
  iface.m_getNumFaces = get_num_faces;
  iface.m_getNumVerticesOfFace = get_num_vertices_of_face;

  iface.m_getNormal = get_normal;
  iface.m_getPosition = get_position;
  iface.m_getTexCoord = get_tex_coords;
  iface.m_setTSpaceBasic = set_tspace_basic;

  context.m_pInterface = &iface;
}

void CalcTangents::calc(Mesh *mesh) {

  context.m_pUserData = mesh;

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] with Mesh: {}", mesh->name);
  }

  genTangSpaceDefault(&this->context);
}

int CalcTangents::get_num_faces(const SMikkTSpaceContext *context) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  float f_size = (float)working_mesh->indices.size() / 3.f;
  int i_size = (int)working_mesh->indices.size() / 3;

  assert((f_size - (float)i_size) == 0.f);

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] get_num_faces: {}", i_size);
  }

  return i_size;
}

int CalcTangents::get_num_vertices_of_face(const SMikkTSpaceContext *context,
                                           const int iFace) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  if(working_mesh->draw_mode == GL_TRIANGLES) {
    return 3;
  }
  throw std::logic_error("no vertices with less than 3 and more than 3 supported");
}

void CalcTangents::get_position(const SMikkTSpaceContext *context,
                                float *outpos,
                                const int iFace, const int iVert) {

  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  auto vertex = working_mesh->vertices[index];

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] get_position({}): {}", index,
                  glm::to_string(vertex.position));
  }

  outpos[0] = vertex.position.x;
  outpos[1] = vertex.position.y;
  outpos[2] = vertex.position.z;
}

void CalcTangents::get_normal(const SMikkTSpaceContext *context,
                              float *outnormal,
                              const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  auto vertex = working_mesh->vertices[index];

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] get_normal({}): {}", index,
                  glm::to_string(vertex.normal));
  }

  outnormal[0] = vertex.normal.x;
  outnormal[1] = vertex.normal.y;
  outnormal[2] = vertex.normal.z;
}

void CalcTangents::get_tex_coords(const SMikkTSpaceContext *context,
                                  float *outuv,
                                  const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto index = get_vertex_index(context, iFace, iVert);
  auto vertex = working_mesh->vertices[index];

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] get_tex_coords({}): {}", index,
                  glm::to_string(vertex.tex_coords));
  }

  outuv[0] = vertex.tex_coords.x;
  outuv[1] = vertex.tex_coords.y;
}

void CalcTangents::set_tspace_basic(const SMikkTSpaceContext *context,
                                    const float *tangentu,
                                    const float fSign, const int iFace, const int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);


  auto index = get_vertex_index(context, iFace, iVert);
  auto *vertex = &working_mesh->vertices[index];

  vertex->tangent.x = tangentu[0];
  vertex->tangent.y = tangentu[1];
  vertex->tangent.z = tangentu[2];
  vertex->tangent.w = fSign;

  if(CALC_TANGENTS_DEBUG) {
    spdlog::debug("[CalcTangents] set_tspace_basic({}) fSign:{}  {}", index, fSign,
                  glm::to_string(vertex->tangent));
  }
}

int CalcTangents::get_vertex_index(const SMikkTSpaceContext *context, int iFace, int iVert) {
  Mesh *working_mesh = static_cast<Mesh*> (context->m_pUserData);

  auto face_size = get_num_vertices_of_face(context, iFace);

  auto indices_index = (iFace * face_size) + iVert;

  int index = working_mesh->indices[indices_index];
  return index;
}
