# rgl2gltf 1.0.0

* Added a `NEWS.md` file to track changes to the package.
* Added "rigid" method animation to `playgltf()`.
* Added `gltfwidget()` to play in WebGL.
* Added debugging function `showTags()`.
* Added shader support for skins.
* Added `findEntry()` debugging function.
* Added `modifyShaders()` function to edit shaders.
* Added shader for normal textures.
* Added `getTangents()` to use MikkTSpace code to assign tangents 
if they are not specified.  These can be used in normal textures.
* Textures may now be specified in JPEG format, not just PNG.
* `writeglTF()` now writes the binary part of the file as
well as the JSON part.
* Added "physically based rendering" (PBR) methods.
* Fixed `baseColorTexture` and `bufferView.target` errors (issue #38).
* Fixed type errors (issue #38).
* Now writes colors using normalized bytes (issue #40).
* Modified mikktspace.c for C99 compatibility.
