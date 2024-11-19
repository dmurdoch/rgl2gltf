# rgl2gltf 1.0.7

* Avoid writing to library during execution.

# rgl2gltf 1.0.5

* Support transparency.
* Fix noRemap issue as requested by CRAN.

# rgl2gltf 1.0.3

* Added print support for sparse accessors; `rgl` added
support in its `Buffer` object in version 0.109.11 (issue #8).
* Re-ran Roxygen to generate correct HTML for the Gltf
help topic.
* Updated link to 'glTF' web page.
* Fixed write to system dir in the animation control.

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
