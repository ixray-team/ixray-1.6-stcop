#pragma once

class CEditableObject;

namespace XRay::Importer::Mesh
{
	// True for .gltf and .glb file names
	ECORE_API bool IsGLTF(const char* FileName);

	// Builds static meshes and surfaces of Out from a glTF 2.0 file (.gltf or .glb).
	// Node transforms are baked; the right-handed glTF space is mirrored by Z into the engine LH space.
	ECORE_API bool ImportGLTF(const char* FileName, CEditableObject* Out);
}
