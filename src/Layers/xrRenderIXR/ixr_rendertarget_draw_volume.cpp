#include "stdafx.h"
#include "du_sphere_part.h"
#include "du_cone.h"
#include "du_sphere.h"

namespace
{
struct VolumeMesh
{
	const Fvector* Vertices;
	const u16* Faces;
	u32 Indices;
};

const VolumeMesh PointVolume = { du_sphere_vertices, du_sphere_faces, DU_SPHERE_NUMFACES * 3 };
const VolumeMesh SpotVolume = { du_cone_vertices, du_cone_faces, DU_CONE_NUMFACES * 3 };
const VolumeMesh OmnipartVolume = { du_sphere_part_vertices, du_sphere_part_faces, DU_SPHERE_PART_NUMFACES * 3 };
} // namespace

void CRenderTarget::draw_volume		(GfxCmdList& Cmd, light* L)
{
	const VolumeMesh* Mesh = nullptr;

	switch(L->flags.type) {
	case IRender_Light::REFLECTED	:
	case IRender_Light::POINT		:
		Mesh = &PointVolume;
		break;
	case IRender_Light::SPOT		:
		Mesh = &SpotVolume;
		break;
	case IRender_Light::OMNIPART	:
		Mesh = &OmnipartVolume;
		break;
	default:
		return;
	}

	// The faces' vertices one after another: the stream draws no index buffer
	DynamicGeometry& Stream = RImplementation.Dynamic;
	u32 Offset = 0;
	PackedVertex* Records = static_cast<PackedVertex*>(Stream.Lock(Mesh->Indices, EVertexFormat::Packed, Offset));

	for (u32 Index = 0; Index < Mesh->Indices; ++Index)
	{
		Records[Index] = { Mesh->Vertices[Mesh->Faces[Index]], 0, 0, 0, 0, 0 };
	}

	Stream.Unlock(Mesh->Indices);
	Cmd.SetGeometry(g_menu);
	Stream.Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Mesh->Indices, Offset);
}
