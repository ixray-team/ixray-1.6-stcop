#pragma once

#include <NVMeshMender.h>
#include <mender_input_output.h>

template <class VertVec, class FaceVec>
void CalculateMeshTB(VertVec& verts, FaceVec& faces)
{
	xr_vector<MeshMender::Vertex> mender_in_out_verts;
	xr_vector<unsigned int> mender_in_out_indices;
	xr_vector<unsigned int> mender_mapping_out_to_in_vert;

	fill_mender_input(verts, faces, mender_in_out_verts, mender_in_out_indices);

	MeshMender mender;
	if (!mender.Mend(
			mender_in_out_verts, mender_in_out_indices, mender_mapping_out_to_in_vert, 1, 0.5, 0.5, 0.0f,
			MeshMender::DONT_CALCULATE_NORMALS, MeshMender::RESPECT_SPLITS, MeshMender::DONT_FIX_CYLINDRICAL))
	{
		Debug.fatal(DEBUG_INFO, "NVMeshMender failed ");
	}

	retrive_data_from_mender_otput(verts, faces, mender_in_out_verts, mender_in_out_indices, mender_mapping_out_to_in_vert);
}

template <class Vert, class GetUV>
void OptimizeMeshUVs(xr_vector<Vert>& verts, GetUV get_uv, const char* texture = nullptr, const char* shader = nullptr)
{
	Fvector2 Tdelta;
	Fvector2 Tmin, Tmax;
	Tmin.set(flt_max, flt_max);
	Tmax.set(flt_min, flt_min);

	for (Vert& v : verts)
	{
		Tmin.min(get_uv(v));
		Tmax.max(get_uv(v));
	}

	Tdelta.x = floorf((Tmax.x - Tmin.x) / 2 + Tmin.x);
	Tdelta.y = floorf((Tmax.y - Tmin.y) / 2 + Tmin.y);

	Fvector2 Tsize;
	Tsize.sub(Tmax, Tmin);
	if (texture && shader && ((Tsize.x > 32) || (Tsize.y > 32)))
		Msg("#!Surface [T:'%s', S:'%s'] has UV tiled more than 32 times.", texture, shader);

	for (Vert& v : verts)
		get_uv(v).sub(Tdelta);
}
