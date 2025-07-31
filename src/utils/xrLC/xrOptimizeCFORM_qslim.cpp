#include "StdAfx.h"
#include "Build.h"
#include "../../xrCDB/xrCDB.h"
#include "../xrLC_Light/face_smoth_flags.h"
#include <meshoptimizer/meshoptimizer.h>

#define MAX_DECIMATE_ERROR 0.0005f
#define COMPACTNESS_RATIO  0.001f

void SaveAsSMF(LPCSTR fname, CDB::CollectorPacked& CL)
{
	IWriter* W = FS.w_open(fname);
	string256 tmp;

	// vertices
	for (u32 v_idx = 0; v_idx < CL.getVS(); v_idx++) 
	{
		Fvector* v = CL.getV() + v_idx;
		xr_sprintf(tmp, "v %f %f %f", v->x, v->y, -v->z);
		W->w_string(tmp);
	}

	// transfer faces
	for (u32 f_idx = 0; f_idx < CL.getTS(); f_idx++) 
	{
		CDB::TRI& t = CL.getT(f_idx);
		xr_sprintf(tmp, "f %d %d %d", t.verts[0] + 1, t.verts[2] + 1, t.verts[1] + 1);
		W->w_string(tmp);
	}
	FS.w_close(W);
}

bool SaveCForm = false;

struct face_props 
{
    u16 material;
    u16 sector;
    u32 flags;
    Fvector norm;
    void set(u16 mtl, u16 sect, const Fvector& n, u32 _flags) 
    {
        material = mtl;
        sector = sect;
        norm.set(n);
        flags = _flags;
    }
};

using FPVec = xr_vector<face_props>;
using FPVecIt = FPVec::iterator;

void SimplifyCFORM(CDB::CollectorPacked& CL)
{
#if 0
    FPVec FPs;

    u32 base_verts_cnt = u32(CL.getVS());
    u32 base_faces_cnt = u32(CL.getTS());

    bool keep_temp_files = SaveCForm;
    if (keep_temp_files) 
    {
        string_path fn;
        SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_source.obj"), CL);
    }

    xr_vector<float> vertices(base_verts_cnt * 3);
    xr_vector<u32> indices(base_faces_cnt * 3);
    xr_vector<float> attributes(base_verts_cnt);
    FPs.resize(base_faces_cnt);

    // Переносим вершины и атрибуты
    for (u32 v_idx = 0; v_idx < base_verts_cnt; v_idx++)
    {
        Fvector* v = CL.getV() + v_idx;
        vertices[v_idx * 3 + 0] = v->x;
        vertices[v_idx * 3 + 1] = v->y;
        vertices[v_idx * 3 + 2] = v->z;

        for (u32 f_idx = 0; f_idx < base_faces_cnt; f_idx++) 
        {
            const CDB::TRI& t = CL.getT(f_idx);
            if (t.verts[0] == v_idx || t.verts[1] == v_idx || t.verts[2] == v_idx)
            {
                attributes[v_idx] = (t.material << 16) | t.sector;
                break;
            }
        }
    }

    for (u32 f_idx = 0; f_idx < base_faces_cnt; f_idx++) 
    {
        const CDB::TRI& t = CL.getT(f_idx);
        indices[f_idx * 3 + 0] = t.verts[0];
        indices[f_idx * 3 + 1] = t.verts[1];
        indices[f_idx * 3 + 2] = t.verts[2];
        FPs[f_idx].set(t.material, t.sector, Fvector().mknormal(*(CL.getV() + t.verts[0]), *(CL.getV() + t.verts[1]), *(CL.getV() + t.verts[2])), CL.getfFlags(f_idx));
    }
    CL.clear();

    // Буфер для упрощенных индексов
    xr_vector<u32> lod_indices(indices.size()); 

    size_t lod_index_count = meshopt_simplifyWithAttributes
    (
        lod_indices.data(),        
        indices.data(),            
        indices.size(),            
        vertices.data(),           
        base_verts_cnt,            
        sizeof(float) * 3,         
        attributes.data(),         
        sizeof(u32),               
        nullptr,                   
        0,                         
        nullptr,                   
        0,                         
        MAX_DECIMATE_ERROR,        
        base_faces_cnt * 3,        
        nullptr                    
    );

    lod_indices.resize(lod_index_count);

    for (u32 f_idx = 0; f_idx < lod_indices.size() / 3; f_idx++) 
    {
        u32 i0 = lod_indices[f_idx * 3 + 0];
        u32 i1 = lod_indices[f_idx * 3 + 1];
        u32 i2 = lod_indices[f_idx * 3 + 2];

        Fvector v0 = { vertices[i0 * 3 + 0], vertices[i0 * 3 + 1], vertices[i0 * 3 + 2] };
        Fvector v1 = { vertices[i1 * 3 + 0], vertices[i1 * 3 + 1], vertices[i1 * 3 + 2] };
        Fvector v2 = { vertices[i2 * 3 + 0], vertices[i2 * 3 + 1], vertices[i2 * 3 + 2] };

        u32 attr = attributes[i0];
        u16 material = (attr >> 16) & 0xFFFF;
        u16 sector = attr & 0xFFFF;

        face_props& FP = FPs[f_idx];
        CL.add_face(v0, v1, v2, material, sector, FP.flags);
    }

    if (keep_temp_files)
    {
        string_path fn;
        SaveAsSMF(xr_strconcat(fn, pBuild->path, "cform_optimized.obj"), CL);
    }
#endif
}