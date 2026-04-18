#include "stdafx.h"

#define LEVEL_LODS_TEX_NAME "level_lods"
#define LEVEL_LODS_NRM_NAME "level_lods_nm"

// lod build functions
using Fvector4Vec = xr_vector<Fvector4>;
using Fvector4It = Fvector4Vec::iterator;

bool GetPointColor(SPickQuery::SResult* R, u32& alpha)
{
    CSurface* surf = R->e_mesh->GetSurfaceByFaceID(R->tag); VERIFY(surf);
    Shader_xrLC* c_sh = EDevice->ShaderXRLC.Get(surf->_ShaderXRLCName());
    if (!c_sh->flags.bRendering) 
        return FALSE;

    const Fvector2* cuv[3];
    R->e_mesh->GetFaceTC(R->tag, cuv);

    // barycentric coords
    // note: W,U,V order
    Fvector B;
    B.set(1.0f - R->u - R->v, R->u, R->v);

    // calc UV
    Fvector2 uv;
    uv.x = cuv[0]->x * B.x + cuv[1]->x * B.y + cuv[2]->x * B.z;
    uv.y = cuv[0]->y * B.x + cuv[1]->y * B.y + cuv[2]->y * B.z;

    int U = iFloor(uv.x * float(surf->m_ImageData->w) + .5f);
    int V = iFloor(uv.y * float(surf->m_ImageData->h) + .5f);
    U %= surf->m_ImageData->w;	if (U < 0) U += surf->m_ImageData->w;
    V %= surf->m_ImageData->h;	if (V < 0) V += surf->m_ImageData->h;

    alpha = color_get_A(surf->m_ImageData->layers.back()[V * surf->m_ImageData->w + U]);
    return TRUE;
}

int	SceneBuilder::BuildObjectLOD(const Fmatrix& parent, CEditableObject* E, int sector_num)
{
    if (!E->m_objectFlags.is(CEditableObject::eoUsingLOD)) 
        return -1;

    xr_string lod_name = E->GetLODTextureName();

    b_material mtl;
    mtl.surfidx = (u16)BuildTexture(LEVEL_LODS_TEX_NAME);
    mtl.shader = (u16)BuildShader(E->GetLODShaderName());
    mtl.sector = (u16)sector_num;
    mtl.shader_xrlc = -1;

    if ((u16(-1) == mtl.surfidx) || (u16(-1) == mtl.shader)) 
        return -2;

    int mtl_idx = FindInMaterials(&mtl);
    if (mtl_idx < 0)
    {
        l_materials.push_back(mtl);
        mtl_idx = l_materials.size() - 1;
    }

    l_lods.push_back(e_b_lod());
    e_b_lod& b = l_lods.back();
    Fvector p[4];
    Fvector2 t[4];

    for (int frame = 0; frame < LOD_SAMPLE_COUNT; frame++)
    {
        E->GetLODFrame(frame, p, t, &parent);
        for (int k = 0; k < 4; k++) {
            b.lod.faces[frame].v[k].set(p[k]);
            b.lod.faces[frame].t[k].set(t[k]);
        }
    }

    b.lod.dwMaterial = mtl_idx;
    b.lod_name = lod_name.c_str();

    xr_string l_name = lod_name.c_str();
    u32 w, h;
    time_t age;
    if (!ImageLib.LoadTextureData(l_name.c_str(), b.data, w, h, &age))
    {
        Msg("!Can't find LOD texture: '%s'", l_name.c_str());
        return -2;
    }

    l_name += "_nm";
    if (!ImageLib.LoadTextureData(l_name.c_str(), b.ndata, w, h, &age))
    {
        Msg("!Can't find LOD texture: '%s'", l_name.c_str());
        return -2;
    }

    return l_lods.size() - 1;
}