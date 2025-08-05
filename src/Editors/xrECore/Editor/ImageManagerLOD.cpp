#include "stdafx.h"


#include "ImageManager.h"
#include "EditObject.h"
#include "EditMesh.h"
#include "ui_main.h"
#include "../xrEngine/xrHemisphere.h"

using Fvector4Vec = xr_vector<Fvector4>;
using Fvector4It = Fvector4Vec::iterator;

BOOL GetPointColor(SPickQuery::SResult* R, u32& alpha, u32& color)
{
    CSurface* surf			= R->e_mesh->GetSurfaceByFaceID(R->tag); VERIFY(surf);
    Shader_xrLC* c_sh		= EDevice->ShaderXRLC.Get(surf->_ShaderXRLCName());
    if (!c_sh->flags.bRendering) return FALSE;
    const Fvector2*			cuv[3];
    R->e_mesh->GetFaceTC	(R->tag,cuv);

    // barycentric coords
    // note: W,U,V order
    Fvector B;
    B.set	(1.0f - R->u - R->v,R->u,R->v);

    // calc UV
    Fvector2	uv;
    uv.x = cuv[0]->x*B.x + cuv[1]->x*B.y + cuv[2]->x*B.z;
    uv.y = cuv[0]->y*B.x + cuv[1]->y*B.y + cuv[2]->y*B.z;

    int U = iFloor(uv.x*float(surf->m_ImageData->w) + .5f);
    int V = iFloor(uv.y*float(surf->m_ImageData->h)+ .5f);

    if (surf->m_ImageData->w != 0)
    {
        U %= surf->m_ImageData->w;
        if (U < 0)
            U += surf->m_ImageData->w;
    }

    if (surf->m_ImageData->h != 0)
    {
        V %= surf->m_ImageData->h;
        if (V < 0)
            V += surf->m_ImageData->h;
    }


    auto& Layer = surf->m_ImageData->layers;

    if (!Layer.empty())
    {
        color = Layer.back()[V * surf->m_ImageData->w + U];
        alpha = color_get_A(color);
    }
    else
    {
        Msg("! Error creating lod. Try again!");
        return false;
    }

    return TRUE;
}

struct SBuildLight{
    Flight					light;
    float					energy;
};
using BLVec = xr_vector<SBuildLight>;
using BLIt = BLVec::iterator;

ICF static void  simple_hemi_callback(float x, float y, float z, float E, LPVOID P)
{
    BLVec* dst 					= (BLVec*)P;
    SBuildLight 				T;
    T.energy     				= E;
    T.light.direction.set		(x,y,z);
    dst->push_back  			(T);
}

const u32 lod_ss_quality  = 8;

void CreateLODSamples(const Fbox& bbox, U32Vec& tgt_data, u32 tgt_w, u32 tgt_h)
{
	U32Vec 	s_pixels,d_pixels;
    Fmatrix save_projection		= EDevice->mProject;
    Fmatrix save_view			= EDevice->mView;

    // save render params
    Flags32 old_flag			= psDeviceFlags;
	u32 old_dwFillMode			= EDevice->dwFillMode;
    u32 old_dwShadeMode			= EDevice->dwShadeMode;
    // set render params

    u32 cc						= 	EPrefs->scene_clear_color;
    EPrefs->scene_clear_color 	= 	0x0000000;
    psDeviceFlags.zero			();
	psDeviceFlags.set			(rsFilterLinear,TRUE);
	EDevice->dwFillMode			= D3DFILL_SOLID;
    EDevice->dwShadeMode			= D3DSHADE_GOURAUD;

    Fvector vP,vD,vN,vR;
    Fmatrix mV,mP;
    
    Fvector S;
    bbox.getradius				(S);
    float R 					= 2.f*_max(S.x,S.z);

    u32 pitch 					= tgt_w*LOD_SAMPLE_COUNT;
    tgt_data.resize				(pitch*tgt_h);
    for (int frame=0; frame<LOD_SAMPLE_COUNT; frame++){
        float angle 			= frame*(PI_MUL_2/LOD_SAMPLE_COUNT);

		Fbox bb 				= bbox;
        // build camera matrix
        bb.getcenter			( vP );
        vN.set					( 0.f,1.f,0.f );
        vD.setHP				( angle, 0 );
        vR.crossproduct			( vN,vD );
        mV.build_camera_dir		(vP,vD,vN);
        bb.xform				(mV);
        // build project matrix
        mP.build_projection_ortho(R,bb.max.y-bb.min.y,bb.min.z,bb.max.z);
	    RCache.set_xform_project(mP);
	    RCache.set_xform_view	(mV);
        EDevice->mFullTransform.mul(mP,mV);
        EDevice->MakeScreenshot	(s_pixels,tgt_w*lod_ss_quality,tgt_h*lod_ss_quality);
        d_pixels.resize 		(tgt_w*tgt_h);
        DXTUtils::Filter::Process				(d_pixels.data(),tgt_w,tgt_h,s_pixels.data(),tgt_w*lod_ss_quality,tgt_h*lod_ss_quality, DXTUtils::Filter::imf_box);
        // copy LOD to final
		for (u32 y=0; y<tgt_h; y++)
    		CopyMemory			(tgt_data.data()+y*pitch+frame*tgt_w,d_pixels.data()+y*tgt_w,tgt_w*sizeof(u32));
	}

    // flip data
	for (u32 y=0; y<tgt_h/2; y++){
		u32 y2 = tgt_h-y-1;
		for (u32 x=0; x<pitch; x++){
        	std::swap	(tgt_data[y*pitch+x],tgt_data[y2*pitch+x]);	
		}
	}

    // restore render params
	EDevice->dwFillMode			= old_dwFillMode;
    EDevice->dwShadeMode			= old_dwShadeMode;
    psDeviceFlags 				= old_flag;
    EPrefs->scene_clear_color 	= cc;

    RCache.set_xform_view		(save_view);
    RCache.set_xform_project	(save_projection);
}

void CImageManager::CreateLODTexture(CEditableObject* OBJECT, U32Vec& lod_pixels, U32Vec& nm_pixels, u32 tgt_w, u32 tgt_h, int _samples, int quality)
{
    // build hemi light
    BLVec						simple_hemi;
    // fill simple hemi
    simple_hemi.clear();
    xrHemisphereBuild(1, 2.f, simple_hemi_callback, &simple_hemi);

    Fbox bb = OBJECT->GetBox();

    // build lod normals
    lod_pixels.resize(LOD_IMAGE_SIZE * LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, 0);
    nm_pixels.resize(LOD_IMAGE_SIZE * LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, 0);
    U32Vec hemi_tmp(LOD_IMAGE_SIZE * LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, 0);
    Fvector 					o_center, o_size;
    Fmatrix 					M, Mi;
    bb.getradius(o_size);
    bb.getcenter(o_center);
    SPBItem* PB = UI->ProgressStart(LOD_SAMPLE_COUNT * LOD_IMAGE_SIZE, OBJECT->GetName());
    float dW = _max(o_size.x, o_size.z) / (LOD_IMAGE_SIZE / 2);
    float dH = o_size.y / (LOD_IMAGE_SIZE / 2);
    float dR = bb.getradius();
    float d2R = dR * 2.f;
    //ETOOLS::ray_options			(CDB::OPT_CULL);

    CreateLODSamples(bb, lod_pixels, LOD_IMAGE_SIZE, LOD_IMAGE_SIZE);

    float tN = 0.f, tH = 0.f, tT = 0.f, tR = 0.f;

    float 	LOD_CALC_SAMPLES = quality;
    s32		LOD_CALC_SAMPLES_LIM = LOD_CALC_SAMPLES / 2;

    // preload textures
    for (SurfaceIt surf_it = OBJECT->Surfaces().begin(); surf_it != OBJECT->Surfaces().end(); surf_it++) {
        CSurface* surf = *surf_it;
        Shader_xrLC* c_sh = EDevice->ShaderXRLC.Get(surf->_ShaderXRLCName());
        if (!c_sh->flags.bRendering) continue;
        if (0 == surf->m_ImageData)surf->CreateImageData();
    }

    // calculate
    for (u32 sample_idx = 0; sample_idx < LOD_SAMPLE_COUNT; sample_idx++)
    {
        float angle = sample_idx * (PI_MUL_2 / LOD_SAMPLE_COUNT);
        M.setXYZ(0, angle, 0);
        M.translate_over(o_center);
        Mi.invert(M);
        for (s32 iH = 0; iH < LOD_IMAGE_SIZE; iH++)
        {
            PB->Inc();
            float Y = (iH - (LOD_IMAGE_SIZE - 1) / 2) * dH;
            for (s32 iW = 0; iW < LOD_IMAGE_SIZE; iW++)
            {
                float X = (iW - (LOD_IMAGE_SIZE) / 2) * dW;

                u32 pixel = (LOD_IMAGE_SIZE - iH - 1) * LOD_SAMPLE_COUNT * LOD_IMAGE_SIZE + LOD_IMAGE_SIZE * sample_idx + iW;
                u32& tgt_c = lod_pixels[pixel];
                u32& tgt_n = nm_pixels[pixel];
                u32& tgt_h = hemi_tmp[pixel];

                FvectorVec		n_vec;
                Fvector4Vec		sample_pt_vec;
                Fvector			start;
                CTimer 	TT, TT1;
                TT.Start();
                SPickQuery 		PQ;
                for (s32 iiH = -LOD_CALC_SAMPLES_LIM; iiH <= LOD_CALC_SAMPLES_LIM; iiH++)
                {
                    float dY = iiH * (dH / LOD_CALC_SAMPLES);
                    for (s32 iiW = -LOD_CALC_SAMPLES_LIM; iiW <= LOD_CALC_SAMPLES_LIM; iiW++) 
                    {
                        float dX = iiW * (dW / LOD_CALC_SAMPLES);
                        start.set(X + dX, Y + dY, 0);
                        M.transform_tiny(start);
                        start.mad(M.k, -dR);
                        PQ.prepare_rq(start, M.k, d2R, CDB::OPT_CULL);
                        OBJECT->RayQuery(PQ);

                        if (PQ.r_count()) 
                        {
                            PQ.r_sort();
                            Fvector N = { 0,0,0 };

                            for (s32 k = PQ.r_count() - 1; k >= 0; k--) 
                            {
                                SPickQuery::SResult* R = PQ.r_begin() + k;
                                u32 	uA, uC;
                                if (!GetPointColor(R, uA, uC)) continue;
                                float	fA = float(uA) / 255.f;
                                if (uA) 
                                {
                                    Fvector  pt; 	pt.mad(PQ.m_Start, PQ.m_Direction, R->range - EPS_L);
                                    Fvector4 ptt;	ptt.set(pt.x, pt.y, pt.z, fA);
                                    sample_pt_vec.push_back(ptt);
                                }
                                // normal
                                Fvector Nn;
                                Nn.mknormal(R->verts[0], R->verts[1], R->verts[2]);
                                Nn.mul(fA);

                                N.mul(1.f - fA);
                                N.add(Nn);
                            }

                            float n_mag = N.magnitude();
                            if (!fis_zero(n_mag, EPS))
                                n_vec.push_back(N.div(n_mag));
                        }
                    }
                }
                tN += TT.GetElapsed_sec();

                Fvector N = { 0,0,0 };
                if (!n_vec.empty()) 
                {
                    for (FvectorIt it = n_vec.begin(); it != n_vec.end(); it++) N.add(*it);
                    N.div(n_vec.size());
                    N.normalize_safe();
                    Mi.transform_dir(N);
                }

                N.mul(0.5f);
                N.add(0.5f);
                N.mul(255.f);
                tgt_n = color_rgba(iFloor(N.x), iFloor(N.y), iFloor(N.z), color_get_A(tgt_c));

                if (0 == color_get_A(tgt_c)) 
                    continue;

                TT.Start();
                // light points
                float res_transp = 0.f;
                for (Fvector4It pt_it = sample_pt_vec.begin(); pt_it != sample_pt_vec.end(); pt_it++)
                {
                    float avg_transp = 0.f;
                    for (BLIt it = simple_hemi.begin(); it != simple_hemi.end(); it++) 
                    {
                        TT1.Start();
                        Fvector 		start;
                        start.mad(Fvector().set(pt_it->x, pt_it->y, pt_it->z), it->light.direction, -dR);
                        PQ.prepare_rq(start, it->light.direction, dR, CDB::OPT_CULL);
                        OBJECT->RayQuery(PQ);
                        tR += TT1.GetElapsed_sec();
                        float ray_transp = 1.f;

                        if (PQ.r_count()) 
                        {
                            for (s32 k = 0; k < PQ.r_count(); k++)
                            {
                                u32 	a, uC;
                                TT1.Start();
                                if (!GetPointColor(PQ.r_begin() + k, a, uC)) continue;
                                tT += TT1.GetElapsed_sec();
                                ray_transp *= (1.f - float(a) / 255.f);
                                if (fis_zero(ray_transp, EPS_L)) break;
                            }
                        }
                        avg_transp += ray_transp;
                    }
                    avg_transp /= simple_hemi.size();
                    res_transp = res_transp * (1.f - pt_it->w) + avg_transp * pt_it->w;
                }
                tH += TT.GetElapsed_sec();
                u8 h = (u8)iFloor(res_transp * 255.f);
                tgt_h = color_rgba(h, h, h, color_get_A(tgt_c));
            }
        }
    }

    Msg("Normal: %3.2fsec, Hemi: %3.2f, PC: %3.2f, RP: %3.2f", tN, tH, tT, tR);
    ImageLib.ApplyBorders(lod_pixels, LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, LOD_IMAGE_SIZE);
    ImageLib.ApplyBorders(nm_pixels, LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, LOD_IMAGE_SIZE);
    ImageLib.ApplyBorders(hemi_tmp, LOD_IMAGE_SIZE * LOD_SAMPLE_COUNT, LOD_IMAGE_SIZE);

    // fill alpha to N-channel (HEMI)
    for (int px_idx = 0; px_idx<int(nm_pixels.size()); px_idx++)
        nm_pixels[px_idx] = subst_alpha(nm_pixels[px_idx], color_get_R(hemi_tmp[px_idx]));

    UI->ProgressEnd(PB);
}

//------------------------------------------------------------------------------
// 
//------------------------------------------------------------------------------
void CImageManager::CreateLODTexture(CEditableObject* OBJECT, LPCSTR tex_name, u32 tgt_w, u32 tgt_h, int samples, int age, int quality)
{
    U32Vec lod_pixels, nm_pixels;

    CreateLODTexture(OBJECT, lod_pixels, nm_pixels, tgt_w, tgt_h, samples, quality);

    string_path out_name, src_name;
    // save lod
    strcpy(src_name, tex_name);

    strcpy(src_name, EFS.ChangeFileExt(src_name, ".thm").c_str());
    FS.file_delete(_textures_, src_name);

    strcpy(src_name, EFS.ChangeFileExt(src_name, ".tga").c_str());
    FS.update_path(out_name, _textures_, src_name);
    DXTUtils::Converter::MakeTGA(tgt_w * samples, tgt_h, lod_pixels, out_name);
    FS.set_file_age(out_name, age);
    SynchronizeTexture(src_name, age);

    // save normal map
    xr_strconcat(src_name, tex_name, "_nm");
    strcpy(src_name, EFS.ChangeFileExt(src_name, ".thm").c_str());
    FS.file_delete(_textures_, src_name);

    strcpy(src_name, EFS.ChangeFileExt(src_name, ".tga").c_str());
    FS.update_path(out_name, _textures_, src_name);
    DXTUtils::Converter::MakeTGA(tgt_w * samples, tgt_h, nm_pixels, out_name);
    FS.set_file_age(out_name, age);
    SynchronizeTexture(src_name, age);
}