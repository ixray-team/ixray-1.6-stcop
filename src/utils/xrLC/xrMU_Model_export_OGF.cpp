#include "StdAfx.h"


#include "utils/xrLC_Light/xrMU_Model.h"
#include "utils/xrLC_Light/xrMU_Model_Reference.h"

#include "Build.h"
#include "OGF_Face.h"
#include "utils/xrForms/CompilersUI.h"

#define	TRY(a) try { a; } catch (...) { clMsg("* E: %s", #a); }
xrCriticalSection csThreadLock;

template <class T> static void BuildLODNode(T*& pNode,b_lod& LOD,const xr_vector<u32>& generated_ids,xrMU_Model* model,xrMU_Reference& mu_reference)
{
    pNode->lod_Material = LOD.dwMaterial;

    // Fill faces
    for (int lf = 0; lf < 8; lf++)
    {
        b_lod_face& F = LOD.faces[lf];
        typename T::_face& D = pNode->lod_faces[lf];

        for (int lv = 0; lv < 4; lv++)
        {
            mu_reference.xform.transform_tiny(D.v[lv].v, F.v[lv]);

            D.v[lv].t = F.t[lv];
            D.v[lv].c_rgb_hemi = 0xffffffff;
            D.v[lv].c_sun = 0xff;
        }
    }

    // Add children
    for (u32 o = 0; o < generated_ids.size(); o++)
        pNode->AddChield(generated_ids[o]);

    if (pNode->chields.empty())
        return;

    // Register node
    R_ASSERT(pNode->chields.size());

    pNode->CalcBounds();

    csThreadLock.Enter();
    g_tree.push_back(pNode);
    csThreadLock.Leave();

    // Calculate colors
    for (int lf = 0; lf < 8; lf++)
    {
        typename T::_face& F = pNode->lod_faces[lf];

        for (int lv = 0; lv < 4; lv++)
        {
            Fvector ptPos = F.v[lv].v;

            base_color_c _C;
            float _N = 0;

            for (u32 v_it = 0; v_it < model->m_vertices.size(); v_it++)
            {
                Fvector baseP;
                mu_reference.xform.transform_tiny(
                    baseP,
                    model->m_vertices[v_it]->P
                );

                base_color_c baseC;
                mu_reference.color[v_it]._get(baseC);

                float oD = ptPos.distance_to(baseP);
                float oA = 1 / (1 + 100 * oD * oD);

                base_color_c vC = baseC;
                vC.mul(oA);

                _C.add(vC);
                _N += oA;
            }

            _C.mul(1 / (_N + EPS));

            F.v[lv].c_rgb_hemi =
                color_rgba(
                    u8_clr(_C.rgb.x),
                    u8_clr(_C.rgb.y),
                    u8_clr(_C.rgb.z),
                    u8_clr(_C.hemi)
                );

            F.v[lv].c_sun = u8_clr(_C.sun);
        }
    }
}

void export_ogf( xrMU_Reference& mu_reference )
{
	xr_vector<u32>		generated_ids;
	xrMU_Model *model = mu_reference.model;
	// Export nodes
	auto MakeRef = [](xrMU_Model& Model, xr_vector<u32>& GeneratedIds, const xrMU_Reference& Ref) -> void{
		for (xrMU_Model::v_subdivs_it it=Model.m_subdivs.begin(); it!=Model.m_subdivs.end(); it++)
		{
			OGF_Reference* pOGF = new OGF_Reference ();
			//b_material*		M		= &(pBuild->materials()[it->material]);	// and it's material
			//R_ASSERT		(M);

			// Common data
			pOGF->Sector = mu_reference.sector;
			pOGF->material = it->material;
			pOGF->bSharedMaterial = it->bSharedMaterial;

			pOGF->debug_name = mu_reference.debug_name;
			pOGF->debug_name += ":subdiv ";
			pOGF->debug_name += std::to_string(it-model->m_subdivs.begin()).c_str();

			// Collect textures
			auto& Tex = pBuild->GetTexture(it->material, it->bSharedMaterial);
			OGF_Texture T;
			T.name = Tex.name;
			T.pBuildSurface	= &Tex;
			pOGF->textures.push_back(T);

			// Special
			pOGF->model = it->ogf;
			pOGF->vb_id	= it->vb_id;
			pOGF->vb_start = it->vb_start;
			pOGF->ib_id = it->ib_id;
			pOGF->ib_start = it->ib_start;
			pOGF->sw_id = it->sw_id;
			if (gCompilerMode.LC_UseExternalRefs)
			{
				pOGF->external_path = it->external_path;
				pOGF->SplitID = it-model->m_subdivs.begin();
			}
			pOGF->xform.set(mu_reference.xform);
			pOGF->c_scale = mu_reference.c_scale;
			pOGF->c_bias = mu_reference.c_bias;

			pOGF->CalcBounds();
			generated_ids.push_back((u32)g_tree.size());

			csThreadLock.Enter();
			GeneratedIds.push_back((u32)g_tree.size());
 			g_tree.push_back(pOGF);
			csThreadLock.Leave();
		}
	};
	MakeRef(*mu_reference.model, generated_ids, mu_reference);

	if (model->color.empty())
	{
		return;
	}
	 
	// Now, let's fuck with LODs
	if (u16(-1) != model->m_lod_ID)
	{
		// Vanilla way
		b_lod& LOD = pBuild->lods[model->m_lod_ID];
		OGF_LOD* pNode = new OGF_LOD(1, mu_reference.sector);
		BuildLODNode(pNode, LOD, generated_ids, model, mu_reference);
		return;
	}
	
	if (model->UseBillboard)
	{
		return;
	}
	
	// New way
	auto LODNode = new OGF_MESH_LODS(1, mu_reference.sector);
	auto AttackLOD = [&](OGF_Node* LOD)
	{
		for (auto Ref : generated_ids)
		{
			LOD->AddChield(Ref);
		}
		Fvector E;
		LOD->bbox.get_CD(LOD->C, E);
		LOD->R = E.magnitude();
		csThreadLock.Enter();
		auto ID = g_tree.size();
		g_tree.push_back(LOD);
		csThreadLock.Leave();
		LODNode->AddChield(ID);
	};
	AttackLOD(new OGF_LOD_MU0(1, mu_reference.sector));
	{
		generated_ids.clear();
		auto& LOD1Model = *pBuild->mu_models()[mu_reference.model->LODsID[0]];
		MakeRef(LOD1Model, generated_ids, mu_reference);
		AttackLOD(new OGF_LOD_MU1(1, mu_reference.sector));
	}
	{
		generated_ids.clear();
		auto& LOD2Model = *pBuild->mu_models()[mu_reference.model->LODsID[1]];
		MakeRef(LOD2Model, generated_ids, mu_reference);
		AttackLOD(new OGF_LOD_MU2(1, mu_reference.sector));
	}
	{
		generated_ids.clear();
		auto& LOD3Model = *pBuild->mu_models()[mu_reference.model->LODsID[2]];
		MakeRef(LOD3Model, generated_ids, mu_reference);
		AttackLOD(new OGF_LOD_MU3(1, mu_reference.sector));
	}
	{
		generated_ids.clear();
		auto& LOD4Model = *pBuild->mu_models()[mu_reference.model->LODsID[3]];
		MakeRef(LOD4Model, generated_ids, mu_reference);
		AttackLOD(new OGF_LOD_MU4(1, mu_reference.sector));
	} 
	Fvector E;
	LODNode->bbox.get_CD(LODNode->C, E);
	LODNode->R = E.magnitude();
	csThreadLock.Enter();
	g_tree.push_back(LODNode);
	csThreadLock.Leave();
}
