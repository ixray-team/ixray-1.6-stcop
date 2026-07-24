#include "StdAfx.h"
#include "utils/xrLC_Light/xrMU_Model.h"
#include "OGF_Face.h"
#include "utils/xrForms/CompilersUI.h"

const u32	max_tile	= 16;
const s32	quant		= 32768/max_tile;

s16 QC	(float v)
{
	int t		=	iFloor(v*float(quant)); clamp(t,-32768,32767);
	return	s16	(t);
}

D3DVERTEXELEMENT9	decl[] = // 12+4+4+4+8=32
{
	{0, 0,  D3DDECLTYPE_FLOAT3,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },
	{0, 12, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_NORMAL,	0 },
	{0, 16, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TANGENT,	0 },
	{0, 20, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_BINORMAL,	0 },
	{0, 24, D3DDECLTYPE_SHORT4,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TEXCOORD,	0 },
	D3DDECL_END()
};

void	export_geometry		( xrMU_Model &	mu_model )
{
	// Declarator
	VDeclarator D;
	D.set(decl);

	// RT-check, BOX, low-point, frac-size
	Fbox BB; 
	BB.invalidate();
	for (auto elem : mu_model.m_vertices)
	{
		BB.modify(elem->P);
	}

	Fvector frac_low;
	float frac_Ysize;
	BB.getcenter(frac_low);
	frac_low.y = BB.min.y;
	frac_Ysize = BB.max.y - BB.min.y;
	
	VBContainer* VB = nullptr;
	IBContainer* IB = nullptr;
	SWIContainer* SWI = nullptr;
	str_c SavePath = nullptr;
	if (gCompilerMode.LC_UseExternalRefs)
	{
		auto It = g_MUGeomData.try_emplace(&mu_model).first;
		VB = &It->second.VB;
		IB = &It->second.IB;
		SWI = &It->second.SWI;
		xr_stack_string_path relative = "static\\";
		relative.append(It->first->m_name.c_str());
		relative.append(".ogf");
		It->second.SavePath = relative.c_str();
		SavePath = It->second.SavePath.c_str();
	} else
	{
		VB = &g_VB;
		IB = &g_IB;
		SWI = &g_SWI;
	}

	// Begin building
	for (auto& elem : mu_model.m_subdivs)
	{
		// Vertices
		{
			VB->Begin(D);
			
			vecOGF_V& verts = elem.ogf->data.vertices;
			for (u32 v_it=0; v_it<verts.size(); v_it++)
			{
				OGF_Vertex& oV = verts[v_it];

				// Position
				VB->Add(&oV.P,3*sizeof(float));

				// Normal
				{
					base_color_c oV_c; oV.Color._get(oV_c);
					Fvector N = oV.N;
					N.add(1.f);
					N.mul(.5f*255.f);
					s32 nx = iFloor(N.x); clamp(nx,0,255);
					s32 ny = iFloor(N.y); clamp(ny,0,255);
					s32 nz = iFloor(N.z); clamp(nz,0,255);
					s32 cc = iFloor(oV_c.hemi*255.f);	clamp(cc,0,255);
					u32	uN = color_rgba(nx,ny,nz,cc);
					VB->Add(&uN,4);
				}

				// Tangent
				{
					u32	uT = color_rgba(oV.T.x,oV.T.y,oV.T.z,0);
					VB->Add(&uT,4);
				}

				// Binormal
				{
					u32	uB = color_rgba(oV.B.x,oV.B.y,oV.B.z,0);
					VB->Add(&uB,4);
				}

				// TC
				s16	tu,tv,frac,dummy;
				tu = QC(oV.UV.begin()->x);
				tv = QC(oV.UV.begin()->y);
				VB->Add(&tu,2);
				VB->Add(&tv,2);

				// frac
				float f1 = (oV.P.y - frac_low.y)/frac_Ysize;
				float f2 = oV.P.distance_to(frac_low)/frac_Ysize;
				frac = QC((f1+f2)/2.f);
				dummy = 0;
				VB->Add(&frac,	2);
				VB->Add(&dummy,2);
			}

			VB->End(&elem.vb_id,&elem.vb_start);
		}

		// Indices
		IB->Register((u16*)(elem.ogf->data.faces.data()),(u16*)(&*elem.ogf->data.faces.end()),&elem.ib_id,&elem.ib_start);

		// SW
		if (elem.ogf->progressive_test())
		{
			SWI->Register(&elem.sw_id,&elem.ogf->data.m_SWI);
		}
		if (gCompilerMode.LC_UseExternalRefs)
		{
			elem.external_path = SavePath;
		}
	}
}
