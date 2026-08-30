#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"

#include "../../xrEngine/Fmesh.h"
#include "xrOcclusion.h"

#include "../../Editors/Public/VIMP_Processor.h"

using namespace std;

void set_status(char* N, int id, int f, int v)
{
	string1024 status_str;

	xr_sprintf	(status_str,"Model #%4d [F:%5d, V:%5d]: %s...",id,f,v,N);
	Status	(status_str);
	clMsg	(status_str);
}

bool OGF_Vertex::similar(OGF* ogf, OGF_Vertex& V)
{
	const float ntb		= std::cos	(deg2rad(5.f));
	if (!P.similar(V.P)) 		return false;
	if (!N.similar(V.N)) 		return false;
	if (!T.similar(V.T)) 		return false;
	if (!B.similar(V.B)) 		return false;
	
	R_ASSERT(UV.size()==V.UV.size());
	for (u32 i=0; i<V.UV.size(); i++) {
		OGF_Texture *T = &*ogf->textures.begin()+i;
		b_texture	*B = T->pBuildSurface;
		float		eu = 1.f/float(B->dwWidth );
		float		ev = 1.f/float(B->dwHeight);
		if (!UV[i].similar(V.UV[i],eu,ev)) return false;
	}
	return true;
}
void OGF_Vertex::dump	(u32 id)
{
//	Msg	("%d: ");
}
bool x_vertex::similar	(OGF* ogf, x_vertex& V)
{
	return P.similar(V.P);
}
u16 OGF::x_BuildVertex	(x_vertex&& V1)
{
	for (itXV it=fast_path_data.vertices.begin(); it!=fast_path_data.vertices.end(); it++)
		if (it->similar(this,V1)) return u16(it-fast_path_data.vertices.begin());
	fast_path_data.vertices.push_back	(V1);
	return (unsigned short) fast_path_data.vertices.size()-1;
}
u16 OGF::_BuildVertex	(OGF_Vertex& V1)
{
	try {
		for (itOGF_V it=data.vertices.begin(); it!=data.vertices.end(); it++)
		{
			if (it->similar(this,V1)) return u16(it-data.vertices.begin());
		}
	} catch (...) { clMsg("* ERROR: OGF::_BuildVertex");	}

	data.vertices.push_back	(V1);
	return (unsigned short) data.vertices.size()-1;
}
void OGF::x_BuildFace	(OGF_Vertex& V1, OGF_Vertex& V2, OGF_Vertex& V3, bool _tc_)
{
	if (_tc_)	return	;	// make empty-list for stuff that has relevant TCs
	x_face	F;
	u32		VertCount	= (u32)fast_path_data.vertices.size();
	F.v[0]	= x_BuildVertex(x_vertex(V1));
	F.v[1]	= x_BuildVertex(x_vertex(V2));
	F.v[2]	= x_BuildVertex(x_vertex(V3));
	if (!F.Degenerate()) {
		fast_path_data.faces.push_back(F);
	} else {
		if (fast_path_data.vertices.size()>VertCount) 
			fast_path_data.vertices.erase(fast_path_data.vertices.begin()+VertCount,fast_path_data.vertices.end());
	}
}
void OGF::_BuildFace	(OGF_Vertex& V1, OGF_Vertex& V2, OGF_Vertex& V3, bool _tc_)
{
	OGF_Face			F;
	u32		VertCount	= (u32)data.vertices.size();
	F.v[0]	= _BuildVertex(V1);
	F.v[1]	= _BuildVertex(V2);
	F.v[2]	= _BuildVertex(V3);
	if (!F.Degenerate()) {
		for (auto& face : data.faces)
		{
			if (face.Equal(F))
			{
				return;
			}
		}
		data.faces.push_back	(F);
		x_BuildFace		(V1,V2,V3,_tc_);
	} else {
		if (data.vertices.size()>VertCount)
		{
			data.vertices.erase(data.vertices.begin()+VertCount,data.vertices.end());
		}
	}
}
bool OGF::dbg_SphereContainsVertex(Fvector& c, float R)
{
	Fsphere	S;	S.set(c,R);
	for (u32 it=0; it<data.vertices.size(); it++)
		if (S.contains(data.vertices[it].P))	return	true;
	return false	;
}

void OGF::adjacent_select	(xr_vector<u32>& dest, xr_vector<bool>& vmark, xr_vector<bool>& fmark)
{
	// 0. Search for the group
	for (u32 fit=0; fit<data.faces.size(); fit++)	{
		OGF_Face&	F		= data.faces	[fit];
		if (fmark[fit])		continue;			// already registered

		// new face - if empty - just put it in, else check connectivity
		if (dest.empty())	{
			fmark[fit]		= true	;
			dest.push_back	(F.v[0]);	vmark[F.v[0]]=true;
			dest.push_back	(F.v[1]);	vmark[F.v[1]]=true;
			dest.push_back	(F.v[2]);	vmark[F.v[2]]=true;
		} else {
			// check connectivity
			bool	bConnected	=	false;
			for (u32 vid=0; vid<3; vid++)	{
				u32		id = F.v	[vid];	// search in already registered verts
				for (u32 sid=0; sid<dest.size(); sid++)
				{
					if (id==dest[sid])	{
						bConnected	= true;	// this face shares at least one vertex with already selected faces
						break;
					}
				}
				if (bConnected)	break;
			}
			if (bConnected)		{
				// add this face's vertices
				fmark[fit]	= true	;
				if (!vmark[F.v[0]])	{ dest.push_back	(F.v[0]);	vmark[F.v[0]]=true; }
				if (!vmark[F.v[1]])	{ dest.push_back	(F.v[1]);	vmark[F.v[1]]=true; }
				if (!vmark[F.v[2]])	{ dest.push_back	(F.v[2]);	vmark[F.v[2]]=true; }
			}
		}
	}
}

void OGF::Optimize()
{
	if (data.vertices.size() == 0)
	{
		//Msg("* ERROR Optimize OGF: %d Verts: data.vertices.size() == 0");
		return;
	}

	//////////////////////////////////////////////////////////////////////////
	// Detect relevant number of UV pairs
	R_ASSERT(data.vertices.size());
	dwRelevantUV = data.vertices.front().UV.size();
	auto& SH = pBuild->shaders().Get(pBuild->GetMaterialReserved(material, bSharedMaterial));
	if (!SH.flags.bOptimizeUV)
	{
		return;
	}

	// Optimize texture coordinates
	xr_vector<bool>	vmarker;	vmarker.assign	(data.vertices.size(),false);
	xr_vector<bool>	fmarker;	fmarker.assign	(data.faces.size(),false);

	for (;;)
	{
		// 0. Search for the group
		xr_vector<u32>	selection		;
		for (;;)	{
			u32		_old	= (u32)selection.size();
			adjacent_select	(selection,vmarker,fmarker);
			u32		_new	= (u32)selection.size();
			if (_old==_new)	break;		// group selected !
		}
		if (selection.empty())		break;

		// 1. Calc bounds
		Fvector2 Tdelta;
		try {
			Fvector2 Tmin,Tmax;
			Tmin.set(flt_max,flt_max);
			Tmax.set(flt_min,flt_min);
			for (u32 j=0; j<selection.size(); j++)
			{
				OGF_Vertex& V = data.vertices[selection[j]];
				Tmin.min(V.UV[0]);
				Tmax.max(V.UV[0]);
			}
			Tdelta.x = floorf((Tmax.x-Tmin.x)/2+Tmin.x);
			Tdelta.y = floorf((Tmax.y-Tmin.y)/2+Tmin.y);
		} catch(...) {
			Msg	("* ERROR: optimize: std-geom : delta UV");
		}

		// 2. Recalc UV mapping
		try 
		{
			for (u32 i=0; i<selection.size(); i++)
				data.vertices[selection[i]].UV[0].sub(Tdelta);
		} catch(...) {
			Msg	("* ERROR: optimize: std-geom : recalc UV");
		}
		selection.clear	();
	}
}

thread_local VIMP_Processor make_progressive_vimp;

#include "../xrForms/CompilersUI.h"
// Make Progressive
void OGF::MakeProgressive	(float metric_limit)
{
	// test
	// there is no-sense to simplify small models
	// for batch size 50,100,200 - we are CPU-limited anyway even on nv30
	// for nv40 and up the better guess will probably be around 500
	if (data.faces.size() < c_PM_FaceLimit * 4)		return;			// nv40 Теперь только

 	if (g_params().m_quality==ebqDraft)				return;
	if (!gCompilerMode.LC_OGF_PROGRESSIVE)			return;

	// Есть шанс словить вылет
	if (data.faces.size() > 32 * 1024)
	{
		clMsg("xmesh : Processing to big faces : %u", data.faces.size());
		return;
	}
 

	//////////////////////////////////////////////////////////////////////////
	// NORMAL
	vecOGF_V	_saved_vertices		=	data.vertices	;
	vecOGF_F	_saved_faces		=	data.faces		;

	{
		// prepare progressive geom
		make_progressive_vimp.VIPM_Init				();
		//clMsg("--- append v start .");
		for (u32 v_idx=0;  v_idx<data.vertices.size(); v_idx++)	
			make_progressive_vimp.VIPM_AppendVertex	(data.vertices[v_idx].P,	data.vertices[v_idx].UV[0]					);
		//clMsg("--- append f start .");
		for (u32 f_idx=0;  f_idx<data.faces.size();    f_idx++)	
			make_progressive_vimp.VIPM_AppendFace		(data.faces[f_idx].v[0],	data.faces[f_idx].v[1],	data.faces[f_idx].v[2]	);
		//clMsg("--- append end.");

		// Convert
		VIPM_Result*	VR		= 0;
		try						{
						VR		= make_progressive_vimp.VIPM_Convert			(u32(25),1.f,1);
		} 
		catch (...)	
		{
			progressive_clear	()		;
			// clMsg				("* mesh simplification failed: access violation");
		}
		if (0==VR)				{
			progressive_clear	()		;
			// clMsg				("* mesh simplification failed");
		}
		
		while (VR && VR->swr_records.size()>0)
		{
			// test metric
			u32		_full	= (u32)data.vertices.size	()		;
			u32		_remove	=	VR->swr_records.size()	;
			u32		_simple	=	_full - _remove			;
			float	_metric	=	float(_remove)/float(_full);
			if		(_metric<metric_limit)	
			{
				progressive_clear				()		;
				//clMsg	("* mesh simplified from [%4dv] to [%4dv], nf[%4d] ==> em[%0.2f]-discarded",_full,_simple,VR->indices.size()/3,metric_limit);
				break									;
			} 
			else 
			{
				// clMsg	("* mesh simplified from [%4dv] to [%4dv], nf[%4d] ==> em[%0.2f]-accepted", _full,_simple,VR->indices.size()/3,metric_limit);
			}

			// OK
			// Permute vertices
			for(u32 i=0; i<data.vertices.size(); i++)
				data.vertices[VR->permute_verts[i]]=_saved_vertices[i];

			// Fill indices
			data.faces.resize			(VR->indices.size()/3);
			for (u32 f_idx=0; f_idx<data.faces.size(); f_idx++){
				data.faces[f_idx].v[0]	= VR->indices[f_idx*3+0];
				data.faces[f_idx].v[1]	= VR->indices[f_idx*3+1];
				data.faces[f_idx].v[2]	= VR->indices[f_idx*3+2];
			}
			// Fill SWR
			data.m_SWI.count				= VR->swr_records.size();
			data.m_SWI.sw				= xr_alloc<FSlideWindow>(data.m_SWI.count);
			for (u32 swr_idx=0; swr_idx!=data.m_SWI.count; swr_idx++){
				FSlideWindow& dst	= data.m_SWI.sw[swr_idx];
				VIPM_SWR& src		= VR->swr_records[swr_idx];
				dst.num_tris		= src.num_tris;
				dst.num_verts		= src.num_verts;
				dst.offset			= src.offset;
			}

			break	;
		}
		// cleanup
		make_progressive_vimp.VIPM_Destroy			();
	}

	//////////////////////////////////////////////////////////////////////////
	// FAST-PATH
	if (progressive_test() && fast_path_data.vertices.size() && fast_path_data.faces.size())
	{
		// prepare progressive geom
		make_progressive_vimp.VIPM_Init				();
		Fvector2				zero; zero.set		(0,0);
		for (u32 v_idx=0;  v_idx<fast_path_data.vertices.size(); v_idx++)	
			make_progressive_vimp.VIPM_AppendVertex	(fast_path_data.vertices[v_idx].P,	zero);

		for (u32 f_idx=0;  f_idx<fast_path_data.faces.size();    f_idx++)	
			make_progressive_vimp.VIPM_AppendFace		( fast_path_data.faces[f_idx].v[0],	fast_path_data.faces[f_idx].v[1], fast_path_data.faces[f_idx].v[2]);

		VIPM_Result*	VR		= 0;
		try						{
			VR		= make_progressive_vimp.VIPM_Convert			(u32(25),1.f,1);
		} 
		catch (...)		
		{
			data.faces				= _saved_faces		;
			data.vertices			= _saved_vertices	;
			progressive_clear	()		;
			// clMsg				("* X-mesh simplification failed: access violation");
		}
		
		if (0==VR)				
		{
			data.faces				= _saved_faces		;
			data.vertices			= _saved_vertices	;
			progressive_clear	()		;
			// clMsg				("* X-mesh simplification failed");
		}
		else
		{
			// test metric
			u32		_full	= (u32)data.vertices.size	()		;
			u32		_remove	=	VR->swr_records.size()	;
			u32		_simple	=	_full - _remove			;
			float	_metric	=	float(_remove)/float(_full);
			// clMsg	("X mesh simplified from [%4dv] to [%4dv], nf[%4d]",_full,_simple,VR ? VR->indices.size()/3 : 0);

			// OK
			vec_XV					vertices_saved;

			// Permute vertices
			vertices_saved			= fast_path_data.vertices;
			for(u32 i=0; i<fast_path_data.vertices.size(); i++)
				fast_path_data.vertices[VR->permute_verts[i]]=vertices_saved[i];

			// Fill indices
			fast_path_data.faces.resize			(VR->indices.size()/3);
			for (u32 f_idx=0; f_idx<fast_path_data.faces.size(); f_idx++){
				fast_path_data.faces[f_idx].v[0]	= VR->indices[f_idx*3+0];
				fast_path_data.faces[f_idx].v[1]	= VR->indices[f_idx*3+1];
				fast_path_data.faces[f_idx].v[2]	= VR->indices[f_idx*3+2];
			}

			// Fill SWR
			fast_path_data.m_SWI.count				= VR->swr_records.size();
			fast_path_data.m_SWI.sw					= xr_alloc<FSlideWindow>(fast_path_data.m_SWI.count);
			for (u32 swr_idx=0; swr_idx!=fast_path_data.m_SWI.count; swr_idx++){
				FSlideWindow& dst	= fast_path_data.m_SWI.sw[swr_idx];
				VIPM_SWR& src		= VR->swr_records[swr_idx];
				dst.num_tris		= src.num_tris;
				dst.num_verts		= src.num_verts;
				dst.offset			= src.offset;
			}
		}

		// cleanup
		make_progressive_vimp.VIPM_Destroy			();
	}
}

void OGF_Base::Save	(IWriter &fs)
{
}

// Represent a node as HierrarhyVisual
void OGF_Node::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_HIERRARHY;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

extern u16	RegisterShader	(const char* T);

//LOD0
void OGF_LOD::Save		(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	string1024			sid;
	xr_strconcat(sid,
		pBuild->shader_render[pBuild->materials()[lod_Material].shader].name,
		"/",
		pBuild->textures()[pBuild->materials()[lod_Material].surfidx].name
		);
	fs.make_chunk(OGF_HEADER, [this, sid](IWriter& F)
	{
		ogf_header H;
		H.format_version = xrOGF_FormatVersion;
		H.type = MT_LOD;
		H.shader_id = RegisterShader(sid);
		H.bb.min = bbox.min;
		H.bb.max = bbox.max;
		H.bs.c = C;
		H.bs.r = R;
		F.w(&H,sizeof(H));
	});

	// Chields
	fs.make_chunk(OGF_CHILDREN_L, [this, sid](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});

	// Lod-def
	fs.make_chunk(OGF_LODDEF2, [this, sid](IWriter& F)
	{
		F.w(lod_faces,sizeof(lod_faces));
	});

	fs.make_chunk(OGF_DEBUG_DATA, [this](IWriter& F)
	{
		F.w_stringZ(debug_name.c_str());
	});
}

void OGF_MESH_LODS::Save(IWriter& fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_MESH_LODS;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

void OGF_LOD_MU0::Save(IWriter& fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_LOD0;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

//LOD1
void OGF_LOD_MU1::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_LOD1;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

//LOD2
void OGF_LOD_MU2::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_LOD2;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

//LOD3
void OGF_LOD_MU3::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_LOD3;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}

//LOD4
void OGF_LOD_MU4::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// Header
	fs.make_chunk(OGF_HEADER, [this](IWriter& F)
	{
		ogf_header H;
		H.format_version	= xrOGF_FormatVersion;
		H.type				= MT_LOD4;
		H.shader_id			= 0;
		H.bb.min			= bbox.min;
		H.bb.max			= bbox.max;
		H.bs.c				= C;
		H.bs.r				= R;
		F.w				(&H,sizeof(H));
	});

	// Children
	fs.make_chunk(OGF_CHILDREN_L, [this](IWriter& F)
	{
		F.w_u32(chields.size());
		F.w(chields.data(),chields.size()*sizeof(u32));
	});
}
