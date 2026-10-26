#include "StdAfx.h"
#include "../Build/Build.h"
#include "OGF_Face.h"
//#include "std_classes.h"
#include "../../../../xrCore/FS.h"
#include "../../../../xrEngine/Fmesh.h"

using namespace std;

extern u16	RegisterShader		(const char* T);
extern void	geom_batch_average	(u32 verts, u32 faces);

u32						u8_vec4			(Fvector N, u8 A=0)
{
	N.add				(1.f);
	N.mul				(.5f*255.f);
	s32 nx				= iFloor(N.x);	clamp(nx,0,255);
	s32 ny				= iFloor(N.y);	clamp(ny,0,255);
	s32 nz				= iFloor(N.z);	clamp(nz,0,255);
	return				color_rgba(nx,ny,nz,A);
}
u32						u8_vec4			(base_basis N, u8 A=0)
{
	return				color_rgba		(N.x,N.y,N.z,A);
}
std::pair<s16,u8>		s24_tc_base		(float uv)		// [-32 .. +32]
{
	const u32	max_tile	=	32;
	const s32	quant		=	32768/max_tile;

	float		rebased		=	uv*float	(quant);
	s32			_primary	=	iFloor		(rebased);							clamp(_primary,		-32768,	32767	);
	s32			_secondary	=	iFloor		(255.5f*(rebased-float(_primary)));	clamp(_secondary,	0,		255		);
	return		std::make_pair		(s16(_primary),u8(_secondary));
}

s16						s16_tc_lmap		(float uv)		// [-1 .. +1]
{
	const u32	max_tile	=	1;
	const s32	quant		=	32768/max_tile;

	s32			t			=	iFloor	(uv*float(quant)); clamp(t,-32768,32767);
	return	s16	(t);
}

D3DVERTEXELEMENT9		r1_decl_lmap	[] =	// 12+4+4+4+4+4	= 32
{
	{0, 0,  D3DDECLTYPE_FLOAT3,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },
	{0, 12, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_NORMAL,	0 },
	{0, 16, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TANGENT,	0 },
	{0, 20, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_BINORMAL,	0 },
	{0, 24, D3DDECLTYPE_SHORT2,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TEXCOORD,	0 },
	{0, 28, D3DDECLTYPE_SHORT2,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TEXCOORD,	1 },
	D3DDECL_END()
};
D3DVERTEXELEMENT9		r1_decl_vert	[] =	// 12+4+4+4+4+4 = 32
{
	{0, 0,  D3DDECLTYPE_FLOAT3,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },
	{0, 12, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_NORMAL,	0 },
	{0, 16, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TANGENT,	0 },
	{0, 20, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_BINORMAL,	0 },
	{0, 24, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_COLOR,		0 },
	{0, 28, D3DDECLTYPE_SHORT2,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TEXCOORD,	0 },
	D3DDECL_END()
};
D3DVERTEXELEMENT9		x_decl_vert		[] =	// 12
{
	{0, 0,  D3DDECLTYPE_FLOAT3,		D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },
	D3DDECL_END()
};
#pragma pack(push,1)
struct  x_vert	{
	Fvector3	P;
	x_vert		(Fvector3 _P)		{
		P		= _P;
	}
};
struct  r1v_lmap	{
	Fvector3	P;
	u32			N;
	u32			T;
	u32			B;
	s16			tc0x,tc0y;
	s16			tc1x,tc1y;

	r1v_lmap	(Fvector3 _P, Fvector _N, base_basis _T, base_basis _B, base_color _CC, Fvector2 tc_base, Fvector2 tc_lmap )
	{
		base_color_c	_C;	_CC._get	(_C);
		_N.normalize		();
		std::pair<s16,u8>	tc_u		= s24_tc_base	(tc_base.x);
		std::pair<s16,u8>	tc_v		= s24_tc_base	(tc_base.y);
		P				= _P;
		N				= u8_vec4		(_N,u8_clr(_C.hemi));
		T				= u8_vec4		(_T,tc_u.second);
		B				= u8_vec4		(_B,tc_v.second);
		tc0x			= tc_u.first	;	
		tc0y			= tc_v.first	;
		tc1x			= s16_tc_lmap	(tc_lmap.x);
		tc1y			= s16_tc_lmap	(tc_lmap.y);
	}
};
struct  r1v_vert	{
	Fvector3	P;
	u32			N;
	u32			T;
	u32			B;
	u32			C;
	s16			tc0x,tc0y;

	r1v_vert	(Fvector3 _P, Fvector _N, base_basis _T, base_basis _B, base_color _CC, Fvector2 tc_base)
	{
		base_color_c	_C;	_CC._get	(_C);
		_N.normalize	();
		std::pair<s16,u8>	tc_u		= s24_tc_base	(tc_base.x);
		std::pair<s16,u8>	tc_v		= s24_tc_base	(tc_base.y);
		P				= _P;
		N				= u8_vec4		(_N,u8_clr(_C.hemi));
		T				= u8_vec4		(_T,tc_u.second	);
		B				= u8_vec4		(_B,tc_v.second	);
		C				= color_rgba	(u8_clr(_C.rgb.x),u8_clr(_C.rgb.y),u8_clr(_C.rgb.z),u8_clr(_C.sun));
		tc0x			= tc_u.first	;	
		tc0y			= tc_v.first	;
	}
};
#pragma pack(pop)

void OGF::Save			(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// clMsg			("* %d faces",faces.size());
	geom_batch_average	((u32)data.vertices.size(),(u32)data.faces.size());

	// Create header
	ogf_header			H;
	H.format_version	= xrOGF_FormatVersion;
	H.type				= data.m_SWI.count?MT_PROGRESSIVE:MT_NORMAL;
	if (bSharedMaterial)
	{
		H.shader_id = 0;
	} else
	{
		// Texture & shader
		std::string			Tname;
		for (u32 i=0; i<textures.size(); i++)	{
			if (!Tname.empty()) Tname += ',';
			string256		t;
			xr_strcpy			(t,*textures[i].name);
			if (strchr(t,'.')) *strchr(t,'.')=0;
			Tname			+= t;
		}
		string1024			sid;
		xr_strconcat(sid,
			pBuild->GetMaterialShaderName(material, bSharedMaterial),
			"/",
			Tname.c_str()
			);
		H.shader_id = RegisterShader(sid);
	}
	H.bb.min			= bbox.min;
	H.bb.max			= bbox.max;
	H.bs.c				= C;
	H.bs.r				= R;

	// Vertices
	auto& SH = pBuild->shaders().Get(pBuild->GetMaterialReserved(material, bSharedMaterial));
	bool bVertexColors	= SH.flags.bLIGHT_Vertex;
	
	switch (H.type) 
	{
	case MT_NORMAL		:
	case MT_PROGRESSIVE	:
		Save_Normal_PM	(fs,H,bVertexColors);		
		break;
	}

	// Header
	fs.make_chunk(OGF_HEADER, [this, H](IWriter& F)
	{
		F.w(&H,sizeof(H));
	});
	
	fs.make_chunk(OGF_SHARED_MATERIAL_SETTINGS, [this](IWriter& F)
	{
		F.w_u8(bSharedMaterial);
		if (bSharedMaterial)
		{
			F.w_stringZ(pBuild->materials_shared()[material].Name);
			R_ASSERT(textures.size()>0);
			xr_string ShaderDescr = "";
			for (u32 i = 1; i < textures.size(); i++)
			{
				ShaderDescr.append(",");
				ShaderDescr.append(textures[i].name.c_str());
			}
			F.w_stringZ(ShaderDescr);
		}
	});

	fs.make_chunk(OGF_DEBUG_DATA, [this](IWriter& F)
	{
		F.w_stringZ(debug_name.c_str());
	});
}

void OGF_Reference::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);
	
	// Create header
	ogf_header			H;
	H.format_version	= xrOGF_FormatVersion;
	H.type				= model->data.m_SWI.count?MT_TREE_PM:MT_TREE_ST;
	if (bSharedMaterial)
	{
		H.shader_id = 0;
	} else
	{
		std::string			Tname;
		for (u32 i=0; i<textures.size(); i++)
		{
			if (!Tname.empty()) Tname += ',';
			string256		t;
			xr_strcpy			(t,*textures[i].name);
			if (strchr(t,'.')) *strchr(t,'.')=0;
			Tname			+= t;
		}
		string1024			sid	;
		xr_strconcat(sid,
			pBuild->GetMaterialShaderName(material, bSharedMaterial),
			"/",
			Tname.c_str()
			);
		
		H.shader_id = RegisterShader(sid);
	}
	H.bb.min			= bbox.min;
	H.bb.max			= bbox.max;
	H.bs.c				= C;
	H.bs.r				= R;

	// Vertices
	fs.make_chunk(OGF_GCONTAINER, [this](IWriter& F)
	{
		F.w_u32(vb_id);
		F.w_u32(vb_start);
		F.w_u32(model->data.vertices.size());
		F.w_u32(ib_id);
		F.w_u32(ib_start);
		F.w_u32(model->data.faces.size()*3);
	});

	// Special
	fs.make_chunk(OGF_TREEDEF2, [this](IWriter& F)
	{
		F.w(&xform, sizeof(xform));
		F.w(&c_scale, 5*sizeof(float));
		F.w(&c_bias, 5*sizeof(float));
	});

	// Header
	fs.make_chunk(OGF_HEADER, [this, H](IWriter& F)
	{
		F.w(&H,sizeof(H));
	});
	
	fs.make_chunk(OGF_SHARED_MATERIAL_SETTINGS, [this](IWriter& F)
	{
		F.w_u8(bSharedMaterial);
		if (bSharedMaterial)
		{
			F.w_stringZ(pBuild->materials_shared()[material].Name);
			xr_string ShaderDescr = "";
			for (u32 i = 1; i < textures.size(); i++)
			{
				ShaderDescr.append(",");
				ShaderDescr.append(textures[i].name.c_str());
			}
			F.w_stringZ(ShaderDescr);
		}
	});

	fs.make_chunk(OGF_DEBUG_DATA, [this](IWriter& F)
	{
		F.w_stringZ(debug_name.c_str());
	});

	// progressive
	if (H.type==MT_TREE_PM){
		// SW
		fs.make_chunk(OGF_SWICONTAINER, [this](IWriter& F)
		{
			F.w_u32(sw_id);
		});
	}
}

void	OGF::PreSave		(u32 tree_id)
{
	auto& SH = pBuild->shaders().Get(pBuild->GetMaterialReserved(material, bSharedMaterial));
	bool bVertexColored	= SH.flags.bLIGHT_Vertex;

	// X-vertices/faces
	if (!fast_path_data.vertices.empty() && !fast_path_data.faces.empty())
	{
		// clMsg			("%4d: v(%3d)/f(%3d)",tree_id,fast_path_data.vertices.size(),fast_path_data.faces.size());

		VDeclarator		x_D;
		x_D.set			(x_decl_vert);
		x_VB.Begin		(x_D);
		for (auto& vertice : fast_path_data.vertices)
		{
			x_vert		v	(vertice.P);
			x_VB.Add		(&v,sizeof(v));
		}
		x_VB.End		(&fast_path_data.vb_id,&fast_path_data.vb_start);
		x_IB.Register	(LPWORD(fast_path_data.faces.data()),LPWORD(fast_path_data.faces.data() + fast_path_data.faces.size()),&fast_path_data.ib_id,&fast_path_data.ib_start);
	}

	// Vertices
	VDeclarator		D;
	if	(bVertexColored){
		// vertex-colored
		D.set			(r1_decl_vert);
		g_VB.Begin		(D);
		for (auto& vertice : data.vertices)
		{
			r1v_vert	v	(vertice.P, vertice.N, vertice.T, vertice.B, vertice.Color, vertice.UV[0]);
			g_VB.Add		(&v,sizeof(v));
		}
		g_VB.End		(&data.vb_id,&data.vb_start);
	}else{
		// lmap-colored
		D.set			(r1_decl_lmap);
		g_VB.Begin		(D);
		for (auto& vertice : data.vertices)
		{
			r1v_lmap	v	(vertice.P, vertice.N, vertice.T, vertice.B, vertice.Color, vertice.UV[0], vertice.UV[1]);
			g_VB.Add		(&v,sizeof(v));
		}
		g_VB.End		(&data.vb_id,&data.vb_start);
	}

	// Faces
	g_IB.Register		(LPWORD(data.faces.data()),LPWORD(data.faces.data() + data.faces.size()),&data.ib_id,&data.ib_start);
}

template<typename ogf_data_type>
void	write_ogf_container( IWriter &fs, const ogf_data_type& ogf_cnt )
{
	fs.open_chunk	( OGF_GCONTAINER );
	fs.w_u32		( ogf_cnt.vb_id );
	fs.w_u32		( ogf_cnt.vb_start );
	fs.w_u32		( (u32)ogf_cnt.vertices.size() );

	fs.w_u32		( ogf_cnt.ib_id );
	fs.w_u32		( ogf_cnt.ib_start );
	fs.w_u32		( (u32)ogf_cnt.faces.size()*3 );
	fs.close_chunk	( );
}
 
void	write_ogf_swidata( IWriter &fs, const FSlideWindowItem& swi )
{
	fs.open_chunk		( OGF_SWIDATA		);
	fs.w_u32			( swi.reserved[0]	);
	fs.w_u32			( swi.reserved[1]	);
	fs.w_u32			( swi.reserved[2]	);
	fs.w_u32			( swi.reserved[3]	);
	fs.w_u32			( swi.count		);
	fs.w				( swi.sw, swi.count*sizeof(FSlideWindow) );
	fs.close_chunk		();
} 

void	write_ogf_fastpath( IWriter &fs, const OGF& ogf, bool progresive )
{
	fs.open_chunk			( OGF_FASTPATH		);
	{
		// Vertices
		write_ogf_container( fs, ogf.fast_path_data );

		// progressive-data, if need it
		if ( progresive )//H.type == MT_PROGRESSIVE
				write_ogf_swidata( fs, ogf.fast_path_data.m_SWI );
	}
	fs.close_chunk			();
}

void	OGF::Save_Normal_PM		(IWriter &fs, ogf_header& H, bool bVertexColored)
{
//	clMsg			("- saving: normal or clod");

	// Vertices
	write_ogf_container( fs, data ); 

	// progressive-data, if need it
	if (H.type == MT_PROGRESSIVE)
		write_ogf_swidata( fs, data.m_SWI );// SW

	// if has x-vertices/x-faces
	if (!fast_path_data.vertices.empty() && !fast_path_data.faces.empty())
		write_ogf_fastpath( fs, *this, H.type == MT_PROGRESSIVE );
	
}