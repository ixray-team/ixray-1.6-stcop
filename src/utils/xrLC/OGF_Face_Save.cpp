#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"
//#include "std_classes.h"
#include "../../xrCore/FS.h"
#include "../../xrEngine/Fmesh.h"

using namespace std;

extern u16	RegisterShader		(LPCSTR T);
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
		pBuild->shader_render[pBuild->materials()[material].shader].name,
		"/",
		Tname.c_str()
		);

	// Create header
	ogf_header			H;
	H.format_version	= xrOGF_FormatVersion;
	H.type				= data.m_SWI.count?MT_PROGRESSIVE:MT_NORMAL;
	H.shader_id			= RegisterShader			(sid);
	H.bb.min			= bbox.min;
	H.bb.max			= bbox.max;
	H.bs.c				= C;
	H.bs.r				= R;

	// Vertices
	const Shader_xrLC*	SH	=	pBuild->shaders().Get		(pBuild->materials()[material].reserved);
	bool bVertexColors	=	(SH->flags.bLIGHT_Vertex);
	
	switch (H.type) 
	{
	case MT_NORMAL		:
	case MT_PROGRESSIVE	:
		Save_Normal_PM	(fs,H,bVertexColors);		
		break;
	}

	// Header
	fs.open_chunk			(OGF_HEADER);
	fs.w					(&H,sizeof(H));
	fs.close_chunk			();
}

void OGF_Reference::Save	(IWriter &fs)
{
	OGF_Base::Save		(fs);

	// geom_batch_average	(vertices.size(),faces.size());	// don't use reference(s) as batch estimate

	// Texture & shader
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
		pBuild->shader_render[pBuild->materials()[material].shader].name,
		"/",
		Tname.c_str()
		);

	// Create header
	ogf_header			H;
	H.format_version	= xrOGF_FormatVersion;
	H.type				= model->data.m_SWI.count?MT_TREE_PM:MT_TREE_ST;
	H.shader_id			= RegisterShader	(sid);
	H.bb.min			= bbox.min;
	H.bb.max			= bbox.max;
	H.bs.c				= C;
	H.bs.r				= R;

	// Vertices
	fs.open_chunk		(OGF_GCONTAINER);
	fs.w_u32			(vb_id);
	fs.w_u32			(vb_start);
	fs.w_u32			((u32)model->data.vertices.size());

	fs.w_u32			(ib_id);
	fs.w_u32			(ib_start);
	fs.w_u32			((u32)model->data.faces.size()*3);
	fs.close_chunk		();

	// Special
	fs.open_chunk		(OGF_TREEDEF2);
	fs.w				(&xform,	sizeof(xform));
	fs.w				(&c_scale,	5*sizeof(float));
	fs.w				(&c_bias,	5*sizeof(float));
	fs.close_chunk		();

	// Header
	fs.open_chunk		(OGF_HEADER);
	fs.w				(&H,sizeof(H));
	fs.close_chunk		();

	// progressive
	if (H.type==MT_TREE_PM){
		// SW
		fs.open_chunk		(OGF_SWICONTAINER);
		fs.w_u32			(sw_id);
		fs.close_chunk		();
	}
}

void	OGF::PreSave		(u32 tree_id)
{
	const Shader_xrLC*	SH	=	pBuild->shaders().Get		(pBuild->materials()[material].reserved);
	bool bVertexColored	=	(SH->flags.bLIGHT_Vertex);

	// X-vertices/faces
	if (fast_path_data.vertices.size() && fast_path_data.faces.size())
	{
		// clMsg			("%4d: v(%3d)/f(%3d)",tree_id,fast_path_data.vertices.size(),fast_path_data.faces.size());

		VDeclarator		x_D;
		x_D.set			(x_decl_vert);
		x_VB.Begin		(x_D);
		for (itXV V=fast_path_data.vertices.begin(); V!=fast_path_data.vertices.end(); V++)
		{
			x_vert		v	(V->P);
			x_VB.Add		(&v,sizeof(v));
		}
		x_VB.End		(&fast_path_data.vb_id,&fast_path_data.vb_start);
		x_IB.Register	(LPWORD(&*fast_path_data.faces.begin()),LPWORD(&*fast_path_data.faces.end()),&fast_path_data.ib_id,&fast_path_data.ib_start);
	}

	// Vertices
	VDeclarator		D;
	if	(bVertexColored){
		// vertex-colored
		D.set			(r1_decl_vert);
		g_VB.Begin		(D);
		for (itOGF_V V=data.vertices.begin(); V!=data.vertices.end(); V++)
		{
			r1v_vert	v	(V->P,V->N,V->T,V->B,V->Color,V->UV[0]);
			g_VB.Add		(&v,sizeof(v));
		}
		g_VB.End		(&data.vb_id,&data.vb_start);
	}else{
		// lmap-colored
		D.set			(r1_decl_lmap);
		g_VB.Begin		(D);
		for (itOGF_V V=data.vertices.begin(); V!=data.vertices.end(); V++)
		{
			r1v_lmap	v	(V->P,V->N,V->T,V->B,V->Color,V->UV[0],V->UV[1]);
			g_VB.Add		(&v,sizeof(v));
		}
		g_VB.End		(&data.vb_id,&data.vb_start);
	}

	// Faces
	g_IB.Register		(LPWORD(&*data.faces.begin()),LPWORD(&*data.faces.end()),&data.ib_id,&data.ib_start);
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

void	write_ogf_fastpath( IWriter &fs, const OGF& ogf, BOOL progresive )
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

void	OGF::Save_Normal_PM		(IWriter &fs, ogf_header& H, BOOL bVertexColored)
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

enum OGF_FILE_SAVE
{
	eSaveHeaderData = 1,
	eSaveHeaderSWIData = 2,
	eSaveHeaderOGF = 3,
	eSaveHeaderRef = 4,
	eSaveHeaderBase = 5
};

// se7kills Saving OGF Files pre sectors build

// OGF BASE Save
void OGF_Base::SaveForCompile(IWriter* W)
{
	W->open_chunk(eSaveHeaderBase);

	W->w_s32(iLevel);
	W->w_u16(Sector);
	W->w_u8(bConnected);
	W->w(&bbox, sizeof(Fbox));
	W->w_fvector3(C);
	W->w_float(R);

	W->close_chunk();
}

void OGF_Base::LoadForCompile(IReader* Reader)
{
	if (Reader->open_chunk(eSaveHeaderBase))
	{
		iLevel = Reader->r_s32();
		Sector = Reader->r_u16();
		bConnected = Reader->r_u8();
		Reader->r(&bbox, sizeof(Fbox));
		Reader->r_fvector3(C);
		R = Reader->r_float();
	}
}


// OGF MAIN
void OGF::SaveForCompile(IWriter* W)
{
	OGF_Base::SaveForCompile(W);

	{
		W->open_chunk(eSaveHeaderData);

		// Save Data
		W->w_u32(data.vb_id);
		W->w_u32(data.vb_start);
		W->w_u32(data.ib_id);
		W->w_u32(data.ib_start);
		W->w_u32(data.sw_id);


		W->w_u32(data.m_SWI.count);
		W->w_u32(data.m_SWI.reserved[0]);
		W->w_u32(data.m_SWI.reserved[1]);
		W->w_u32(data.m_SWI.reserved[2]);
		W->w_u32(data.m_SWI.reserved[3]);

		if (data.m_SWI.sw != nullptr)
		{
			W->w_u8(1);
			W->w_u32(data.m_SWI.sw->num_tris);
			W->w(data.m_SWI.sw, data.m_SWI.count * sizeof(FSlideWindow));
		}
		else
		{
			W->w_u8(0);
		}
		

		// Saving Faces

		W->w_u32(data.vertices.size());
		for (auto& V : data.vertices)
		{
			W->w(&V, sizeof(OGF_Vertex));
		}

		W->w_u32(data.faces.size());
		for (auto& F : data.faces)
		{
			W->w(&F, sizeof(OGF_Face));
		}

		W->close_chunk();
	}

	{
		W->open_chunk(eSaveHeaderSWIData);

		// Saving FastPath
		W->w_u32(fast_path_data.vb_id);
		W->w_u32(fast_path_data.vb_start);
		W->w_u32(fast_path_data.ib_id);
		W->w_u32(fast_path_data.ib_start);
		W->w_u32(fast_path_data.sw_id);
		W->w_u32(fast_path_data.m_SWI.count);
		W->w_u32(fast_path_data.m_SWI.reserved[0]);
		W->w_u32(fast_path_data.m_SWI.reserved[1]);
		W->w_u32(fast_path_data.m_SWI.reserved[2]);
		W->w_u32(fast_path_data.m_SWI.reserved[3]);

		if (fast_path_data.m_SWI.sw)
		{
			W->w_u8(1);
			W->w_u32(fast_path_data.m_SWI.sw->num_tris);
			W->w(fast_path_data.m_SWI.sw, fast_path_data.m_SWI.count * sizeof(FSlideWindow));
		}
		else 
			W->w_u8(0);

		// Saving Faces

		W->w_u32(fast_path_data.vertices.size());
		for (auto& V : fast_path_data.vertices)
		{
			W->w(&V.P, sizeof(V.P));
		}

		W->w_u32(fast_path_data.faces.size());
		for (auto& F : fast_path_data.faces)
		{
			W->w(&F, sizeof(OGF_Face));
		}
		W->close_chunk();
	}

	// Save Other OGF INFO
	{
		W->open_chunk(eSaveHeaderOGF);

		W->w_u32(material);
		W->w_u32(dwRelevantUV);
		W->w_u32(dwRelevantUVMASK);

		// Textures
		W->w_u32(textures.size());
		for (auto& T : textures)
		{
 			W->w_stringZ(T.name);
			W->w(&T.pBuildSurface, sizeof(b_vertex));
		}

		W->close_chunk();
	}
}

void OGF::LoadForCompile(IReader* R)
{
	OGF_Base::LoadForCompile(R);

 	if (R->open_chunk(eSaveHeaderData))
	{
 		// Save Data
		data.vb_id						= R->r_u32();
		data.vb_start					= R->r_u32();
		data.ib_id						= R->r_u32();
		data.ib_start					= R->r_u32();
		data.sw_id						= R->r_u32();
		data.m_SWI.count				= R->r_u32();
		data.m_SWI.reserved[0]			= R->r_u32();
		data.m_SWI.reserved[1]			= R->r_u32();
		data.m_SWI.reserved[2]			= R->r_u32();
		data.m_SWI.reserved[3]			= R->r_u32();

		bool isExistSW = R->r_u8();
		if (isExistSW)
		{
			u32 cnt = R->r_u32();
 			FSlideWindow* fslide = xr_alloc<FSlideWindow>(cnt * sizeof(FSlideWindow));
			R->r(fslide, cnt * sizeof(FSlideWindow));

			data.m_SWI.sw = fslide;
			data.m_SWI.sw->num_tris = cnt;
		}
		

		// Saving Faces

		int cntV = R->r_u32();
		data.vertices.resize(cntV);
		data.vertices.clear();
		for (int V = 0; V< cntV; V++)
		{
 			R->r(&data.vertices[V], sizeof(OGF_Vertex));
		}

		int cntF = R->r_u32();
		data.faces.clear();
		data.faces.resize(cntF);
		for (int F = 0; F < cntF; F++)
		{
			R->r(&data.faces[F], sizeof(OGF_Face));
		}
 	}

  
	if (R->open_chunk(eSaveHeaderSWIData) )
	{
 		// Saving FastPath
		fast_path_data.vb_id				= R->r_u32();
		fast_path_data.vb_start				= R->r_u32();
		fast_path_data.ib_id				= R->r_u32();
		fast_path_data.ib_start				= R->r_u32();
		fast_path_data.sw_id				= R->r_u32();
		fast_path_data.m_SWI.count			= R->r_u32();
		fast_path_data.m_SWI.reserved[0]	= R->r_u32();
		fast_path_data.m_SWI.reserved[1]	= R->r_u32();
		fast_path_data.m_SWI.reserved[2]	= R->r_u32();
		fast_path_data.m_SWI.reserved[3]	= R->r_u32();
		 
		bool Load = R->r_u8();
		if (Load)
		{
			u32 cnt = R->r_u32();

			FSlideWindow* fslide = xr_alloc<FSlideWindow>( cnt * sizeof(FSlideWindow) );
			R->r(fslide, cnt * sizeof(FSlideWindow));
			
			fast_path_data.m_SWI.sw = fslide;
			fast_path_data.m_SWI.sw->num_tris = cnt;
		}
 
		// Saving Faces
 		int cntV = R->r_u32();
		fast_path_data.vertices.clear();
		for (int V = 0; V< cntV; V++)
		{
			x_vertex vert;
			R->r(&vert, sizeof(x_vertex));
			fast_path_data.vertices.push_back(vert);
		}

		int cntF = R->r_u32();
		fast_path_data.faces.clear();
		fast_path_data.faces.resize(cntF);
		for (int F = 0; F < cntF; F++)
		{
			R->r(&fast_path_data.faces[F], sizeof(OGF_Face));
		}
 	}

	// Save Other OGF INFO
	if (R->open_chunk(eSaveHeaderOGF))
	{
 		material		 = R->r_u32();
		dwRelevantUV	 = R->r_u32();
		dwRelevantUVMASK = R->r_u32();

		// Textures
		int cntT = R->r_u32();
		textures.clear();
		textures.resize(cntT);
		for (int T = 0; T< cntT; T++)
		{
			R->r_stringZ(textures[T].name);

			b_texture* tex = new b_texture();
 			R->r(&tex , sizeof(b_vertex));
			textures[T].pBuildSurface = tex;
			
		}
	}
}


// Save Reference


void OGF_Reference::SaveForCompile(IWriter* W)
{
	model->SaveForCompile(W);
	
	W->open_chunk(eSaveHeaderRef);

	W->w_u32(material);	
	W->w_u32(vb_id);
	W->w_u32(vb_start);
	W->w_u32(ib_id);
	W->w_u32(ib_start);
	W->w_u32(sw_id);
	W->w(&xform, sizeof(Fmatrix));
	W->w(&c_scale, sizeof(base_color_c));
	W->w(&c_bias, sizeof(base_color_c));

	W->w_u32(textures.size());
	for (auto& T : textures)
	{
		W->w_stringZ(T.name);
		
		
		W->w(T.pBuildSurface, sizeof(b_texture));
	}

	W->close_chunk();
}

void OGF_Reference::LoadForCompile(IReader* R)
{
	model->LoadForCompile(R);

	if (R->open_chunk(eSaveHeaderRef))
	{
		material = R->r_u32();
		vb_id = R->r_u32();
		vb_start = R->r_u32();
		ib_id = R->r_u32();
		ib_start = R->r_u32();
		sw_id = R->r_u32();
		R->r(&xform, sizeof(Fmatrix));
		R->r(&c_scale, sizeof(base_color_c));
		R->r(&c_bias, sizeof(base_color_c));

		int cntT = R->r_u32();
		textures.clear();
		textures.resize(cntT);
		for (auto T = 0; T < cntT; T++)
		{
			R->r_stringZ(textures[T].name);
			
			b_texture* tex = new b_texture();
			R->r(tex, sizeof(b_texture));
			textures[T].pBuildSurface = tex;
		}
	}
}

