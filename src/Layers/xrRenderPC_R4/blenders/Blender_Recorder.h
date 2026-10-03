#pragma once
#include "../tss.h"
#pragma pack(push,4)

class  CBlender_Compile  
{
public:
	sh_list				L_textures;
	sh_list				L_constants;
	sh_list				L_matrices;

	const char*				detail_texture;
	RHIShaderConstant::Setup*	detail_scaler;

	bool				bEditor;
	bool				bDetail;
	bool				bDetail_Diffuse;
	bool				bDetail_Bump;
	bool				bUseSteepParallax;
	bool				bHudElement;
	int					iElement;

public:
	CSimulator			RS;
	IBlender*			BT;
	ShaderElement*		SH;
	enum {
		NO_TESS = 0,
		TESS_PN = 1 << 0,
		TESS_HM = 1 << 1,
		TESS_PN_HM = TESS_PN | TESS_HM
	};
	u32	TessMethod;

private:
	SPass				dest;
	R_constant_table	ctable;

	STextureList		passTextures;
	SMatrixList			passMatrices;
	SConstantList		passConstants;
	u32					dwStage;

	string128			pass_vs;
	string128			pass_ps;
	string128			pass_gs;
	string128			pass_hs;
	string128			pass_ds;
	string128			pass_cs;

	struct PassBind
	{
		char name[64];
		u32 slot;
		char space;
	};
	PassBind			pass_binds[32] = {};
	u32					pass_bind_count = 0;
	u32					next_tex = 0;
	u32					next_samp = 0;
	bool				shaders_pending = false;
	bool				pass_compute = false;

	const PassBind*		FindBind(const char* name, char space) const;
	void				AddBind(const char* name, u32 slot, char space);
	RHIShaderConstant*	AddConstant(const char* name);
	void				r_Setup(const char* name, RHIShaderConstant::Setup* s);
	void				CreatePassShaders();

	u32					BC					(bool v)	{ return v?0x01:0; }
public:
	CSimulator&			R()					{ return RS; }
	
	void				SetParams			(int iPriority, bool bStrictB2F);
	void				SetMapping			();
	void				SetPassPriority		(int iPriority);
	// R1-compiler
	void				PassBegin			();
	u32					Pass				()  { return SH->passes.size(); }
	void				PassSET_ZB			(bool bZTest,	bool bZWrite, bool bInvertZTest=false);
	
	void				PassSET_Blend		(u32 idx, bool bABlend, u32 abSRC, u32 abDST, bool aTest, u32 aRef);
	void				PassSET_ablend_mode	(u32 idx, bool bABlend,	u32 abSRC, u32 abDST);
	void				PassSET_ablend_aref	(u32 idx, bool aTest,	u32 aRef);

	void				PassSET_Blend(bool bABlend, u32 abSRC, u32 abDST, bool aTest, u32 aRef);
	void				PassSET_ablend_mode	(bool bABlend,	u32 abSRC, u32 abDST);
	void				PassSET_ablend_aref	(bool aTest,	u32 aRef);

	void				PassSET_Blend_BLEND	(bool bAref=false, u32 ref=0)	{ PassSET_Blend	(true,D3DBLEND_SRCALPHA,D3DBLEND_INVSRCALPHA,bAref,ref);	}
	void				PassSET_Blend_SET	(bool bAref=false, u32 ref=0)	{ PassSET_Blend	(false,D3DBLEND_ONE,D3DBLEND_ZERO,bAref,ref);				}
	void				PassSET_Blend_ADD	(bool bAref=false, u32 ref=0)	{ PassSET_Blend	(true, D3DBLEND_ONE,D3DBLEND_ONE, bAref,ref);				}
	void				PassSET_Blend_MUL	(bool bAref=false, u32 ref=0)	{ PassSET_Blend	(true, D3DBLEND_DESTCOLOR,D3DBLEND_ZERO,bAref,ref);			}
	void				PassSET_Blend_MUL2X	(bool bAref=false, u32 ref=0)	{ PassSET_Blend	(true, D3DBLEND_DESTCOLOR,D3DBLEND_SRCCOLOR,bAref,ref);		}
	void				PassSET_LightFog	(bool bLight, bool bFog);
	void				PassSET_PS			(const char* name);
	void				PassSET_VS			(const char* name);
	void				PassEnd				();

	void				StageBegin			();
	u32					Stage				()	{ return dwStage; }
	void				StageSET_Address	(u32 adr);
	void				StageSET_Color(u32 a1, u32 op, u32 a2);
	void				Stage_Texture		(const char* name, u32 address=D3DTADDRESS_WRAP,	u32	 fmin=D3DTEXF_LINEAR, u32 fmip=D3DTEXF_LINEAR,	u32 fmag=D3DTEXF_LINEAR);
	void				Stage_Matrix(const char* name, int UVW_channel);
	void				StageSET_Alpha(u32 a1, u32 op, u32 a2);
	void				StageSET_XForm(u32 tf, u32 tc);
	void				StageSET_Color3(u32 a1, u32 op, u32 a2, u32 a3);
	void				StageSET_TMC(const char* T, const char* M, const char* C, int UVW_channel);
	void				Stage_Constant		(const char* name);
	void				StageEnd			();

	void				i_Address		(u32 s, u32		address);
	void				i_Filter_Min	(u32 s, u32		f);
	void				i_Filter_Mip	(u32 s, u32		f);
	void				i_Filter_Mag	(u32 s, u32		f);
	void				i_FilterAnizo	(u32 s, bool	value);
	void				i_Filter		(u32 s, u32 _min, u32 _mip, u32 _mag);
	void				i_BorderColor	(u32 s, u32 color);

	u32					i_Sampler			(const char* name);
	void				i_Texture			(u32 s, const char*	name);
	void				i_Projective		(u32 s, bool	b);

	// R1/R2-compiler	[programmable]		- templates
	void				r_Pass				(const char* vs,		const char* ps,		bool bFog,	bool	bZtest=true,				bool	bZwrite=true,			bool	bABlend=false,			D3DBLEND	abSRC=D3DBLEND_ONE,		D3DBLEND abDST=D3DBLEND_ZERO,	bool aTest=false,	u32 aRef=0);
	void				r_Pass				(const char* vs,		const char* gs, const char* ps,		bool bFog,	bool	bZtest=true,				bool	bZwrite=true,			bool	bABlend=false,			D3DBLEND	abSRC=D3DBLEND_ONE,		D3DBLEND abDST=D3DBLEND_ZERO,	bool aTest=false,	u32 aRef=0);
	void				r_Constant			(const char* name, RHIShaderConstant::Setup* s);
	void				r_TessPass			(const char* vs,	const char* hs, const char* ds, const char* gs, const char* ps, bool bFog, bool bZtest=true, bool bZwrite=true, bool bABlend=false,	D3DBLEND abSRC=D3DBLEND_ONE, D3DBLEND abDST=D3DBLEND_ZERO, bool aTest=false, u32 aRef=0);
	void				r_ComputePass		(const char* cs );
	void				r_Stencil(bool Enable, u32 Func=D3DCMP_ALWAYS, u32 Mask=0x00, u32 WriteMask=0x00, u32 Fail=D3DSTENCILOP_KEEP, u32 Pass=D3DSTENCILOP_KEEP, u32 ZFail=D3DSTENCILOP_KEEP);
	void				r_StencilRef(u32 Ref);
	void				r_CullMode(D3DCULL Mode);
	
	void				r_dx10Texture(const char* ResourceName, const char* texture, u32 slot = u32(-1));
	void				r_dx10Texture(const char* ResourceName, shared_str texture) { r_dx10Texture(ResourceName, texture.c_str()); }
	u32					r_dx10Sampler(const char* ResourceName);
	void				r_dx10Unbind(const char* ResourceName);
	void				r_End(bool clear = true);
	void				r_ColorWriteEnable( bool cR=true, bool cG=true, bool cB=true, bool cA=true);

	//

	CBlender_Compile	();
	~CBlender_Compile	();
	
	void				_cpp_Compile		(ShaderElement* _SH);
	ShaderElement* 		_lua_Compile		(const char* namesp, const char* name);
};
#pragma pack(pop)