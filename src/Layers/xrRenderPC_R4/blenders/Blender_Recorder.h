#pragma once
#include "../RenderAPI.h"
#include "../tss.h"
#pragma pack(push,4)

class RENDER_API CBlender_Compile
{
public:
	sh_list				L_textures;
	sh_list				L_matrices;

	const char*				detail_texture;
	const float*			detail_scaler;

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
	ShaderExternalMap	Defines;

	void				AddDefine			(const char* Name, const char* Value = "") { Defines[Name] = Value; }

private:
	SPass				dest;
	R_constant_table	ctable;

	STextureList		passTextures;
	SMatrixList			passMatrices;

	string128			pass_vs;
	string128			pass_ps;
	string128			pass_gs;
	string128			pass_hs;
	string128			pass_ds;
	string128			pass_cs;

	bool				shaders_pending = false;
	bool				pass_compute = false;

	void				CreatePassShaders();

	u32					BC					(bool v)	{ return v?0x01:0; }
public:
	CSimulator&			R()					{ return RS; }
	
	void				SetParams			(int iPriority, bool bStrictB2F);
	void				SetPassPriority		(int iPriority);
	void				PassSET_ZB			(bool bZTest,	bool bZWrite, bool bInvertZTest=false);
	
	void				PassSET_Blend		(u32 idx, bool bABlend, u32 abSRC, u32 abDST, bool aTest, u32 aRef);
	void				PassSET_ablend_mode	(u32 idx, bool bABlend,	u32 abSRC, u32 abDST);
	void				PassSET_ablend_aref	(u32 idx, bool aTest,	u32 aRef);

	void				PassSET_Blend(bool bABlend, u32 abSRC, u32 abDST, bool aTest, u32 aRef);
	void				PassSET_ablend_mode	(bool bABlend,	u32 abSRC, u32 abDST);
	void				PassSET_ablend_aref	(bool aTest,	u32 aRef);

	void				r_Pass				(const char* vs,		const char* ps,		bool bFog,	bool	bZtest=true,				bool	bZwrite=true,			bool	bABlend=false,			D3DBLEND	abSRC=D3DBLEND_ONE,		D3DBLEND abDST=D3DBLEND_ZERO,	bool aTest=false,	u32 aRef=0);
	void				r_Pass				(const char* vs,		const char* gs, const char* ps,		bool bFog,	bool	bZtest=true,				bool	bZwrite=true,			bool	bABlend=false,			D3DBLEND	abSRC=D3DBLEND_ONE,		D3DBLEND abDST=D3DBLEND_ZERO,	bool aTest=false,	u32 aRef=0);
	void				r_TessPass			(const char* vs,	const char* hs, const char* ds, const char* gs, const char* ps, bool bFog, bool bZtest=true, bool bZwrite=true, bool bABlend=false,	D3DBLEND abSRC=D3DBLEND_ONE, D3DBLEND abDST=D3DBLEND_ZERO, bool aTest=false, u32 aRef=0);
	void				r_ComputePass		(const char* cs );
	void				r_Stencil(bool Enable, u32 Func=D3DCMP_ALWAYS, u32 Mask=0x00, u32 WriteMask=0x00, u32 Fail=D3DSTENCILOP_KEEP, u32 Pass=D3DSTENCILOP_KEEP, u32 ZFail=D3DSTENCILOP_KEEP);
	void				r_StencilRef(u32 Ref);
	void				r_CullMode(D3DCULL Mode);
	
	void				r_dx10Texture(const char* ResourceName, const char* texture);
	void				r_dx10Texture(const char* ResourceName, shared_str texture) { r_dx10Texture(ResourceName, texture.c_str()); }
	void				r_Matrix(const char* name);
	void				r_dx10Unbind(const char* ResourceName);
	void				r_End(bool clear = true);
	void				r_ColorWriteEnable( bool cR=true, bool cG=true, bool cB=true, bool cA=true);

	void				_cpp_Compile		(ShaderElement* _SH);
};
#pragma pack(pop)