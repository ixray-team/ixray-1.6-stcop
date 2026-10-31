// Shader.h: interface for the CShader class.
//
//////////////////////////////////////////////////////////////////////

#pragma once
#include "RenderAPI.h"

#include "r_constants.h"
#include "../../xrCore/xr_resource.h"

#include "SH_Atomic.h"
#include "SH_Texture.h"
#include "SH_Matrix.h"
#include "SH_Constant.h"
#include "SH_RT.h"

typedef xr_vector<shared_str>	sh_list;
class					CBlender_Compile;
class					IBlender;
#define	SHADER_PASSES_MAX		2
#define SHADER_ELEMENTS_MAX	7

#pragma pack(push,4)

//////////////////////////////////////////////////////////////////////////
struct RENDER_API STextureList : 
	public xr_resource_flagged, 
	public xr_vector<std::pair<u32,ref_texture> >	
{
	typedef xr_vector<std::pair<u32,ref_texture> > inherited_vec;
						~STextureList	();

						IC bool		equal (const STextureList& base) const
						{
							if (size()!=base.size())			return false;
							for (u32 cmp=0; cmp<size(); cmp++)	{
								if ((*this)[cmp].first	!=base[cmp].first)	return false;
								if ((*this)[cmp].second	!=base[cmp].second)	return false;
							}
							return true;
						}
	virtual		void	clear();
	virtual		void	clear_not_free();

	//	Avoid using this function.
	//	If possible use precompiled texture list.
	u32		find_texture_stage(const shared_str &TexName) const;

	STextureList();
	STextureList& operator=(const STextureList& Other) = delete;

	void _copy(const STextureList& Other);
};
typedef	resptr_core<STextureList,resptr_base<STextureList> >								ref_texture_list;
//////////////////////////////////////////////////////////////////////////
struct RENDER_API SMatrixList : public xr_resource_flagged, public FixedVector<ref_matrix, 4>
{
	SMatrixList();
	SMatrixList& operator=(const SMatrixList& Other) = delete;
	~SMatrixList();	
	
	void _copy(const SMatrixList& Other);
};

typedef	resptr_core<SMatrixList,resptr_base<SMatrixList> >									ref_matrix_list;
//////////////////////////////////////////////////////////////////////////
struct RENDER_API SGeometry
	: public xr_resource_flagged
{
	ref_declaration		dcl;
	IRHIBuffer*	vb;
	IRHIBuffer*	ib;
	u32					vb_stride;
						~SGeometry		();
						SGeometry& operator=(const SGeometry& Other) = delete;
};

struct RENDER_API resptrcode_geom:
	public resptr_base<SGeometry>
{
	void 				create			(RHIInputElementDesc* DescList, size_t DeclSize, IRHIBuffer* vb, IRHIBuffer* ib);
	void 				create			(D3DVERTEXELEMENT9* decl, IRHIBuffer* vb, IRHIBuffer* ib);
	void				create			(u32 FVF				, IRHIBuffer* vb, IRHIBuffer* ib);
	void				destroy			()			{ _set(nullptr);		}
	u32					stride			()	const	{ return _get()->vb_stride;	}
};

typedef	resptr_core<SGeometry,resptrcode_geom>												ref_geom;

//////////////////////////////////////////////////////////////////////////
struct	  RENDER_API		SPass			: public xr_resource_flagged									{
	ref_state							state;		// Generic state, like Z-Buffering, samplers, etc
	ref_ps								ps;			// may be NULL = FFP, in that case "state" must contain TSS setup
	ref_vs								vs;			// may be NULL = FFP, in that case "state" must contain RS setup, *and* FVF-compatible declaration must be used
	ref_gs								gs;			// may be NULL = don't use geometry shader at all
	ref_hs								hs;			// may be NULL = don't use hull shader at all
	ref_ds								ds;			// may be NULL = don't use domain shader at all
	ref_cs								cs;			// may be NULL = don't use compute shader at all
	ref_ctable							constants;	// may be NULL

	ref_texture_list					T;
	u8 iPriority = u8(-1);

	ref_matrix_list						M;

						~SPass			();

	bool equal(const SPass& other);
};
typedef	resptr_core<SPass,resptr_base<SPass> >												ref_pass;

//////////////////////////////////////////////////////////////////////////
struct 	 RENDER_API	ShaderElement	: public xr_resource_flagged									{
public:
	struct Sflags
	{
		u32	iPriority	:	2;
		u32	bStrictB2F	:	1;
		u32	bEmissive	:	1;
		u32	bDistort	:	1;
		u32	bWmark		:	1;
		u32 bLandscape	:	1;
		u32 bScopeMask	:	1;
	};
public:
	Sflags								flags;
	FixedVector<ref_pass,SHADER_PASSES_MAX>	passes;

						ShaderElement	();
						~ShaderElement	();

						ShaderElement& operator=(const ShaderElement& Other) = delete;

	bool				equal			(ShaderElement& S);
	bool				equal			(ShaderElement* S);
	void _copy(const ShaderElement& Other);
};
typedef	resptr_core<ShaderElement,resptr_base<ShaderElement> >								ref_selement;

//////////////////////////////////////////////////////////////////////////
struct 	 RENDER_API	Shader			: public xr_resource_flagged									{
public:
	ref_selement		E		[SHADER_ELEMENTS_MAX];	// material shaders: indexed by ShaderVariant, internal blenders: pass index

	shared_str			src_shader, src_textures, src_constants, src_matrices;
	int					src_skinning = -1;

	Shader();
	Shader& operator=(const Shader& Other) = delete;

						~Shader			();
	bool				equal			(Shader& S);
	bool				equal			(Shader* S);

	void _copy(Shader& Other);
};
struct 	 RENDER_API	resptrcode_shader	: public resptr_base<Shader>
{
	void				create			(const char* s_shader=nullptr, const char* s_textures=nullptr, const char* s_constants=nullptr, const char* s_matrices=nullptr);
	void				create			(IBlender*	B,	const char* s_shader=nullptr, const char* s_textures=nullptr, const char* s_constants=nullptr, const char* s_matrices=nullptr);
	void				destroy			()	{ _set(nullptr);		}
};
typedef	resptr_core<Shader,resptrcode_shader>												ref_shader;

namespace ShaderVariant
{
	enum : u32
	{
		NormalHQ,
		NormalLQ,
		Shadow,
		Reflection,
		Special,
		HUD,
		UI,
		Count,

		PointLight = Shadow,
		SpotLight = Reflection,
		ModelLighting = Special,
	};
}
static_assert(ShaderVariant::Count == SHADER_ELEMENTS_MAX);

#pragma pack(pop)

