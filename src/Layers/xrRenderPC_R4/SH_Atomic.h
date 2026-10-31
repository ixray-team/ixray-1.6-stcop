#ifndef sh_atomicH
#define sh_atomicH
#pragma once
#include "RenderAPI.h"
#include "../../xrCore/xr_resource.h"
#include "tss_def.h"

#	include "StateManager/GfxPipelineState.h"

#pragma pack(push,4)


//////////////////////////////////////////////////////////////////////////
// Atomic resources
//////////////////////////////////////////////////////////////////////////
enum class EVertexInput : u8
{
	None,
	Legacy,
	Pulled
};

struct RENDER_API SVS : public xr_resource_uniq
{
	RHIObject*					vs;
	RHIBlob*							Signature = nullptr;
	u32									instance_buffer_slot = u32(-1);
	u32									editor_instance_slot = u32(-1);
	EVertexInput						Input = EVertexInput::None;
	bool								MismatchLogged = false;
	SVS				();
	~SVS			();
};
typedef	resptr_core<SVS,resptr_base<SVS> >	ref_vs;

//////////////////////////////////////////////////////////////////////////
struct RENDER_API SPS : public xr_resource_uniq
{
	RHIObject*					ps;
	~SPS			();
};
typedef	resptr_core<SPS,resptr_base<SPS> > ref_ps;

//////////////////////////////////////////////////////////////////////////
struct RENDER_API SGS : public xr_resource_uniq
{
	RHIObject*					gs;
	~SGS			();
};
typedef	resptr_core<SGS,resptr_base<SGS> > ref_gs;

struct RENDER_API SHS : public xr_resource_uniq
{
	RHIObject*					sh;
	~SHS			();
};
typedef	resptr_core< SHS, resptr_base<SHS> >	ref_hs;

struct RENDER_API SDS : public xr_resource_uniq
{
	RHIObject*					sh;
	~SDS			();
};
typedef	resptr_core< SDS, resptr_base<SDS> >	ref_ds;

struct RENDER_API SCS : public xr_resource_uniq
{
	RHIObject*					sh;
	~SCS			();
};
typedef	resptr_core< SCS, resptr_base<SCS> >	ref_cs;


//////////////////////////////////////////////////////////////////////////
struct RENDER_API SState : public xr_resource_flagged
{
	GfxPipelineState*							state;
	SimulatorStates						state_code;
	~SState			();
};
typedef	resptr_core<SState,resptr_base<SState> >	ref_state;

#pragma pack(pop)
#endif //sh_atomicH
