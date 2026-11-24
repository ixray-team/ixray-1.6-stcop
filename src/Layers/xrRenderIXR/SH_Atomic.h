#ifndef sh_atomicH
#define sh_atomicH
#pragma once
#include "RenderAPI.h"
#include "../../xrCore/xr_resource.h"
#include "tss_def.h"

#	include "StateManager/GfxPipelineState.h"

#pragma pack(push,4)

struct RENDER_API SShaderCompileState
{
	std::atomic<bool>					IsCompiled = false;

	void								MarkCompiled();
	void								WaitCompiled();
};


//////////////////////////////////////////////////////////////////////////
// Atomic resources
//////////////////////////////////////////////////////////////////////////
enum class EVertexInput : u8
{
	None,
	Legacy,
	Pulled
};

// What a shader reads at a register: where nothing is bound, NRI binds a zero view of that kind
enum class ESlotKind : u8
{
	Flat,
	Array,
	Cube,
	CubeArray,
	Volume,
	Structured,
	Other,
	Count
};

struct RENDER_API SVS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					vs;
	RHIBlob*							Signature = nullptr;
	u32									instance_buffer_slot = u32(-1);
	u32									editor_instance_slot = u32(-1);
	u32 RainInstanceSlot = u32(-1);
	// As SPS's, and the buffers among the reads
	u64 ReadSlots = 0;
	u64 BufferSlots = 0;
	ESlotKind Kinds[64] = {};
	EVertexInput						Input = EVertexInput::None;
	bool								MismatchLogged = false;
	SVS				();
	virtual ~SVS			() override;
};
typedef	resptr_core<SVS,resptr_base<SVS> >	ref_vs;

//////////////////////////////////////////////////////////////////////////
struct RENDER_API SPS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					ps;
	u64							Texture2DSlots = 0;
	// Every register it reads a texture or a buffer from
	u64							ReadSlots = 0;
	ESlotKind					Kinds[64] = {};
	virtual ~SPS			() override;
};
typedef	resptr_core<SPS,resptr_base<SPS> > ref_ps;

//////////////////////////////////////////////////////////////////////////
struct RENDER_API SGS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					gs;
	virtual ~SGS			() override;
};
typedef	resptr_core<SGS,resptr_base<SGS> > ref_gs;

struct RENDER_API SHS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					sh;
	virtual ~SHS			() override;
};
typedef	resptr_core< SHS, resptr_base<SHS> >	ref_hs;

struct RENDER_API SDS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					sh;
	virtual ~SDS			() override;
};
typedef	resptr_core< SDS, resptr_base<SDS> >	ref_ds;

struct RENDER_API SCS : public xr_resource_uniq
{
	SShaderCompileState					CompileState;
	RHIObject*					sh;
	// As SPS's, and the buffers among the reads; the registers it writes, and the textures among them
	u64							Texture2DSlots = 0;
	u64							ReadSlots = 0;
	u64							BufferSlots = 0;
	u32							WriteSlots = 0;
	u32							StorageSlots = 0;
	ESlotKind					Kinds[64] = {};
	virtual ~SCS			() override;
};
typedef	resptr_core< SCS, resptr_base<SCS> >	ref_cs;


//////////////////////////////////////////////////////////////////////////
struct RENDER_API SState : public xr_resource_flagged
{
	GfxPipelineState*							state;
	SimulatorStates						state_code;
	virtual ~SState			() override;
};
typedef	resptr_core<SState,resptr_base<SState> >	ref_state;

#pragma pack(pop)
#endif //sh_atomicH
