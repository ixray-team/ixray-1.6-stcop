#include "stdafx.h"


#pragma warning(disable:4995)
#include "../../xrEngine/Render.h"
#pragma warning(default:4995)

#include "ResourceManager.h"
#include "tss.h"
#include "blenders/Blender.h"
#include "blenders/Blender_Recorder.h"

#include "ShaderResourceTraits.h"

SHS*	CResourceManager::_CreateHS			(const char* Name, const ShaderExternalMap& Defines)
{
	return CreateShader<SHS>(Name, Defines);
}

void	CResourceManager::_DeleteHS			(const SHS*	HS	)
{
	DestroyShader(HS);
}

SDS*	CResourceManager::_CreateDS			(const char* Name, const ShaderExternalMap& Defines)
{
	return CreateShader<SDS>(Name, Defines);
}

void	CResourceManager::_DeleteDS			(const SDS*	DS	)
{
	DestroyShader(DS);
}

SCS*	CResourceManager::_CreateCS			(const char* Name, const ShaderExternalMap& Defines)
{
	return CreateShader<SCS>(Name, Defines);
}

void	CResourceManager::_DeleteCS			(const SCS*	CS	)
{
	DestroyShader(CS);
}

void fix_texture_name(LPSTR fn);

template <class T>
bool	reclaim		(xr_vector<T*>& vec, const T* ptr)
{
	typename xr_vector<T*>::iterator it	= vec.begin	();
	typename xr_vector<T*>::iterator end	= vec.end	();
	for (; it!=end; it++)
		if (*it == ptr)	{ vec.erase	(it); return true; }
		return false;
}

//--------------------------------------------------------------------------------------------------------------
SState*		CResourceManager::_CreateState		(SimulatorStates& state_code)
{
	xrCriticalSectionGuard guard(creationGuard);
	// Search equal state-code 
	for (u32 it=0; it<v_states.size(); it++)
	{
		SState*				C		= v_states[it];;
		SimulatorStates&	base	= C->state_code;
		if (base.equal(state_code))	return C;
	}

	// Create New
	v_states.push_back				(new SState());
	v_states.back()->dwFlags		|= xr_resource_flagged::RF_REGISTERED;
	v_states.back()->state			= GfxPipelineState::Create(state_code);
	v_states.back()->state_code		= state_code;
	return v_states.back();
}

void		CResourceManager::_DeleteState		(const SState* state)
{
	if (0==(state->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_states,state))						return;
	Msg	("! ERROR: Failed to find compiled stateblock");
}

//--------------------------------------------------------------------------------------------------------------
SPass* CResourceManager::_CreatePass(const SPass& proto)
{
	xrCriticalSectionGuard guard(creationGuard);

	for (u32 it=0; it<v_passes.size(); it++)
	{
		if (v_passes[it]->equal(proto))
		{
			return v_passes[it];
		}
	}

	SPass* P = new SPass();
	P->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	P->state = proto.state;
	P->ps = proto.ps;
	P->vs = proto.vs;
	P->gs = proto.gs;
	P->hs = proto.hs;
	P->ds = proto.ds;
	P->cs = proto.cs;
	P->constants = proto.constants;
	P->iPriority = proto.iPriority;
	P->T = proto.T;
	P->M = proto.M;

	v_passes.push_back(P);
	return v_passes.back();
}

void CResourceManager::_DeletePass(const SPass* P)
{
	if (0==(P->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_passes,P))						return;
	Msg	("! ERROR: Failed to find compiled pass");
}

//--------------------------------------------------------------------------------------------------------------
SVS*	CResourceManager::_CreateVS		(const char* _name, const ShaderExternalMap& Defines)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name;
	res_name += RImplementation.getShaderParams(Defines);
	const char* name = res_name.c_str();

	LPSTR N = LPSTR(name);
	map_VS::iterator I = m_vs.find(N);

	if (I!=m_vs.end())	
	{
		return I->second;
	}
	else
	{
		SVS* _vs = new SVS();
		_vs->dwFlags |= xr_resource_flagged::RF_REGISTERED;
		m_vs.insert(std::make_pair(_vs->set_name(name), _vs));
		RememberStage(_vs, _name, Defines);

		if (0==_stricmp(_name,"null"))
		{
			return _vs;
		}

		string_path cname;
		xr_strconcat(cname, ::Render->getShaderPath(), _name, ".vs.hlsl");
		FS.update_path(cname, _game_shaders_, cname);
		//		const char*						target		= nullptr;

		// duplicate and zero-terminate
		IReader* file			= FS.r_open(cname);
		//	TODO: DX10: HACK: Implement all shaders. Remove this for PS
		if (!file)
		{
			string1024 tmp;
			xr_sprintf(tmp, "DX10: %s is missing. Replace with stub_default.vs.hlsl", cname);
			Msg(tmp);
			xr_strconcat(cname, ::Render->getShaderPath(), "stub_default", ".vs.hlsl");
			FS.update_path(cname, _game_shaders_, cname);
			file = FS.r_open(cname);
		}

		u32	const size = file->length();
		char* const data = (LPSTR)_alloca(size + 1);
		CopyMemory(data, file->pointer(), size);
		data[size] = 0;
		FS.r_close(file);

		// Select target
		const char* c_target = "vs_5_0";
		const char* c_entry = "main";

		DWORD flags = RHI_SHADER_PACK_MATRIX_ROW_MAJOR;

		HRESULT	const _hr = RImplementation.shader_compile(name, (DWORD const*)data, size, c_entry, c_target, flags, (void*&)_vs, Defines);

		R_ASSERT4(SUCCEEDED(_hr), "Can't compile shader", cname, RImplementation.getShaderParamsDebug(Defines).c_str());

		// ����������� �������� � �������
#if 0 //ndef _EDITOR
		for (const auto& [_, vs] : m_vs)
		{
			if(vs->dwFlags & xr_resource_flagged::RF_REGISTERED)
			{
				if (vs != _vs && vs->m_crc1 == _vs->m_crc1)
				{
					xr_delete(_vs);
					return vs;
				}
			}
		}
#endif
		return _vs;
	}
}

void	CResourceManager::_DeleteVS			(const SVS* vs)
{
	if (0==(vs->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	LPSTR N				= LPSTR		(*vs->cName);
	map_VS::iterator I	= m_vs.find	(N);
	if (I!=m_vs.end())	
	{
		ForgetStage(vs);
		m_vs.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find compiled vertex-shader '%s'",*vs->cName);
}

//--------------------------------------------------------------------------------------------------------------
SPS*	CResourceManager::_CreatePS			(const char* _name, const ShaderExternalMap& Defines)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name + RImplementation.getShaderParams(Defines);
	const char* name = res_name.c_str();

	LPSTR N				= LPSTR(name);
	map_PS::iterator I	= m_ps.find	(N);
	if (I!=m_ps.end())	return		I->second;
	else
	{
		SPS*	_ps					=	new SPS	();
		_ps->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_ps.insert					(std::make_pair(_ps->set_name(name),_ps));
		RememberStage(_ps, _name, Defines);
		if (0==_stricmp(_name,"null"))	{
			_ps->ps				= nullptr;
			return _ps;
		}

		// Open file
		string_path					cname;
		xr_strconcat(cname,::Render->getShaderPath(),_name,".ps.hlsl");
		FS.update_path				(cname,	_game_shaders_, cname);

		// duplicate and zero-terminate
		IReader*		R		= FS.r_open(cname);
		//	TODO: DX10: HACK: Implement all shaders. Remove this for PS
		if (!R)
		{
			string1024			tmp;
			//	TODO: HACK: Test failure
			//Memory.mem_compact();
			xr_sprintf				(tmp, "DX10: %s is missing. Replace with stub_default.ps.hlsl", cname);
			Msg					(tmp);
			xr_strconcat(cname,::Render->getShaderPath(),"stub_default",".ps.hlsl");
			FS.update_path				(cname,	_game_shaders_, cname);
			R		= FS.r_open(cname);
		}

		R_ASSERT2(R, cname);
		u32	const size = R->length();
		char* const data		= (LPSTR)_alloca(size + 1);
		CopyMemory(data, R->pointer(), size);
		data[size]				= 0;
		FS.r_close(R);

		// Select target
		const char* c_target	= "ps_5_0";
		const char* c_entry		= "main";

		DWORD flags = RHI_SHADER_PACK_MATRIX_ROW_MAJOR;

		HRESULT	const _hr = RImplementation.shader_compile(name, (DWORD const*)data, size, c_entry, c_target, flags, (void*&)_ps, Defines);

		R_ASSERT4(SUCCEEDED(_hr), "Can't compile shader", cname, RImplementation.getShaderParamsDebug(Defines).c_str());

		// ����������� �������� � �������

#if 0 //ndef _EDITOR
		for (const auto& [_, ps] : m_ps)
		{
			if (ps->dwFlags & xr_resource_flagged::RF_REGISTERED)
			{
				if (ps != _ps && ps->m_crc1 == _ps->m_crc1)
				{
					xr_delete(_ps);
					return ps;
				}
			}
		
		}
#endif

		return _ps;
	}
}

void	CResourceManager::_DeletePS			(const SPS* ps)
{
	if (0==(ps->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	LPSTR N				= LPSTR		(*ps->cName);
	map_PS::iterator I	= m_ps.find	(N);
	if (I!=m_ps.end())	{
		ForgetStage(ps);
		m_ps.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find compiled pixel-shader '%s'",*ps->cName);
}

//--------------------------------------------------------------------------------------------------------------
SGS*	CResourceManager::_CreateGS			(const char* _name, const ShaderExternalMap& Defines)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name + RImplementation.getShaderParams(Defines);
	const char* name = res_name.c_str();

	LPSTR N = LPSTR(name);
	map_GS::iterator I	= m_gs.find	(N);
	if (I!=m_gs.end())	return		I->second;
	else
	{
		SGS*	_gs					=	new SGS	();
		_gs->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_gs.insert					(std::make_pair(_gs->set_name(name),_gs));
		RememberStage(_gs, _name, Defines);
		if (0==_stricmp(_name,"null"))	{
			_gs->gs				= nullptr;
			return _gs;
		}

		// Open file
		string_path					cname;
		xr_strconcat(cname,::Render->getShaderPath(),_name,".gs.hlsl");
		FS.update_path				(cname,	_game_shaders_, cname);

		// duplicate and zero-terminate
		IReader*		R		= FS.r_open(cname);
		//	TODO: DX10: HACK: Implement all shaders. Remove this for PS
		if (!R)
		{
			string1024			tmp;
			//	TODO: HACK: Test failure
			//Memory.mem_compact();
			xr_sprintf				(tmp, "DX10: %s is missing. Replace with stub_default.gs.hlsl", cname);
			Msg					(tmp);
			xr_strconcat(cname,::Render->getShaderPath(),"stub_default",".gs.hlsl");
			FS.update_path				(cname,	_game_shaders_, cname);
			R		= FS.r_open(cname);
		}
		IReader* file			= FS.r_open(cname);
		R_ASSERT2				( file, cname );

		// Select target
		const char*						c_target	= "gs_5_0";
		const char*						c_entry		= "main";

		DWORD flags = RHI_SHADER_PACK_MATRIX_ROW_MAJOR;

		HRESULT	const _hr = RImplementation.shader_compile(name, (DWORD const*)file->pointer(), file->length(), c_entry, c_target, flags, (void*&)_gs, Defines);

		R_ASSERT4(SUCCEEDED(_hr), "Can't compile shader", cname, RImplementation.getShaderParamsDebug(Defines).c_str());

		FS.r_close				( file );

		return					_gs;
	}
}
void	CResourceManager::_DeleteGS			(const SGS* gs)
{
	if (0==(gs->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	LPSTR N				= LPSTR		(*gs->cName);
	map_GS::iterator I	= m_gs.find	(N);
	if (I!=m_gs.end())	{
		ForgetStage(gs);
		m_gs.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find compiled geometry shader '%s'",*gs->cName);
}

void CResourceManager::RememberStage(xr_resource_uniq* stage, const char* source, const ShaderExternalMap& Defines)
{
	StageCompileInfo info;
	info.source = source;
	info.options = Defines;
	m_stage_info[stage] = std::move(info);
}

void CResourceManager::ForgetStage(const xr_resource_uniq* stage)
{
	m_stage_info.erase(const_cast<xr_resource_uniq*>(stage));
}

static bool ReadShaderFile(const char* stem, const char* ext, xr_vector<char>& data)
{
	string_path cname;
	xr_strconcat(cname, ::Render->getShaderPath(), stem, ext);
	FS.update_path(cname, _game_shaders_, cname);
	IReader* file = FS.r_open(cname);
	if (!file)
	{
		return false;
	}
	const u32 size = file->length();
	data.resize(size + 1);
	CopyMemory(data.data(), file->pointer(), size);
	data[size] = 0;
	FS.r_close(file);
	return true;
}

struct ShaderCacheBypassScope
{
	ShaderCacheBypassScope() { RImplementation.SetShaderCacheBypass(true); }
	~ShaderCacheBypassScope() { RImplementation.SetShaderCacheBypass(false); }
};

void CResourceManager::RecompileDX12Shaders()
{
	if (!GRHI || GRHI->APILevel != ERHI_API_LAYER::D3D12)
	{
		Msg("! r_recompile_shaders requires the DX12 renderer");
		return;
	}
	if (GRHI->DevicePtr)
	{
		GRHI->DevicePtr->Flush();
	}

	xrCriticalSectionGuard guard(creationGuard);
	ShaderCacheBypassScope bypass;
	u32 compiled = 0;
	u32 failed = 0;

	auto compile_stage = [&](xr_resource_uniq* stage, RHIObject* hw, const char* ext, const char* target) -> int
	{
		if (!hw)
		{
			return 0;
		}
		auto info = m_stage_info.find(stage);
		if (info == m_stage_info.end() || !info->second.source.size() || !xr_strcmp(info->second.source.c_str(), "null"))
		{
			return 0;
		}

		xr_vector<char> data;
		if (!ReadShaderFile(info->second.source.c_str(), ext, data))
		{
			Msg("! r_recompile_shaders: missing %s%s", info->second.source.c_str(), ext);
			return -1;
		}

		void* result = stage;
		const HRESULT hr = RImplementation.shader_compile(
			*stage->cName,
			(DWORD const*)data.data(),
			u32(data.size() - 1),
			"main",
			target,
			RHI_SHADER_PACK_MATRIX_ROW_MAJOR,
			result,
			info->second.options);
		return SUCCEEDED(hr) ? 1 : -1;
	};

	auto note = [&](int status)
	{
		if (status > 0)
		{
			++compiled;
		}
		else if (status < 0)
		{
			++failed;
		}
	};

	for (auto& [name, vs] : m_vs)
	{
		(void)name;
		note(compile_stage(vs, vs->vs, ".vs.hlsl", "vs_5_0"));
	}
	for (auto& [name, ps] : m_ps)
	{
		(void)name;
		note(compile_stage(ps, ps->ps, ".ps.hlsl", "ps_5_0"));
	}
	for (auto& [name, gs] : m_gs)
	{
		(void)name;
		note(compile_stage(gs, gs->gs, ".gs.hlsl", "gs_5_0"));
	}
	for (auto& [name, hs] : m_hs)
	{
		(void)name;
		note(compile_stage(hs, hs->sh, ".hs.hlsl", "hs_5_0"));
	}
	for (auto& [name, ds] : m_ds)
	{
		(void)name;
		note(compile_stage(ds, ds->sh, ".ds.hlsl", "ds_5_0"));
	}
	for (auto& [name, cs] : m_cs)
	{
		(void)name;
		note(compile_stage(cs, cs->sh, ".cs.hlsl", "cs_5_0"));
	}

	Msg("* r_recompile_shaders: %u compiled, %u failed", compiled, failed);
}

//--------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------
R_constant_table*	CResourceManager::_CreateConstantTable	(R_constant_table& C)
{
	if (!C.DetailScale)		return nullptr;

	xrCriticalSectionGuard guard(creationGuard);

	for (u32 it=0; it<v_constant_tables.size(); it++)
		if (v_constant_tables[it]->equal(C))	return v_constant_tables[it];

	R_constant_table* NewElem = v_constant_tables.emplace_back((new R_constant_table()));
	NewElem->_copy(C);
	NewElem->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	return NewElem;

	return v_constant_tables.back		();
}
void				CResourceManager::_DeleteConstantTable	(const R_constant_table* C)
{
	if (0==(C->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_constant_tables,C))				return;
	Msg	("! ERROR: Failed to find compiled constant-table");
}

//--------------------------------------------------------------------------------------------------------------
CRT* CResourceManager::_CreateRT(const char* Name, u32 w, u32 h, ERHI_FORMAT f, u32 SampleCount, CRT::CRTCreationFlags CreationFlags)
{
	R_ASSERT(Name && Name[0] && w && h);

	// ***** first pass - search already created RT
	LPSTR N = LPSTR(Name);
	xrCriticalSectionGuard guard(creationGuard);
	map_RT::iterator I = m_rtargets.find	(N);
	if (I != m_rtargets.end())
	{
		return I->second;
	}
	else
	{
		CRT *RT					=	new CRT();
		RT->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_rtargets.insert		(std::make_pair(RT->set_name(Name),RT));
		if (Device.b_is_Ready)	RT->create	(Name,w,h,f, SampleCount, CreationFlags);
		return					RT;
	}
}
void	CResourceManager::_DeleteRT		(const CRT* RT)
{
	if (0==(RT->dwFlags&xr_resource_flagged::RF_REGISTERED))
		return;

	LPSTR N = LPSTR(*RT->cName);
	xrCriticalSectionGuard guard(creationGuard);
	map_RT::iterator I	= m_rtargets.find	(N);
	if (I!=m_rtargets.end())
	{
		m_rtargets.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find render-target '%s'",*RT->cName);
}

CRTC* CResourceManager::_CreateRTC(const char* Name, u32 size, ERHI_FORMAT f, CRT::CRTCreationFlags CreationFlags)
{
	R_ASSERT(Name && Name[0] && size);

	// ***** first pass - search already created RTC
	LPSTR N = LPSTR(Name);
	map_RTC::iterator I = m_rtargets_c.find(N);
	if(I != m_rtargets_c.end())	return I->second;
	else {
		CRTC* RT = new CRTC();
		RT->dwFlags |= xr_resource_flagged::RF_REGISTERED;
		m_rtargets_c.insert(std::make_pair(RT->set_name(Name), RT));
		if(Device.b_is_Ready) RT->create(Name, size, f, CreationFlags);
		return RT;
	}
}

void	CResourceManager::_DeleteRTC(const CRTC* RT) {
	if(0 == (RT->dwFlags & xr_resource_flagged::RF_REGISTERED))	return;
	LPSTR N = LPSTR(*RT->cName);
	map_RTC::iterator I = m_rtargets_c.find(N);
	if(I != m_rtargets_c.end()) {
		m_rtargets_c.erase(I);
		return;
	}
	Msg("! ERROR: Failed to find render-target '%s'", *RT->cName);
}

SGeometry* CResourceManager::CreatePulledGeom(EGeomSource Source, u32 Page, IRHIBuffer* RingIndices)
{
	VERIFY(Source == EGeomSource::Ring ? Page == 0 : Page < RImplementation.Geometry.PageCount() && RingIndices == nullptr);
	xrCriticalSectionGuard guard(creationGuard);
	IRHIBuffer* Indices = Source == EGeomSource::Ring ? RingIndices : RImplementation.Geometry.IndexBuffer(Page);
	for (SGeometry* G : v_geoms)
	{
		if (G->Source == Source && G->Page == Page && G->ib == Indices)
		{
			return G;
		}
	}

	SGeometry* Geom = new SGeometry();
	Geom->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	Geom->ib = Indices;
	Geom->Source = Source;
	Geom->Page = u8(Page);
	v_geoms.push_back(Geom);
	return Geom;
}

void CResourceManager::DeleteGeom(const SGeometry* Geom)
{
	if (0==(Geom->dwFlags&xr_resource_flagged::RF_REGISTERED))	
		return;	

	xrCriticalSectionGuard guard(creationGuard);

	if (reclaim(v_geoms,Geom))							
		return;

	Msg	("! ERROR: Failed to find compiled geometry-declaration");
}

//--------------------------------------------------------------------------------------------------------------
CTexture* CResourceManager::_CreateTexture	(const char* _Name)
{
	PROF_EVENT("_CreateTexture");
	// DBG_VerifyTextures	();
	if (0==xr_strcmp(_Name,"null"))	return 0;
	R_ASSERT		(_Name && _Name[0]);
	string_path		Name;
	xr_strcpy			(Name,_Name); //. andy if (strext(Name)) *strext(Name)=0;

	xrCriticalSectionGuard guard(creationGuard);

	fix_texture_name (Name);
	// ***** first pass - search already loaded texture
	LPSTR N			= LPSTR(Name);
	map_TextureIt I = m_textures.find	(N);
	if (I!=m_textures.end())	return	I->second;
	else
	{
		CTexture *	T		=	new CTexture();
		T->dwFlags			|=	xr_resource_flagged::RF_REGISTERED;
		m_textures.insert	(std::make_pair(T->set_name(Name),T));
		T->Preload			();
		LoadTexture(T);
		return		T;
	}
}

CTexture* CResourceManager::_CreateEmptyTexture(const char* _Name, u32 w, u32 h)
{
	// DBG_VerifyTextures	();

	R_ASSERT(_Name && _Name[0]);
	R_ASSERT(w > 0 && "must be valid width!");
	R_ASSERT(h > 0 && &"must be valid height!");

	if (0 == xr_strcmp(_Name, "null") || w == 0 || h == 0)	
		return nullptr;

	string_path		Name;
	xr_strcpy(Name, _Name); //. andy if (strext(Name)) *strext(Name)=0;

	xrCriticalSectionGuard guard(creationGuard);

	fix_texture_name(Name);

	for (u32 i = 0; i < strlen(Name); ++i)
		Name[i] = std::tolower(Name[i]);

	// ***** first pass - search already loaded texture
	LPSTR N = LPSTR(Name);
	map_TextureIt I = m_textures.find(N);
	if (I != m_textures.end())	return	I->second;
	else
	{
		CTexture* T = new CTexture();
		T->dwFlags |= xr_resource_flagged::RF_REGISTERED;
		m_textures.insert(std::make_pair(T->set_name(Name), T));
		T->Preload();
		
		if (Device.b_is_Ready) 
			T->CreateEmpty(w,h);

		return		T;
	}
}

void	CResourceManager::_DeleteTexture		(const CTexture* T)
{
	if (0==(T->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;

	xrCriticalSectionGuard guard(creationGuard);

	LPSTR N					= LPSTR		(*T->cName);

	map_Texture::iterator I	= m_textures.find	(N);
	if (I!=m_textures.end())	{
		m_textures.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find texture surface '%s'",*T->cName);
}

//--------------------------------------------------------------------------------------------------------------
CMatrix*	CResourceManager::_CreateMatrix	(const char* Name)
{
	R_ASSERT(Name && Name[0]);
	if (0==_stricmp(Name,"$null"))	return nullptr;

	LPSTR N = LPSTR(Name);

	xrCriticalSectionGuard guard(creationGuard);

	map_Matrix::iterator I = m_matrices.find	(N);
	if (I!=m_matrices.end())	return I->second;
	else
	{
		CMatrix* M			=	new CMatrix();
		M->dwFlags			|=	xr_resource_flagged::RF_REGISTERED;
		M->dwReference		=	1;
		m_matrices.insert	(std::make_pair(M->set_name(Name),M));
		return			M;
	}
}
void	CResourceManager::_DeleteMatrix		(const CMatrix* M)
{
	if (0==(M->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;

	xrCriticalSectionGuard guard(creationGuard);

	LPSTR N					= LPSTR		(*M->cName);
	map_Matrix::iterator I	= m_matrices.find	(N);
	if (I!=m_matrices.end())	{
		m_matrices.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find xform-def '%s'",*M->cName);
}
//--------------------------------------------------------------------------------------------------------------
CConstant*	CResourceManager::_CreateConstant	(const char* Name)
{
	R_ASSERT(Name && Name[0]);
	if (0==_stricmp(Name,"$null"))	return nullptr;

	LPSTR N = LPSTR(Name);

	xrCriticalSectionGuard guard(creationGuard);

	map_Constant::iterator I	= m_constants.find	(N);
	if (I!=m_constants.end())	return I->second;
	else
	{
		CConstant* C		=	new CConstant();
		C->dwFlags			|=	xr_resource_flagged::RF_REGISTERED;
		C->dwReference		=	1;
		m_constants.insert	(std::make_pair(C->set_name(Name),C));
		return	C;
	}
}
void	CResourceManager::_DeleteConstant		(const CConstant* C)
{
	if (0==(C->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	LPSTR N				= LPSTR				(*C->cName);
	map_Constant::iterator I	= m_constants.find	(N);
	if (I!=m_constants.end())	{
		m_constants.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find R1-constant-def '%s'",*C->cName);
}

//--------------------------------------------------------------------------------------------------------------
bool	cmp_tl	(const std::pair<u32,ref_texture>& _1, const std::pair<u32,ref_texture>& _2)	{
	return _1.first < _2.first;
}
STextureList*	CResourceManager::_CreateTextureList(STextureList& L)
{
	xrCriticalSectionGuard guard(creationGuard);
	std::sort	(L.begin(),L.end(),cmp_tl);
	for (u32 it=0; it<lst_textures.size(); it++)
	{
		STextureList*	base		= lst_textures[it];
		if (L.equal(*base))			return base;
	}

	STextureList* lst = new STextureList();
	lst->_copy(L);	
	lst->dwFlags |= xr_resource_flagged::RF_REGISTERED;

	lst_textures.push_back	(lst);
	return lst;
}
void			CResourceManager::_DeleteTextureList(const STextureList* L)
{
	if (0==(L->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(lst_textures,L))					return;
	Msg	("! ERROR: Failed to find compiled list of textures");
}
//--------------------------------------------------------------------------------------------------------------
SMatrixList*	CResourceManager::_CreateMatrixList(SMatrixList& L)
{
	bool bEmpty = true;
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 i=0; i<L.size(); i++)	if (L[i]) { bEmpty=false; break; }
	if (bEmpty)	return nullptr;

	for (u32 it=0; it<lst_matrices.size(); it++)
	{
		SMatrixList*	base		= lst_matrices[it];
		if (L.equal(*base))			return base;
	}

	SMatrixList* lst = new SMatrixList();
	lst->_copy(L);

	lst->dwFlags			|=	xr_resource_flagged::RF_REGISTERED;
	lst_matrices.push_back	(lst);
	return lst;
}
void			CResourceManager::_DeleteMatrixList ( const SMatrixList* L )
{
	if (0==(L->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(lst_matrices,L))					return;
	Msg	("! ERROR: Failed to find compiled list of xform-defs");
}
