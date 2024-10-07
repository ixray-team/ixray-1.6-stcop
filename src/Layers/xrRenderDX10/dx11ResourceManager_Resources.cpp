#include "stdafx.h"
#pragma hdrstop

#pragma warning(disable:4995)
#ifndef _EDITOR
#include "../../xrEngine/render.h"
#endif
#pragma warning(default:4995)

#include "../xrRender/ResourceManager.h"
#include "../xrRender/tss.h"
#include "../xrRender/blenders/blender.h"
#include "../xrRender/blenders/blender_recorder.h"

#include "../xrRenderDX10/dx10BufferUtils.h"
#include "../xrRenderDX10/dx10ConstantBuffer.h"

#include "../xrRender/ShaderResourceTraits.h"

#include <FlexibleVertexFormat.h>
using namespace FVF;

SHS*	CResourceManager::_CreateHS			(LPCSTR Name)
{
	return CreateShader<SHS>(Name);
}

void	CResourceManager::_DeleteHS			(const SHS*	HS	)
{
	DestroyShader(HS);
}

SDS*	CResourceManager::_CreateDS			(LPCSTR Name)
{
	return CreateShader<SDS>(Name);
}

void	CResourceManager::_DeleteDS			(const SDS*	DS	)
{
	DestroyShader(DS);
}

SCS*	CResourceManager::_CreateCS			(LPCSTR Name)
{
	return CreateShader<SCS>(Name);
}

void	CResourceManager::_DeleteCS			(const SCS*	CS	)
{
	DestroyShader(CS);
}

void fix_texture_name(LPSTR fn);

template <class T>
BOOL	reclaim		(xr_vector<T*>& vec, const T* ptr)
{
	typename xr_vector<T*>::iterator it	= vec.begin	();
	typename xr_vector<T*>::iterator end	= vec.end	();
	for (; it!=end; it++)
		if (*it == ptr)	{ vec.erase	(it); return TRUE; }
		return FALSE;
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
	v_states.back()->state			= ID3DState::Create(state_code);
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
SPass*		CResourceManager::_CreatePass			(const SPass& proto)
{
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 it=0; it<v_passes.size(); it++)
		if (v_passes[it]->equal(proto))
			return v_passes[it];

	SPass*	P					=	new SPass();
	P->dwFlags					|=	xr_resource_flagged::RF_REGISTERED;
	P->state					=	proto.state;
	P->ps						=	proto.ps;
	P->vs						=	proto.vs;
	P->gs						=	proto.gs;
	P->hs						=	proto.hs;
	P->ds						=	proto.ds;
	P->cs						=	proto.cs;
	P->constants				=	proto.constants;
	P->T						=	proto.T;
#ifdef _EDITOR
	P->M						=	proto.M;
#endif
	P->C						=	proto.C;

	v_passes.push_back			(P);
	return v_passes.back();
}

void		CResourceManager::_DeletePass			(const SPass* P)
{
	if (0==(P->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_passes,P))						return;
	Msg	("! ERROR: Failed to find compiled pass");
}

//--------------------------------------------------------------------------------------------------------------
SVS*	CResourceManager::_CreateVS		(LPCSTR _name)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name;

	if (Render->m_skinning > 0) {
		res_name += "_" + std::to_string(Render->m_skinning);
	}

	res_name += RImplementation.getShaderParams();
	LPCSTR name = res_name.c_str();

	LPSTR N				= LPSTR		(name);
	map_VS::iterator I	= m_vs.find	(N);
	if (I!=m_vs.end())	return I->second;
	else
	{
		SVS*	_vs					= new SVS	();
		_vs->dwFlags				|= xr_resource_flagged::RF_REGISTERED;
		m_vs.insert					(std::make_pair(_vs->set_name(name),_vs));
		if (0==_stricmp(_name,"null"))	{
			return _vs;
		}

		string_path					cname;
		xr_strconcat(cname,::Render->getShaderPath(), _name,".vs.hlsl");
		FS.update_path				(cname,	"$game_shaders$", cname);
		//		LPCSTR						target		= nullptr;

		// duplicate and zero-terminate
		IReader* file			= FS.r_open(cname);
		//	TODO: DX10: HACK: Implement all shaders. Remove this for PS
		if (!file)
		{
			string1024			tmp;
			xr_sprintf			(tmp, "DX10: %s is missing. Replace with stub_default.vs.hlsl", cname);
			Msg					(tmp);
			xr_strconcat(cname,::Render->getShaderPath(),"stub_default",".vs.hlsl");
			FS.update_path		(cname,	"$game_shaders$", cname);
			file				= FS.r_open(cname);
		}
		u32	const size			= file->length();
		char* const data		= (LPSTR)_alloca(size + 1);
		CopyMemory				( data, file->pointer(), size );
		data[size]				= 0;
		FS.r_close				( file );

		// Select target
		LPCSTR						c_target	= "vs_2_0";
		LPCSTR						c_entry		= "main";

		if (strstr(data, "main_vs_1_1"))	{ c_target = "vs_1_1"; c_entry = "main_vs_1_1";	}
		if (strstr(data, "main_vs_2_0"))	{ c_target = "vs_2_0"; c_entry = "main_vs_2_0";	}
		if (strstr(data, "main_vs_4_0"))	{ c_target = "vs_4_0"; c_entry = "main_vs_4_0"; }

		DWORD flags = D3DCOMPILE_PACK_MATRIX_ROW_MAJOR;

		if (Core.ParamsData.test(ECoreParams::renderdoc) || Core.ParamsData.test(ECoreParams::dxdebug)) {
			flags |= D3DCOMPILE_DEBUG;
			flags |= D3DCOMPILE_SKIP_OPTIMIZATION;
			flags |= D3DCOMPILE_DEBUG_NAME_FOR_SOURCE;
		}

		HRESULT	const _hr = ::Render->shader_compile(name, (DWORD const*)data, size, c_entry, c_target, flags, (void*&)_vs);

		R_ASSERT3(SUCCEEDED(_hr), "Can't compile shader", name);

		return					_vs;
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
		m_vs.erase(I);
		xr_vector<SDeclaration*>::iterator iDecl;
		for (iDecl = v_declarations.begin(); iDecl!=v_declarations.end(); ++iDecl)
		{
			xr_map<ID3DBlob*, ID3DInputLayout*>::iterator iLayout;
			iLayout = (*iDecl)->vs_to_layout.find(vs->signature->signature);
			if (iLayout!=(*iDecl)->vs_to_layout.end())
			{
				//	Release vertex layout
				_RELEASE(iLayout->second);
				(*iDecl)->vs_to_layout.erase(iLayout);
			}
		}
		return;
	}
	Msg	("! ERROR: Failed to find compiled vertex-shader '%s'",*vs->cName);
}

//--------------------------------------------------------------------------------------------------------------
SPS*	CResourceManager::_CreatePS			(LPCSTR _name)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name + RImplementation.getShaderParams();
	LPCSTR name = res_name.c_str();

	LPSTR N				= LPSTR(name);
	map_PS::iterator I	= m_ps.find	(N);
	if (I!=m_ps.end())	return		I->second;
	else
	{
		SPS*	_ps					=	new SPS	();
		_ps->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_ps.insert					(std::make_pair(_ps->set_name(name),_ps));
		if (0==_stricmp(_name,"null"))	{
			_ps->ps				= nullptr;
			return _ps;
		}

		// Open file
		string_path					cname;
		xr_strconcat(cname,::Render->getShaderPath(),_name,".ps.hlsl");
		FS.update_path				(cname,	"$game_shaders$", cname);

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
			FS.update_path				(cname,	"$game_shaders$", cname);
			R		= FS.r_open(cname);
		}

		R_ASSERT2(R, cname);
		u32	const size = R->length();
		char* const data		= (LPSTR)_alloca(size + 1);
		CopyMemory(data, R->pointer(), size);
		data[size]				= 0;
		FS.r_close(R);

		// Select target
		LPCSTR						c_target	= "ps_2_0";
		LPCSTR						c_entry		= "main";
		if (strstr(data,"main_ps_1_1"))			{ c_target = "ps_1_1"; c_entry = "main_ps_1_1";	}
		if (strstr(data,"main_ps_1_2"))			{ c_target = "ps_1_2"; c_entry = "main_ps_1_2";	}
		if (strstr(data,"main_ps_1_3"))			{ c_target = "ps_1_3"; c_entry = "main_ps_1_3";	}
		if (strstr(data,"main_ps_1_4"))			{ c_target = "ps_1_4"; c_entry = "main_ps_1_4";	}
		if (strstr(data,"main_ps_2_0"))			{ c_target = "ps_2_0"; c_entry = "main_ps_2_0";	}
		if (strstr(data,"main_ps_4_0"))			{ c_target = "ps_4_0"; c_entry = "main_ps_4_0"; }

		DWORD flags = D3DCOMPILE_PACK_MATRIX_ROW_MAJOR;

		if (Core.ParamsData.test(ECoreParams::renderdoc) || Core.ParamsData.test(ECoreParams::dxdebug))
		{
			flags |= D3DCOMPILE_DEBUG;
			flags |= D3DCOMPILE_SKIP_OPTIMIZATION;
			flags |= D3DCOMPILE_DEBUG_NAME_FOR_SOURCE;
		}

		HRESULT	const _hr = ::Render->shader_compile(name, (DWORD const*)data, size, c_entry, c_target, flags, (void*&)_ps);

		R_ASSERT3(SUCCEEDED(_hr), "Can't compile shader", name);

		return			_ps;
	}
}

void	CResourceManager::_DeletePS			(const SPS* ps)
{
	if (0==(ps->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	LPSTR N				= LPSTR		(*ps->cName);
	map_PS::iterator I	= m_ps.find	(N);
	if (I!=m_ps.end())	{
		m_ps.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find compiled pixel-shader '%s'",*ps->cName);
}

//--------------------------------------------------------------------------------------------------------------
SGS*	CResourceManager::_CreateGS			(LPCSTR _name)
{
	xrCriticalSectionGuard guard(creationGuard);
	xr_string res_name = _name + RImplementation.getShaderParams();
	LPCSTR name = res_name.c_str();

	LPSTR N = LPSTR(name);
	map_GS::iterator I	= m_gs.find	(N);
	if (I!=m_gs.end())	return		I->second;
	else
	{
		SGS*	_gs					=	new SGS	();
		_gs->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_gs.insert					(std::make_pair(_gs->set_name(name),_gs));
		if (0==_stricmp(_name,"null"))	{
			_gs->gs				= nullptr;
			return _gs;
		}

		// Open file
		string_path					cname;
		xr_strconcat(cname,::Render->getShaderPath(),_name,".gs.hlsl");
		FS.update_path				(cname,	"$game_shaders$", cname);

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
			FS.update_path				(cname,	"$game_shaders$", cname);
			R		= FS.r_open(cname);
		}
		IReader* file			= FS.r_open(cname);
		R_ASSERT2				( file, cname );

		// Select target
		LPCSTR						c_target	= "gs_4_0";
		LPCSTR						c_entry		= "main";

		DWORD flags = D3DCOMPILE_PACK_MATRIX_ROW_MAJOR;

		if (Core.ParamsData.test(ECoreParams::renderdoc) || Core.ParamsData.test(ECoreParams::dxdebug))
		{
			flags |= D3DCOMPILE_DEBUG;
			flags |= D3DCOMPILE_SKIP_OPTIMIZATION;
			flags |= D3DCOMPILE_DEBUG_NAME_FOR_SOURCE;
		}

		HRESULT	const _hr = ::Render->shader_compile(name, (DWORD const*)file->pointer(), file->length(), c_entry, c_target, flags, (void*&)_gs);

		R_ASSERT3(SUCCEEDED(_hr), "Can't compile shader", name);

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
		m_gs.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find compiled geometry shader '%s'",*gs->cName);
}

//--------------------------------------------------------------------------------------------------------------
static BOOL	dcl_equal			(D3DVERTEXELEMENT9* a, D3DVERTEXELEMENT9* b)
{
	// check sizes
	size_t a_size = GetDeclLength(a);
	size_t b_size = GetDeclLength(b);
	if (a_size!=b_size)	return FALSE;
	return 0==memcmp	(a,b,a_size*sizeof(D3DVERTEXELEMENT9));
}

SDeclaration*	CResourceManager::_CreateDecl	(D3DVERTEXELEMENT9* dcl)
{
	xrCriticalSectionGuard guard(creationGuard);
	// Search equal code
	for (u32 it=0; it<v_declarations.size(); it++)
	{
		SDeclaration*		D		= v_declarations[it];;
		if (dcl_equal(dcl,&*D->dcl_code.begin()))	return D;
	}

	// Create _new
	SDeclaration* D			= new SDeclaration();
	u32 dcl_size = (u32)GetDeclLength(dcl) + 1;
	//	Don't need it for DirectX 10 here
	//CHK_DX					(RDevice->CreateVertexDeclaration(dcl,&D->dcl));
	D->dcl_code.assign		(dcl,dcl+dcl_size);
	dx10BufferUtils::ConvertVertexDeclaration(D->dcl_code, D->dx10_dcl_code);
	D->dwFlags				|= xr_resource_flagged::RF_REGISTERED;
	v_declarations.push_back(D);
	return D;
}

void		CResourceManager::_DeleteDecl		(const SDeclaration* dcl)
{
	if (0==(dcl->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_declarations,dcl))					return;
	Msg	("! ERROR: Failed to find compiled vertex-declarator");
}

//--------------------------------------------------------------------------------------------------------------
R_constant_table*	CResourceManager::_CreateConstantTable	(R_constant_table& C)
{
	if (C.empty())		return nullptr;

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
CRT*	CResourceManager::_CreateRT(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount, bool useUAV )
{
	R_ASSERT(Name && Name[0] && w && h);

	// ***** first pass - search already created RT
	LPSTR N = LPSTR(Name);
	xrCriticalSectionGuard guard(creationGuard);
	map_RT::iterator I = m_rtargets.find	(N);
	if (I!=m_rtargets.end())	return		I->second;
	else
	{
		CRT *RT					=	new CRT();
		RT->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
		m_rtargets.insert		(std::make_pair(RT->set_name(Name),RT));
		if (Device.b_is_Ready)	RT->create	(Name,w,h,f, SampleCount, useUAV );
		return					RT;
	}
}
void	CResourceManager::_DeleteRT		(const CRT* RT)
{
	if (0==(RT->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	LPSTR N				= LPSTR		(*RT->cName);
	xrCriticalSectionGuard guard(creationGuard);
	map_RT::iterator I	= m_rtargets.find	(N);
	if (I!=m_rtargets.end())	{
		m_rtargets.erase(I);
		return;
	}
	Msg	("! ERROR: Failed to find render-target '%s'",*RT->cName);
}

//--------------------------------------------------------------------------------------------------------------
void	CResourceManager::DBG_VerifyGeoms	()
{
	/*
	for (u32 it=0; it<v_geoms.size(); it++)
	{
	SGeometry* G					= v_geoms[it];

	D3DVERTEXELEMENT9		test	[MAXD3DDECLLENGTH + 1];
	u32						size	= 0;
	G->dcl->GetDeclaration			(test,(unsigned int*)&size);
	u32 vb_stride = ComputeVertexSize(test,0);
	u32 vb_stride_cached			= G->vb_stride;
	R_ASSERT						(vb_stride == vb_stride_cached);
	}
	*/
}

SGeometry*	CResourceManager::CreateGeom	(D3DVERTEXELEMENT9* decl, IBuffer* vb, IBuffer* ib)
{
	xrCriticalSectionGuard guard(creationGuard);
	R_ASSERT			(decl && vb);

	SDeclaration* dcl	= _CreateDecl			(decl);
	u32 vb_stride = (u32)ComputeVertexSize(decl, 0);

	// ***** first pass - search already loaded shader
	for (u32 it=0; it<v_geoms.size(); it++)
	{
		SGeometry& G	= *(v_geoms[it]);
		if ((G.dcl==dcl) && (G.vb==vb) && (G.ib==ib) && (G.vb_stride==vb_stride))	return v_geoms[it];
	}

	SGeometry *Geom		=	new SGeometry	();
	Geom->dwFlags		|=	xr_resource_flagged::RF_REGISTERED;
	Geom->dcl			=	dcl;
	Geom->vb			=	vb;
	Geom->vb_stride		=	vb_stride;
	Geom->ib			=	ib;
	v_geoms.push_back	(Geom);
	return	Geom;
}
SGeometry*	CResourceManager::CreateGeom		(u32 FVF, IBuffer* vb, IBuffer* ib)
{
	auto dcl = std::vector<D3DVERTEXELEMENT9>(MAXD3DDECLLENGTH + 1);
	xrCriticalSectionGuard guard(creationGuard);
	CHK_DX(CreateDeclFromFVF(FVF, dcl));
	SGeometry* g = CreateGeom(dcl.data(), vb, ib);

	return	g;
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
CTexture* CResourceManager::_CreateTexture	(LPCSTR _Name)
{
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
		if (Device.b_is_Ready && !bDeferredLoad) T->Load();
		return		T;
	}
}
void	CResourceManager::_DeleteTexture		(const CTexture* T)
{
	// DBG_VerifyTextures	();

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

#ifdef DEBUG
void	CResourceManager::DBG_VerifyTextures	()
{
	map_Texture::iterator I		= m_textures.begin	();
	map_Texture::iterator E		= m_textures.end	();
	for (; I!=E; I++) 
	{
		R_ASSERT(I->first);
		R_ASSERT(I->second);
		R_ASSERT(I->second->cName);
		R_ASSERT(0==xr_strcmp(I->first,*I->second->cName));
	}
}
#endif

//--------------------------------------------------------------------------------------------------------------
CMatrix*	CResourceManager::_CreateMatrix	(LPCSTR Name)
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
CConstant*	CResourceManager::_CreateConstant	(LPCSTR Name)
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
	BOOL bEmpty = TRUE;
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 i=0; i<L.size(); i++)	if (L[i]) { bEmpty=FALSE; break; }
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
//--------------------------------------------------------------------------------------------------------------
SConstantList*	CResourceManager::_CreateConstantList(SConstantList& L)
{
	BOOL bEmpty = TRUE;
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 i=0; i<L.size(); i++)	if (L[i]) { bEmpty=FALSE; break; }
	if (bEmpty)	return nullptr;

	for (u32 it=0; it<lst_constants.size(); it++)
	{
		SConstantList*	base		= lst_constants[it];
		if (L.equal(*base))			return base;
	}

	SConstantList* lst = new SConstantList();
	lst->_copy(L);

	lst->dwFlags			|=	xr_resource_flagged::RF_REGISTERED;
	lst_constants.push_back	(lst);
	return lst;
}
void			CResourceManager::_DeleteConstantList(const SConstantList* L )
{
	if (0==(L->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(lst_constants,L))					return;
	Msg	("! ERROR: Failed to find compiled list of r1-constant-defs");
}
//--------------------------------------------------------------------------------------------------------------
dx10ConstantBuffer* CResourceManager::_CreateConstantBuffer(ID3DShaderReflectionConstantBuffer* pTable)
{
	VERIFY(pTable);
	xrCriticalSectionGuard guard(creationGuard);
	dx10ConstantBuffer	*pTempBuffer = new dx10ConstantBuffer(pTable);

	for (u32 it=0; it<v_constant_buffer.size(); it++)
	{
		dx10ConstantBuffer*	buf		= v_constant_buffer[it];
		if (pTempBuffer->Similar(*buf))			
		{
			xr_delete(pTempBuffer);
			return buf;
		}
	}

	pTempBuffer->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	v_constant_buffer.push_back	(pTempBuffer);
	return pTempBuffer;
}
//--------------------------------------------------------------------------------------------------------------
void CResourceManager::_DeleteConstantBuffer(const dx10ConstantBuffer* pBuffer)
{
	if (0==(pBuffer->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;

	xrCriticalSectionGuard guard(creationGuard);

	if (reclaim(v_constant_buffer,pBuffer))						return;
	Msg	("! ERROR: Failed to find compiled constant buffer");
}

//--------------------------------------------------------------------------------------------------------------
SInputSignature* CResourceManager::_CreateInputSignature(ID3DBlob* pBlob)
{
	VERIFY(pBlob);

	xrCriticalSectionGuard guard(creationGuard);
	for (u32 it=0; it<v_input_signature.size(); it++)
	{
		SInputSignature*	sign		= v_input_signature[it];
		if ( (pBlob->GetBufferSize() == sign->signature->GetBufferSize()) &&
			(!(memcmp(pBlob->GetBufferPointer(), sign->signature->GetBufferPointer(), pBlob->GetBufferSize()))))
		{
			return sign;
		}
	}

	SInputSignature	*pSign = new SInputSignature(pBlob);

	pSign->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	v_input_signature.push_back	(pSign);

	return pSign;
}
//--------------------------------------------------------------------------------------------------------------
void CResourceManager::_DeleteInputSignature(const SInputSignature* pSignature)
{
	if (0==(pSignature->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_input_signature, pSignature))						return;
	Msg	("! ERROR: Failed to find compiled constant buffer");
}