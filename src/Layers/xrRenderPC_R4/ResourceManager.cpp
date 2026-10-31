// TextureManager.cpp: implementation of the CResourceManager class.
//
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "SVGStorage.h"
#include "ResourceManager.h"
#include "../../xrEngine/EngineAPI.h"
#include "tss.h"
#include "blenders/Blender.h"
#include "blenders/Blender_Recorder.h"

#include "3DFluid/dx103DFluidManager.h"
//	Already defined in Texture.cpp
void fix_texture_name(LPSTR fn);
static xrCriticalSection ResSafe;

void CResourceManager::LoadXMLBlends()
{
	m_XMLBlends.clear();
	xr_vector<char*>* Folder = FS.file_list_open(_game_shaders_, RImplementation.getShaderPath(), FS_ListFiles | FS_RootOnly);
	if (!Folder)
	{
		return;
	}
	for (char* File : *Folder)
	{
		if (const char* Ext = strext(File); Ext && !xr_strcmp(Ext, ".xml"))
		{
			m_XMLBlends[xr_string(File, Ext - File)] = xr_make_unique<CXMLBlend>(File);
		}
	}
	FS.file_list_close(Folder);
}

CXMLBlend* CResourceManager::FindXMLBlend(const char* s_shader)
{
	xr_string Name = s_shader ? s_shader : "";
	std::replace(Name.begin(), Name.end(), '\\', '_');
	const auto It = m_XMLBlends.find(Name);
	return It == m_XMLBlends.end() ? nullptr : It->second.get();
}

//--------------------------------------------------------------------------------------------------------------
template <class T>
bool	reclaim		(xr_vector<T*>& vec, const T* ptr)
{
	typename xr_vector<T*>::iterator it	= vec.begin	();
	typename xr_vector<T*>::iterator end = vec.end	();
	for (; it!=end; it++)
		if (*it == ptr)	{ vec.erase	(it); return true; }
		return false;
}

//--------------------------------------------------------------------------------------------------------------
IBlender* CResourceManager::_GetBlender		(const char* Name)
{
	R_ASSERT(Name && Name[0]);

	LPSTR N = LPSTR(Name);
	map_Blender::iterator I = m_blenders.find	(N);

//	TODO: DX10: When all shaders are ready switch to common path
	if (I==m_blenders.end())
	{
		Msg("DX10: Shader '%s' not found in library.", Name);
		return nullptr;
	}
	
	return I->second;
}

IBlender* CResourceManager::_FindBlender		(const char* Name)
{
	if (!(Name && Name[0])) return nullptr;

	LPSTR N = LPSTR(Name);
	map_Blender::iterator I = m_blenders.find	(N);
	if (I==m_blenders.end())	return nullptr;
	else						return I->second;
}

void	CResourceManager::ED_UpdateBlender	(const char* Name, IBlender* data)
{
	LPSTR N = LPSTR(Name);
	map_Blender::iterator I = m_blenders.find	(N);
	if (I!=m_blenders.end())	{
		R_ASSERT	(data->getDescription().CLS == I->second->getDescription().CLS);
		xr_delete	(I->second);
		I->second	= data;
	} else {
		m_blenders.insert	(std::make_pair(xr_strdup(Name),data));
	}
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
void	CResourceManager::_ParseList(sh_list& dest, const char* names)
{
	if (nullptr==names || 0==names[0])
 		names 	= "$null";

	ZeroMemory			(&dest, sizeof(dest));
	char*	P			= (char*) names;
	FixedVector<char,128>	N;

	while (*P)
	{
		if (*P == ',') {
			// flush
			N.push_back	(0);
			_strlwr		(N.begin());

			fix_texture_name( N.begin() );
//. andy			if (strext(N.begin())) *strext(N.begin())=0;
			dest.push_back(N.begin());
			N.clear		();
		} else {
			N.push_back	(*P);
		}
		P++;
	}
	if (N.size())
	{
		// flush
		N.push_back	(0);
		_strlwr		(N.begin());

		fix_texture_name( N.begin() );
//. andy		if (strext(N.begin())) *strext(N.begin())=0;
		dest.push_back(N.begin());
	}
}

ShaderElement* CResourceManager::_CreateElement			(ShaderElement& S)
{
	if (S.passes.empty())		return	nullptr;

	// Search equal in shaders array
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 it=0; it<v_elements.size(); it++)
		if (S.equal(*(v_elements[it])))	return v_elements[it];

	// Create _new_ entry
	ShaderElement* N = new ShaderElement();
	N->_copy(S);

	N->dwFlags				|=	xr_resource_flagged::RF_REGISTERED;
	v_elements.push_back	(N);
	return N;
}

void CResourceManager::_DeleteElement(const ShaderElement* S)
{
	if (0==(S->dwFlags&xr_resource_flagged::RF_REGISTERED))	return;
	if (reclaim(v_elements,S))						return;
	Msg	("! ERROR: Failed to find compiled 'shader-element'");
}

Shader*	CResourceManager::_cpp_Create	(IBlender* B, const char* s_shader, const char* s_textures, const char*, const char* s_matrices)
{
	xrCriticalSectionGuard guard(creationGuard);

	CBlender_Compile	C;
	Shader				S;

	// Access to template
	C.BT				= B;

	C.bEditor			= false;
	C.bDetail			= false;
	C.bHudElement		= false;

	if (Device.IsEditorMode())
	{
		if (!C.BT)		{ return nullptr; }
		C.bEditor		= true;
	}

	// Parse names
	_ParseList			(C.L_textures,	s_textures	);
	_ParseList			(C.L_matrices,	s_matrices	);

	const bool Detail = m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
	const bool UseDetail[ShaderVariant::Count] = { Detail, Detail, false, false, true, false, !LightingModeIsStaticRender() };
	for (u32 Variant = 0; Variant < ShaderVariant::Count; ++Variant)
	{
		C.iElement = Variant;
		C.bDetail = UseDetail[Variant];
		ShaderElement E;
		C._cpp_Compile(&E);
		S.E[Variant] = _CreateElement(E);
	}

	Shader* ResultShader = _CreateShader(&S);
	return ResultShader;
}

Shader*	CResourceManager::_cpp_Create(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	if (!g_dedicated_server)
	{
		//	TODO: DX10: When all shaders are ready switch to common path
		IBlender* pBlender = _GetBlender(s_shader ? s_shader : "null");
		if (!pBlender)
		{
			return nullptr;
		}
		return	_cpp_Create(pBlender, s_shader, s_textures, s_constants, s_matrices);
	}

	return nullptr;
}

Shader*CResourceManager::Create(IBlender* B, const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	if (!g_dedicated_server)
	{
		return	_cpp_Create	(B,s_shader,s_textures,s_constants,s_matrices);
	}

	return nullptr;
}

Shader* CResourceManager::_Compile(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	if (IsForceBlenderShader(s_shader))
	{
		if (Shader* pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices))
			return pShader;
	}

	CXMLBlend* XMLBlend = FindXMLBlend(s_shader);
#if !defined(_EDITOR)
	// Editors render the deferred path, so they never take the static (R1) blenders
	if (LightingModeIsStaticRender() && !(XMLBlend && XMLBlend->IsStatic()))
	{
		if (IBlender* blender = _FindBlender(s_shader))
		{
			if (blender->getDescription().CLS != B_SCREEN_SET && blender->getDescription().CLS != B_SCREEN_GRAY)
				return _cpp_Create(blender, s_shader, s_textures, s_constants, s_matrices);
		}
	}
#endif

	if (XMLBlend)
	{
		if (Shader* pShader = XMLBlend->Compile(s_textures))
			return pShader;
	}

	if (Shader* pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices))
		return pShader;

	CXMLBlend* Stub = FindXMLBlend("stub_default");
	if (!Stub)
		FATAL("Can't find stub_default.xml");

	return Stub->Compile(s_textures);
}

Shader* CResourceManager::Create	(const char* s_shader,	const char* s_textures,	const char* s_constants,	const char* s_matrices)
{
	xrCriticalSectionGuard guard(ResSafe);

	if (g_dedicated_server)
		return nullptr;

	Shader* pShader = _Compile(s_shader, s_textures, s_constants, s_matrices);
	if (pShader && !pShader->src_shader)
	{
		pShader->src_shader = s_shader;
		pShader->src_textures = s_textures;
		pShader->src_constants = s_constants;
		pShader->src_matrices = s_matrices;
	}

	return pShader;
}

void CResourceManager::RecompileShaders()
{
	xrCriticalSectionGuard guard(ResSafe);

	for (size_t i = 0; i < v_shaders.size(); ++i)
	{
		Shader* S = v_shaders[i];
		if (!S->src_shader)
			continue;

		Shader* N = _Compile(S->src_shader.c_str(), S->src_textures.c_str(), S->src_constants.c_str(), S->src_matrices.c_str());
		if (N == S)
			continue;

		S->_copy(*N);
		if (!N->dwReference)
			xr_delete(N);
	}
}

void CResourceManager::Delete(const Shader* S)
{
	if (0 == (S->dwFlags & xr_resource_flagged::RF_REGISTERED))
		return;

	xrCriticalSectionGuard guard(creationGuard);

	if (reclaim(v_shaders, S))
		return;

	Msg("! ERROR: Failed to find complete shader");
}

void CResourceManager::LoadTexture(CTexture* Texture)
{
	if (!RDEVICE.b_is_Ready)
	{
		return;
	}

	if (!Device.IsEditorMode() && ps_r__common_flags.test(RFLAG_MT_TEX_LOAD) && GRHI->APILevel != ERHI_API_LAYER::D3D12 && Texture->StartLoading())
	{
		return;
	}

	if (!bDeferredLoad)
	{
		Texture->Load();
	}
}

void CResourceManager::WaitForTextureLoading()
{
	PROF_EVENT("CResourceManager::WaitForTextureLoading");
	// LoadImpl does not acquire creationGuard or access texture descriptions.
	xrCriticalSectionGuard Guard(creationGuard);
	for (auto& [Name, Texture] : m_textures)
	{
		Texture->WaitForLoad();
	}
}

void CResourceManager::ReloadTextureDescriptions()
{
	xrCriticalSectionGuard Guard(creationGuard);
	WaitForTextureLoading();
	m_textures_description.UnLoad();
	m_textures_description.Load();
}

// Direct3D 9 keeps textures in D3DPOOL_DEFAULT. IDirect3DDevice9::Reset fails while any of them
// exists, so R1 and R2 always unload and upload textures in bulk, whatever user.ltx says.
static bool DeferredTextureLoadEnabled()
{
	return Device.IsEditorMode() || ps_r__common_flags.test(RFLAG_DD_TEX_LOAD);
}

void CResourceManager::DeferredUpload()
{
	WaitForTextureLoading();
	if (!RDEVICE.b_is_Ready)
	{
		return;
	}

	if (!DeferredTextureLoadEnabled())
	{
		return;
	}
	PROF_EVENT("CResourceManager::DeferredUpload");
	Log("Loading textures via DeferredUpload");

	xrCriticalSectionGuard Guard(creationGuard);
	const bool IsParallel = !Device.IsEditorMode() && ps_r__common_flags.test(RFLAG_MT_TEX_LOAD) && GRHI->APILevel != ERHI_API_LAYER::D3D12;
	for (auto& [Name, Texture] : m_textures)
	{
		if (!IsParallel || !Texture->StartLoading())
		{
			Texture->Load();
		}
	}
	WaitForTextureLoading();

	FluidManager.Initialize(70, 70, 70);
	FluidManager.SetScreenSize((u32)GfxCmdList::GetWidth(), (u32)GfxCmdList::GetHeight());
}

void CResourceManager::DeferredUnload() 
{
	xrCriticalSectionGuard Guard(creationGuard);
	WaitForTextureLoading();
	if (!RDEVICE.b_is_Ready)
		return;

	if (!DeferredTextureLoadEnabled())
		return;

	FluidManager.Destroy();

	for (auto& texture : m_textures)
		texture.second->Unload();
}

void CResourceManager::ED_UpdateTextures(xr_vector<xr_string>* names)
{
	xrCriticalSectionGuard Guard(creationGuard);
	WaitForTextureLoading();
	// 1. Unload
	if (names){
		for (u32 nid=0; nid<names->size(); nid++)
		{
			map_TextureIt I = m_textures.find	((*names)[nid].c_str());
			if (I!=m_textures.end())	I->second->Unload();
		}
	}else{
		for (map_TextureIt t=m_textures.begin(); t!=m_textures.end(); t++)
			t->second->Unload();
	}

	// 2. Load
	// DeferredUpload	();
}

Shader* CResourceManager::_CreateShader(Shader* InShader)
{
	xrCriticalSectionGuard guard(creationGuard);

	// Search equal in shaders array
	for (Shader* it : v_shaders)
	{
		if (InShader->equal(it))
			return it;
	}

	// Create _new_ entry
	Shader* N = new Shader();
	N->_copy(*InShader);
	N->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	v_shaders.push_back(N);

	return N;
}

void	CResourceManager::_GetMemoryUsage(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps)
{
	xrCriticalSectionGuard Guard(creationGuard);
	WaitForTextureLoading();
	m_base=c_base=m_lmaps=c_lmaps=0;

	map_Texture::iterator I = m_textures.begin	();
	map_Texture::iterator E = m_textures.end	();
	for (; I!=E; I++)
	{
		u32 m = I->second->flags.MemoryUsage;
		if (strstr(I->first,"lmap"))
		{
			c_lmaps	++;
			m_lmaps	+= m;
		} else {
			c_base	++;
			m_base	+= m;
		}
	}
}

void CResourceManager::_DumpMemoryUsage		()
{
	xrCriticalSectionGuard Guard(creationGuard);
	WaitForTextureLoading();
	xr_multimap<u32,std::pair<u32,shared_str> >		mtex	;

	// sort
	{
		map_Texture::iterator I = m_textures.begin	();
		map_Texture::iterator E = m_textures.end	();
		for (; I!=E; I++)
		{
			u32			m = I->second->flags.MemoryUsage;
			shared_str	n = I->second->cName;
			mtex.insert (std::make_pair(m,std::make_pair((u32)I->second->dwReference,n) ));
		}
	}

	// dump
	{
		xr_multimap<u32,std::pair<u32,shared_str> >::iterator I = mtex.begin	();
		xr_multimap<u32,std::pair<u32,shared_str> >::iterator E = mtex.end		();
		for (; I!=E; I++)
			Msg			("* %4.1f : [%4d] %s",float(I->first)/1024.f, I->second.first, I->second.second.c_str());
	}
}

void CResourceManager::Evict()
{
	GRHI->EvictManagedResources();
}

void CResourceManager::Initialize_SVGStorage()
{
	// we don't use storage svg if rendering ui is raster because there's no need in such creation
	if (m_pStorageSVG == nullptr)
	{
		m_pStorageSVG = new CSVGStorage(static_cast<u32>(eSVGStorageFlags::kFeatureSVGStorage_Static_Allocation));
		m_pStorageSVG->init();
	}
}
