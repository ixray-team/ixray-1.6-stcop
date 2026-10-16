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
#include "XMLBlendV2.h"

#include "3DFluid/dx103DFluidManager.h"
//	Already defined in Texture.cpp
void fix_texture_name(LPSTR fn);

namespace
{
	xrSRWLock ShaderCreateLock;
	thread_local u32 ShaderCreateDepth = 0;

	class ShaderCreateScope
	{
	public:
		explicit ShaderCreateScope(bool IsExclusive = false) : IsExclusive(IsExclusive)
		{
			if (ShaderCreateDepth++ != 0)
			{
				return;
			}

			IsOwner = true;
			if (IsExclusive)
			{
				ShaderCreateLock.AcquireExclusive();
			}
			else
			{
				ShaderCreateLock.AcquireShared();
			}
			RImplementation.clearAllShaderOptions();
		}

		~ShaderCreateScope()
		{
			--ShaderCreateDepth;
			if (!IsOwner)
			{
				return;
			}

			if (IsExclusive)
			{
				ShaderCreateLock.ReleaseExclusive();
			}
			else
			{
				ShaderCreateLock.ReleaseShared();
			}
		}

		ShaderCreateScope(const ShaderCreateScope&) = delete;
		ShaderCreateScope& operator=(const ShaderCreateScope&) = delete;

	private:
		bool IsExclusive = false;
		bool IsOwner = false;
	};
}

#ifndef MASTER_GOLD
static u32 CalculateXMLCRC(const char* path)
{
	IReader* reader = FS.r_open(path);
	if (!reader)
	{
		return 0;
	}

	u32 crc = crc32(reader->pointer(), reader->length());
	FS.r_close(reader);
	return crc;
}

#endif

static xr_string MakeXMLBlendKey(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	xr_string key = s_shader ? s_shader : "";
	key += "|";
	key += s_textures ? s_textures : "";
	key += "|";
	key += s_constants ? s_constants : "";
	key += "|";
	key += s_matrices ? s_matrices : "";
	key += "|";
	key += xr_string::ToString(Engine.External.GetSkinningMode());
	if (RImplementation.EditorInstancing)
	{
		key += "|inst";
	}
	return key;
}

void CResourceManager::ClearXMLBlendCache()
{
	ShaderCreateScope Scope(true);
	xrCriticalSectionGuard CacheGuard(creationGuard);
	m_xmlBlendCache.clear();
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
		if (ResourceLifetime::IsAlive(v_elements[it]) && S.equal(*(v_elements[it])))	return v_elements[it];

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
	xrCriticalSectionGuard guard(creationGuard);
	if (reclaim(v_elements,S))						return;
	Msg	("! ERROR: Failed to find compiled 'shader-element'");
}

Shader*	CResourceManager::_cpp_Create	(IBlender* B, const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
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
	_ParseList			(C.L_constants,	s_constants	);
	_ParseList			(C.L_matrices,	s_matrices	);

	// Compile element	(LOD0 - HQ)
	{
		C.iElement			= 0;
		C.bDetail			= m_textures_description.GetDetailTexture(C.L_textures[0],C.detail_texture,C.detail_scaler);
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[0]				= _CreateElement	(E);
	}

	// Compile element	(LOD1)
	{
		C.iElement			= 1;
		C.bDetail			= m_textures_description.GetDetailTexture(C.L_textures[0],C.detail_texture,C.detail_scaler);
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[1]				= _CreateElement	(E);
	}

	// Compile element
	{
		C.iElement			= 2;
		C.bDetail			= false;
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[2]				= _CreateElement	(E);
	}

	// Compile element
	{
		C.iElement			= 3;
		C.bDetail			= false;
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[3]				= _CreateElement	(E);
	}

	// Compile element
	{
		C.iElement			= 4;
		C.bDetail			= true;	//.$$$ HACK :)
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[4]				= _CreateElement	(E);
	}

	// Compile element
	{
		C.iElement			= 5;
		C.bDetail			= false;
		ShaderElement		E;
		C._cpp_Compile		(&E);
		S.E[5]				= _CreateElement	(E);
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
	ShaderCreateScope Scope;

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
#if !defined(_EDITOR)
	// Editors render the deferred path, so they never take the static (R1) blenders
	if (LightingModeIsStaticRender() && !XMLBlendV2::IsStaticBlend(s_shader))
	{
		if (IBlender* blender = _FindBlender(s_shader))
		{
			if (blender->getDescription().CLS != B_SCREEN_SET && blender->getDescription().CLS != B_SCREEN_GRAY)
				return _cpp_Create(blender, s_shader, s_textures, s_constants, s_matrices);
		}
	}
#endif

	if (CXMLBlend::Check(s_shader))
		return xr_make_unique<CXMLBlend>(s_shader)->Compile(s_textures);

	if (Shader* pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices))
		return pShader;

	if (!CXMLBlend::Check("stub_default"))
		FATAL("Can't find stub_default.xml");

	return xr_make_unique<CXMLBlend>("stub_default")->Compile(s_textures);
}

Shader* CResourceManager::Create	(const char* s_shader,	const char* s_textures,	const char* s_constants,	const char* s_matrices)
{
	ShaderCreateScope Scope;

	if (g_dedicated_server)
		return nullptr;

	Shader* pShader = nullptr;
	if (!IsForceBlenderShader(s_shader) && CXMLBlend::Check(s_shader))
	{
		xr_string key = MakeXMLBlendKey(s_shader, s_textures, s_constants, s_matrices);

#ifndef MASTER_GOLD
		u32 current_crc = CalculateXMLCRC(s_shader);
#endif

		{
			xrCriticalSectionGuard CacheGuard(creationGuard);
			auto it = m_xmlBlendCache.find(key);
			if (it != m_xmlBlendCache.end() && ResourceLifetime::IsAlive(it->second.shader))
			{
#ifndef MASTER_GOLD
				if (it->second.crc == current_crc)
					return it->second.shader;
#else
				return it->second.shader;
#endif
			}
		}

		pShader = _Compile(s_shader, s_textures, s_constants, s_matrices);

		if (pShader)
		{
			XMLBlendCacheEntry entry;
			entry.shader = pShader;

#ifndef MASTER_GOLD
			entry.crc = current_crc;
#endif

			xrCriticalSectionGuard CacheGuard(creationGuard);
			m_xmlBlendCache[key] = entry;
		}
	}
	else
		pShader = _Compile(s_shader, s_textures, s_constants, s_matrices);

	if (pShader && !pShader->src_shader)
	{
		pShader->src_shader = s_shader;
		pShader->src_textures = s_textures;
		pShader->src_constants = s_constants;
		pShader->src_matrices = s_matrices;
		pShader->src_skinning = Engine.External.GetSkinningMode();
	}

	return pShader;
}

void CResourceManager::RecompileShaders()
{
	if (RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
	ShaderCreateScope Scope(true);

	const int skinning = Engine.External.GetSkinningMode();

	for (size_t i = 0; i < v_shaders.size(); ++i)
	{
		Shader* S = v_shaders[i];
		if (!S->src_shader)
			continue;

		Engine.External.SetSkinningMode(S->src_skinning);
		RImplementation.clearAllShaderOptions();
		Shader* N = _Compile(S->src_shader.c_str(), S->src_textures.c_str(), S->src_constants.c_str(), S->src_matrices.c_str());
		if (N == S)
			continue;

		S->_copy(*N);
		if (!N->dwReference)
			xr_delete(N);
	}

	Engine.External.SetSkinningMode(skinning);
}

void CResourceManager::Delete(const Shader* S)
{
	if (0 == (S->dwFlags & xr_resource_flagged::RF_REGISTERED))
		return;

	xrCriticalSectionGuard guard(creationGuard);

	for (auto it = m_xmlBlendCache.begin(); it != m_xmlBlendCache.end(); )
	{
		if (it->second.shader == S)
			it = m_xmlBlendCache.erase(it);
		else
			++it;
	}

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

	if (!Device.IsEditorMode() && ps_r__common_flags.test(RFLAG_MT_TEX_LOAD) && Texture->StartLoading())
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
	if (RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
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
	const bool IsParallel = !Device.IsEditorMode() && ps_r__common_flags.test(RFLAG_MT_TEX_LOAD);
	for (auto& [Name, Texture] : m_textures)
	{
		if (!IsParallel || !Texture->StartLoading())
		{
			Texture->Load();
		}
	}
	WaitForTextureLoading();

	FluidManager.Initialize(70, 70, 70);
	FluidManager.SetScreenSize((u32)RCache.get_width(), (u32)RCache.get_height());
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
		if (ResourceLifetime::IsAlive(it) && InShader->equal(it))
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

static u32 GetFormatBitsPerPixel(ERHI_FORMAT Format)
{
	const u32 Value = static_cast<u32>(Format);

	if (Value >= 1 && Value <= 4)		return 128;
	if (Value >= 5 && Value <= 8)		return 96;
	if (Value >= 9 && Value <= 22)		return 64;
	if (Value >= 23 && Value <= 47)		return 32;
	if (Value >= 48 && Value <= 59)		return 16;
	if (Value >= 60 && Value <= 65)		return 8;
	if (Value == 66)					return 1;
	if (Value >= 70 && Value <= 72)		return 4;
	if (Value >= 73 && Value <= 78)		return 8;
	if (Value >= 79 && Value <= 81)		return 4;
	if (Value >= 82 && Value <= 84)		return 8;
	if (Value >= 94 && Value <= 99)		return 8;
	if (Value == 85 || Value == 86 || Value == 115) return 16;

	return 32;
}

void CResourceManager::_GetVideoMemoryUsage(u64& RenderTargets, u64& ShadowMaps, u64& Textures)
{
	xrCriticalSectionGuard Guard(creationGuard);
	RenderTargets = ShadowMaps = Textures = 0;

	for (const auto& [Name, Target] : m_rtargets)
	{
		const u64 Size = u64(Target->dwWidth) * Target->dwHeight * GetFormatBitsPerPixel(Target->fmt) / 8;

		if (strstr(Name, "smap"))
		{
			ShadowMaps += Size;
		}
		else
		{
			RenderTargets += Size;
		}
	}

	for (const auto& [Name, Texture] : m_textures)
	{
		Textures += Texture->flags.MemoryUsage;
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
