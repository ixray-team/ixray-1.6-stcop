// TextureManager.cpp: implementation of the CResourceManager class.
//
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "SVGStorage.h"
#include "ResourceManager.h"
#include "tss.h"
#include "blenders/Blender.h"
#include "blenders/Blender_Recorder.h"
#include <execution>

#ifdef USE_DX11
#include "../xrRenderDX10/3DFluid/dx103DFluidManager.h"
#endif
//	Already defined in Texture.cpp
void fix_texture_name(LPSTR fn);
static xrCriticalSection ResSafe;

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

#ifdef USE_DX11
static xr_string MakeXMLBlendKey(const char* s_shader, const char* s_textures)
{
	xr_string key = s_shader ? s_shader : "";
	key += "|";
	key += s_textures ? s_textures : "";
	return key;
}

void CResourceManager::ClearXMLBlendCache()
{
	xrCriticalSectionGuard guard(ResSafe);
	m_xmlBlendCache.clear();
}
#endif

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
#ifdef USE_DX11
		Msg("DX10: Shader '%s' not found in library.", Name);
#else
		if (!Device.IsEditorMode())
		{
			Debug.fatal(DEBUG_INFO, "Shader '%s' not found in library.", Name);
		}
#endif
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
	svector<char,128>	N;

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

Shader*	CResourceManager::_cpp_Create	(IBlender* B, const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	xrCriticalSectionGuard guard(creationGuard);

	CBlender_Compile	C;
	Shader				S;

	// Access to template
	C.BT				= B;
	C.bEditor			= false;
	C.bDetail			= false;
#ifdef _EDITOR
	if (!C.BT)			{ /*ELog.Msg(mtError, "Can't find shader '%s'", s_shader); */return nullptr; }
	C.bEditor			= true;
#endif

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
#ifdef USE_DX11
		IBlender* pBlender = _GetBlender(s_shader ? s_shader : "null");
		if (!pBlender)
		{
			return nullptr;
		}
		return	_cpp_Create(pBlender, s_shader, s_textures, s_constants, s_matrices);
#else //USE_DX11
		return	_cpp_Create(_GetBlender(s_shader ? s_shader : "null"), s_shader, s_textures, s_constants, s_matrices);
#endif
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

Shader* CResourceManager::Create	(const char* s_shader,	const char* s_textures,	const char* s_constants,	const char* s_matrices)
{
	xrCriticalSectionGuard guard(ResSafe);

	if (!g_dedicated_server)
	{
		//	TODO: DX10: When all shaders are ready switch to common path
#ifdef USE_DX11
		if (CXMLBlend::Check(s_shader))
		{
			xr_string key = MakeXMLBlendKey(s_shader, s_textures);
			auto it = m_xmlBlendCache.find(key);

#ifndef MASTER_GOLD
			u32 current_crc = CalculateXMLCRC(s_shader);
#endif

			if (it != m_xmlBlendCache.end())
			{
#ifndef MASTER_GOLD
				if (it->second.crc == current_crc)
					return it->second.shader;
#else
				return it->second.shader;
#endif
			}

			// Компиляция
			xr_unique_ptr<CXMLBlend> BlendXML = xr_make_unique<CXMLBlend>(s_shader);
			Shader* pShader = BlendXML->Compile(s_textures);

			if (pShader)
			{
				XMLBlendCacheEntry entry;
				entry.shader = pShader;

#ifndef MASTER_GOLD
				entry.crc = current_crc;
#endif

				m_xmlBlendCache[key] = entry;
			}

			return pShader;
		}
		else if	(_lua_HasShader(s_shader))		
			return	_lua_Create	(s_shader,s_textures);
		else
		{
			Shader* pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices);
			if (pShader)
				return pShader;
			else
			{
				if (_lua_HasShader("stub_default"))
					return	_lua_Create("stub_default", s_textures);
				else
				{
					FATAL("Can't find stub_default.s");
					return 0;
				}
			}
		}
#else //USE_DX11
		if	(_lua_HasShader(s_shader))		
			return	_lua_Create	(s_shader,s_textures);
		else
			return	_cpp_Create	(s_shader,s_textures,s_constants,s_matrices);
#endif
	}

	return nullptr;
}

void CResourceManager::Delete(const Shader* S)
{
	if (0 == (S->dwFlags & xr_resource_flagged::RF_REGISTERED))
		return;

	xrCriticalSectionGuard guard(creationGuard);

#ifdef USE_DX11
	for (auto it = m_xmlBlendCache.begin(); it != m_xmlBlendCache.end(); )
	{
		if (it->second.shader == S)
			it = m_xmlBlendCache.erase(it);
		else
			++it;
	}
#endif

	if (reclaim(v_shaders, S))
		return;

	Msg("! ERROR: Failed to find complete shader");
}

void CResourceManager::DeferredUpload()
{
	if (!RDEVICE.b_is_Ready) return;

	PROF_EVENT("CResourceManager::DeferredUpload");
	Log("Loading textures via DeferredUpload");

	// Build list of textures that actually need loading to avoid extra work
	xr_vector<CTexture*> to_load;
	to_load.reserve(m_textures.size());
	for (auto& pair : m_textures) {
		CTexture* T = pair.second;
		// Only enqueue textures that are not already marked as loaded
		if (T && !T->flags.bLoaded)
			to_load.push_back(T);
	}

#ifndef _EDITOR
	if (ps_r__common_flags.test(RFLAG_MT_TEX_LOAD)) {
		// Parallel: load filtered list
		xr_parallel_foreach(to_load.begin(), to_load.end(), [](CTexture* texPtr) { texPtr->Load(); });
	}
	else
#endif // _EDITOR
	{
		// Single-threaded: load filtered list
		for (CTexture* T : to_load)
			T->Load();
	}

#ifdef USE_DX11
	FluidManager.Initialize(70, 70, 70);
	FluidManager.SetScreenSize((u32)RCache.get_width(), (u32)RCache.get_height());
#endif
}

void CResourceManager::DeferredUnload() 
{
	if (!RDEVICE.b_is_Ready)
		return;

#ifdef USE_DX11
	FluidManager.Destroy();
#endif

	for (auto& texture : m_textures)
		texture.second->Unload();
}

void CResourceManager::ED_UpdateTextures(xr_vector<xr_string>* names)
{
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
