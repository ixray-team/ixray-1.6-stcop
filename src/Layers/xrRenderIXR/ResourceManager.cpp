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
#include "EditorSurface.h"

//	Already defined in Texture.cpp
void fix_texture_name(char* fn);

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

void CResourceManager::LoadXMLBlends()
{
	m_XMLBlends.clear();
	for (const char* Directory : { "blends\\", "", "d3d11\\" })
	{
		xr_vector<char*>* Folder = FS.file_list_open(_game_shaders_, Directory, FS_ListFiles | FS_RootOnly);
		if (!Folder)
		{
			continue;
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

	char* N = (char*)(Name);
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

	char* N = (char*)(Name);
	map_Blender::iterator I = m_blenders.find	(N);
	if (I==m_blenders.end())	return nullptr;
	else						return I->second;
}

void	CResourceManager::ED_UpdateBlender	(const char* Name, IBlender* data)
{
	char* N = (char*)(Name);
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

ref_selement CResourceManager::_CreateElement(ShaderElement& S)
{
	ref_selement Result;
	if (S.passes.empty())		return	nullptr;

	// Search equal in shaders array
	xrCriticalSectionGuard guard(creationGuard);
	for (u32 it=0; it<v_elements.size(); it++)
		if (ResourceLifetime::IsAlive(v_elements[it]) && S.equal(*(v_elements[it])) && ResourceLifetime::TryAcquire(v_elements[it], Result))
		{
			return Result;
		}

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

ref_shader CResourceManager::_cpp_Create(IBlender* B, const char* s_shader, const char* s_textures, const char*, const char* s_matrices)
{
	CBlender_Compile	C;
	Shader				S;

	// Access to template
	C.BT				= B;

	C.bDetail			= false;
	C.bHudElement		= false;

	if (Device.IsEditorMode() && !C.BT)
	{
		return nullptr;
	}

	// Parse names
	_ParseList			(C.L_textures,	s_textures	);
	_ParseList			(C.L_matrices,	s_matrices	);

	const bool Detail = m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
	const bool UseDetail[ShaderVariant::Count] = { Detail, Detail, false, false, true, false, false, Detail, false, Detail, Detail };
	// The blenders whose surfaces the forward frame can draw from the scene's clusters, and the models it takes from the
	// graph (GpuScene::TakeModels), which are not drawn into the cube around the eye from the scene
	const CLASS_ID Blender = C.BT ? C.BT->getDescription().CLS : CLASS_ID(0);
	// The editors draw each surface of theirs by itself (CompileEditorSurface): no clusters, no cube around the eye
	const bool IsForwardLevel = !Device.IsEditorMode();
	const bool IsSceneModel = IsForwardLevel && (Blender == B_MODEL || Blender == B_MODEL_EbB);
	const bool HasCluster = (Blender == B_DETAIL && !Device.IsEditorMode()) || IsSceneModel || (IsForwardLevel && (Blender == B_DEFAULT || Blender == B_DEFAULT_AREF || Blender == B_VERT || Blender == B_VERT_AREF || Blender == B_TREE || Blender == B_BmmD || Blender == B_LmBmmD || Blender == B_LmEbB || Blender == B_LaEmB));
	// The surfaces the forward level frame draws into the cube around the eye. An internal blender's elements are its
	// passes by number: none of them is touched
	const bool HasFace = IsForwardLevel && IsSurfaceBlender(C.BT);
	// The level blenders a moving object may wear: drawn by the scene as a model (ShaderVariant::ClusterModel)
	const bool IsMovingLevel = HasCluster && !IsSceneModel && Blender != B_DETAIL && Blender != B_BmmD && Blender != B_LmBmmD && Blender != B_LaEmB;
	for (u32 Variant = 0; Variant < ShaderVariant::Count; ++Variant)
	{
		const bool IsClusterFace = Variant == ShaderVariant::ClusterFace;
		const bool IsClusterSorted = Variant == ShaderVariant::ClusterSorted;
		const bool IsClusterModel = Variant == ShaderVariant::ClusterModel;
		// Only a material its own element sorts back to front is drawn by the sorted list
		const bool IsSorted = S.E[ShaderVariant::NormalHQ] && S.E[ShaderVariant::NormalHQ]->flags.bStrictB2F;
		const bool HasHud = S.E[ShaderVariant::HUD] && !S.E[ShaderVariant::HUD]->passes.empty();
		// A model's 3D icon is drawn by the scene only: its element is its cluster pass, its state as written
		const bool IsClusterIcon = Variant == ShaderVariant::UI && IsSceneModel;
		C.IsCluster = Variant == ShaderVariant::Cluster || IsClusterFace || IsClusterSorted || IsClusterModel || IsClusterIcon;
		C.IsSortedCluster = IsClusterSorted || IsClusterIcon || (IsClusterModel && (IsSorted || IsSceneModel));
		C.IsModelCluster = IsClusterModel && !IsSceneModel;
		// The forward level frame draws the cube around the eye with its plain passes, made once more for it
		C.IsReflectionFace = HasFace && (Variant == ShaderVariant::Reflection || IsClusterFace);
		C.iElement = C.IsReflectionFace ? u32(ShaderVariant::NormalLQ) : IsClusterModel && IsSceneModel ? u32(ShaderVariant::HUD) : C.IsCluster && !IsClusterIcon ? u32(ShaderVariant::NormalHQ) : Variant;
		C.bDetail = UseDetail[Variant];
		ShaderElement E;

		C.IsClusterRejected = false;
		const bool IsMade = IsClusterModel ? (IsSceneModel ? HasHud : IsMovingLevel) : IsClusterSorted ? IsSorted : !IsClusterFace || (HasFace && !IsSceneModel);

		if (!C.IsCluster || (HasCluster && IsMade))
		{
			C._cpp_Compile(&E);
		}

		ShaderElement None;
		S.E[Variant] = _CreateElement(C.IsClusterRejected ? None : E);
	}

	C.IsCluster = false;
	C.IsSortedCluster = false;
	C.IsModelCluster = false;
	C.IsReflectionFace = false;

	ref_shader ResultShader = _CreateShader(&S);
	return ResultShader;
}

ref_shader CResourceManager::_cpp_Create(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
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

ref_shader CResourceManager::Create(IBlender* B, const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	ShaderCreateScope Scope;

	if (!g_dedicated_server)
	{
		return	_cpp_Create	(B,s_shader,s_textures,s_constants,s_matrices);
	}

	return nullptr;
}

ref_shader CResourceManager::_Compile(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	if (IsForceBlenderShader(s_shader))
	{
		if (ref_shader pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices))
			return pShader;
	}

	CXMLBlend* XMLBlend = FindXMLBlend(s_shader);
	if (!(XMLBlend && XMLBlend->IsSimplified()))
	{
		if (IBlender* blender = _FindBlender(s_shader))
		{
			if (blender->getDescription().CLS != B_SCREEN_SET && blender->getDescription().CLS != B_SCREEN_GRAY)
				return _cpp_Create(blender, s_shader, s_textures, s_constants, s_matrices);
		}
	}

	if (XMLBlend)
	{
		if (ref_shader pShader = XMLBlend->Compile(s_textures))
			return pShader;
	}

	if (ref_shader pShader = _cpp_Create(s_shader, s_textures, s_constants, s_matrices))
		return pShader;

	CXMLBlend* Stub = FindXMLBlend("stub_default");
	if (!Stub)
		FATAL("Can't find stub_default.xml");

	return Stub->Compile(s_textures);
}

ref_shader CResourceManager::Create(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	ShaderCreateScope Scope;

	if (g_dedicated_server)
		return nullptr;

	ref_shader pShader = _Compile(s_shader, s_textures, s_constants, s_matrices);

	xrCriticalSectionGuard MetadataGuard(creationGuard);
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
	if (RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}

	RImplementation.Scene.ForgetIcons();

	// Every shader is compiled on the workers first and kept: the loop after this one, which swaps them in one at a
	// time with nothing else creating shaders, then finds every stage made. Compiled where it stood, a change of a
	// setting took as long as all its shaders take one after another
	struct Source
	{
		shared_str Shader, Textures, Constants, Matrices;
	};

	xr_vector<Source> Sources;
	{
		xrCriticalSectionGuard Guard(creationGuard);

		for (const Shader* S : v_shaders)
		{
			if (S->src_shader)
			{
				Sources.push_back({ S->src_shader, S->src_textures, S->src_constants, S->src_matrices });
			}
		}
	}

	xr_vector<ref_shader> Made(Sources.size());
	CTimer Timer;
	Timer.Start();
	xr_parallel_for(size_t(0), Sources.size(), [&](size_t Index)
	{
		ShaderCreateScope Shared;
		Made[Index] = _Compile(Sources[Index].Shader.c_str(), Sources[Index].Textures.c_str(), Sources[Index].Constants.c_str(), Sources[Index].Matrices.c_str());
	});
	Msg("* shaders recompiled for new settings: %u in %u ms", u32(Sources.size()), Timer.GetElapsed_ms());

	// What a setting made stays for the run: going back to it finds its stages, where they would be compiled again
	{
		xrCriticalSectionGuard Guard(PendingGuard);

		for (const ref_shader& Item : Made)
		{
			if (Item && KeptSet.insert(Item._get()).second)
			{
				KeptShaders.push_back(Item);
			}
		}
	}

	ShaderCreateScope Scope(true);

	for (size_t i = 0; i < v_shaders.size(); ++i)
	{
		Shader* S = v_shaders[i];
		if (!S->src_shader)
			continue;

		ref_shader N = _Compile(S->src_shader.c_str(), S->src_textures.c_str(), S->src_constants.c_str(), S->src_matrices.c_str());
		if (N == S)
			continue;

		S->_copy(*N);
	}

	RImplementation.Scene.RefreshForward();
}

void CResourceManager::GetShaders(xr_vector<ref_shader>& Shaders)
{
	xrCriticalSectionGuard Guard(creationGuard);
	xrCriticalSectionGuard Kept(PendingGuard);

	for (Shader* Item : v_shaders)
	{
		ref_shader Held;

		// What only the kept list holds is drawn by nothing, and keeps the stages of the setting it was made for
		if (KeptSet.contains(Item) && Item->dwReference.load(std::memory_order_acquire) == 1)
		{
			continue;
		}

		if (ResourceLifetime::IsAlive(Item) && ResourceLifetime::TryAcquire(Item, Held))
		{
			Shaders.push_back(Held);
		}
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
		return;
	}

	xrCriticalSectionGuard Guard(PendingGuard);
	PendingTextures.push_back(Texture->cName);
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
		// A level texture the scene reads from its buckets has no whole copy until a draw binds it: loaded here, every
		// one of them came back and a large level held several times the card's memory
		if (!ResourceLifetime::IsAlive(Texture) || RImplementation.Scene.HasNoWholeCopy(Texture))
		{
			continue;
		}

		if (!IsParallel || !Texture->StartLoading())
		{
			Texture->Load();
		}
	}
	WaitForTextureLoading();
}

void CResourceManager::KeepStage(char Stage, const char* Name, const ShaderExternalMap& Defines)
{
	static const struct
	{
		char Stage;
		const char* Extension;
	} Kinds[] = { { 'v', ".vs.hlsl" }, { 'p', ".ps.hlsl" }, { 'g', ".gs.hlsl" }, { 'h', ".hs.hlsl" }, { 'd', ".ds.hlsl" }, { 'c', ".cs.hlsl" } };

	const auto Kind = std::find_if(std::begin(Kinds), std::end(Kinds), [Stage](const auto& Item) { return Item.Stage == Stage; });

	if (Kind == std::end(Kinds))
	{
		return;
	}

	// A stage whose file is gone since the manifest was written is left out
	string_path Path;
	string16 Entry;
	IReader* File = RImplementation.OpenShader(Path, Name, Kind->Extension, Entry);

	if (!File)
	{
		return;
	}

	FS.r_close(File);
	ShaderCreateScope Scope;

	switch (Stage)
	{
	case 'v':
	{
		ref_vs Made = _CreateVS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptVs.push_back(Made);
		break;
	}
	case 'p':
	{
		ref_ps Made = _CreatePS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptPs.push_back(Made);
		break;
	}
	case 'g':
	{
		ref_gs Made = _CreateGS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptGs.push_back(Made);
		break;
	}
	case 'h':
	{
		ref_hs Made = _CreateHS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptHs.push_back(Made);
		break;
	}
	case 'd':
	{
		ref_ds Made = _CreateDS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptDs.push_back(Made);
		break;
	}
	default:
	{
		ref_cs Made = _CreateCS(Name, Defines);
		xrCriticalSectionGuard Guard(PendingGuard);
		KeptCs.push_back(Made);
		break;
	}
	}
}

void CResourceManager::ReleaseKeptStages()
{
	xrCriticalSectionGuard Guard(PendingGuard);
	KeptShaders.clear();
	KeptSet.clear();
	KeptVs.clear();
	KeptPs.clear();
	KeptGs.clear();
	KeptHs.clear();
	KeptDs.clear();
	KeptCs.clear();
}

void CResourceManager::ForgetPending()
{
	xrCriticalSectionGuard Guard(PendingGuard);
	PendingTextures.clear();
}

void CResourceManager::UploadPending()
{
	xrCriticalSectionGuard Guard(creationGuard);
	xr_vector<shared_str> Names;
	{
		xrCriticalSectionGuard Pending(PendingGuard);
		Names.swap(PendingTextures);
	}

	u32 Loaded = 0;

	for (const shared_str& Name : Names)
	{
		const auto Found = m_textures.find(Name.c_str());

		if (Found == m_textures.end())
		{
			continue;
		}

		CTexture* Texture = Found->second;

		if (ResourceLifetime::IsAlive(Texture) && !Texture->pSurface && !Texture->IsLoadPending() && !RImplementation.Scene.HasNoWholeCopy(Texture))
		{
			Texture->Load();
			++Loaded;
		}
	}

	Msg("* [scene] textures loaded after the scene: %u of %u that waited", Loaded, u32(Names.size()));
}

void CResourceManager::DeferredUnload() 
{
	xrCriticalSectionGuard Guard(creationGuard);
	if (!RDEVICE.b_is_Ready)
		return;

	if (!DeferredTextureLoadEnabled())
		return;

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

ref_shader CResourceManager::_CreateShader(Shader* InShader)
{
	ref_shader Result;
	xrCriticalSectionGuard guard(creationGuard);

	// Search equal in shaders array
	for (Shader* it : v_shaders)
	{
		if (ResourceLifetime::IsAlive(it) && InShader->equal(it) && ResourceLifetime::TryAcquire(it, Result))
		{
			return Result;
		}
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
	m_base=c_base=m_lmaps=c_lmaps=0;

	map_Texture::iterator I = m_textures.begin	();
	map_Texture::iterator E = m_textures.end	();
	for (; I!=E; I++)
	{
		if (I->second->IsLoadPending())
		{
			continue;
		}
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
		if (!Texture->IsLoadPending())
		{
			Textures += Texture->flags.MemoryUsage;
		}
	}
}

void CResourceManager::LogVideoMemory()
{
	xrCriticalSectionGuard Guard(creationGuard);
	const auto SurfaceBytes = [](IRHISurface* Surface)
	{
		u64 Texels = 0;

		for (u32 Mip = 0; Mip < Surface->GetMipLevels(); ++Mip)
		{
			Texels += u64(std::max(Surface->GetWidth() >> Mip, 1u)) * std::max(Surface->GetHeight() >> Mip, 1u) * std::max(Surface->GetDepth() >> Mip, 1u);
		}

		return Texels * std::max(Surface->GetArraySize(), 1u) * GetFormatBitsPerPixel(Surface->GetFormat()) / 8;
	};

	xr_vector<std::pair<u64, const char*>> Largest;
	u64 Textures = 0;
	u64 Uploaded = 0;
	u32 Loaded = 0;

	for (const auto& [Name, Texture] : m_textures)
	{
		if (Texture->pSurface && !Texture->IsLoadPending())
		{
			const u64 Bytes = SurfaceBytes(Texture->pSurface);
			Textures += Bytes;
			Uploaded += Texture->flags.MemoryUsage;
			++Loaded;
			Largest.push_back({ Bytes, Name });
		}
	}

	u64 Targets = 0;

	for (const auto& [Name, Target] : m_rtargets)
	{
		if (Target->pSurface)
		{
			const u64 Bytes = SurfaceBytes(Target->pSurface);
			Targets += Bytes;
			Largest.push_back({ Bytes, Name });
		}
	}

	std::sort(Largest.begin(), Largest.end(), [](const auto& Left, const auto& Right) { return Left.first > Right.first; });
	Msg("* video memory: %u of %u textures loaded, %llu MB on the device (%llu MB uploaded); %u render targets, %llu MB", Loaded, u32(m_textures.size()), Textures >> 20, Uploaded >> 20, u32(m_rtargets.size()), Targets >> 20);

	for (size_t Index = 0; Index < std::min<size_t>(Largest.size(), 24); ++Index)
	{
		Msg("* video memory: %6.1f MB %s", float(Largest[Index].first) / (1024.f * 1024.f), Largest[Index].second);
	}
}

void CResourceManager::_DumpMemoryUsage		()
{
	xrCriticalSectionGuard Guard(creationGuard);
	xr_multimap<u32,std::pair<u32,shared_str> >		mtex	;

	// sort
	{
		map_Texture::iterator I = m_textures.begin	();
		map_Texture::iterator E = m_textures.end	();
		for (; I!=E; I++)
		{
			if (I->second->IsLoadPending())
			{
				continue;
			}
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
