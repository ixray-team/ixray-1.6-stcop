// TextureManager.h: interface for the CTextureManager class.
//
//////////////////////////////////////////////////////////////////////

#ifndef ResourceManagerH
#define ResourceManagerH
#pragma once

#include	"shader.h"
#include	"tss_def.h"
#include	"TextureDescrManager.h"
// refs
struct		lua_State;

class dx10ConstantBuffer;

// defs
class ECORE_API CResourceManager
{
private:
	struct str_pred {
		IC bool operator()(LPCSTR x, LPCSTR y) const
		{	return xr_strcmp(x,y)<0;	}
	};
	struct texture_detail	{
		const char*			T;
		R_constant_setup*	cs;
	};
public:
	using map_Blender = xr_map<const char*, IBlender*, str_pred>;
	using map_BlenderIt = map_Blender::iterator;

	using map_Texture = xr_map<const char*, CTexture*, str_pred>;
	using map_TextureIt = map_Texture::iterator;

	using map_Matrix = xr_map<const char*, CMatrix*, str_pred>;
	using map_MatrixIt = map_Matrix::iterator;

	using map_Constant = xr_map<const char*, CConstant*, str_pred>;
	using map_ConstantIt = map_Constant::iterator;

	using map_RT = xr_map<const char*, CRT*, str_pred>;
	using map_RTIt = map_RT::iterator;

	// DX10 cut
	//using map_RTC = xr_map<const char*, CRTC*, str_pred>;
	//using map_RTCIt = map_RTC::iterator;

	using map_VS = xr_map<const char*, SVS*, str_pred>;
	using map_VSIt = map_VS::iterator;

#ifdef USE_DX11
	using map_GS = xr_map<const char*, SGS*, str_pred>;
	using map_GSIt = map_GS::iterator;
#endif

#ifdef USE_DX11
	using map_HS = xr_map<const char*, SHS*, str_pred>;
	using map_HSIt = map_HS::iterator;

	using map_DS = xr_map<const char*, SDS*, str_pred>;
	using map_DSIt = map_DS::iterator;

	using map_CS = xr_map<const char*, SCS*, str_pred>;
	using map_CSIt = map_CS::iterator;
#endif

	using map_PS = xr_map<const char*, SPS*, str_pred>;
	using map_PSIt = map_PS::iterator;

	using map_TD = xr_map<const char*, texture_detail, str_pred>;
	using map_TDIt = map_TD::iterator;

private:
	// data
	map_Blender											m_blenders;
	map_Texture											m_textures;
	map_Matrix											m_matrices;
	map_Constant										m_constants;
	map_RT												m_rtargets;

	xr_vector<CRT*>										m_rt_sorter;
	//	DX10 cut map_RTC												m_rtargets_c;
	map_VS												m_vs;
	map_PS												m_ps;
#ifdef USE_DX11
	map_GS												m_gs;
#endif //USE_DX11
	map_TD												m_td;

	xr_vector<SState*>									v_states;
	xr_vector<SDeclaration*>							v_declarations;
	xr_vector<SGeometry*>								v_geoms;
	xr_vector<R_constant_table*>						v_constant_tables;

#ifdef USE_DX11
	xr_vector<dx10ConstantBuffer*>						v_constant_buffer;
	xr_vector<SInputSignature*>							v_input_signature;
#endif //USE_DX11

	// lists
	xr_vector<STextureList*>							lst_textures;
	xr_vector<SMatrixList*>								lst_matrices;
	xr_vector<SConstantList*>							lst_constants;

	// main shader-array
	xr_vector<SPass*>									v_passes;
	xr_vector<ShaderElement*>							v_elements;
	xr_vector<Shader*>									v_shaders;
	
	xr_vector<ref_texture>								m_necessary;

	// misc
	xrCriticalSection creationGuard;
public:
	CTextureDescrMngr									m_textures_description;
	xr_vector<std::pair<shared_str,R_constant_setup*> >	v_constant_setup;
	lua_State*											LSVM;
	BOOL												bDeferredLoad;
private:
	void							LS_Load				();
	void							LS_Unload			();
public:
	// Miscelaneous
	void							_ParseList			(sh_list& dest, LPCSTR names);
	IBlender*						_GetBlender			(LPCSTR Name);
	IBlender* 						_FindBlender		(LPCSTR Name);
	void							_GetMemoryUsage		(u32& m_base, u32& c_base, u32& m_lmaps, u32& c_lmaps);
	void							_DumpMemoryUsage	();
//.	BOOL							_GetDetailTexture	(LPCSTR Name, LPCSTR& T, R_constant_setup* &M);

	map_Blender&					_GetBlenders		()		{	return m_blenders;	}

	// Debug
	void							DBG_VerifyGeoms		();
	void							DBG_VerifyTextures	();

	// Editor cooperation
	void							ED_UpdateBlender	(LPCSTR Name, IBlender*		data);
#ifdef _EDITOR
	void							ED_UpdateTextures	(xr_vector<xr_string>* names);
#endif

	// Low level resource creation
	CTexture*						_CreateTexture		(LPCSTR Name);
	void							_DeleteTexture		(const CTexture* T);

	CMatrix*						_CreateMatrix		(LPCSTR Name);
	void							_DeleteMatrix		(const CMatrix*  M);
	Shader*							_CreateShader		(Shader* InShader);
	CConstant*						_CreateConstant		(LPCSTR Name);
	void							_DeleteConstant		(const CConstant* C);

	R_constant_table*				_CreateConstantTable(R_constant_table& C);
	void							_DeleteConstantTable(const R_constant_table* C);

#ifdef USE_DX11
	dx10ConstantBuffer*				_CreateConstantBuffer(ID3DShaderReflectionConstantBuffer* pTable);
	void							_DeleteConstantBuffer(const dx10ConstantBuffer* pBuffer);

	SInputSignature*				_CreateInputSignature(ID3DBlob* pBlob);
	void							_DeleteInputSignature(const SInputSignature* pSignature);
#endif //USE_DX11

#ifdef USE_DX11
	CRT*							_CreateRT			(LPCSTR Name, u32 w, u32 h, DxgiFormat f, u32 SampleCount = 1, bool useUAV=false );
#else
	CRT*							_CreateRT			(LPCSTR Name, u32 w, u32 h,	D3DFORMAT f, u32 SampleCount = 1 );
#endif
	void							_DeleteRT			(const CRT*	RT	);

	//	DX10 cut CRTC*							_CreateRTC			(LPCSTR Name, u32 size,	D3DFORMAT f);
	//	DX10 cut void							_DeleteRTC			(const CRTC*	RT	);
#ifdef USE_DX11
	SGS*							_CreateGS			(LPCSTR Name);
	void							_DeleteGS			(const SGS*	GS	);
#endif //USE_DX11

#ifdef USE_DX11
	SHS*							_CreateHS			(LPCSTR Name);
	void							_DeleteHS			(const SHS*	HS	);

	SDS*							_CreateDS			(LPCSTR Name);
	void							_DeleteDS			(const SDS*	DS	);

    SCS*							_CreateCS			(LPCSTR Name);
	void							_DeleteCS			(const SCS*	CS	);
#endif //USE_DX11

	SPS*							_CreatePS			(LPCSTR Name);
	void							_DeletePS			(const SPS*	PS	);

	SVS*							_CreateVS			(LPCSTR Name);
	void							_DeleteVS			(const SVS*	VS	);

	SPass*							_CreatePass			(const SPass& proto);
	void							_DeletePass			(const SPass* P	);

	// Shader compiling / optimizing
	SState*							_CreateState		(SimulatorStates& Code);
	void							_DeleteState		(const SState* SB);

	SDeclaration*					_CreateDecl			(D3DVERTEXELEMENT9* dcl);
	void							_DeleteDecl			(const SDeclaration* dcl);

	STextureList*					_CreateTextureList	(STextureList& L);
	void							_DeleteTextureList	(const STextureList* L);

	SMatrixList*					_CreateMatrixList	(SMatrixList& L);
	void							_DeleteMatrixList	(const SMatrixList* L);

	SConstantList*					_CreateConstantList	(SConstantList& L);
	void							_DeleteConstantList	(const SConstantList* L);

	ShaderElement*					_CreateElement		(ShaderElement& L);
	void							_DeleteElement		(const ShaderElement* L);

	Shader*							_cpp_Create			(LPCSTR		s_shader,	LPCSTR s_textures=0,	LPCSTR s_constants=0,	LPCSTR s_matrices=0);
	Shader*							_cpp_Create			(IBlender*	B,			LPCSTR s_shader=0,		LPCSTR s_textures=0,	LPCSTR s_constants=0, LPCSTR s_matrices=0);
	Shader*							_lua_Create			(LPCSTR		s_shader,	LPCSTR s_textures);
	BOOL							_lua_HasShader		(LPCSTR		s_shader);

	CResourceManager						()	: bDeferredLoad(TRUE){	}
	~CResourceManager						()	;

	void			OnDeviceCreate			(IReader* F);
	void			OnDeviceCreate			(LPCSTR name);
	void			OnDeviceDestroy			(BOOL   bKeepTextures);

	void			reset_begin				();
	void			reset_end				();

	// Creation/Destroying
	Shader*			Create					(LPCSTR s_shader=0, LPCSTR s_textures=0,	LPCSTR s_constants=0,	LPCSTR s_matrices=0);
	Shader*			Create					(IBlender*	B,		LPCSTR s_shader=0,		LPCSTR s_textures=0,	LPCSTR s_constants=0, LPCSTR s_matrices=0);
	void			Delete					(const Shader*		S	);
	void			RegisterConstantSetup	(LPCSTR name,		R_constant_setup* s)	{	v_constant_setup.push_back(std::make_pair(shared_str(name),s));	}

	SGeometry*		CreateGeom				(D3DVERTEXELEMENT9* decl, IBuffer* vb, IBuffer* ib);
	SGeometry*		CreateGeom				(u32 FVF				, IBuffer* vb, IBuffer* ib);
	void			DeleteGeom				(const SGeometry* VS		);
	void			DeferredLoad			(BOOL E)					{ bDeferredLoad=E;	}
	void			DeferredUpload			();
	void			DeferredUnload			();
	void			Evict					();
	void			StoreNecessaryTextures	();
	void			DestroyNecessaryTextures();
	void			Dump					(bool bBrief);

private:
#ifdef USE_DX11
	map_DS	m_ds;
	map_HS	m_hs;
	map_CS	m_cs;

	template<typename T>
	T& GetShaderMap();

	template<typename T>
	T* CreateShader(const char* name);

	template<typename T>
	void DestroyShader(const T* sh);

#endif //USE_DX11
};

#endif //ResourceManagerH
