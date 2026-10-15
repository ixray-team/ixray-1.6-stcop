#include "stdafx.h"
#ifdef IXR_APPLE_SERIES
#include <luajit.h>
#endif
#include "../../xrEngine/EngineAPI.h"


#include	"../../xrEngine/Render.h"
#include	"ResourceManager.h"
#include	"tss.h"
#include "blenders/Blender.h"
#include "blenders/Blender_Recorder.h"
//	adopt_compiler don't have = operator And it can't have = operator
#pragma warning( push )
#pragma warning( disable : 4512)
#include	"../../xrEngine/ai_script_space.h"
#pragma warning( pop )
#include	"../../xrEngine/ai_script_lua_extension.h"
#include	"luabind/return_reference_to_policy.hpp"

#include	"dxRenderDeviceRender.h"
#include "blenders/BlendOps.h"

using namespace				luabind;

// wrapper
class	adopt_dx10sampler
{
	CBlender_Compile*		m_pC;
	u32						m_SI;	//	Sampler index
public:
	adopt_dx10sampler	(CBlender_Compile*	C, u32 SamplerIndex)	: m_pC(C), m_SI(SamplerIndex)		{ if (u32(-1)==m_SI) m_pC=0;}
	adopt_dx10sampler	(const adopt_dx10sampler&	_C)				: m_pC(_C.m_pC), m_SI(_C.m_SI)	{ if (u32(-1)==m_SI) m_pC=0;}

//	adopt_sampler&			_texture		(const char* texture)		{ if (C) C->i_Texture	(stage,texture);											return *this;	}
//	adopt_sampler&			_projective		(bool _b)				{ if (C) C->i_Projective(stage,_b);													return *this;	}
//	adopt_sampler&			_clamp			()						{ if (C) C->i_Address	(stage,D3DTADDRESS_CLAMP);									return *this;	}
//	adopt_sampler&			_wrap			()						{ if (C) C->i_Address	(stage,D3DTADDRESS_WRAP);									return *this;	}
//	adopt_sampler&			_mirror			()						{ if (C) C->i_Address	(stage,D3DTADDRESS_MIRROR);									return *this;	}
//	adopt_sampler&			_f_anisotropic	()						{ if (C) C->i_Filter	(stage,D3DTEXF_ANISOTROPIC,D3DTEXF_LINEAR,D3DTEXF_ANISOTROPIC);	return *this;	}
//	adopt_sampler&			_f_trilinear	()						{ if (C) C->i_Filter	(stage,D3DTEXF_LINEAR,D3DTEXF_LINEAR,D3DTEXF_LINEAR);		return *this;	}
//	adopt_sampler&			_f_bilinear		()						{ if (C) C->i_Filter	(stage,D3DTEXF_LINEAR,D3DTEXF_POINT, D3DTEXF_LINEAR);		return *this;	}
//	adopt_sampler&			_f_linear		()						{ if (C) C->i_Filter	(stage,D3DTEXF_LINEAR,D3DTEXF_NONE,  D3DTEXF_LINEAR);		return *this;	}
//	adopt_sampler&			_f_none			()						{ if (C) C->i_Filter	(stage,D3DTEXF_POINT, D3DTEXF_NONE,  D3DTEXF_POINT);		return *this;	}
//	adopt_sampler&			_fmin_none		()						{ if (C) C->i_Filter_Min(stage,D3DTEXF_NONE);										return *this;	}
//	adopt_sampler&			_fmin_point		()						{ if (C) C->i_Filter_Min(stage,D3DTEXF_POINT);										return *this;	}
//	adopt_sampler&			_fmin_linear	()						{ if (C) C->i_Filter_Min(stage,D3DTEXF_LINEAR);										return *this;	}
//	adopt_sampler&			_fmin_aniso		()						{ if (C) C->i_Filter_Min(stage,D3DTEXF_ANISOTROPIC);								return *this;	}
//	adopt_sampler&			_fmip_none		()						{ if (C) C->i_Filter_Mip(stage,D3DTEXF_NONE);										return *this;	}
//	adopt_sampler&			_fmip_point		()						{ if (C) C->i_Filter_Mip(stage,D3DTEXF_POINT);										return *this;	}
//	adopt_sampler&			_fmip_linear	()						{ if (C) C->i_Filter_Mip(stage,D3DTEXF_LINEAR);										return *this;	}
//	adopt_sampler&			_fmag_none		()						{ if (C) C->i_Filter_Mag(stage,D3DTEXF_NONE);										return *this;	}
//	adopt_sampler&			_fmag_point		()						{ if (C) C->i_Filter_Mag(stage,D3DTEXF_POINT);										return *this;	}
//	adopt_sampler&			_fmag_linear	()						{ if (C) C->i_Filter_Mag(stage,D3DTEXF_LINEAR);										return *this;	}
};
/*
class	adopt_dx10texture
{
	CBlender_Compile*		m_pC;
	u32						m_TI;	//	Sampler index
public:
	adopt_dx10texture	(CBlender_Compile*	C, u32 TextureIndex)	: m_pC(C), m_TI(TextureIndex)		{ if (u32(-1)==m_TI) m_pC=0;}
	adopt_dx10texture	(const adopt_dx10texture&	_C)				: m_pC(_C.m_pC), m_TI(_C.m_TI)	{ if (u32(-1)==m_TI) m_pC=0;}
};
*/

#pragma warning( push )
#pragma warning( disable : 4512)
// wrapper																																					
class	adopt_compiler
{
	CBlender_Compile*		C;
	bool					&m_bFirstPass;
	BlendOpList*			Record = nullptr;

	adopt_compiler&			Emit(const BlendOp& Op)
	{
		if (Record)
			Record->push_back(Op);
		else
			BlendOps::Apply(*C, Op, m_bFirstPass, BlendOps::TextureArgs());
		return *this;
	}

	static BlendOp			MakeOp(EBlendOp Type, std::initializer_list<u32> Values = {}, std::initializer_list<const char*> Strings = {})
	{
		BlendOp Op;
		Op.Type = Type;
		u32 Index = 0;
		for (u32 Value : Values)
			Op.Values[Index++] = Value;
		Index = 0;
		for (const char* String : Strings)
			Op.Strings[Index++] = String ? String : "";
		return Op;
	}
public:
	adopt_compiler			(CBlender_Compile*	_C, bool& bFirstPass, BlendOpList* RecordTo = nullptr)	: C(_C), m_bFirstPass(bFirstPass), Record(RecordTo)		{ m_bFirstPass = true;}
	adopt_compiler			(const adopt_compiler&	_C)	: C(_C.C), m_bFirstPass(_C.m_bFirstPass), Record(_C.Record){ }

	adopt_compiler&			_options		(int	P,		bool	S)				{	return Emit(MakeOp(EBlendOp::Sorting, { u32(P), u32(S) }));		}
	adopt_compiler&			_o_emissive		(bool	E)								{	return Emit(MakeOp(EBlendOp::Emissive, { u32(E) }));			}
	adopt_compiler&			_o_scope		(bool	E)								{	return Emit(MakeOp(EBlendOp::Scope, { u32(E) }));				}
	adopt_compiler&			_o_distort		(bool	E)								{	return Emit(MakeOp(EBlendOp::Distort, { u32(E) }));				}
	adopt_compiler&			_o_wmark		(bool	E)								{	return Emit(MakeOp(EBlendOp::Wmark, { u32(E) }));				}
	adopt_compiler&			_pass			(const char*	vs,		const char* ps)				{	return Emit(MakeOp(EBlendOp::Pass, {}, { vs, "", ps }));	}
	adopt_compiler&			_passgs			(const char*	vs,		const char*	gs,		const char* ps){	return Emit(MakeOp(EBlendOp::Pass, {}, { vs, gs, ps }));	}
	adopt_compiler&			_fog			(bool	_fog)							{	return Emit(MakeOp(EBlendOp::Fog, { u32(_fog) }));				}
	adopt_compiler&			_ZB				(bool	_test,	bool _write)			{	return Emit(MakeOp(EBlendOp::ZB, { u32(_test), u32(_write) }));	}
	adopt_compiler&			_blend			(bool	_blend, u32 abSRC, u32 abDST)	{	return Emit(MakeOp(EBlendOp::Blend, { u32(_blend), abSRC, abDST }));	}
	adopt_compiler&			_iblend			(int idx, bool	_blend, u32 abSRC, u32 abDST)	{	return Emit(MakeOp(EBlendOp::IBlend, { u32(idx), u32(_blend), abSRC, abDST }));	}
	adopt_compiler&			_aref			(bool	_aref,  u32 aref)				{	return Emit(MakeOp(EBlendOp::Aref, { u32(_aref), aref }));		}
	adopt_compiler&			_dx10texture	(const char* _resname, const char* _texname)		{	return Emit(MakeOp(EBlendOp::Texture, {}, { _resname, _texname }));	}
	adopt_dx10sampler		_dx10sampler	(const char* _name)							{	Emit(MakeOp(EBlendOp::Sampler, {}, { _name }));	return	adopt_dx10sampler(C, Record ? u32(-1) : 0);	}

	adopt_compiler& ScopePostprocess(bool Enabled)
	{
		return Emit(MakeOp(EBlendOp::ScopePostprocess, { u32(Enabled) }));
	}

	//	DX10 specific
	adopt_compiler&			_dx10color_write_enable (bool cR, bool cG, bool cB, bool cA)		{	return Emit(MakeOp(EBlendOp::ColorWrite, { u32(cR), u32(cG), u32(cB), u32(cA) }));	}
	adopt_compiler&			_dx10Stencil	(bool Enable, u32 Func, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail) {	return Emit(MakeOp(EBlendOp::Stencil, { u32(Enable), Func, Mask, WriteMask, Fail, Pass, ZFail }));	}
	adopt_compiler&			_dx10StencilRef	(u32 Ref) {	return Emit(MakeOp(EBlendOp::StencilRef, { Ref }));	}
	adopt_compiler&			_dx10ZFunc		(u32 Func)								{	return Emit(MakeOp(EBlendOp::ZFunc, { Func }));	}
};
#pragma warning( pop )

class	adopt_blend
{
public:
};

class	adopt_cmp_func
{
public:
};

class	adopt_stencil_op
{
public:
};


void LuaLog(const char* caMessage)
{
//	Lua::LuaOut	(Lua::eLuaMessageTypeMessage,"%s",caMessage);
	Log(caMessage);
}

void LuaError(lua_State* L)
{
	Debug.fatal(DEBUG_INFO,"LUA error: %s",lua_tostring(L,-1));
}

void LuaAddShaderOption(const char* _name, const char* _def) 
{ 
	RImplementation.addShaderOption(_name, _def);
}

bool LuaGetShaderOption(const char* _name)
{
#if !defined(_EDITOR)
	if (!xr_strcmp(_name, "USE_R1_STATIC_LIGHTING"))
		return LightingModeIsStaticRender();
#endif

	return EngineExternal().ShadersOptions.contains(xr_string(_name));
}

void LuaClearAllShaderOptions()
{ 
	RImplementation.clearAllShaderOptions(); 
}

// export
void	CResourceManager::LS_Load			()
{
	LSVM			= luaL_newstate();
	if (!LSVM)		{
		Msg			("! ERROR : Cannot initialize LUA VM!");
		return;
	}

	// initialize lua standard library functions 
	luaopen_base	(LSVM); 
	luaopen_table	(LSVM);
	luaopen_string	(LSVM);
	luaopen_math	(LSVM);
	luaopen_jit		(LSVM);
#ifdef IXR_APPLE_SERIES
	// HACK: this LuaJIT build has no MAP_JIT support, and macOS denies plain RWX mmap,
	// so the first compiled trace panics with MCODEAL. Interpreter-only until LuaJIT
	// gets the macOS MAP_JIT port (same as script_storage.cpp). Must run AFTER luaopen_jit.
	if (!luaJIT_setmode(LSVM, 0, LUAJIT_MODE_ENGINE | LUAJIT_MODE_OFF))
		Msg("! LuaJIT: failed to disable JIT for the shader pipeline VM");
#endif

	luabind::open						(LSVM);
#ifdef LUABIND_NO_EXCEPTIONS
	if (0==luabind::get_error_callback())
		luabind::set_error_callback		(LuaError);
#endif

	module			(LSVM)
	[
		class_<adopt_dx10sampler>("_dx10sampler")
		,

		class_<adopt_compiler>("_compiler")
			.def(								constructor<const adopt_compiler&>())
			.def("begin",						&adopt_compiler::_pass			,return_reference_to<1>())
			.def("begin",						&adopt_compiler::_passgs		,return_reference_to<1>())
			.def("sorting",						&adopt_compiler::_options		,return_reference_to<1>())
			.def("emissive",					&adopt_compiler::_o_emissive	,return_reference_to<1>())
			.def("scope",						&adopt_compiler::_o_scope		,return_reference_to<1>())
			.def("scope_postprocess", &adopt_compiler::ScopePostprocess, return_reference_to<1>())
			.def("distort",						&adopt_compiler::_o_distort		,return_reference_to<1>())
			.def("wmark",						&adopt_compiler::_o_wmark		,return_reference_to<1>())
			.def("fog",							&adopt_compiler::_fog			,return_reference_to<1>())
			.def("zb",							&adopt_compiler::_ZB			,return_reference_to<1>())
			.def("blend",						&adopt_compiler::_blend			,return_reference_to<1>())
			.def("iblend",						&adopt_compiler::_iblend		,return_reference_to<1>())
			.def("aref",						&adopt_compiler::_aref			,return_reference_to<1>())

			//	For compatibility only
			.def("dx10color_write_enable",		&adopt_compiler::_dx10color_write_enable,return_reference_to<1>())
			.def("color_write_enable",			&adopt_compiler::_dx10color_write_enable,return_reference_to<1>())
			.def("dx10texture",					&adopt_compiler::_dx10texture	,return_reference_to<1>())
			.def("dx10stencil",					&adopt_compiler::_dx10Stencil	,return_reference_to<1>())
			.def("dx10stencil_ref",				&adopt_compiler::_dx10StencilRef,return_reference_to<1>())
			.def("dx10zfunc",					&adopt_compiler::_dx10ZFunc		,return_reference_to<1>())			

			.def("dx10sampler",					&adopt_compiler::_dx10sampler		),	// returns sampler-object

		class_<adopt_blend>("blend")
			.enum_("blend")
			[
				value("zero",					int(D3DBLEND_ZERO)),
				value("one",					int(D3DBLEND_ONE)),
				value("srccolor",				int(D3DBLEND_SRCCOLOR)),
				value("invsrccolor",			int(D3DBLEND_INVSRCCOLOR)),
				value("srcalpha",				int(D3DBLEND_SRCALPHA)),
				value("invsrcalpha",			int(D3DBLEND_INVSRCALPHA)),
				value("destalpha",				int(D3DBLEND_DESTALPHA)),
				value("invdestalpha",			int(D3DBLEND_INVDESTALPHA)),
				value("destcolor",				int(D3DBLEND_DESTCOLOR)),
				value("invdestcolor",			int(D3DBLEND_INVDESTCOLOR)),
				value("srcalphasat",			int(D3DBLEND_SRCALPHASAT))
			],

		class_<adopt_cmp_func>("cmp_func")
			.enum_("cmp_func")
			[
				value("never",					int(D3DCMP_NEVER)),
				value("less",					int(D3DCMP_LESS)),
				value("equal",					int(D3DCMP_EQUAL)),
				value("lessequal",				int(D3DCMP_LESSEQUAL)),
				value("greater",				int(D3DCMP_GREATER)),
				value("notequal",				int(D3DCMP_NOTEQUAL)),
				value("greaterequal",			int(D3DCMP_GREATEREQUAL)),
				value("always",					int(D3DCMP_ALWAYS))
			],

		class_<adopt_stencil_op>("stencil_op")
			.enum_("stencil_op")
			[
				value("keep",					int(D3DSTENCILOP_KEEP)),
				value("zero",					int(D3DSTENCILOP_ZERO)),
				value("replace",				int(D3DSTENCILOP_REPLACE)),
				value("incrsat",				int(D3DSTENCILOP_INCRSAT)),
				value("decrsat",				int(D3DSTENCILOP_DECRSAT)),
				value("invert",					int(D3DSTENCILOP_INVERT)),
				value("incr",					int(D3DSTENCILOP_INCR)),
				value("decr",					int(D3DSTENCILOP_DECR))
			],

		def("log", LuaLog),

		def("AddShaderOption", LuaAddShaderOption),
		def("GetShaderOption", LuaGetShaderOption),
		def("ClearAllShaderOptions", LuaClearAllShaderOptions)
	];

	// load shaders
	xr_vector<char*>*	folder			= FS.file_list_open	(_game_shaders_,::Render->getShaderPath(),FS_ListFiles|FS_RootOnly);
	VERIFY								(folder);
	for (u32 it=0; it<folder->size(); it++)	{
		string_path						namesp,fn;
		xr_strcpy							(namesp,(*folder)[it]);
		if	(0==strext(namesp) || 0!=xr_strcmp(strext(namesp),".lua"))	continue;
		*strext	(namesp)=0;
		if		(0==namesp[0])			xr_strcpy	(namesp,"_G");
		xr_strconcat(fn,::Render->getShaderPath(),(*folder)[it]);
		FS.update_path					(fn,_game_shaders_,fn);
		try {
			Script::bfLoadFileIntoNamespace	(LSVM,fn,namesp,true);
		} catch (...)
		{
			Log(lua_tostring(LSVM,-1));
		}
	}
	FS.file_list_close			(folder);
}

void	CResourceManager::LS_Unload			()
{
	lua_close	(LSVM);
	LSVM		= nullptr;
}

bool	CResourceManager::_lua_HasShader	(const char* s_shader)
{
	string256	undercorated;
	for (int i=0, l=xr_strlen(s_shader)+1; i<l; i++)
		undercorated[i]=('\\'==s_shader[i])?'_':s_shader[i];

	return	Script::bfIsObjectPresent(LSVM,undercorated,"normal",LUA_TFUNCTION)		||
			Script::bfIsObjectPresent(LSVM,undercorated,"l_special",LUA_TFUNCTION)
		|| (Device.IsEditorMode() && Script::bfIsObjectPresent(LSVM, undercorated, "editor", LUA_TFUNCTION))
	;
}

bool CResourceManager::_lua_HasStatic(const char* s_shader)
{
	string256 undercorated;
	for (int i = 0, l = xr_strlen(s_shader) + 1; i < l; i++)
		undercorated[i] = ('\\' == s_shader[i]) ? '_' : s_shader[i];

	return Script::bfIsObjectPresent(LSVM, undercorated, "r1_static", LUA_TBOOLEAN);
}

Shader* CResourceManager::_lua_Create(const char* d_shader, const char* s_textures)
{
	CBlender_Compile C;
	Shader S;

	// undecorate
	string256	undercorated;
	for (int i=0, l=xr_strlen(d_shader)+1; i<l; i++)
		undercorated[i]=('\\'==d_shader[i])?'_':d_shader[i];
	const char*		s_shader = undercorated;

	// Access to template
	C.BT				= nullptr;
	C.bEditor			= false;
	C.bDetail			= false;

	// Prepare
	_ParseList			(C.L_textures,	s_textures	);
	C.detail_texture	= nullptr;
	C.detail_scaler		= nullptr;

	// Compile element	(LOD0 - HQ)
	if (Script::bfIsObjectPresent(LSVM,s_shader,"normal_hq",LUA_TFUNCTION))
	{
		// Analyze possibility to detail this shader
		C.iElement			= 0;
		C.bDetail			= DEV->m_textures_description.GetDetailTexture(C.L_textures[0],C.detail_texture,C.detail_scaler);

		if (C.bDetail)		S.E[0]	= C._lua_Compile(s_shader,"normal_hq");
		else				S.E[0]	= C._lua_Compile(s_shader,"normal");
	} 
	else
	{
		if (Script::bfIsObjectPresent(LSVM,s_shader,"normal",LUA_TFUNCTION))
		{
			C.iElement			= 0;
			C.bDetail			= DEV->m_textures_description.GetDetailTexture(C.L_textures[0],C.detail_texture,C.detail_scaler);
			S.E[0]				= C._lua_Compile(s_shader,"normal");
		}
	}

	// Compile the UI variant with the same detail lookup as the normal pass.
	if (Script::bfIsObjectPresent(LSVM, s_shader, "normal_ui", LUA_TFUNCTION))
	{
		C.iElement = SE_R2_UI;
		C.bDetail = DEV->m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
		S.UIElement = C._lua_Compile(s_shader, "normal_ui");
	}

	// Compile element (HUD)
	if (!LightingModeIsStaticRender() && Script::bfIsObjectPresent(LSVM, s_shader, "normal_hud", LUA_TFUNCTION))
	{
		C.iElement = 5;
		C.bDetail = dxRenderDeviceRender::Instance().Resources->m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
		S.E[5] = C._lua_Compile(s_shader, "normal_hud");
	}

	// Compile element (LOD1)
	if (Script::bfIsObjectPresent(LSVM,s_shader,"normal",LUA_TFUNCTION))
	{
		C.iElement			= 1;
		C.bDetail			= DEV->m_textures_description.GetDetailTexture(C.L_textures[0],C.detail_texture,C.detail_scaler);
		S.E[1]				= C._lua_Compile(s_shader,"normal");
	}

	// Compile element
	if (Script::bfIsObjectPresent(LSVM,s_shader,"l_point",LUA_TFUNCTION))
	{
		C.iElement			= 2;
		C.bDetail			= false;
		S.E[2]				= C._lua_Compile(s_shader,"l_point");;
	}

	// Compile element
	if (Script::bfIsObjectPresent(LSVM,s_shader,"l_spot",LUA_TFUNCTION))
	{
		C.iElement			= 3;
		C.bDetail			= false;
		S.E[3]				= C._lua_Compile(s_shader,"l_spot");;
	}

	// Compile element
	if (Script::bfIsObjectPresent(LSVM,s_shader,"l_special",LUA_TFUNCTION))
	{
		C.iElement			= 4;
		C.bDetail			= false;
		S.E[4]				= C._lua_Compile(s_shader,"l_special");
	}

	// Search equal in shaders array
	for (u32 it=0; it<v_shaders.size(); it++)
	{
		if (S.equal(v_shaders[it]))	
		{
			return v_shaders[it];
		}
	}

	// Create _new_ entry
	Shader* N = new Shader();
	N->_copy(S);

	N->dwFlags |= xr_resource_flagged::RF_REGISTERED;
	v_shaders.push_back(N);

	return N;
}

ShaderElement*		CBlender_Compile::_lua_Compile	(const char* namesp, const char* name)
{
	ShaderElement		E;
	SH =				&E;
	RS.Invalidate		();

	// Compile
	const char*				t_0		= *L_textures[0]			? *L_textures[0] : "null";
	const char*				t_1		= (L_textures.size() > 1)	? *L_textures[1] : "null";
	const char*				t_d		= (L_textures.size() > 2 && L_textures[2].size() > 0)
		? *L_textures[2]
		: (detail_texture ? detail_texture : "null");
	lua_State*			LSVM	= DEV->LSVM;
	object				shader	= get_globals(LSVM)[namesp];
	functor<void>		element	= object_cast<functor<void> >(shader[name]);
	bool				bFirstPass = false;
	adopt_compiler		ac		= adopt_compiler(this, bFirstPass);
	element						(ac,t_0,t_1,t_d);
	r_End				();
	ShaderElement*	_r	= DEV->_CreateElement(E);
	return			_r;
}
