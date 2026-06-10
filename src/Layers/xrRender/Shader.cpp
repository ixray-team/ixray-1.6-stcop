// Shader.cpp: implementation of the CShader class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "Shader.h"
#include "ResourceManager.h"

#include "dxRenderDeviceRender.h"


//
STextureList::~STextureList				()			{	if (DEV) DEV->_DeleteTextureList	(this);			}
SMatrixList::~SMatrixList				()			{	if (DEV) DEV->_DeleteMatrixList		(this);			}
SConstantList::~SConstantList			()			{	if (DEV) DEV->_DeleteConstantList	(this);			}
SPass::~SPass							()			{	if (DEV) DEV->_DeletePass			(this);			}
ShaderElement::~ShaderElement			()			{	if (DEV) DEV->_DeleteElement		(this);			}
SGeometry::~SGeometry					()			{	if (DEV) DEV->DeleteGeom			(this);			}
Shader::~Shader							()			{	if (DEV) DEV->Delete				(this);			}
																							 
//////////////////////////////////////////////////////////////////////////					 
void	resptrcode_shader::create		(const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	_set(DEV->Create		(s_shader,s_textures,s_constants,s_matrices));
}
void	resptrcode_shader::create		(IBlender* B, const char* s_shader, const char* s_textures, const char* s_constants, const char* s_matrices)
{
	_set(DEV->Create		(B,s_shader,s_textures,s_constants,s_matrices));
}

//////////////////////////////////////////////////////////////////////////
void resptrcode_geom::create(u32 FVF, IRHIBuffer* vb, IRHIBuffer* ib)
{
	_set(DEV->CreateGeom(FVF, vb, ib));
}

void resptrcode_geom::create(RHIInputElementDesc* DescList, size_t DeclSize, IRHIBuffer* vb, IRHIBuffer* ib)
{
	_set(DEV->CreateGeom(DescList, DeclSize, vb, ib));
}

void resptrcode_geom::create(D3DVERTEXELEMENT9* decl, IRHIBuffer* vb, IRHIBuffer* ib)
{
	_set(DEV->CreateGeom(decl, vb, ib));
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
bool SPass::equal(const SPass& other)
{
	if (state		!= other.state)		return false;
	if (ps			!= other.ps)			return false;
	if (vs			!= other.vs)			return false;
#ifdef USE_DX11
	if (gs			!= other.gs)			return false;
	if (hs			!= other.hs)			return false;
	if (ds			!= other.ds)			return false;
	if (cs			!= other.cs)			return false;
	if (iPriority != other.iPriority)		return false;
#endif //USE_DX11
	if (constants	!= other.constants)		return false;	// is this nessesary??? (ps+vs already combines)

	if (T != other.T)					return false;
	if (C != other.C)					return false;
#ifdef _EDITOR
	if (M != other.M)					return false;
#endif
	return true;
}

//
ShaderElement::ShaderElement()
{
	flags.iPriority		= 1;
	flags.bStrictB2F	= false;
	flags.bEmissive		= false;
	flags.bScopeMask	= false;
	flags.bDistort		= false;
	flags.bWmark		= false;
	flags.bLandscape	= false;
}

bool ShaderElement::equal	(ShaderElement& S)
{
	if (flags.iPriority		!= S.flags.iPriority)	return false;
	if (flags.bStrictB2F	!= S.flags.bStrictB2F)	return false;
	if (flags.bEmissive		!= S.flags.bEmissive)	return false;
	if (flags.bScopeMask	!= S.flags.bScopeMask)	return false;
	if (flags.bWmark		!= S.flags.bWmark)		return false;
	if (flags.bLandscape	!= S.flags.bLandscape)	return false;
	if (flags.bDistort		!= S.flags.bDistort)	return false;
	if (passes.size() != S.passes.size())			return false;
	for (u32 p=0; p<passes.size(); p++)
		if (passes[p] != S.passes[p])				return false;
	return true;
}

bool ShaderElement::equal	(ShaderElement* S)
{	
	if (nullptr==S && nullptr==this)	return true;
	if (nullptr==S || nullptr==this)	return false;
	return	equal	(*S);	
}

//
bool Shader::equal	(Shader& S)
{
	return
		E[0] && E[0]->equal(&*S.E[0]) &&
		E[1] && E[1]->equal(&*S.E[1]) &&
		E[2] && E[2]->equal(&*S.E[2]) &&
		E[3] && E[3]->equal(&*S.E[3]) &&
		E[4] && E[4]->equal(&*S.E[4]);
}
bool Shader::equal	(Shader* S)
{	return	equal(*S);	}

void STextureList::clear()
{
	iterator it			= begin();
	iterator it_e		= end();
	for(;it!=it_e; ++it)
		(*it).second.destroy();

	erase(begin(),end());
}

void STextureList::clear_not_free()
{
	iterator it			= begin();
	iterator it_e		= end();
	for(;it!=it_e; ++it)
		(*it).second.destroy();

	erase(begin(),end());
}

u32 STextureList::find_texture_stage(const shared_str &TexName) const
{
	u32	dwTextureStage	= 0;

	STextureList::const_iterator	_it		= this->begin	();
	STextureList::const_iterator	_end	= this->end	();
	for (; _it!=_end; _it++)
	{
		const std::pair<u32,ref_texture>&		loader	=	*_it;

		//	Shadowmap texture always uses 0 texture unit
		if (loader.second->cName==TexName)
		{
			//	Assign correct texture
			dwTextureStage	= loader.first;
			break;
		}
	}

	VERIFY(_it!=_end);

	return dwTextureStage;
}

STextureList::STextureList()
{}

void STextureList::_copy(const STextureList& Other)
{
	assign(Other.begin(), Other.end());
}

Shader::Shader()
{}

void Shader::_copy(Shader& Other)
{
	for (u32 i = 0; i < SHADER_ELEMENTS_MAX; i++)
	{
		E[i] = Other.E[i];
	}
}

SConstantList::SConstantList()
{}

void SConstantList::_copy(const SConstantList& Other)
{
	clear();
	for (u32 i = 0; i < Other.size(); i++)
	{
		push_back(Other[i]);
	}
}

void SMatrixList::_copy(const SMatrixList& Other)
{
	clear();
	for (u32 i = 0; i < Other.size(); i++)
	{
		push_back(Other[i]);
	}
}

SMatrixList::SMatrixList()
{}

void ShaderElement::_copy(const ShaderElement& Other)
{
	flags = Other.flags;

	passes.clear();
	for (u32 i = 0; i < Other.passes.size(); i++)
	{
		passes.push_back(Other.passes[i]);
	}
}