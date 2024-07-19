#include "stdafx.h"
#pragma hdrstop

#include "ResourceManager.h"
#ifndef _EDITOR
#include	"../../xrEngine/Render.h"
#else
	#include "../../xrCore/API/xrAPI.h"
#endif

void	CResourceManager::reset_begin			()
{
	// destroy everything, renderer may use
	::Render->reset_begin		();

	// destroy state-blocks
	for (u32 _it=0; _it<v_states.size(); _it++)
		_RELEASE(v_states[_it]->state);

	// destroy RTs
	for (map_RTIt rt_it=m_rtargets.begin(); rt_it!=m_rtargets.end(); rt_it++)
		rt_it->second->reset_begin();
//	DX10 cut 	for (map_RTCIt rtc_it=m_rtargets_c.begin(); rtc_it!=m_rtargets_c.end(); rtc_it++)
//	DX10 cut 		rtc_it->second->reset_begin();

	// destroy DStreams
 	RCache.old_QuadIB					= RCache.QuadIB;
 	_RELEASE							(RCache.QuadIB);

	RCache.Index.reset_begin	();
	RCache.Vertex.reset_begin	();
}

bool	cmp_rt	(const CRT* A,const CRT* B)		{ return A->_order < B->_order; }
//	DX10 cut bool	cmp_rtc	(const CRTC* A,const CRTC* B)	{ return A->_order < B->_order; }

void	CResourceManager::reset_end				()
{
	// create RDStreams
	RCache.Vertex.reset_end		();
	RCache.Index.reset_end		();
	Evict(); 
	RCache.CreateQuadIB();

	// remark geom's which point to dynamic VB/IB
	{
		for (u32 _it=0; _it<v_geoms.size(); _it++)
		{
			SGeometry*	_G = v_geoms[_it];
			if	(_G->vb == RCache.Vertex.old_pVB) 
				_G->vb = RCache.Vertex.Buffer	();

			// Here we may recover the buffer using one of 
			// RCache's index buffers.
			// Do not remove else.
			if	(_G->ib == RCache.Index.old_pIB) 
			{
				_G->ib = RCache.Index.Buffer	();
			}
			else if	(_G->ib == RCache.old_QuadIB) 
			{
				_G->ib = RCache.QuadIB;
			}
		}
	}

	// create RTs in the same order as them was first created
	{
		// RT
		m_rt_sorter.clear();

		for (auto&[Name, Target] : m_rtargets)
			m_rt_sorter.push_back(Target);

		std::sort(m_rt_sorter.begin(), m_rt_sorter.end(),cmp_rt);

		for (CRT* Target : m_rt_sorter)
			Target->reset_end	();
	}
	// create state-blocks
	{
		for (u32 _it=0; _it<v_states.size(); _it++)
#ifdef USE_DX11
			v_states[_it]->state = ID3DState::Create(v_states[_it]->state_code);
#else //USE_DX11
			v_states[_it]->state = v_states[_it]->state_code.record();
#endif
	}

	// create everything, renderer may use
	::Render->reset_end		();
#ifdef DEBUG
	Dump(true);
#endif
}

template<class C>	void mdump(C c)
{
	if (0==c.size())	return;
	for (auto &[ref, val] : c) {
		Msg	("*        : %3d: %s", (u32)val->dwReference, val->cName.c_str());
	}
}

CResourceManager::~CResourceManager		()
{
	DestroyNecessaryTextures	();
#ifdef DEBUG
	Dump(false);
#endif
}

void CResourceManager::Dump(bool bBrief)
{
	Msg		("* RM_Dump: textures  : %d",		m_textures.size());		if(!bBrief) mdump(m_textures);
	Msg		("* RM_Dump: rtargets  : %d",		m_rtargets.size());		if(!bBrief) mdump(m_rtargets);
//	DX10 cut 	Msg		("* RM_Dump: rtargetsc : %d",		m_rtargets_c.size());	if(!bBrief) mdump(m_rtargets_c);
	Msg		("* RM_Dump: vs        : %d",		m_vs.size());			if(!bBrief) mdump(m_vs);
	Msg		("* RM_Dump: ps        : %d",		m_ps.size());			if(!bBrief) mdump(m_ps);
	Msg		("* RM_Dump: dcl       : %d",		v_declarations.size());
	Msg		("* RM_Dump: states    : %d",		v_states.size());
	Msg		("* RM_Dump: tex_list  : %d",		lst_textures.size());
	Msg		("* RM_Dump: matrices  : %d",		lst_matrices.size());
	Msg		("* RM_Dump: lst_constants: %d",	lst_constants.size());
	Msg		("* RM_Dump: v_passes  : %d",		v_passes.size());
	Msg		("* RM_Dump: v_elements: %d",		v_elements.size());
	Msg		("* RM_Dump: v_shaders : %d",		v_shaders.size());
}
