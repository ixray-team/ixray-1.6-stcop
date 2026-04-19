#pragma once

#include "../Layers/xrRender/DetailModel.h"

// refs
class CEditableObject;

class ECORE_API EDetail: public CDetail{
	friend class EDetailManager;
	friend class CDetailManager;
	friend class UIDOShuffle;

	struct EVertexIn: public fvfVertexIn
	{
						EVertexIn	(const Fvector& _P, float _u, float _v){P.set(_P); u=_u; v=_v;};
		IC void			set			(EVertexIn& src){P.set(src.P); u=src.u; v=src.v;};
		IC void			set			(const Fvector& _P, float _u, float _v){P.set(_P); u=_u; v=_v;};
		IC bool			similar		(EVertexIn& V)
		{
			if (!fsimilar	(u,V.u,EPS_L))	return false;
			if (!fsimilar	(v,V.v,EPS_L))	return false;
			if (!P.similar	(V.P,EPS_L))	return false;
			return true;
		}
		void			remapUV		(const fvfVertexIn& src, const Fvector2& offs, const Fvector2& scale, bool bRotate);
	};
	
	bool                m_bLoadFromLibrary;
	float 				m_fDensityFactor;

public:
	// references
	xr_string			m_sRefs;
	CEditableObject*	m_pRefs;

	u16 				_AddVert		(const Fvector& p, float u, float v);
public:
//    bool				m_bMarkDel;
public:
						EDetail			(bool lib = true);
	virtual             ~EDetail		();

	bool				Load            (IReader&);
	void				Save            (IWriter&);
	bool   				LoadLTX			(CInifile& ini, const char* sect_name);
	void   				SaveLTX			(CInifile& ini, const char* sect_name);
	void				Export			(IWriter&, const char* tex_name, const Fvector2& offs, const Fvector2& scale, bool rot);
	void				Export			(const char* name);

	bool				Update			(const char* name);
	virtual void		Unload			();

	const char*				GetName			();
	const char*				GetTextureName	();
	void				OnDeviceCreate	();
	void				OnDeviceDestroy	();
	void				DefferedLoad	();
};

using DOVec = xr_vector<EDetail*>;
using DOIt = DOVec::iterator;

