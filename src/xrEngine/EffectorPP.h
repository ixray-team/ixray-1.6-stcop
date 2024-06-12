#pragma once

#include "CameraDefs.h"
struct SPPInfo;

// постпроцесс
class ENGINE_API CEffectorPP:
	public SBaseEffector
{
	EEffectorPPType		eType;
	bool				bFreeOnRemove;

public:
	shared_str			m_Name;
	float				fLifeTime;
	bool				bOverlap;

public:
						CEffectorPP		(EEffectorPPType type, f32 lifeTime, bool free_on_remove=true);
						CEffectorPP		():bFreeOnRemove(true),fLifeTime(0.0f),bOverlap(true){};
	virtual				~CEffectorPP	();

	virtual	BOOL		Process			(SPPInfo &PPInfo);
	virtual	BOOL		Valid			()							{return fLifeTime>0.0f;}
	IC EEffectorPPType	Type			()	const					{return eType;}
	IC bool				FreeOnRemove	()	const					{return bFreeOnRemove;}
	IC void				SetType			(EEffectorPPType t)			{eType=t;}
	virtual void		Stop            (float speed)				{fLifeTime=0.0f;};
	virtual float		GetRealLifeTime	() const { return fLifeTime; }
	virtual float		GetLifeTimeRemaining() const { return fLifeTime; }
	virtual bool		IsCyclic() const { return false; }
};
