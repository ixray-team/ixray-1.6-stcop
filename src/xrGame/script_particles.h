////////////////////////////////////////////////////////////////////////////
//	Module 		: script_particles.h
//	Created 	: 27.07.2004
//  Modified 	: 27.07.2004
//	Author		: Alexander Maximchuk
//	Description : XRay Script particles class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_export_space.h"
#include "ParticlesObject.h"

// refs
class CObjectAnimator;
class CScriptParticles;

class CScriptParticlesCustom: 
	public CParticlesObject
{
public:
	CObjectAnimator*			m_animator;
	virtual						~CScriptParticlesCustom();
	CScriptParticles* m_owner;
								CScriptParticlesCustom(CScriptParticles* owner, const char* caParticlesName);
	virtual void				Update		(u32 dt, CFrustum& viewbase);

	void						LoadPath			(const char* caPathName);
	void						StartPath			(bool looped);
	void						StopPath			();
	void						PausePath			(bool val);
	void						remove_owner		();
};

class CScriptParticles
{
public:
	xr_shared_ptr<CScriptParticlesCustom> m_particles;
								CScriptParticles	(const char* caParticlesName);
	virtual						~CScriptParticles	();

	void						Play				();
	void						PlayAtPos			(const Fvector &pos);
	void						Stop				();
	void						StopDeffered		();

	bool						IsPlaying			() const;
	bool						IsLooped			() const;

	void						MoveTo				(const Fvector &pos, const Fvector& vel);
	void						SetXFORM_DIR_X			(const Fvector& pos, const Fvector& dir, const Fvector& vel);
	void						SetXFORM_DIR_Y			(const Fvector& pos, const Fvector& dir, const Fvector& vel);
	void						SetXFORM_DIR_Z			(const Fvector& pos, const Fvector& dir, const Fvector& vel);
	void						LoadPath			(const char* caPathName);
	void						StartPath			(bool looped);
	void						StopPath			();
	void						PausePath			(bool val);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};