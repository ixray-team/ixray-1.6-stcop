#pragma once
#include "MosquitoBald.h"
#include "../xrScripts/script_export_space.h"

class CZoneCampfire : public CMosquitoBald
{
	typedef CMosquitoBald	inherited;
protected:
	xr_shared_ptr<CParticlesObject> m_pEnablingParticles;
	xr_shared_ptr<CParticlesObject> m_pDisabledParticles;
	ref_sound				m_disabled_sound;
	bool					m_turned_on;
	u32						m_turn_time;

		virtual	void		PlayIdleParticles			();
		virtual	void		StopIdleParticles			();
		virtual BOOL		AlwaysTheCrow				();
		virtual	void		UpdateWorkload				(u32 dt);

public:
							CZoneCampfire				();
	virtual					~CZoneCampfire				();
	virtual		void		Load						(LPCSTR section);
	virtual		void		GoEnabledState				();
	virtual		void		GoDisabledState				();

				void		turn_on_script				();
				void		turn_off_script				();
				bool		is_on						();
	virtual		void		shedule_Update				(u32	dt	);
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
