#pragma once

class CLAItem;
class CParticlesObject;

#include "hud_item_object.h"

class CFlare final : public CHudItemObject
{
	using inherited = CHudItemObject;

	enum FlareStates : u8
	{
		eFlareHidden,
		eFlareShowing,
		eFlareIdle,
		eFlareHiding,
		eFlareDropping
	};

	CLAItem*					light_lanim;
	ref_light					light_render;
	xr_shared_ptr<CParticlesObject> m_pFlareParticles;
	float						m_work_time_sec;
	void						SwitchOn						();
	void						SwitchOff						();
	void						FirePoint						(Fvector&);
	void						ParticlesMatrix					(Fmatrix&);
public:

	CFlare() = default;
	virtual ~CFlare() override = default;

	virtual void				UpdateCL						() override;
	virtual void				Load							(const char* section) override;
	virtual bool				net_Spawn						(CSE_Abstract* DC) override;
	virtual void				net_Destroy						() override;

	virtual void				OnStateSwitch					(u8 S) override;
	virtual void				OnAnimationEnd					(u8 state) override;

	virtual void				UpdateXForm						() override;

	void						ActivateFlare					();
	void						DropFlare						();
	bool						IsFlareActive					();
};