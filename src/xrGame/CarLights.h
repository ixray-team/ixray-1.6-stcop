#ifndef	CAR_LIGHTS_H
#define CAR_LIGHTS_H
#pragma once
#include "../../Include/xrRender/Kinematics.h"
#include "../xrEngine/render.h"
#include "../xrEngine/xr_efflensflare.h"
class CCarLights;
class CCar;

struct SCarLight
{
	ref_light				light_render;
	ref_glow				glow_render;
	CLensFlare*				eff_LensFlare;
	CLensFlareDescriptor*	m_Fl_desc;
	u16						bone_id;
	bool					visible;
	int						light_type;
	
	Fvector					l_direction;
	Fvector					l_position;
	Fvector					f_position;
	CCarLights*				m_holder;				
			SCarLight		();
			~SCarLight		();
	void	Switch			();
	void	TurnOn			();
	void	TurnOff			();
	bool	isOn			();
	void	Init			(CCarLights* holder);
	void	Destroy			();
	void	Update			();
	void	ParseDefinitions(LPCSTR section);
	static void		_BCL	Set_bone_visible		(CBoneInstance*);
	

};

using LIGHTS_STORAGE = xr_vector<SCarLight*>;
using LIGHTS_I = LIGHTS_STORAGE::iterator;
class CCarLights
{
public:
	enum ELightsType
	{
		flHeadLights = 0,
		flIndoorLights,
		flStopLights,
		flReverseLights,
		flEngineLights,
		flMax
	};
	ELightsType l_type;
	void	ParseDefinitions(LPCSTR S, ELightsType type = flMax);
	void	Init				(CCar* pcar)					;
	void	Update				()								;
	CCar*	PCar				()								{return m_pcar;}
	void	SwitchLights	()								;
	void	TurnOnLights	()								;
	void	TurnOffLights	()								;
	bool    IsAvailable		(){return !m_lights.empty(); };
	bool	IsOn				()					{ for (SCarLight* light : m_lights) if (light && light->isOn()) return true; return false; }
	void	SwitchHeadLights	()					{ SwitchLights(); }
	bool	IsLight				(u16 bone_id)					;
	bool	findLight			(u16 bone_id,SCarLight* &light);
			CCarLights			()								;
			~CCarLights			()								;
protected:
	struct SFindLightPredicate {
		const SCarLight *m_light;

		SFindLightPredicate	(const SCarLight *light) :
		m_light(light)
		{
		}

		bool		operator()					(const SCarLight *light) const
		{
			return light->bone_id==m_light->bone_id;
		}
	};
	LIGHTS_STORAGE	m_lights								;
	CCar*			m_pcar									;
/*
	Ivector2		m_head_near_lights						;
	Ivector2		m_head_far_lights						;
	Ivector2		m_left_turns							;
	Ivector2		m_stops									;
	Ivector2		m_gabarites								;
	Ivector2		m_door_gabarites						;
*/
private:
};






#endif
