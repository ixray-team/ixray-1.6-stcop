#include "stdafx.h"
#include "CarLights.h"
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "../Include/xrRender/Kinematics.h"
#include "PHWorld.h"
#include "HUDManager.h"


SCarLight::SCarLight()
{
	light_render	= NULL;
	glow_render		= NULL;
	bone_id			= BI_NONE;
	m_holder		= NULL;
	light_type		= 0;
	l_direction		= Fvector().set(0.f, 0.f, 0.f);
	l_position		= Fvector().set(0.f, 0.f, 0.f);
	// f_position		= Fvector().set(0.f, 0.f, 0.f);
	// eff_LensFlare	= NULL;
	// m_Fl_desc		= NULL;
}

SCarLight::~SCarLight()
{
	light_render.destroy	()	;
	glow_render.destroy		()	;
	bone_id			=	BI_NONE	;
	// xr_delete(eff_LensFlare);
	// xr_delete(m_Fl_desc);
}

void SCarLight::Destroy()
{
	light_render.destroy();
	glow_render.destroy();
	bone_id = BI_NONE;
	// if (eff_LensFlare) {}
}

void SCarLight::Init(CCarLights* holder)
{
	m_holder=holder;
}
#include "Actor.h"
void SCarLight::ParseDefinitions(LPCSTR section)
{
	IKinematics*			pKinematics=smart_cast<IKinematics*>(m_holder->PCar()->Visual());
	CInifile* ini			= pKinematics->LL_UserData();
	Fcolor					clr;
	clr.set					(READ_IF_EXISTS(ini,r_fcolor,section,"color",Fcolor().set(1.f,1.f,1.f,1.f)));
	BOOL default_visible = READ_IF_EXISTS(ini,r_bool,section,"default_visible",FALSE);
	/*shared_str light_name = "";
	light_name.printf("car light %s %s", m_holder->PCar()->cName().c_str(), (bone_id != BI_NONE && bone_id < pKinematics->LL_BoneCount()) ? pKinematics->LL_BoneName_dbg(bone_id) : "");*/
	light_type				= READ_IF_EXISTS(ini,r_u8,section,"type",2);
	light_render			= ::Render->light_create();
	light_render->set_type(IRender_Light::LT(light_type));
	light_render->set_shadow(READ_IF_EXISTS(ini,r_bool,section,"shadow",TRUE));
	light_render->set_range	(READ_IF_EXISTS(ini,r_float,section,"range",40.f));
    light_render->set_volumetric_distance(READ_IF_EXISTS(ini,r_float,section,"volumetric_distance",1.0f));
    light_render->set_volumetric_intensity(READ_IF_EXISTS(ini,r_float,section,"volumetric_intensity",1.0f));
    light_render->set_volumetric_quality(READ_IF_EXISTS(ini,r_float,section,"volumetric_quality",1.0f));
    light_render->set_volumetric(READ_IF_EXISTS(ini,r_bool,section,"volumetric",FALSE));
	light_render->set_color	(clr);
	light_render->set_cone	(deg2rad(READ_IF_EXISTS(ini,r_float,section,"cone_angle",50.f)));
	light_render->set_texture(READ_IF_EXISTS(ini,r_string,section,"spot_texture",""));
	light_render->set_active(default_visible);

	if(!(m_holder->l_type == CCarLights::ELightsType::flHeadLights && light_type == 2))
		light_render->set_decor_object(m_holder->PCar()->dcast_CObject(), 0);
	
	glow_render				= ::Render->glow_create();
	glow_render->set_texture(READ_IF_EXISTS(ini,r_string,section,"glow_texture",""));
	glow_render->set_color	(clr);
	glow_render->set_radius	(READ_IF_EXISTS(ini,r_float,section,"glow_radius",1.0f));
	glow_render->set_active(default_visible);
	l_direction = READ_IF_EXISTS(ini,r_fvector3,section,"direction", l_direction);
	l_position = READ_IF_EXISTS(ini,r_fvector3,section,"position", l_position);
	LPCSTR bone_name = READ_IF_EXISTS(ini,r_string,section,"bone",pKinematics->LL_BoneName_dbg(pKinematics->LL_GetBoneRoot()));
	bone_id	= pKinematics->LL_BoneID(bone_name);
	if(bone_id != BI_NONE && bone_id < pKinematics->LL_BoneCount())
		pKinematics->LL_SetBoneVisible(bone_id,default_visible,TRUE);	
	else
		Msg("! CarLight bone %s not found in model!", bone_name);
}

void SCarLight::Switch()
{
	VERIFY(!physics_world()->Processing());
	if(isOn())TurnOff();
	else	  TurnOn();
}
void SCarLight::TurnOn()
{
	VERIFY(!physics_world()->Processing());
	if(isOn()) return;
	IKinematics* K=PKinematics(m_holder->PCar()->Visual());
	if(bone_id != BI_NONE && bone_id < K->LL_BoneCount())
	{
		K->LL_SetBoneVisible(bone_id,TRUE,TRUE);
	}
	glow_render ->set_active(true);
	light_render->set_active(true);
	// if(eff_LensFlare) {}

	m_holder->PCar()->calc_bones_at_once = true;
}
void SCarLight::TurnOff()
{
	VERIFY(!physics_world()->Processing());
	if(!isOn()) return;
 	glow_render ->set_active(false);
	light_render->set_active(false);
	IKinematics* K=PKinematics(m_holder->PCar()->Visual());
	if(bone_id != BI_NONE && bone_id < K->LL_BoneCount())
	{
		K->LL_SetBoneVisible(bone_id,FALSE,TRUE);
	}
	// if(eff_LensFlare) {}
}

bool SCarLight::isOn()
{
	VERIFY(!physics_world()->Processing());
	VERIFY(light_render->get_active()==glow_render->get_active());
	return light_render->get_active();
}

void SCarLight::Update()
{
	//TurnOn();
	VERIFY(!physics_world()->Processing());
	if(!isOn()) return;
	CCar* pcar=m_holder->PCar();
	IKinematics* K=PKinematics(pcar->Visual());
	if(bone_id == BI_NONE || bone_id >= K->LL_BoneCount())
		return;
	CBoneInstance& BI = K->LL_GetBoneInstance(bone_id);

	Fmatrix M, BI_M, L_M;
	BI_M = BI.mTransform;

	L_M.setXYZ(l_direction);
	L_M.c = l_position;

	BI_M.mulA_43(L_M);

	M.mul(pcar->XFORM(),BI_M);
	light_render->set_position	(M.c);
	light_render->set_rotation	(M.k,M.i);
	glow_render->set_direction(M.k);
	glow_render->set_position	(M.c);
}


CCarLights::CCarLights()
{
	m_pcar=NULL;
}

void CCarLights::Init(CCar* pcar)
{
	m_pcar=pcar;
	LIGHTS_I i = m_lights.begin(), e = m_lights.end();
	for (; i != e; ++i) (*i)->Destroy();
	m_lights.clear();
}

void CCarLights::ParseDefinitions(LPCSTR S, ELightsType type)
{
	CInifile* ini = smart_cast<IKinematics*>(m_pcar->Visual())->LL_UserData();
	if (!ini->section_exist("lights")) return;
	if (!ini->line_exist("lights", S)) return;
	l_type = type;
	LPCSTR S_ = ini->r_string("lights", S);
	string64					S1;
	int count = _GetItemCount(S_);
	for (int i = 0; i < count; ++i)
	{
		_GetItem(S_, i, S1);
		m_lights.push_back(new SCarLight());
		m_lights.back()->Init(this);
		m_lights.back()->ParseDefinitions(S1);
	}
	//TurnOnLights();
}

void CCarLights::Update()
{
	VERIFY(!physics_world()->Processing());
	LIGHTS_I i = m_lights.begin(), e = m_lights.end();
	for (; i != e; ++i) (*i)->Update();
}

void CCarLights::SwitchLights()
{
	VERIFY(!physics_world()->Processing());
	LIGHTS_I i =m_lights.begin(),e=m_lights.end();
	for(;i!=e;++i) (*i)->Switch();

	if (l_type == flHeadLights)
	{
		bool hideaways_up = false;
		LIGHTS_I i = m_lights.begin();
		for (; i != m_lights.end(); ++i)
			if ((*i)->isOn())
				hideaways_up = true;

		auto b_b = m_pcar->headlight_hideaways.begin();
		for (; b_b != m_pcar->headlight_hideaways.end(); ++b_b)
			b_b->Switch(hideaways_up);
	}
}

void CCarLights::TurnOnLights()
{
	VERIFY(!physics_world()->Processing());
	LIGHTS_I i =m_lights.begin(),e=m_lights.end();
	for(;i!=e;++i) (*i)->TurnOn();
}
void CCarLights::TurnOffLights()
{
	VERIFY(!physics_world()->Processing());
	LIGHTS_I i =m_lights.begin(),e=m_lights.end();
	for(;i!=e;++i) (*i)->TurnOff();
}

// UNUSED: IsLight
// bool CCarLights::IsLight(u16 bone_id)
// {
// 	SCarLight* light=NULL;
// 	return findLight(bone_id,light);
// }
bool CCarLights::findLight(u16 bone_id,SCarLight* &light)
{
	LIGHTS_I i,e=m_lights.end();
	SCarLight find_light;
	find_light.bone_id=bone_id;
	i=std::find_if(m_lights.begin(),e,SFindLightPredicate(&find_light));
	light=*i;
	return i!=e;
}
CCarLights::~CCarLights()
{
	LIGHTS_I i =m_lights.begin(),e=m_lights.end();
	for(;i!=e;++i) xr_delete(*i);
	m_lights.clear();
}
