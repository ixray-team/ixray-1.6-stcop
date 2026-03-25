#include "StdAfx.h"
#include "RadioactiveZone.h"
#include "Level.h"
#include "xrMessages.h"
#include "../xrEngine/bone.h"
#include "Actor.h"
#include "game_base_space.h"
#include "Hit.h"
#include "../xrEngine/xr_collide_form.h"

CRadioactiveZone::CRadioactiveZone(void) 
{}

CRadioactiveZone::~CRadioactiveZone(void) 
{}

void CRadioactiveZone::Load(const char* section) 
{
	inherited::Load(section);
	legacyHit = pSettings->read_if_exists<bool>(section, "pure_hit_damage", false);
}

bool  CRadioactiveZone::BlowoutState	()
{
	bool result = inherited::BlowoutState();
	if(!result) UpdateBlowout();
	return result;
}

void CRadioactiveZone::Affect(SZoneObjectInfo* O) 
{
	float one				= 0.1f;
	float tg				= Device.fTimeGlobal;

	if(!O->object || O->f_time_affected+one > Device.fTimeGlobal) 
		return;

	clamp					(O->f_time_affected, tg-(one*3), tg);

	Fvector					pos; 
	XFORM().transform_tiny	(pos,CFORM()->getSphere().P);

	Fvector dir				={0,0,0}; 
	fHitPower = Power(O->object->Position().distance_to(pos),nearest_shape_radius(O));

	float impulse			= 0.0f;
	if(fHitPower < EPS)
	{
		O->f_time_affected	= tg;
		return;
	}
	
	float send_power		= fHitPower;
	if (!legacyHit)
	{
		send_power *= one;
	}

	while(O->f_time_affected+one < tg)
	{
		CreateHit	(	O->object->ID(),
						ID(),
						dir,
						send_power,
						BI_NONE,
						Fvector().set(0.0f,0.0f,0.0f),
						impulse,
						m_eHitTypeBlowout);
#ifdef DEBUG
//		if(bDebug)
/*		Msg			(	"Zone[%s]-hit->[%s] Power=%3.3f Frame=%d Time=%3.3f", 
						cName().c_str(), 
						O->object->cName().c_str(), 
						send_power, 
						Device.dwFrame, 
						tg);*/
///		Msg( "Zone hit ___   damage = %.4f    Frame=%d ", send_power, Device.dwFrame );
#endif
		O->f_time_affected += one;
	}//while
}

void CRadioactiveZone::feel_touch_new(CObject* O)
{
	inherited::feel_touch_new(O);
	if (!IsGameTypeSingle())
	{
		if (O->cast_actor() != nullptr)
		{
			CreateHit(O->ID(), ID(), zero_vel, 0.0f, BI_NONE, zero_vel, 0.0f, m_eHitTypeBlowout);
		}
	};
};

bool CRadioactiveZone::feel_touch_contact(CObject* O)
{
	if (CActor* A = O != nullptr ? O->cast_actor() : nullptr)
	{
		if (!((CCF_Shape*)CFORM())->Contact(O))
		{
			return false;
		}

		return A->feel_touch_on_contact(this);
	}

	return false;
}

void CRadioactiveZone::UpdateWorkload					(u32	dt)
{
	if (IsEnabled() && !IsGameTypeSingle())
	{
		Fvector pos; 
		XFORM().transform_tiny(pos,CFORM()->getSphere().P);
		for (SZoneObjectInfo& info : m_ObjectInfoMap)
		{
			if(info.object && !info.object->getDestroy() && info.object->cast_actor())
			{
				//=====================================
				NET_Packet	l_P;
				l_P.write_start();
				l_P.read_start();

				float dist			= info.object->Position().distance_to(pos);
				float power			= Power(dist,nearest_shape_radius(&info))*dt/1000;

				SHit				HS;
				HS.GenHeader		(GE_HIT, info.object->ID());
				HS.whoID			= ID();
				HS.weaponID			= ID();
				HS.dir				= Fvector().set(0,0,0);
				HS.power			= power;
				HS.boneID			= BI_NONE;
				HS.p_in_bone_space	= Fvector().set(0, 0, 0);
				HS.impulse			= 0.0f;
				HS.hit_type			= m_eHitTypeBlowout;
				
				HS.Write_Packet_Cont(l_P);

				info.object->OnEvent(l_P, HS.PACKET_TYPE);
				//=====================================
			};
		}
	}
	inherited::UpdateWorkload(dt);
}

float CRadioactiveZone::nearest_shape_radius(SZoneObjectInfo* O)
{
	CCF_Shape* Sh		= (CCF_Shape*)CFORM();

	if(Sh->Shapes().size()==1)
	{
		return			Radius();
	}else
	{
		xr_vector<CCF_Shape::shape_def>& Shapes = Sh->Shapes();
		CCF_Shape::shape_def& s = Shapes[0];
		return s.data.sphere.R;
	}
}

