#include "StdAfx.h"
#include "fire_disp_controller.h"
#include "Actor.h"
#include "Inventory.h"
#include "Weapon.h"
#include "Level.h"

float const CFireDispertionController::default_inertion = 5.91f;	//time to pass value of dispertion = 1

CFireDispertionController::CFireDispertionController()
{
	start_disp		= 0.f;
	end_disp		= 0.f;
	start_time		= 0.f;
	current_disp	= 0.f;
}

void	CFireDispertionController::SetDispertion(float const new_disp)
{
	if (!fsimilar(new_disp, end_disp))
	{
		start_disp	= fsimilar(start_disp, 0.f) ? new_disp : current_disp;
		end_disp	= new_disp;
		start_time	= Device.fTimeGlobal;
	}
	Update();
}

float	CFireDispertionController::GetCurrentDispertion() const
{
	return current_disp;
}

void	CFireDispertionController::Update()
{
	float tmp_inertion = default_inertion;
	CObject* current_entity = Level().CurrentEntity();
	CActor* tmp_actor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;

	if (tmp_actor)
	{
		PIItem active_item = tmp_actor->inventory().ActiveItem();
		CWeapon* tmp_weapon = active_item != nullptr ? active_item->cast_weapon() : nullptr;
		if (tmp_weapon)
		{
			tmp_inertion = tmp_weapon->GetCrosshairInertion();
		}
	}
	float diff_time		= tmp_inertion * std::abs(end_disp - start_disp);
	float end_time		= start_time + diff_time;
	float current_time = Device.fTimeGlobal;
	if (end_time == start_time)
	{
		current_disp = end_disp;
		return;
	}
	if (current_time > end_time)
	{
		current_disp = end_disp;
		return;
	}
	current_disp	= start_disp + ((end_disp - start_disp) * 
		((current_time - start_time) / (end_time - start_time))
	);
}