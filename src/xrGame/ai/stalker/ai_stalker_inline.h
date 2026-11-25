////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_stalker_inline.h
//	Created 	: 25.02.2003
//  Modified 	: 25.02.2003
//	Author		: Dmitriy Iassenev
//	Description : AI Behaviour for monster "Stalker" (inline functions)
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	bool CAI_Stalker::UsedAI_Locations							()
{
	return					(true);
}

IC	CStalkerAnimationManager &CAI_Stalker::animation			() const
{
	VERIFY					(m_animation_manager);
	return					(*m_animation_manager);
}

IC	CStalkerPlanner &CAI_Stalker::brain			() const
{
	VERIFY					(m_brain);
	return					(*m_brain);
}

IC	CSightManager &CAI_Stalker::sight							() const
{
	VERIFY					(m_sight_manager);
	return					(*m_sight_manager);
}

IC	const char* CAI_Stalker::Name									() const
{
	return					(CInventoryOwner::Name());
}

IC	stalker_movement_manager_smart_cover	&CAI_Stalker::movement				() const
{
	VERIFY					(m_movement_manager);
	return					(*m_movement_manager);
}

IC	bool CAI_Stalker::frame_check								(u32 &frame)
{
	if (Device.dwFrame == frame)
		return				(false);

	frame					= Device.dwFrame;
	return					(true);
}

IC	bool CAI_Stalker::group_behaviour							() const
{
	return					(m_group_behaviour);
}

IC	CWeaponShotEffector &CAI_Stalker::weapon_shot_effector		() const
{
	VERIFY					(m_weapon_shot_effector);
	return					(*m_weapon_shot_effector);
}

IC	bool CAI_Stalker::wounded									() const
{
	return					(m_wounded);
}

IC	const CAI_Stalker::CRITICAL_WOUND_WEIGHTS &CAI_Stalker::critical_wound_weights	() const
{
	VERIFY					(!m_critical_wound_weights.empty());
	return					(m_critical_wound_weights);
}

IC	const bool &CAI_Stalker::throw_enabled						()
{
	if (!m_throw_actual)
		update_throw_params	();

	return					(m_throw_enabled);
}

IC	const u32 &CAI_Stalker::last_throw_time						() const
{
	return					(m_last_throw_time);
}

IC	const bool &CAI_Stalker::can_throw_grenades					() const
{
	return					(m_can_throw_grenades);
}

IC	void CAI_Stalker::can_throw_grenades						(const bool &value)
{
	m_can_throw_grenades	= value;
}

IC	const u32 &CAI_Stalker::throw_time_interval					() const
{
	return					(m_throw_time_interval);
}

IC	void CAI_Stalker::throw_time_interval						(const u32 &value)
{
	m_throw_time_interval	= value;
}

IC	const Fvector &CAI_Stalker::throw_target					() const
{
	return					(m_throw_target_position);
}

IC	void CAI_Stalker::sniper_update_rate						(bool value)
{
	m_sniper_update_rate	= value;
}

IC	bool CAI_Stalker::sniper_update_rate						() const
{
	return					(m_sniper_update_rate);
}

IC	void CAI_Stalker::hit_callback								(HitCallback const &hit_callback)
{
	m_hit_callback			= hit_callback;
}

IC	CCoverPoint const* const& CAI_Stalker::current_best_cover	() const
{
	return					(m_best_cover);
}

IC	void CAI_Stalker::sniper_fire_mode							(bool value)
{
	m_sniper_fire_mode		= value;
}

IC	bool CAI_Stalker::sniper_fire_mode							() const
{
	return					(m_sniper_fire_mode);
}

IC	void CAI_Stalker::take_items_enabled						(bool value)
{
	m_take_items_enabled	= value;
}

IC	bool CAI_Stalker::take_items_enabled						() const
{
	return					(m_take_items_enabled);
}

IC	void CAI_Stalker::death_sound_enabled						(bool value)
{
	m_death_sound_enabled	= value;
}

IC	bool CAI_Stalker::death_sound_enabled						() const
{
	return					(m_death_sound_enabled);
}

IC	u32 CAI_Stalker::pstl_min_queue_size_far						() const
{
	return					StalkerDesc().Queues.Pistol.MinSizeFar;
}

IC	u32 CAI_Stalker::pstl_max_queue_size_far						() const
{
	return					StalkerDesc().Queues.Pistol.MaxSizeFar;
}

IC	u32 CAI_Stalker::pstl_min_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Pistol.MinIntervalFar;
}

IC	u32 CAI_Stalker::pstl_max_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Pistol.MaxIntervalFar;
}

IC	u32 CAI_Stalker::pstl_min_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Pistol.MinSizeMedium;
}

IC	u32 CAI_Stalker::pstl_max_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Pistol.MaxSizeMedium;
}

IC	u32 CAI_Stalker::pstl_min_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Pistol.MinIntervalMedium;
}

IC	u32 CAI_Stalker::pstl_max_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Pistol.MaxIntervalMedium;
}

IC	u32 CAI_Stalker::pstl_min_queue_size_close						() const
{
	return					StalkerDesc().Queues.Pistol.MinSizeClose;
}

IC	u32 CAI_Stalker::pstl_max_queue_size_close						() const
{
	return					StalkerDesc().Queues.Pistol.MaxSizeClose;
}

IC	u32 CAI_Stalker::pstl_min_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Pistol.MinIntervalClose;
}

IC	u32 CAI_Stalker::pstl_max_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Pistol.MaxIntervalClose;
}

IC	u32 CAI_Stalker::shtg_min_queue_size_far						() const
{
	return					StalkerDesc().Queues.Shotgun.MinSizeFar;
}

IC	u32 CAI_Stalker::shtg_max_queue_size_far						() const
{
	return					StalkerDesc().Queues.Shotgun.MaxSizeFar;
}

IC	u32 CAI_Stalker::shtg_min_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Shotgun.MinIntervalFar;
}

IC	u32 CAI_Stalker::shtg_max_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Shotgun.MaxIntervalFar;
}

IC	u32 CAI_Stalker::shtg_min_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Shotgun.MinSizeMedium;
}

IC	u32 CAI_Stalker::shtg_max_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Shotgun.MaxSizeMedium;
}

IC	u32 CAI_Stalker::shtg_min_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Shotgun.MinIntervalMedium;
}

IC	u32 CAI_Stalker::shtg_max_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Shotgun.MaxIntervalMedium;
}

IC	u32 CAI_Stalker::shtg_min_queue_size_close						() const
{
	return					StalkerDesc().Queues.Shotgun.MinSizeClose;
}

IC	u32 CAI_Stalker::shtg_max_queue_size_close						() const
{
	return					StalkerDesc().Queues.Shotgun.MaxSizeClose;
}

IC	u32 CAI_Stalker::shtg_min_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Shotgun.MinIntervalClose;
}

IC	u32 CAI_Stalker::shtg_max_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Shotgun.MaxIntervalClose;
}

IC	u32 CAI_Stalker::snp_min_queue_size_far						() const
{
	return					StalkerDesc().Queues.Sniper.MinSizeFar;
}

IC	u32 CAI_Stalker::snp_max_queue_size_far						() const
{
	return					StalkerDesc().Queues.Sniper.MaxSizeFar;
}

IC	u32 CAI_Stalker::snp_min_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Sniper.MinIntervalFar;
}

IC	u32 CAI_Stalker::snp_max_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Sniper.MaxIntervalFar;
}

IC	u32 CAI_Stalker::snp_min_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Sniper.MinSizeMedium;
}

IC	u32 CAI_Stalker::snp_max_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Sniper.MaxSizeMedium;
}

IC	u32 CAI_Stalker::snp_min_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Sniper.MinIntervalMedium;
}

IC	u32 CAI_Stalker::snp_max_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Sniper.MaxIntervalMedium;
}

IC	u32 CAI_Stalker::snp_min_queue_size_close						() const
{
	return					StalkerDesc().Queues.Sniper.MinSizeClose;
}

IC	u32 CAI_Stalker::snp_max_queue_size_close						() const
{
	return					StalkerDesc().Queues.Sniper.MaxSizeClose;
}

IC	u32 CAI_Stalker::snp_min_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Sniper.MinIntervalClose;
}

IC	u32 CAI_Stalker::snp_max_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Sniper.MaxIntervalClose;
}

IC	u32 CAI_Stalker::mchg_min_queue_size_far						() const
{
	return					StalkerDesc().Queues.MachineGun.MinSizeFar;
}

IC	u32 CAI_Stalker::mchg_max_queue_size_far						() const
{
	return					StalkerDesc().Queues.MachineGun.MaxSizeFar;
}

IC	u32 CAI_Stalker::mchg_min_queue_interval_far					() const
{
	return					StalkerDesc().Queues.MachineGun.MinIntervalFar;
}

IC	u32 CAI_Stalker::mchg_max_queue_interval_far					() const
{
	return					StalkerDesc().Queues.MachineGun.MaxIntervalFar;
}

IC	u32 CAI_Stalker::mchg_min_queue_size_medium						() const
{
	return					StalkerDesc().Queues.MachineGun.MinSizeMedium;
}

IC	u32 CAI_Stalker::mchg_max_queue_size_medium						() const
{
	return					StalkerDesc().Queues.MachineGun.MaxSizeMedium;
}

IC	u32 CAI_Stalker::mchg_min_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.MachineGun.MinIntervalMedium;
}

IC	u32 CAI_Stalker::mchg_max_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.MachineGun.MaxIntervalMedium;
}

IC	u32 CAI_Stalker::mchg_min_queue_size_close						() const
{
	return					StalkerDesc().Queues.MachineGun.MinSizeClose;
}

IC	u32 CAI_Stalker::mchg_max_queue_size_close						() const
{
	return					StalkerDesc().Queues.MachineGun.MaxSizeClose;
}

IC	u32 CAI_Stalker::mchg_min_queue_interval_close					() const
{
	return					StalkerDesc().Queues.MachineGun.MinIntervalClose;
}

IC	u32 CAI_Stalker::mchg_max_queue_interval_close					() const
{
	return					StalkerDesc().Queues.MachineGun.MaxIntervalClose;
}

IC	u32 CAI_Stalker::auto_min_queue_size_far						() const
{
	return					StalkerDesc().Queues.Auto.MinSizeFar;
}

IC	u32 CAI_Stalker::auto_max_queue_size_far						() const
{
	return					StalkerDesc().Queues.Auto.MaxSizeFar;
}

IC	u32 CAI_Stalker::auto_min_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Auto.MinIntervalFar;
}

IC	u32 CAI_Stalker::auto_max_queue_interval_far					() const
{
	return					StalkerDesc().Queues.Auto.MaxIntervalFar;
}

IC	u32 CAI_Stalker::auto_min_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Auto.MinSizeMedium;
}

IC	u32 CAI_Stalker::auto_max_queue_size_medium						() const
{
	return					StalkerDesc().Queues.Auto.MaxSizeMedium;
}

IC	u32 CAI_Stalker::auto_min_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Auto.MinIntervalMedium;
}

IC	u32 CAI_Stalker::auto_max_queue_interval_medium					() const
{
	return					StalkerDesc().Queues.Auto.MaxIntervalMedium;
}

IC	u32 CAI_Stalker::auto_min_queue_size_close						() const
{
	return					StalkerDesc().Queues.Auto.MinSizeClose;
}

IC	u32 CAI_Stalker::auto_max_queue_size_close						() const
{
	return					StalkerDesc().Queues.Auto.MaxSizeClose;
}

IC	u32 CAI_Stalker::auto_min_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Auto.MinIntervalClose;
}

IC	u32 CAI_Stalker::auto_max_queue_interval_close					() const
{
	return					StalkerDesc().Queues.Auto.MaxIntervalClose;
}



//IC	float CAI_Stalker::pstl_queue_fire_dist_close						() const
//{
//	return					(m_pstl_queue_fire_dist_close);
//}
IC	float CAI_Stalker::pstl_queue_fire_dist_med						() const
{
	return					StalkerDesc().Queues.Pistol.FireDistMed;
}
IC	float CAI_Stalker::pstl_queue_fire_dist_far						() const
{
	return					StalkerDesc().Queues.Pistol.FireDistFar;
}
//IC	float CAI_Stalker::shtg_queue_fire_dist_close						() const
//{
//	return					(m_shtg_queue_fire_dist_close);
//}
IC	float CAI_Stalker::shtg_queue_fire_dist_med						() const
{
	return					StalkerDesc().Queues.Shotgun.FireDistMed;
}
IC	float CAI_Stalker::shtg_queue_fire_dist_far						() const
{
	return					StalkerDesc().Queues.Shotgun.FireDistFar;
}
//IC	float CAI_Stalker::snp_queue_fire_dist_close						() const
//{
//	return					(m_snp_queue_fire_dist_close);
//}
IC	float CAI_Stalker::snp_queue_fire_dist_med						() const
{
	return					StalkerDesc().Queues.Sniper.FireDistMed;
}
IC	float CAI_Stalker::snp_queue_fire_dist_far						() const
{
	return					StalkerDesc().Queues.Sniper.FireDistFar;
}
//IC	float CAI_Stalker::mchg_queue_fire_dist_close						() const
//{
//	return					(m_mchg_queue_fire_dist_close);
//}
IC	float CAI_Stalker::mchg_queue_fire_dist_med						() const
{
	return					StalkerDesc().Queues.MachineGun.FireDistMed;
}
IC	float CAI_Stalker::mchg_queue_fire_dist_far						() const
{
	return					StalkerDesc().Queues.MachineGun.FireDistFar;
}
//IC	float CAI_Stalker::auto_queue_fire_dist_close						() const
//{
//	return					(m_auto_queue_fire_dist_close);
//}
IC	float CAI_Stalker::auto_queue_fire_dist_med						() const
{
	return					StalkerDesc().Queues.Auto.FireDistMed;
}
IC	float CAI_Stalker::auto_queue_fire_dist_far						() const
{
	return					StalkerDesc().Queues.Auto.FireDistFar;
}
