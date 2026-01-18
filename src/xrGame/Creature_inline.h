#pragma once

IC	bool CCreature::angle_lerp_bounds(float &a, float b, float c, float d)
{
	if (c*d >= angle_difference(a,b)) {
		a = b;
		return(true);
	}
	
	angle_lerp(a,b,c,d);

	return(false);
};

IC void CCreature::vfNormalizeSafe(Fvector& Vector)
{
	float fMagnitude = Vector.magnitude(); 
	if (fMagnitude > EPS_L) {
		Vector.x /= fMagnitude;
		Vector.y /= fMagnitude;
		Vector.z /= fMagnitude;
	}
	else {
		Vector.x = 1.f;
		Vector.y = 0.f;
		Vector.z = 0.f;
	}
}

ICF	bool left_angle(float y1, float y2)
{
	return			(std::sin(y1)* std::cos(y2) - std::sin(y2)* std::cos(y1) <= 0.f);
}

IC	CMemoryManager &CCreature::memory		() const
{
	VERIFY			(m_memory_manager);
	return			(*m_memory_manager);
}

IC	CMovementManager &CCreature::movement	() const
{
	VERIFY			(m_movement_manager);
	return			(*m_movement_manager);
}

IC	CSoundPlayer &CCreature::sound			() const
{
	VERIFY			(m_sound_player);
	return			(*m_sound_player);
}

IC	CSound_UserDataVisitor *CCreature::sound_user_data_visitor	() const
{
	VERIFY			(m_sound_user_data_visitor);
	return			(m_sound_user_data_visitor);
}

IC	float CCreature::panic_threshold				() const
{
	return			(m_panic_threshold);
}

IC	float CCreature::client_update_fdelta			() const
{
	return			((float)m_client_update_delta/1000.f);
}

IC	const u32 &CCreature::client_update_delta		() const
{
	return			(m_client_update_delta);
}

IC	const u32 &CCreature::last_client_update_time	() const
{
	return			(m_last_client_update_time);
}

IC	const u32 &CCreature::critical_wound_type	() const
{
	return			(m_critical_wound_type);
}

IC bool CCreature::critically_wounded			()
{
	return			(m_critical_wound_type != u32(-1));
}

IC void CCreature::critical_wounded_state_stop	() 
{
	m_critical_wound_type = u32(-1);
}

IC		void CCreature::invulnerable			(const bool &invulnerable)
{
	m_invulnerable	= invulnerable;
}

IC		bool CCreature::invulnerable			() const
{
	return			(m_invulnerable);
}

IC	moving_object *CCreature::get_moving_object() const
{
	VERIFY2			(m_moving_object, make_string<const char*>("object [%d][%s]",ID(),*cName()));
	return			(m_moving_object);
}