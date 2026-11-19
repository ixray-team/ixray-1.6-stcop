#include "StdAfx.h"
#include "pch_script.h"
#include "helicopter.h"
#include "script_game_object.h"

int CHelicopter::GetMovementState()
{
	return m_movement.type;
}

int CHelicopter::GetHuntState()
{
	return m_enemy.type;
}

int CHelicopter::GetBodyState()
{
	return	m_body.type;
}
