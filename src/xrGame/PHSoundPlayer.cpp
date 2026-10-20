#include "StdAfx.h"
#include "PHSoundPlayer.h"
#include "PhysicsShellHolder.h"
#include "GameObject.h"

CPHSoundPlayer::~CPHSoundPlayer()
{
	m_sound.stop();
	m_object=nullptr;
}

void CPHSoundPlayer::Play(SGameMtlPair* mtl_pair, const Fvector& pos)
{
	if (!m_object || m_object->getDestroy() || m_sound.is_playing())
	{
		return;
	}

	CPhysicsShellHolder* Holder = m_object->cast_physics_shell_holder();
	if (!Holder)
	{
		return;
	}

	Fvector vel;
	Holder->PHGetLinearVell(vel);
	if (vel.square_magnitude() > 0.01f)
	{
		CLONE_MTL_SOUND(m_sound, mtl_pair, CollideSounds);
		m_sound.play_at_pos(m_object, pos);
	}
}


