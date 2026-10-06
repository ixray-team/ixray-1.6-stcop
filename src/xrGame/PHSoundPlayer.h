#pragma once
#include "../xrEngine/GameMtlLib.h"

class CGameObject;

struct CPHSoundPlayer
{
	ref_sound m_sound;
	CGameObject *m_object;

	ICF CPHSoundPlayer(CGameObject* obj) : m_object(obj) {};
	void Play(SGameMtlPair* mtl_pair,const Fvector& pos);
	virtual ~CPHSoundPlayer();
};