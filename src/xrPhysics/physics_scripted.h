#pragma once
#include "iphysics_scripted.h"

class cphysics_scripted :
	public iphysics_scripted
{
	iphysics_game_scripted* m_game_scripted;

public:
	cphysics_scripted() :m_game_scripted(0) {}
	virtual	~cphysics_scripted() override;
	virtual	void set(iphysics_game_scripted* g) override;
	virtual	iphysics_game_scripted* get() override { return m_game_scripted; };
};