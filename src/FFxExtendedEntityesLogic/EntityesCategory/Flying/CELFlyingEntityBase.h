#pragma once
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "Include/xrRender/KinematicsAnimated.h"

class CELFlyingEntityBase : public CBaseMonster
{
public:
	CELFlyingEntityBase();
	~CELFlyingEntityBase() override;
	void Load(const char* Section) override;
	bool net_Spawn(CSE_Abstract* ServerEntity) override;
	void UpdateCL() override;
	void shedule_Update(u32 DeltaMilliseconds) override;
	void net_Export(NET_Packet& Packet) override;
	void Die(CObject* Attacker) override;
	bool AlwaysTheCrow() override { return g_Alive(); }
	bool UsedAI_Locations() override { return false; }
	void Exec_Action(float DeltaTime) override;
	void SetFlightGrounded(bool Enabled) { IsGrounded = Enabled; }
	bool IsFlightGrounded() const { return IsGrounded; }
	char* get_monster_class_name() override;
	void eye_pp_s0() override;

private:
	bool IsGrounded = false;
	MotionID FlightAnimation;
	u32 LastFrame = u32(-1);
};
