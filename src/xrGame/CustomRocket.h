//////////////////////////////////////////////////////////////////////
// CustomRocket.h:	ракета, которой стреляет RocketLauncher 
//					(умеет лететь, светиться и отыгрывать партиклы)
//////////////////////////////////////////////////////////////////////

#pragma once

#include "physic_item.h"
#include "../xrPhysics/PHUpdateObject.h"

class CRocketLauncher;
struct dContact;
struct SGameMtl;
struct SRoketContact
{
	bool contact = false;
	Fvector pos = zero_vel;
	Fvector up = zero_vel;
};

class CCustomRocket : public CPhysicItem,
	public CPHUpdateObject
{
private:
	using inherited = CPhysicItem;
	friend CRocketLauncher;
	friend CWeaponRPG7;
public:
	//////////////////////////////////////////////////////////////////////////
	//	Generic
	//////////////////////////////////////////////////////////////////////////

	CCustomRocket();
	virtual ~CCustomRocket();

	virtual void Load(LPCSTR section) override;
	virtual BOOL net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual BOOL AlwaysTheCrow() override { return TRUE; }

	virtual void reinit() override;
	virtual void reload(LPCSTR section) override;

	virtual void OnH_A_Independent() override;
	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void OnH_B_Chield() override;
	virtual void OnH_A_Chield() override;
	virtual void UpdateCL() override;

	virtual BOOL UsedAI_Locations() override { return FALSE; }
	virtual bool Useful() const { return (m_eState == eInactive); }

	virtual void renderable_Render() override { inherited::renderable_Render(); }

	//создание физической оболочки
	virtual void activate_physic_shell() override;
	virtual void create_physic_shell() override;

	virtual void PhDataUpdate(float step) override;
	virtual void PhTune(float step) override;

	virtual CExplosiveRocket* cast_explosive_rocket() override { return nullptr; }
	virtual CGameObject* cast_game_object() override { return this; }
	virtual CPhysicItem* cast_physics_item() override { return this; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CCustomRocket* cast_custom_rocket() override { return this; }

	//////////////////////////////////////////////////////////////////////////
	//	Rocket Properties
	//////////////////////////////////////////////////////////////////////////
public:
#ifdef DEBUG
	CGameObject* owner() { return m_pOwner; }
#endif
	virtual	void StartEngine();
	virtual	void StopEngine();
	virtual	void UpdateEngine();
	virtual	void UpdateEnginePh();

	virtual	void StartFlying();
	virtual	void StopFlying();

	virtual	void SetLaunchParams(const Fmatrix& xform, const Fvector& vel, const Fvector& angular_vel);

	virtual void OnEvent(NET_Packet& P, u16 type) override;
	bool m_bLaunched = false;

	virtual void Contact(const Fvector& pos, const Fvector& normal);

protected:
	//указатель на владельца RocketLauncher - который стреляет ракету
	CGameObject* m_pOwner = nullptr;

	SRoketContact m_contact = {};
	//параметры которые задаются RocketLauncher-ом перед пуском
	Fmatrix m_LaunchXForm;
	Fvector m_vLaunchVelocity = zero_vel;
	Fvector m_vLaunchAngularVelocity = zero_vel;

	enum ERocketState {
		eInactive,		//ракета неактивна и находиться в инвентаре
		eEngine,		//включен двигатель
		eFlying,		//просто летим
		eCollide		//произошло столкновение с препятствием
	};

	//текущее состояние ракеты
	ERocketState m_eState = eInactive;

	//двигатель присутствует
	bool m_bEnginePresent = false;
	//время работы двигателя с момента старта
	int	m_dwEngineWorkTime = 0;
	//сила работы двигателя (размер импульса в секунду)
	float m_fEngineImpulse = 0.0f;
	float m_fEngineImpulseUp = 0.0f;
	//текущее время работы двигателя
	int m_dwEngineTime = 0;

	//обработка столкновения
	void PlayContact();
	static void	ObjectContactCallback(bool& do_colide, bool bo1, dContact& c, SGameMtl* /*material_1*/, SGameMtl* /*material_2*/);

	//////////////////////////////////////////////////////////////////////////
	//	Lights
	//////////////////////////////////////////////////////////////////////////
protected:
	//флаг, что подсветка может быть включена
	bool m_bLightsEnabled = false;
	//флаг, что подсветка будет остановлена
	//вместе с двигателем
	bool m_bStopLightsWithEngine = true;
	//подсветка во время полета и работы двигателя
	ref_light m_pTrailLight = nullptr;
	Fcolor m_TrailLightColor;
	float m_fTrailLightRange = 0.0f;
	ref_sound m_flyingSound = {};

protected:
	virtual void StartLights();
	virtual void StopLights();
	virtual void UpdateLights();

	//////////////////////////////////////////////////////////////////////////
	//	Particles
	//////////////////////////////////////////////////////////////////////////
protected:
	//имя партиклов двигателя
	shared_str m_sEngineParticles;
	xr_shared_ptr<CParticlesObject>	m_pEngineParticles = nullptr;
	//имя партиклов полета
	shared_str m_sFlyParticles;
	xr_shared_ptr<CParticlesObject>	m_pFlyParticles = nullptr;

	Fvector	m_vPrevVel = zero_vel;
	float m_time_to_explode = 0.0f;
#ifdef	DEBUG
	float gbg_rocket_speed1 = 0.0f;
	float gbg_rocket_speed2 = 0.0f;
#endif
protected:
	virtual void StartEngineParticles();
	virtual void StopEngineParticles();
	virtual void StartFlyParticles();
	virtual void StopFlyParticles();

	virtual void UpdateParticles();
#ifdef DEBUG
	virtual void deactivate_physics_shell();
#endif
};