#pragma once
#include "hud_item_object.h"
#include "HudSound.h"
#include "../xrPhysics/ExtendedGeom.h"

struct dContact;
struct SGameMtl;
class CBolt;
class CGrenade;

class CMissile : public CHudItemObject
{
	using inherited = CHudItemObject;
public:
	enum EMissileStates : u8
	{
		eThrowCancel = eLastBaseState+1,
		eThrowStart,
		eReady,
		eThrow,
		eThrowEnd,
	};

	CMissile() = default;
	virtual ~CMissile() override = default;

	virtual bool			AlwaysTheCrow				() override { return true; }
	virtual void			render_item_ui					() override;
	virtual bool			render_item_ui_query					() override;

	virtual void			reinit						() override;
	virtual CMissile*		cast_missile				() override {return this;}
	virtual CBolt* cast_bolt() override { return nullptr; }
	virtual CGrenade* cast_grenade() override { return nullptr; }

	virtual void 			Load						(const char* section) override;
	virtual void 			LoadSounds					(const char* section) override;
	virtual bool 			net_Spawn					(CSE_Abstract* DC) override;
	virtual void 			net_Destroy					() override;

	virtual void 			UpdateCL					() override;
	virtual void 			shedule_Update				(u32 dt) override;

	virtual void 			OnH_A_Chield				() override;
	virtual void 			OnH_B_Independent			(bool just_before_destroy) override;

	virtual void 			OnEvent						(NET_Packet& P, u16 type) override;

	virtual void 			OnAnimationEnd				(u8 state) override;
	virtual void			OnMotionMark				(u8 state, const motion_marks&) override;


	virtual void 			Throw();
	virtual void 			Destroy();

	virtual bool 			Action						(u16 cmd, u32 flags) override;

	virtual void 			State						(u8 state);
	virtual void 			OnStateSwitch				(u8 S) override;
	virtual bool			GetBriefInfo				(II_BriefInfo& info) override;
	bool					NeedBlockSprint				() const;
	void spawn_fake_missile();

protected:
	virtual void			UpdateFireDependencies_internal	();
	virtual void			UpdateXForm						() override;
	void					UpdatePosition					(const Fmatrix& trans);
	bool ThrowAction(u16 cmd, u32 flags);

	virtual void			OnActiveItem		() override;
	virtual void			OnHiddenItem		() override;

	//для сети
	virtual void			net_Relcase			(CObject* O ) override;
protected:

	//время нахождения в текущем состоянии
	u32						m_dwStateTime = 0;
	bool					m_motion_marks_available = true;

	//время уничтожения
	u32						m_dwDestroyTime;
	u32						m_dwDestroyTimeMax;

	u32						dwUpdateSounds_Frame;

	Fvector					m_throw_direction;
	Fmatrix					m_throw_matrix;

	CMissile				*m_fake_missile;

	xr_vector<shared_str>	m_sCheckoutBones;

	bool					m_bUseAltThrow = false;
	bool					m_bUseHudPosition = false;

	//параметры броска
	
	float m_fMinForce, m_fConstForce, m_fMaxForce, m_fForceGrowSpeed;
//private:
	float					m_fThrowForce;
protected:
	//относительная точка и направление вылета гранаты
	Fvector					m_vThrowPoint;
	Fvector					m_vThrowDir;

protected:
			void			setup_throw_params		(bool bForce);
public:
	Fvector const&			throw_point_offset		() const {return m_vThrowPoint;}
	virtual void			activate_physic_shell	() override;
	virtual void			setup_physic_shell		() override;
	virtual void			create_physic_shell		() override;
	IC		void			set_destroy_time		(u32 delta_destroy_time) {m_dwDestroyTime = delta_destroy_time + Device.dwTimeGlobal;}
	virtual void			PH_A_CrPr				() override;

	bool m_constpower;
	bool m_throw;
	bool SuicideThrow = false;
	u32 SuicideFailDestroyTime = 0;
	float SuicideFailForce = 0.0f;
	float SuicideReadyForce = 0.0f;
	float ControllerGAttackMinDist = 0.0f;

protected:
	u32						m_ef_weapon_type;

public:
	virtual u32				ef_weapon_type			() const override;
	IC		u32				destroy_time			() const { return m_dwDestroyTime; }
	IC		u32				destroy_time_max		() const { return m_dwDestroyTimeMax; }
			void			set_destroy_time_max		(u32 time) { m_dwDestroyTimeMax = time; }
	void PrepareGrenadeForSuicideThrow(float force);
	IC		int				time_from_begin_throw	() const { return (Device.dwTimeGlobal + m_dwDestroyTimeMax - m_dwDestroyTime); }
	IC		void			set_destroy_time_now	(u32 time) { m_dwDestroyTime = time; }
	static	void			ExitContactCallback		(bool& do_colide,bool bo1,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/);
	static	void			ExitContactCallback_Patch(dGeomID dxGeom);
};