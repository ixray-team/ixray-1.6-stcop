#pragma once

#include "hud_item_object.h"
#include "hit_immunity.h"
#include "../xrPhysics/PHUpdateObject.h"
#include "../xrScripts/script_export_space.h"
#include "patrol_path.h"

class SArtefactActivation;
struct SArtefactDetectorsSupport;

class CArtefact : public CHudItemObject, 
	public CPHUpdateObject 
{
	using inherited = CHudItemObject;
public:
	CArtefact();
	virtual ~CArtefact() = default;

	virtual void					Load							(const char* section);
	virtual bool					net_Spawn						(CSE_Abstract* DC);
	virtual void					net_Destroy						();

	virtual void					OnH_A_Chield					();
	virtual void					OnH_B_Independent				(bool just_before_destroy);
	virtual void					OnActiveItem					();
	virtual void					OnHiddenItem					();
	
	virtual void					UpdateCL						();
	virtual void					shedule_Update					(u32 dt);	
			void					UpdateWorkload					(u32 dt);

	
	virtual bool					CanTake							() const;

	virtual bool					renderable_ShadowGenerate		()		{ return false;	}
	virtual bool					renderable_ShadowReceive		()		{ return true;	}
	virtual void					create_physic_shell				();

	virtual CArtefact*				cast_artefact					()		{return this;}

	float	GetHealthPower() { return m_fHealthRestoreSpeed; }
	float	GetRadiationPower() { return m_fRadiationRestoreSpeed; }
	float	GetSatietyPower() { return m_fSatietyRestoreSpeed; }
	float	GetThirstPower() { return m_fThirstRestoreSpeed; }
	float	GetSleepinessPower() { return m_fSleepinessRestoreSpeed; }
	float	GetPowerPower() { return m_fPowerRestoreSpeed; }
	float	GetBleedingPower() { return m_fBleedingRestoreSpeed; }
	float	GetEquipmentDurabilityModifier() const { return m_fEquipmentDurabilityModifier; }
	float	GetInventoryWeightModifier() const { return m_fInventoryWeightModifier; }
	float	GetJumpHeightModifier() const { return m_fJumpHeightModifier; }
	float	GetMovementSpeedModifier() const { return m_fMovementSpeedModifier; }

	void	SetHealthPower(float value) { m_fHealthRestoreSpeed = value; }
	void	SetRadiationPower(float value) { m_fRadiationRestoreSpeed = value; }
	void	SetSatietyPower(float value) { m_fSatietyRestoreSpeed = value; }
	void	SetThirstPower(float value) { m_fThirstRestoreSpeed = value; }
	void	SetSleepinessPower(float value) { m_fSleepinessRestoreSpeed = value; }
	void	SetPowerPower(float value) { m_fPowerRestoreSpeed = value; }
	void	SetBleedingPower(float value) { m_fBleedingRestoreSpeed = value; }
	void	SetEquipmentDurabilityModifier(float value) { m_fEquipmentDurabilityModifier = value; }
	void	SetInventoryWeightModifier(float value) { m_fInventoryWeightModifier = value; }
	void	SetJumpHeightModifier(float value) { m_fJumpHeightModifier = value; }
	void	SetMovementSpeedModifier(float value) { m_fMovementSpeedModifier = value; }

	float m_fJumpSpeed;
	float m_fWalkAccel;

	const char* PS_bone													()		{return m_sParticlesBone.c_str(); };
	bool has_detector_visibling;
protected:
	virtual void					UpdateCLChild					()		{};
	virtual void					CreateArtefactActivation			();

	SArtefactActivation*			m_activationObj;
	SArtefactDetectorsSupport*		m_detectorObj;

	u16								m_CarringBoneID;
	u16								m_ParticlesBoneID;
	u16								m_LightBoneID;
	shared_str						m_sParticlesName;
	shared_str						m_sParticlesBone;
	ref_light						m_pTrailLight;
	Fcolor							m_TrailLightColor;
	float							m_fTrailLightRange;
	u8								m_af_rank;
	bool							m_bLightsEnabled;
	float							m_additional_weight;
	float							m_fDegradationRate;

	virtual void					UpdateLights					();
public:
	IC u8							GetAfRank						() const		{return m_af_rank;}
	IC bool							CanBeActivated					()				{return m_bCanSpawnZone;};
	void							ActivateArtefact				();
	void							FollowByPath					(const char* path_name, int start_idx, Fvector magic_force);
	bool							CanBeInvisible					();
	void							SwitchVisibility				(bool);

	void							SwitchAfParticles				(bool bOn);
	virtual void					StartLights();
	virtual void					StopLights();

	virtual void					PhDataUpdate					(float step);
	virtual void					PhTune							(float step)	{};

	float							AdditionalInventoryWeight		() const {return m_additional_weight;}
	bool							m_bCanSpawnZone;
	float							m_fHealthRestoreSpeed;
	float 							m_fRadiationRestoreSpeed;
	float 							m_fSatietyRestoreSpeed;
	float 							m_fThirstRestoreSpeed;
	float 							m_fSleepinessRestoreSpeed;
	float							m_fPowerRestoreSpeed;
	float							m_fBleedingRestoreSpeed;
	float							m_fEquipmentDurabilityModifier;
	float							m_fInventoryWeightModifier;
	float							m_fJumpHeightModifier;
	float							m_fMovementSpeedModifier;
	CHitImmunity 					m_ArtefactHitImmunities;
public:
	enum EAFHudStates {
		eActivating = eLastBaseState+1,
	};
	virtual void					Interpolate			();

	virtual	void					PlayAnimIdle		();
	virtual void					MoveTo(Fvector const & position);
	virtual void					StopActivation		();

	virtual void					ForceTransform		(const Fmatrix& m);

	virtual void					Hide				();
	virtual void					Show				();
	virtual	void					UpdateXForm			();
	virtual bool					Action				(u16 cmd, u32 flags);
	virtual void					OnStateSwitch		(u32 S);
	virtual void					OnAnimationEnd		(u32 state);
	virtual bool					IsHidden			()	const	{return GetState()==eHidden;}
	virtual u32						Cost				() const;
	float							DegradationRate		() {return m_fDegradationRate;}

	// optimization FAST/SLOW mode
	u32						o_render_frame				;
	bool					o_fastmode					;
	IC void					o_switch_2_fast				()	{
		if (o_fastmode)		return	;
		o_fastmode			= true	;
		//processing_activate		();
	}
	IC void					o_switch_2_slow				()	{
		if (!o_fastmode)	return	;
		o_fastmode			= false	;
		//processing_deactivate		();
	}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

struct SArtefactDetectorsSupport
{
	CArtefact*						m_parent;
	ref_sound						m_sound;

	Fvector							m_path_moving_force;
	u32								m_switchVisTime;
	const CPatrolPath*				m_currPatrolPath;
	const CPatrolPath::CVertex*		m_currPatrolVertex;
	Fvector							m_destPoint;

	const char*							det_show_particles;
	const char*							det_hide_particles;
	const char*							det_show_snd;
	const char*							det_hide_snd;
	const char*							particles_bone;

			SArtefactDetectorsSupport		(CArtefact* A);
			~SArtefactDetectorsSupport		();
	void	SetVisible						(bool);
	void	Load							(const char* section);
	void	FollowByPath					(const char* path_name, int start_idx, Fvector force);
	void	UpdateOnFrame					();
	void	Blink							();
};
