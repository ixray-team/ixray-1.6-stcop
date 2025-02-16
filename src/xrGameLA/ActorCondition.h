// ActorCondition.h: класс состояния игрока
//
#pragma once

#include "EntityCondition.h"
#include "actor_defs.h"
#include "hit_immunity.h"
#include "booster.h"

template <typename _return_type>
class CScriptCallbackEx;
class CActor;

struct Eat_Effect //Эффект от съедаемых предметов
{
	float			DurationExpiration; // Должно хранить движковое время + неободмый отступ из конфига предмета
	float			Rate;
	float			UseTimeExpiration;  // Время блокировки рук: Должно хранить движковое время + неободмый отступ из конфига предмета
	u8				AffectedStat;
	u8				BlockingGroup;
	BoosterParams	BoosterParam;

	Eat_Effect()
	{
		DurationExpiration = 0.f;
		Rate = 0.f;
		UseTimeExpiration = 0.f;
		AffectedStat = 0;
		BlockingGroup = 0;
	}
};


class CActorCondition: public CEntityCondition
{
private:
	typedef CEntityCondition inherited;
	enum {	eCriticalPowerReached			=(1<<0),
			eCriticalMaxPowerReached		=(1<<1),
			eCriticalBleedingSpeed			=(1<<2),
			eCriticalSatietyReached			=(1<<3),
			eCriticalThirstyReached			=(1<<4),
			eCriticalRadiationReached		=(1<<5),
			eWeaponJammedReached			=(1<<6),
			ePhyHealthMinReached			=(1<<7),
			eCantWalkWeight					=(1<<8),
			};
	Flags16											m_condition_flags;
private:
	CActor*											m_object;
	void				UpdateTutorialThresholds	();
	void 				UpdateSatiety				();
	void 				UpdateThirsty				();

	void 				UpdateEatablesEffects();

	u16					effects_size; //for save/load process optimization

	void				save_effects(NET_Packet &output_packet);
	void				load_effects(IReader &input_packet);

public:
						CActorCondition				(CActor *object);
	virtual				~CActorCondition			(void);

	virtual void		LoadCondition				(LPCSTR section);
	virtual void		reinit						();

	virtual CWound*		ConditionHit				(SHit* pHDS);
	virtual void		UpdateCondition				();

	virtual void 		ChangeAlcohol				(const float value);
	virtual void 		ChangeSatiety				(const float value);
	virtual void 		ChangeThirsty				(const float value);
	virtual void 		ChangeWalkWeight			(const float value) { m_MaxWalkWeight = value;}

	// хромание при потере сил и здоровья
	virtual	bool		IsLimping					() const;
	virtual bool		IsCantWalk					() const;
	virtual bool		IsCantWalkWeight			();
	virtual bool		IsCantSprint				() const;

			void		ConditionJump				(float weight);
			void		ConditionWalk				(float weight, bool accel, bool sprint);
			void		ConditionStand				(float weight);
			
			float			GetAlcohol			()	{return m_fAlcohol;}
			float			GetPsy				()	{return 1.0f-GetPsyHealth();}
			float			GetSatiety			() 	{return m_fSatiety;}
			float			GetThirsty			() 	{ return m_fThirsty; }
			float			MaxWalkWeight			() 	{return m_MaxWalkWeight;}

			float			m_fBoostersAddWeight;

			xr_vector<Eat_Effect>	Eat_Effects; //список эффектов от съеденных предметов
			float					fHandsHideTime;
public:
	IC		CActor		&object						() const
	{
		VERIFY			(m_object);
		return			(*m_object);
	}
	virtual void			save					(NET_Packet &output_packet);
	virtual void			load					(IReader &input_packet);

protected:
	float m_fAlcohol;
	float m_fV_Alcohol;
//--
	float m_fThirsty;
	float m_fV_Thirsty;
	float m_fV_ThirstyPower;
	float m_fV_ThirstyHealth;
//--
	float m_fSatiety;
	float m_fV_Satiety;
	float m_fV_SatietyPower;
	float m_fV_SatietyHealth;
//--
	float m_fPowerLeakSpeed;

	float m_fJumpPower;
	float m_fStandPower;
	float m_fWalkPower;
	float m_fJumpWeightPower;
	float m_fWalkWeightPower;
	float m_fOverweightWalkK;
	float m_fOverweightJumpK;
	float m_fAccelK;
	float m_fSprintK;
	
	float	m_MaxWalkWeight;

	mutable bool m_bLimping;
	mutable bool m_bCantWalk;
	mutable bool m_bCantSprint;

	//порог силы и здоровья меньше которого актер начинает хромать
	float m_fLimpingPowerBegin;
	float m_fLimpingPowerEnd;
	float m_fCantWalkPowerBegin;
	float m_fCantWalkPowerEnd;

	float m_fCantSprintPowerBegin;
	float m_fCantSprintPowerEnd;

	float m_fLimpingHealthBegin;
	float m_fLimpingHealthEnd;
};
