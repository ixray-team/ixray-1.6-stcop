///////////////////////////////////////////////////////////////
// BastArtifact.h
// BastArtefact - артефакт мочалка
///////////////////////////////////////////////////////////////

#pragma once
#include "Artefact.h"
#include "../xrEngine/Feel_Touch.h"

#include "entity_alive.h"

struct SGameMtl;
class	CPHContact;

using ALIVE_LIST = xr_vector<CEntityAlive*>;
using ALIVE_LIST_it = ALIVE_LIST::iterator;

class CBastArtefact final : public CArtefact,
					  public Feel::Touch
{
private:
	typedef CArtefact inherited;
public:
	CBastArtefact(void);
	virtual ~CBastArtefact(void) override;

	virtual void Load				(const char* section) override;
	virtual void shedule_Update		(u32 dt) override;

	virtual bool net_Spawn			(CSE_Abstract* DC) override;
	virtual void net_Destroy		() override;

	virtual void Hit				(SHit* pHDS) override;

	virtual bool Useful() const override;


	virtual void feel_touch_new	(CObject* O) override;
	virtual void feel_touch_delete	(CObject* O) override;
	virtual bool feel_touch_contact	(CObject* O) override;

	bool IsAttacking() {return NULL!=m_AttakingEntity;}

protected:
	virtual void	UpdateCLChild	() override;

	static	void	ObjectContactCallback(bool& do_colide,bool bo1,CPHContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/);
	//столкновение мочалки с сущностью
	void BastCollision(CEntityAlive* pEntityAlive);


	//параметры артефакта
	
	//пороговое значение импульса после получени€ 
	//которого артефакт активизируетс€
	float m_fImpulseThreshold;
	
	float m_fEnergy;
	float m_fEnergyMax;
	float m_fEnergyDecreasePerTime;
	shared_str	m_sParticleName;


	float m_fRadius;
	float m_fStrikeImpulse;

	//флаг, того что артефакт получил хит 
	//и теперь может совершить бросок
	bool m_bStrike;	

	//список живых существ в зоне дос€гаемости артефакта
	ALIVE_LIST m_AliveList;
	//то, что мы ударили
	CEntityAlive* m_pHitedEntity; 
	//то что атакуем
	CEntityAlive* m_AttakingEntity;

public:
	virtual void setup_physic_shell	() override;
};