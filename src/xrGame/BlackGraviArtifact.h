///////////////////////////////////////////////////////////////
// BlackGraviArtifact.h
// BlackGraviArtefact - гравитационный артефакт, 
// такой же как и обычный, но при получении хита
///////////////////////////////////////////////////////////////

#pragma once
#include "GraviArtifact.h"
#include "../xrEngine/Feel_Touch.h"
#include "PhysicsShellHolder.h"

using GAME_OBJECT_LIST = xr_vector<CPhysicsShellHolder*>;
using GAME_OBJECT_LIST_it = GAME_OBJECT_LIST::iterator;

class CBlackGraviArtefact final : public CGraviArtefact,
						   public Feel::Touch
{
private:
	collide::rq_results		rq_storage;

private:
	typedef CGraviArtefact inherited;
public:
	CBlackGraviArtefact(void);
	virtual ~CBlackGraviArtefact(void) override;

	virtual bool net_Spawn			(CSE_Abstract* DC) override;

	virtual void Load				(const char* section) override;

	virtual void Hit				(SHit* pHDS) override;

	virtual void feel_touch_new		(CObject* O) override;
	virtual void feel_touch_delete	(CObject* O) override;
	virtual bool feel_touch_contact	(CObject* O) override;

protected:
	virtual void	net_Relcase		(CObject* O ) override;
	virtual void	UpdateCLChild	() override;

	//гравитационный удар по всем объектам в зоне дос€гаемости
	void GraviStrike();

	GAME_OBJECT_LIST m_GameObjectList;


	//которого артефакт активизируетс€
	float m_fImpulseThreshold;
	//радиус действи€ артефакта
	float m_fRadius;
	//импульс передаваемый окружающим предметам
	float m_fStrikeImpulse;

	//флаг, того что артефакт получил хит 
	//и теперь может совершить бросок
	bool m_bStrike;	

	shared_str	m_sParticleName;
};
