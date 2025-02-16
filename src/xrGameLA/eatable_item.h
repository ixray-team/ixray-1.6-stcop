#pragma once

#include "inventory_item.h"
#include "booster.h"

class CPhysicItem;
class CEntityAlive;
class CActor;



class CEatableItem : public CInventoryItem {
private:
	typedef CInventoryItem	inherited;

protected:
	CPhysicItem		*m_physic_item;

public:
							CEatableItem				();
	virtual					~CEatableItem				();
	virtual	DLL_Pure*		_construct					();
	virtual CEatableItem	*cast_eatable_item			()	{return this;}

	virtual void			Load						(LPCSTR section);
	virtual bool			Useful						() const;

	// Happens when object gets online. Import saved data from server here. Do oject initialization based on data imported from server
	virtual BOOL			net_Spawn					(CSE_Abstract* DC);
	// constant export of data to server. Here should be stuff, needed to be saved before object switches offline. Than it should be imported in net_spawn, when object gets online again
	virtual void			net_Export					(NET_Packet& P);

	virtual void			OnH_B_Independent			(bool just_before_destroy);
	virtual	bool			UseBy						(CEntityAlive* npc);
	virtual void			save					(NET_Packet &output_packet);
	virtual void			load					(IReader &input_packet);

			bool			Empty						()	const				{return m_iPortionsNum==0;};

	IC	u32				GetPortionsNum		()	const		{	return m_iPortionsNum;}
		void				SetPortionsNum	(u32 value)			{m_iPortionsNum = value; }

protected:	
	//вли€ние при поедании вещи на параметры игрока
	float					m_fHealthInfluence;
	float					m_fPowerInfluence;
	float					m_fSatietyInfluence;
	float					m_fThirstyInfluence;
	float					m_fRadiationInfluence;
	float					m_fPsyHealthInfluence;
	float					m_fMaxPowerUpInfluence;
	float					m_fWoundsHealPerc;	//заживление ран на кол-во процентов
	float					m_falcohol;

	//количество порций еды, 
	//-1 - бесконечно количество порций. 1 - одна.
	u32						m_iPortionsNum;

public:
	// ѕовременное использование
	BOOL					bProlongedEffect;
	u8						iEffectorBlockingGroup;
	float					fItemUseTime;
	xr_vector<shared_str>	sEffectList;
	xr_vector<float>		fEffectsRate;
	xr_vector<float>		fEffectsDur;
	xr_vector<u8>			iEffectsAffectedStat;

	xr_vector<BoosterParams>	VectorBoosterParam;

	// ƒл€ некоторых предметов, которые не должны кластьс€ в быстрый слот
	BOOL					notForQSlot_;
private:
	LPCSTR					use_sound_line;
};

