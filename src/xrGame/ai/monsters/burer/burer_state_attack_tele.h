#pragma once
#include "../state.h"
#include "../../../grenade.h"

class CStateBurerAttackTele : public CState {
protected:
	using inherited = CState;

	xr_vector<CPhysicsShellHolder *>	tele_objects;
	CPhysicsShellHolder					*selected_object;
	xr_vector<CObject*>					m_nearest;
	
	u32									time_started;

	CBurerBase* pBurerBase;

	enum {
		ACTION_TELE_STARTED,
		ACTION_TELE_CONTINUE,
		ACTION_TELE_FIRE,
		ACTION_WAIT_FIRE_END,
		ACTION_COMPLETED,
	} m_action;

public:
						CStateBurerAttackTele	(CBaseMonster* object);
						virtual ~CStateBurerAttackTele() override;

	virtual	void		initialize				() override;
	virtual	void		execute					() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;

private:
			// Поиск объектов для телекинеза	
			void		FindObjects				();

			void		HandleGrenades			();

			// выполнять состояние
			void		ExecuteTeleContinue		();
			void		ExecuteTeleFire			();

			// Проверка, есть ли хоть один объект под контролем
			bool		IsActiveObjects			();

			// Проверить, может ли стартовать телекинез
			bool		CheckTeleStart			();
			// Выбор подходящих объектов для телекинеза
			void		SelectObjects			();

			// internal for FindObjects
			void		FindFreeObjects			(xr_vector<CObject*> &tpObjects, const Fvector &pos);
			void  OnGrenadeDestroyed	(CGrenade* const grenade);

			void		FireAllToEnemy			();
			void		deactivate				();

private:
	TTime				m_last_grenade_scan;
	TTime				m_anim_end_tick;
	TTime				m_end_tick;
	float				m_initial_health;
};
