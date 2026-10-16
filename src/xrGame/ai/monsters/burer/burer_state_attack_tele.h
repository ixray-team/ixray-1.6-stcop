#pragma once
#include "../state.h"

template <typename Object>
class CStateBurerAttackTele : public CState<Object>
{
	using inherited = CState<Object>;

	xr_vector<CPhysicsShellHolder*> tele_objects;
	ALife::_OBJECT_ID SelectedId = ALife::INVALID_OBJECT_ID;
	xr_vector<ISpatialShared> m_nearest;

	u32 time_started;

	TTime m_anim_end_tick;
	TTime m_end_tick;
	float m_initial_health;
	bool CompletionReached = false;

	enum
	{
		ACTION_TELE_STARTED,
		ACTION_TELE_CONTINUE,
		ACTION_TELE_FIRE,
		ACTION_WAIT_FIRE_END,
		ACTION_COMPLETED,
	} m_action;

public:
	CStateBurerAttackTele(Object* obj);

	virtual void initialize();
	virtual void execute();
	virtual void finalize();
	virtual void critical_finalize();
	virtual void remove_links(CObject* Released)
	{
		inherited::remove_links(Released);
		if (Released->ID() == SelectedId)
		{
			SelectedId = ALife::INVALID_OBJECT_ID;
		}
		std::erase(tele_objects, Released->cast_physics_shell_holder());
	}

	virtual bool check_start_conditions();
	virtual bool check_completion();


private:
	// Поиск объектов для телекинеза
	void FindObjects();

	// выполнять состояние
	void ExecuteTeleContinue();
	void ExecuteTeleFire();

	// Проверка, есть ли хоть один объект под контролем
	bool IsActiveObjects();

	// Проверить, может ли стартовать телекинез
	bool CheckTeleStart();
	// Выбор подходящих объектов для телекинеза
	void SelectObjects();

	// internal for FindObjects
	void FindFreeObjects(xr_vector<ISpatialShared>& tpObjects, const Fvector& pos);

	void FireAllToEnemy();
	void deactivate(bool Fire);
};

#include "burer_state_attack_tele_inline.h"
