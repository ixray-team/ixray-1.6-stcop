#include "StdAfx.h"
#include "telekinesis.h"
#include "../../entity_alive.h"
#include "../../../xrPhysics/PhysicsShell.h"

struct SFindPred
{
	CPhysicsShellHolder* obj;

	SFindPred(CPhysicsShellHolder* aobj)
	{
		obj = aobj;
	}

	bool operator ()(STelekineticObject* tele_object) const
	{
		return tele_object->get_object() == obj;
	}
};

static bool RemovePred(STelekineticObject* tele_object)
{
	return !tele_object->get_object() ||
		tele_object->get_object()->getDestroy() ||
		!tele_object->get_object()->PPhysicsShell() ||
		!tele_object->get_object()->PPhysicsShell()->isActive();
}

CTelekinesis::CTelekinesis()
{
	active = false;
}

CTelekinesis::~CTelekinesis()
{
	for (STelekineticObject* object : telekinetic_objects)
	{
		object->release();
		xr_delete(object);
	}
}

void CTelekinesis::append_tobject(STelekineticObject* tele_object)
{
	active = true;

	if (tele_object->object->m_pPhysicsShell)
		tele_object->object->m_pPhysicsShell->set_ApplyByGravity(FALSE);
	
	// добавить объект	
	telekinetic_objects.push_back(tele_object);

	if (!telekinetic_objects.empty())
		Activate();
}

void CTelekinesis::clear()
{
	telekinetic_objects.clear();
}

void CTelekinesis::deactivate()
{
	active = false;

	// отпустить все объекты
	for (STelekineticObject* object : telekinetic_objects)
	{
		object->release();
		xr_delete(object);
	}

	clear();
	Deactivate();
}

void CTelekinesis::clear_deactivate()
{
	active = false;

	// отпустить все объекты
	for (STelekineticObject* object : telekinetic_objects)
	{
		object->switch_state(ETelekineticState::TS_NONE);
		xr_delete(object);
	}

	clear();
	Deactivate();
}

void CTelekinesis::deactivate(CPhysicsShellHolder* obj)
{
	// найти объект
	TELE_OBJECTS_IT it = std::find_if(telekinetic_objects.begin(), telekinetic_objects.end(), SFindPred(obj));

	if (it == telekinetic_objects.end())
		return;

	// отпустить объект
	(*it)->release();

	//remove from list, delete...
	remove_object(it);
}

void CTelekinesis::remove_object(CPhysicsShellHolder* obj)
{
	// найти объект
	TELE_OBJECTS_IT it = std::find_if(telekinetic_objects.begin(), telekinetic_objects.end(), SFindPred(obj));

	if (it == telekinetic_objects.end())
		return;

	//remove from list, delete...
	remove_object(it);
}

void CTelekinesis::remove_object(TELE_OBJECTS_IT it)
{
	// release memory
	xr_delete(*it);

	// удалить
	telekinetic_objects.erase(it);

	// проверить на полную деактивацию
	if (telekinetic_objects.empty())
	{
		clear();
		Deactivate();
		active = false;
	}
}

void CTelekinesis::throw_all_objects(const Fvector& target)
{
	if (!active)
		return;

	for (STelekineticObject* object : telekinetic_objects)
		object->throw_object(target, 1.f);

	deactivate();
}

// бросить объект 'obj' в позицию 'target' с учетом коэф силы 
void CTelekinesis::fire(CPhysicsShellHolder* obj, const Fvector& target, float power)
{
	// найти объект
	TELE_OBJECTS_IT it = std::find_if(telekinetic_objects.begin(), telekinetic_objects.end(), SFindPred(obj));

	if (it == telekinetic_objects.end())
		return;

	// бросить объект
	(*it)->throw_object(target, power);
}

void CTelekinesis::throw_object_time(CPhysicsShellHolder* obj, const Fvector& target, float time)
{
	TELE_OBJECTS_IT it = std::find_if(telekinetic_objects.begin(), telekinetic_objects.end(), SFindPred(obj));

	if (it == telekinetic_objects.end())
		return;
	
	(*it)->throw_object_time(target, time);
}

bool CTelekinesis::is_active_object(CPhysicsShellHolder* obj)
{
	// найти объект
	TELE_OBJECTS_IT it = std::find_if(telekinetic_objects.begin(), telekinetic_objects.end(), SFindPred(obj));

	if (it == telekinetic_objects.end())
		return false;

	return true;
}

void CTelekinesis::schedule_update()
{
	if (!active) 
		return;
	
	for (u32 i = 0; i < telekinetic_objects.size(); i++)
	{
		STelekineticObject* cur_obj = telekinetic_objects[i];
		cur_obj->update_state();

		if (cur_obj->is_released())
			remove_object(telekinetic_objects.begin() + i);
	}
}

void CTelekinesis::PhDataUpdate(float step)
{
	if (!active)
		return;

	for (STelekineticObject* object : telekinetic_objects)
	{
		switch (object->get_state())
		{
		case ETelekineticState::TS_RAISE:
			object->raise(step);
			break;

		case ETelekineticState::TS_KEEP:
			object->perform_keep_object();
			break;

		case ETelekineticState::TS_NONE:
			break;

		default: ;
		}
	}
}

void CTelekinesis::clear_notrelevant()
{
	//убрать все объеты со старыми параметрами
	telekinetic_objects.erase(
		std::remove_if(
			telekinetic_objects.begin(),
			telekinetic_objects.end(),
			&RemovePred
		),
		telekinetic_objects.end()
	);
}

void CTelekinesis::PhTune(float step)
{
	if (!active)
		return;

	clear_notrelevant();

	for (STelekineticObject* telekinetic_object : telekinetic_objects)
	{
		switch (telekinetic_object->get_state())
		{
		case ETelekineticState::TS_RAISE:
		case ETelekineticState::TS_KEEP:
			telekinetic_object->enable();

		case ETelekineticState::TS_NONE:
			break;
		default: ;
		}
	}
}

u32 CTelekinesis::get_controlled_objects_count() const
{
	u32 count = 0;

	for (STelekineticObject* object : telekinetic_objects)
	{
		ETelekineticState state = object->get_state();

		if (state == ETelekineticState::TS_KEEP)
			count++;
	}
	return count;
}

// объект был удален - удалить все связи на объект
void CTelekinesis::remove_links(CObject* O)
{
	remove_object(O->cast_physics_shell_holder());
}
