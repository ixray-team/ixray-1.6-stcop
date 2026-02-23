#pragma once

#include "telekinetic_object.h"
#include "../../../xrPhysics/PHUpdateObject.h"

class ITelekineticEnemy
{
public:
	ITelekineticEnemy() = default;
	virtual ~ITelekineticEnemy() = default;
	
	virtual CEntityAlive* get_enemy() = 0;
	virtual float get_tele_distance() = 0;
	virtual u32 get_tele_keep_time()  = 0;
	virtual CBaseMonster* get_self()  = 0;
};

class CTelekinesis : public CPHUpdateObject
{
public:
	using TELEKINETIC_OBJECTS = xr_vector<STelekineticObject*>;
	using TELE_OBJECTS_IT = TELEKINETIC_OBJECTS::iterator;
protected:
	TELEKINETIC_OBJECTS telekinetic_objects;
	xr_vector<ISpatialShared> m_nearest;
	
	bool active;

public:
	CTelekinesis();
	~CTelekinesis() override;
	
	// активировать объект
	virtual void append_tobject(STelekineticObject* telekinetic_object);
	// деактивировать все объекты
	void deactivate();
	//clear objects (does not call release, but call switch to TS_None)
	void clear_deactivate();
	// clear 
	virtual void clear();
	virtual void clear_notrelevant();
	// деактивировать объект
	void deactivate(CPhysicsShellHolder* obj);
	void remove_object(TELE_OBJECTS_IT it);
	void remove_object(CPhysicsShellHolder* obj);
	// бросить все объекты в позицию 'target'
	void throw_all_objects(const Fvector& target);
	// бросить объект 'obj' в позицию 'target' с учетом коэф силы 
	void fire(CPhysicsShellHolder* obj, const Fvector& target, float power);
	// бросить объект 'obj' в позицию 'target' с учетом коэф силы 
	void throw_object_time(CPhysicsShellHolder* obj, const Fvector& target, float time);
	// void weapon_shoot(CPhysicsShellHolder* weapon);
	// вернуть активность телекинеза
	bool is_active() const { return active; }
	// вернуть активность объекта		
	bool is_active_object(CPhysicsShellHolder* obj);
	// вернуть количество контролируемых объектов (в состо€нии TS_Raise & TS_Keep)
	u32 get_controlled_objects_count() const;
	ICF TELEKINETIC_OBJECTS& get_tele_objects() { return telekinetic_objects; }
	ICF STelekineticObject* get_object_by_index(u32 index)
	{
		VERIFY(telekinetic_objects.size() > index);
		return telekinetic_objects[index];
	}
	// обновить состон€ие на shedule_Update			
	void schedule_update();
	// объект был удален - удалить все св€зи на объект
	void remove_links(CObject* O);
	
private:
	// обновление на шагах физики
	void PhDataUpdate(float step) override;
	void PhTune(float step) override;
};