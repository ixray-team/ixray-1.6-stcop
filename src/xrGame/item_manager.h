////////////////////////////////////////////////////////////////////////////
//	Module 		: item_manager.h
//	Created 	: 27.12.2003
//  Modified 	: 27.12.2003
//	Author		: Dmitriy Iassenev
//	Description : Item manager
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "object_manager.h"
#include "GameObject.h"

class CCreature;

class CItemManager : public CObjectManager<const CGameObject > {
public:
	typedef CObjectManager<const CGameObject >	inherited;
	typedef OBJECTS								ITEMS;

private:
	CCreature		*m_object;
	CAI_Stalker			*m_stalker;

public:
						CItemManager	(CCreature *object);
	virtual bool		useful			(const CGameObject *object) const;
	virtual bool		is_useful		(const CGameObject *object) const;
	virtual	float		evaluate		(const CGameObject *object) const;
	virtual	float		do_evaluate		(const CGameObject *object) const;
	virtual void		update			();
			void		remove_links	(CObject *object);
	virtual	void		on_restrictions_change	();
};