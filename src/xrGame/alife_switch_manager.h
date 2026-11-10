////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_switch_manager.h
//	Created 	: 25.12.2002
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator switch manager
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "alife_simulator_base.h"

class CALifeSwitchManager : public virtual CALifeSimulatorBase, CRandom {
protected:
	typedef CALifeSimulatorBase				inherited;
	typedef ALife::OBJECT_VECTOR			OBJECT_VECTOR;

protected:
	float			m_switch_distance;
	float			m_switch_factor;
	float			m_online_distance;
	float			m_offline_distance;
	mutable Fvector	m_switch_centre;
	mutable u32		m_switch_centre_frame = u32(-8);

private:
	OBJECT_VECTOR	m_saved_chidren;
	using SavedChildrenTree = xr_hash_map<ALife::_OBJECT_ID, OBJECT_VECTOR>;
	SavedChildrenTree SavedChildrenTreeData;
	void CollectChildrenTree(CSE_ALifeDynamicObject* Object);

protected:
			bool	synchronize_location	(CSE_ALifeDynamicObject	*object);

public:
			void	try_switch_online		(CSE_ALifeDynamicObject	*object);
			void	try_switch_offline		(CSE_ALifeDynamicObject	*object);
			void	switch_online			(CSE_ALifeDynamicObject	*object);
			void	switch_offline			(CSE_ALifeDynamicObject	*object);
			void	remove_online			(CSE_ALifeDynamicObject	*object, bool update_registries = true);
			void	add_online				(CSE_ALifeDynamicObject	*object, bool update_registries = true);
			const ALife::OBJECT_VECTOR* SavedChildren(ALife::_OBJECT_ID Id) const;

public:
	IC				CALifeSwitchManager		(xrServer *server, const char* section);
	virtual ~CALifeSwitchManager	() override;
			void	switch_object			(CSE_ALifeDynamicObject	*object);
	IC		float	online_distance			() const;
	IC		float	offline_distance		() const;
	IC		float	switch_distance			() const;
	IC		void	set_switch_distance		(float switch_distance);
	IC		void	set_switch_factor		(float switch_factor);
			void	set_switch_centre		(const Fvector& centre) const;
			const Fvector& switch_centre	() const;
};

#include "alife_switch_manager_inline.h"