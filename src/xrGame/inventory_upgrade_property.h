////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade_property.h
//	Created 	: 22.11.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov
//	Description : inventory upgrade property class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "inventory_upgrade.h"

class inventory::upgrade::Property final
{
public:
	using FunctorParams_type = xr_vector<shared_str>;

private:
	using StrFunctor = functor2<const char*>;

public:
	Property(const Property& other) = delete;
	Property& operator=(const Property& other) = delete;
	Property() = default;
	virtual	~Property() = default;

	void construct(const shared_str& property_id, Manager& manager_r);
	IC shared_str const& id() const { return m_id; }
	IC const char* id_str() const { return m_id.c_str(); }
	IC const char* icon_name() const { return m_icon.c_str(); }
	IC const char* name() const { return m_name.c_str(); }
	IC u32 icon_color() const { return m_color; }

	IC FunctorParams_type const& functor_params() const { return m_functor_params; }

	bool run_functor(const char* parameter, string256& result);

protected:
	shared_str m_id;

	shared_str m_name;
	shared_str m_icon;
	u32 m_color = 0xFFFFFFFF;

	StrFunctor m_desc;
	FunctorParams_type m_functor_params;

}; // class Property

