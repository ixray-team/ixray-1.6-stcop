////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_upgrade.h
//	Created 	: 01.11.2007
//  Modified 	: 27.11.2007
//	Author		: Evgeniy Sokolov
//	Description : inventory upgrade class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "inventory_upgrade_base.h"
#include "inventory_upgrade_group.h"

namespace inventory::upgrade {

	enum UpgradeStateResultScript
	{
		result_script_ok = 0,

		// Call of Pripyat meaning
		result_script_e_cant_do = 1,
		result_script_e_precondition_any = 2,

		// Clear Sky meaning
		result_script_e_precondition_money = 1,
		result_script_e_precondition_quest = 2,
	};

template <typename return_type>
struct functor_base
{
	using functor_type = luabind::functor<return_type>;

	functor_type functr;
	const char* parameter;
};

template <typename return_type>
struct functor : public functor_base<return_type>
{
	IC return_type operator()() const
	{
		return this->functr(this->parameter);
	}
};

template <typename return_type>
struct functor2 : public functor<return_type>
{
	const char* parameter2;

	IC return_type operator()() const
	{
		return this->functr(this->parameter, parameter2);
	}
};

template <typename return_type>
struct functor3 : public functor2<return_type>
{
	int parameter3;

	IC return_type operator()() const
	{
		return this->functr(this->parameter, this->parameter2, parameter3);
	}
};

template <>
struct functor<void> : public functor_base<void>
{
	IC void operator()() const
	{
		functr(parameter);
	}
};

template <>
struct functor2<void> : public functor<void>
{
	const char* parameter2;
	IC	void operator()() const
	{
		functr(parameter, parameter2);
	}
};

template <>
struct functor3<void> final : public functor2<void>
{
	int	parameter3;
	IC void operator()() const
	{
		functr(parameter, parameter2, parameter3);
	}
};

enum EMaxProps
{
	max_properties_count = 3,
};

class Upgrade final : public UpgradeBase
{
private:
	using inherited = UpgradeBase;
public:
	Upgrade() = default;
	virtual	~Upgrade() = default;
	void construct(const shared_str& upgrade_id, Group& parental_group, Manager& manager_r);

	IC const char* section() const { return m_section.c_str(); }
	IC shared_str const& parent_group_id() const { return m_parent_group->id(); }
	IC Group const* parent_group() const { return m_parent_group; }
	IC const char* icon_name() const { return m_icon.c_str(); }
	IC const char* name() const { return m_name.c_str(); }
	IC const char* description_text() const { return m_description.c_str(); }

	const char* get_prerequisites();
	IC bool get_highlight() const { return m_highlight; }
	IC shared_str const& get_property_name(u8 index = 0) const
	{
		VERIFY(index < max_properties_count && index >= 0);
		return m_properties[index];
	}

	IC Ivector2 const& get_scheme_index() const { return m_scheme_index; }

#ifdef DEBUG
	virtual	void log_hierarchy(const char* nest) override;
#endif // DEBUG

	virtual	void fill_root_container(Root* root) override;

	virtual	UpgradeStateResult can_install(CInventoryItem& item, bool loading) override;
	bool check_scheme_index(const Ivector2& scheme_index) const;
	void set_highlight(bool value);
	void run_effects(bool loading);

	void RefreshTranslations();

	virtual	void highlight_up() override;
	virtual	void highlight_down() override;

protected:
	using BoolFunctor = functor<bool>;
	using BoolFunctor2 = functor2<bool>;
	using VoidFunctor = functor<void>;
	using VoidFunctor2 = functor2<void>;
	using VoidFunctor3 = functor3<void>;

	using StrFunctor = functor2<const char*>;
	using IntFunctor = functor2<int>;

protected:
	Group* m_parent_group = nullptr;

	shared_str m_section;
	Ivector2 m_scheme_index;

	shared_str m_name;
	shared_str m_description;
	shared_str m_icon;
	shared_str m_properties[max_properties_count];

	IntFunctor m_preconditions;
	VoidFunctor3 m_effects;
	StrFunctor m_prerequisites;

	bool m_highlight = false;

};
} //namespace inventory::upgrade
