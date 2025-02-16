////////////////////////////////////////////////////////////////////////////
//	Module 		: purchase_list_inline.h
//	Created 	: 12.01.2006
//  Modified 	: 12.01.2006
//	Author		: Dmitriy Iassenev
//	Description : purchase list class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	float CPurchaseList::deficit							(const shared_str &section) const
{
	DEFICITS::const_iterator	I = m_deficits.find(section);
    if (I != m_deficits.end())
		return					((*I).second);

	return						(1.f);
}

IC	const CPurchaseList::DEFICITS &CPurchaseList::deficits	() const
{
	return						(m_deficits);
}

IC	void CPurchaseList::deficit								(const shared_str &section, const float &deficit)
{
	DEFICITS::iterator			I = m_deficits.find(section);
	if (I != m_deficits.end()) {
		(*I).second				= deficit;
		return;
	}

	m_deficits.insert			(std::make_pair(section,deficit));
}

template <typename Container>
IC	void CPurchaseList::process(Container cnt, CInventoryOwner &owner)
{
	const CGameObject &game_object = smart_cast<const CGameObject &>(owner);
	owner.sell_useless_items();
	m_deficits.clear		();

	for (auto I = cnt.begin(); I != cnt.end(); ++I) {
		int					count = _GetItemCount(*(*I).second);
		THROW3				(count <= 2,"Invalid parameters for item", *(*I).first);
		if (count > 0)
		{
			string256			temp0, temp1;
			process			(
				game_object,
				(*I).first,
				atoi(_GetItem(*(*I).second,0,temp0)),
				(float)atof(_GetItem(*(*I).second,1,temp1))
			);
		}
	}
}
