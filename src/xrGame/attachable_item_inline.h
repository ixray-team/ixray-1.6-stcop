////////////////////////////////////////////////////////////////////////////
//	Module 		: attachable_item_inline.h
//	Created 	: 11.02.2004
//  Modified 	: 11.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Attachable item inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC CAttachableItem::CAttachableItem()
{
	m_item = 0;
	m_offset.identity();
	m_bone_name = "";
	m_enabled = true;
#ifdef DEBUG
	m_valid = false;
#endif
}

IC shared_str CAttachableItem::bone_name() const
{
	VERIFY(m_valid);
	return (m_bone_name);
}

IC const Fmatrix& CAttachableItem::offset() const
{
	VERIFY(m_valid);
	m_offset.setHPB(VPUSH(m_offset_rotation));
	m_offset.translate_over(m_offset_position);
	return m_offset;
}

IC u16 CAttachableItem::bone_id() const
{
	VERIFY(m_valid);
	return (m_bone_id);
}

IC void CAttachableItem::set_bone_id(u16 bone_id)
{
	VERIFY(m_valid);
	m_bone_id = bone_id;
}

IC void CAttachableItem::set_bone_name(const shared_str& bone_name)
{
	VERIFY(m_valid);
	m_bone_name = bone_name;
}

IC bool CAttachableItem::enabled() const
{
	return (m_enabled);
}

IC CInventoryItem& CAttachableItem::item() const
{
	VERIFY(m_item);
	return (*m_item);
}
