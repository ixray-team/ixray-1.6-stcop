#include "StdAfx.h"
#include "UIWorldSpace.h"
#include "../Include/xrRender/Kinematics.h"

static Fvector GetBonePosition(const CGameObject* obj, u16 bone_id) // TODO Emmis: ������ �� ��� ���� ���?
{
	Fmatrix matrix;
	matrix.mul_43(obj->XFORM(), PKinematics(obj->Visual())->LL_GetTransform(bone_id));
	return matrix.c;
}

CWorldSpaceElement::CWorldSpaceElement(const CGameObject* attached_object, const char* section)
	: m_hasAttachedBone(xr_strcmp(pSettings->read_if_exists<str_c>(section, "attach_bone", "nil"), "nil") != 0)
	, m_show(pSettings->read_if_exists<bool>(section, "show", true))
	, m_attachedBoneID(BI_NONE)
	, m_showDistance(pSettings->read_if_exists<float>(section, "show_distance", 10.0f))
{
	if (m_hasAttachedBone)
	{
		m_attachedBoneName = pSettings->r_string(section, "attach_bone");

		IKinematics* K = PKinematics(attached_object->Visual());
		if (!K)
		{
			Msg("! WARNING: CWorldSpaceElement: cant attach to bone! Object [%s] has no skeleton!", attached_object->cName().c_str());
			m_hasAttachedBone = false;
		}
		else
		{
			m_attachedBoneID = K->LL_BoneID(m_attachedBoneName);

			if (m_attachedBoneID == BI_NONE)
			{
				Msg("! WARNING: CWorldSpaceElement: cant find bone [%s] in object [%s]", m_attachedBoneName.c_str(), attached_object->cName().c_str());
				m_hasAttachedBone = false;
			}
		}
	}

	m_pBillboard = new CBillboard(pSettings->read_if_exists<str_c>(section, "billboard", "ws_default_element"));
}

CWorldSpaceElement::~CWorldSpaceElement()
{
	xr_delete(m_pBillboard);
}

CUIWorldSpaceManager::~CUIWorldSpaceManager()
{
	for (auto& elements : m_attachedElements)
	{
		for (auto& element : elements.second)
		{
			xr_delete(element.ws_element);
		}
	}
	m_attachedElements.clear();
}

CWorldSpaceElement* CUIWorldSpaceManager::CreateWorldSpaceElement(const CGameObject* attached_object, const char* section)
{
	if (attached_object == nullptr)
	{
		Msg("! WARNING: cant create WorldSpaceElement: attached_object is nullptr!");
		return nullptr;
	}
	else if (!pSettings->section_exist(section))
	{
		Msg("! WARNING: cant create WorldSpaceElement: is no section! [%s]", section);
		return nullptr;
	}

	if (ObjectHasWorldSpaceElement(attached_object, section))
	{
		DestroyWorldSpaceElement(attached_object, section);
	}

	CWorldSpaceElement* newElement = new CWorldSpaceElement(attached_object, section);
	m_attachedElements[attached_object].push_back({newElement, section});

	return newElement;
}

CWorldSpaceElement* CUIWorldSpaceManager::GetWorldSpaceElement(const CGameObject* attached_object, const char* section) const
{
	auto it = m_attachedElements.find(attached_object);
	if (it != m_attachedElements.end())
	{
		for (auto& element : it->second)
		{
			if (element.section == section)
			{
				return element.ws_element;
			}
		}
	}
	return nullptr;
}
// TODO Emmis: ����� ����� ���������� ���������� ��������, �� ������ ��� ���� �����
void CUIWorldSpaceManager::ElementSetText(const CGameObject* attached_object, const char* section, const char* text)
{
	CWorldSpaceElement* ws_element = GetWorldSpaceElement(attached_object, section);
	if (ws_element)
	{
		if (CBillboard* billboard = ws_element->GetBillboard())
		{
			billboard->TextItemControl()->SetTextST(text);
		}
		else
		{
			Msg("! WARNING: cant set text to WorldSpaceElement: object [%s] with world space section [%s] is has no billboard", attached_object->cName().c_str(), section);
		}
	}
	else
	{
		Msg("! WARNING: cant set text to WorldSpaceElement: section [%s] not found for object [%s]", section, attached_object->cName().c_str());
	}
}

void CUIWorldSpaceManager::ElementShow(const CGameObject* attached_object, const char* section, bool show)
{
	CWorldSpaceElement* ws_element = GetWorldSpaceElement(attached_object, section);
	if (ws_element)
	{
		ws_element->SetShow(show);
	}
	else
	{
		Msg("! WARNING: cant set show to WorldSpaceElement: section [%s] not found for object [%s]", section, attached_object->cName().c_str());
	}
}

void CUIWorldSpaceManager::DestroyWorldSpaceElement(const CGameObject* attached_object, const char* section)
{
	if (!ObjectHasWorldSpaceElement(attached_object, section))
	{
		Msg("! WARNING: cant destroy WorldSpaceElement: object [%s] doesnt have WorldSpaceElement with section [%s]", attached_object->cName().c_str(), section);
		return;
	}

	auto it = m_attachedElements.find(attached_object);
	if (it != m_attachedElements.end())
	{
		auto& elements = it->second;
		for (auto element = elements.begin(); element != elements.end(); ++element)
		{
			if (element->section == section)
			{
				xr_delete(element->ws_element);
				elements.erase(element);
				break;
			}
		}

		if (elements.empty())
		{
			m_attachedElements.erase(it);
		}
	}
}

void CUIWorldSpaceManager::DestroyWorldSpaceElements(const CGameObject* attached_object)
{
	auto it = m_attachedElements.find(attached_object);
	if (it != m_attachedElements.end())
	{
		for (auto& element : it->second)
		{
			xr_delete(element.ws_element);
		}
		m_attachedElements.erase(it);
	}
}

bool CUIWorldSpaceManager::ObjectHasWorldSpaceElement(const CGameObject* attached_object, const char* section) const
{
	return GetWorldSpaceElement(attached_object, section) != nullptr;
}

bool CUIWorldSpaceManager::ObjectHasWorldSpaceElements(const CGameObject* attached_object) const
{
	auto it = m_attachedElements.find(attached_object);
	return (it != m_attachedElements.end() && !it->second.empty());
}

void CUIWorldSpaceManager::OnRender()
{
	if (m_attachedElements.empty())
	{
		return;
	}

	for (const auto& [obj, elements] : m_attachedElements)
	{
		if (!obj || obj->getDestroy())
		{
			continue;
		}

		for (const auto& att_element : elements)
		{
			CWorldSpaceElement* element = att_element.ws_element;
			if (!element || !element->GetShow())
			{
				continue;
			}

			Fvector pos;
			pos = element->HasAttachedBone() ? GetBonePosition(obj, element->GetAttachedBoneID()) : obj->Position();

			float dist_to_camera = pos.distance_to_sqr(Device.vCameraPosition);
			float show_dist = element->GetShowDistance();
			if (dist_to_camera > show_dist * show_dist)
			{
				continue;
			}

			element->GetBillboard()->DrawBillboard(pos);
		}
	}
}