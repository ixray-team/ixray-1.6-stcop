#pragma once

#include "GameObject.h"
#include "../xrUI/Widgets/Billboard.h"

class CWorldSpaceElement
{
public:
	CWorldSpaceElement(const CGameObject* attached_object, const char* section);
	~CWorldSpaceElement();

	void SetShow(bool show = true) { m_show = show; };

	bool HasAttachedBone() const { return m_hasAttachedBone; };

	bool GetShow() const { return m_show; };
	shared_str GetAttachedBoneName() const { return m_attachedBoneName; };
	u16 GetAttachedBoneID() const { return m_attachedBoneID; };
	float GetShowDistance() const { return m_showDistance; };
	CBillboard* GetBillboard() const { return m_pBillboard; };

private:
	bool m_hasAttachedBone;
	bool m_show;
	shared_str m_attachedBoneName;
	u16 m_attachedBoneID;
	float m_showDistance;
	CBillboard* m_pBillboard;
};

class CUIWorldSpaceManager // а € мог бы насрать синглтоном
{
public:
	CUIWorldSpaceManager() = default;
	~CUIWorldSpaceManager();

	void OnRender();

	CWorldSpaceElement* CreateWorldSpaceElement(const CGameObject* attached_object, const char* section);
	CWorldSpaceElement* GetWorldSpaceElement(const CGameObject* attached_object, const char* section) const;

	void ElementSetText(const CGameObject* attached_object, const char* section, const char* text);
	void ElementShow(const CGameObject* attached_object, const char* section, bool show);

	void DestroyWorldSpaceElements(const CGameObject* attached_object);
	void DestroyWorldSpaceElement(const CGameObject* attached_object, const char* section);

	bool ObjectHasWorldSpaceElements(const CGameObject* attached_object) const;
	bool ObjectHasWorldSpaceElement(const CGameObject* attached_object, const char* section) const;

private:
	struct SAttachedElement
	{
		CWorldSpaceElement* ws_element;
		shared_str section;
	};

	xr_hash_map<const CGameObject*, xr_vector<SAttachedElement>> m_attachedElements;
};