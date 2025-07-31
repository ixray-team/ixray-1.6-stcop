#include "stdafx.h"
#include "../xrECore/Editor/Library.h"

xr_token sa_token[] = 
{
	{ "+Z", PreviewModel::saZp	},
	{ "-Z", PreviewModel::saZn	},
	{ "+X", PreviewModel::saXp	},
	{ "-X", PreviewModel::saXn	},
	{ 0,	0					}
};

void PreviewModel::OnCreate()
{
	m_Props = new UIPropertiesForm;
}

void PreviewModel::OnDestroy()
{
	xr_delete(m_Props);
}

void PreviewModel::Draw()
{
	UIChooseForm::Update();

	bool change = true;
	shared_str result;
	if (UIChooseForm::GetResult(change, result))
	{
		if (result.size() > 0)
		{
			Lib.RemoveEditObject(m_pObject);
			m_pObject = Lib.CreateEditObject(result.c_str());

			if (!m_pObject)
				ELog.DlgMsg(mtError, "Object '%s' can't find in object library.", result.c_str());
			else
				m_LastObjectName = result.c_str();
		}

		bOpen = false;
		UI->Remove(this);
	}
}

void PreviewModel::Clear()
{
	Lib.RemoveEditObject(m_pObject);
	UI->RedrawScene();
}

void PreviewModel::SelectObject()
{
	UIChooseForm::SelectItem(smObject, 1, 0, 0, 0, 0, 0, 0);
	UI->Push(this, false);
	bOpen = true;
}

void PreviewModel::SetPreferences()
{
	PropItemVec items;
	PHelper().CreateFlag32(items, "Scroll", &m_Flags, pmScroll);
	PHelper().CreateFloat(items, "Speed (m/c)", &m_fSpeed, -10000.f, 10000.f, 0.01f, 2);
	PHelper().CreateFloat(items, "Segment (m)", &m_fSegment, -10000.f, 10000.f, 0.01f, 2);
	PHelper().CreateToken32(items, "Scroll axis", (u32*)&m_ScrollAxis, sa_token);
	m_Props->AssignItems(items);
}

void PreviewModel::Render()
{
	if (m_pObject)
	{
		float angle = 0.f;
		switch (m_ScrollAxis)
		{
		case saZp: angle = 0;		break;
		case saZn: angle = PI;		break;
		case saXp: angle = PI_DIV_2; break;
		case saXn: angle = -PI_DIV_2; break;
		default: THROW;
		}

		Fmatrix R, T;
		R.rotateY(angle);
		T.translate(m_vPosition);
		T.mulA_43(R);
		m_pObject->RenderSkeletonSingle(T);
	}
}

void PreviewModel::Update()
{
	if (m_Flags.is(pmScroll))
	{
		m_vPosition.z += m_fSpeed * EDevice->fTimeDelta;
		if (m_vPosition.z > m_fSegment) m_vPosition.z -= m_fSegment;
	}
}
