///////////////////////////////////////////////////////////////
// InfoDocument.cpp
// InfoDocument - документ, содержащий сюжетную информацию
///////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "InfoDocument.h"
#include "../xrPhysics/PhysicsShell.h"
#include "PDA.h"
#include "InventoryOwner.h"
#include "xrServer_Objects_ALife_Items.h"

CInfoDocument::CInfoDocument(void) 
{
	m_Info = nullptr;
}

CInfoDocument::~CInfoDocument(void) 
{
}


bool CInfoDocument::net_Spawn(CSE_Abstract* DC) 
{
	bool					res = inherited::net_Spawn(DC);

	CSE_Abstract			*l_tpAbstract = static_cast<CSE_Abstract*>(DC);
	CSE_ALifeItemDocument	*l_tpALifeItemDocument = smart_cast<CSE_ALifeItemDocument*>(l_tpAbstract);
	R_ASSERT				(l_tpALifeItemDocument);

	m_Info					= l_tpALifeItemDocument->m_wDoc;

	return					(res);
}

void CInfoDocument::Load(const char* section) 
{
	inherited::Load(section);
}

void CInfoDocument::net_Destroy() 
{
	inherited::net_Destroy();
}

void CInfoDocument::shedule_Update(u32 dt) 
{
	inherited::shedule_Update(dt);
}

void CInfoDocument::UpdateCL() 
{
	inherited::UpdateCL();
}


void CInfoDocument::OnH_A_Chield() 
{
	inherited::OnH_A_Chield		();
	
	//передать информацию содержащуюся в документе
	//объекту, который поднял документ
	CInventoryOwner* pInvOwner = H_Parent() != nullptr ? H_Parent()->cast_inventory_owner() : nullptr;
	if(!pInvOwner) return;
	
	//создать и отправить пакет о получении новой информации
	if(m_Info.size())
	{
		NET_Packet		P;
		u_EventGen		(P,GE_INFO_TRANSFER, H_Parent()->ID());
		P << ID();						//отправитель
		P.w_stringZ		(m_Info);				//сообщение
		P.w_u8			(1);						//добавление сообщения
		u_EventSend		(P);
	}
}

void CInfoDocument::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
}

void CInfoDocument::renderable_Render() 
{
	inherited::renderable_Render();
}