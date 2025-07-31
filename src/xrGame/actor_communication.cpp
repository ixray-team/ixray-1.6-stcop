#include "StdAfx.h"
#include "pch_script.h"
#include "Actor.h"
#include "UIGameSP.h"
#include "PDA.h"
#include "Level.h"
#include "../xrEngine/string_table.h"
#include "PhraseDialog.h"
#include "character_info.h"
#include "relation_registry.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_registry_container.h"
#include "script_game_object.h"
#include "game_cl_base.h"
#include "xrServer.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_registry_wrappers.h"
#include "map_manager.h"
#include "ui/UIMainIngameWnd.h"
#include "ui/UIPdaWnd.h"
#include "ui/UITalkWnd.h"
#include "game_object_space.h"
#include "encyclopedia_article.h"
#include "GametaskManager.h"
#include "GameTaskDefs.h"
#include "InfoPortion.h"
#include "Inventory.h"
#include "CustomDetector.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/trader/ai_trader.h"
#include "../xrScripts/script_callback_ex.h"

void  CActor::AddGameNews			 (GAME_NEWS_DATA& news_data)
{

	GAME_NEWS_VECTOR& news_vector	= game_news_registry->registry().objects();
	news_data.receive_time			= Level().GetGameTime();
	news_vector.push_back			(news_data);

	if ( CurrentGameUI() )
	{
		CurrentGameUI()->UIMainIngameWnd->ReceiveNews(&news_data);
	}
}


bool CActor::OnReceiveInfo(shared_str info_id) const
{
	if(!CInventoryOwner::OnReceiveInfo(info_id))
		return false;

	CInfoPortion info_portion;
	info_portion.Load(info_id);

	callback(GameObject::eInventoryInfo)(lua_game_object(), *info_id);


	//только если находимся в режиме single
 	if(CurrentGameUI() == nullptr) return false;

	if(CurrentGameUI()->TalkMenu->IsShown())
	{
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
	}

	return true;
}


void CActor::OnDisableInfo(shared_str info_id) const
{
	CInventoryOwner::OnDisableInfo(info_id);

	if(CurrentGameUI() == nullptr)
		return;

	//только если находимся в режиме single
	if(CurrentGameUI()->TalkMenu->IsShown())
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
}

void  CActor::ReceivePhrase		(DIALOG_SHARED_PTR& phrase_dialog)
{
	//только если находимся в режиме single
 	if(CurrentGameUI() == nullptr) return;

	if(CurrentGameUI()->TalkMenu->IsShown())
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();

	CPhraseDialogManager::ReceivePhrase(phrase_dialog);
}

void CActor::UpdateAvailableDialogs(CPhraseDialogManager* partner)
{
	m_AvailableDialogs.clear();
	m_CheckedDialogs.clear();

	//добавить актерский диалог собеседника
	CInventoryOwner* pInvOwnerPartner = partner->cast_inventory_owner();
	VERIFY(pInvOwnerPartner);

	for (u32 i = 0; i < pInvOwnerPartner->CharacterInfo().ActorDialogs().size(); i++)
	{
		AddAvailableDialog(pInvOwnerPartner->CharacterInfo().ActorDialogs()[i], partner);
	}

	if (EngineExternal().ClearSkyMode())
	{
		AddAvailableDialog("actor_break_dialog", partner);
	}

	CPhraseDialogManager::UpdateAvailableDialogs(partner);
}

void CActor::TryToTalk()
{
	if(!IsTalking())
	{
		RunTalkDialog(m_pPersonWeLookingAt, false);
	}
}

void CActor::RunTalkDialog(CInventoryOwner* talk_partner, bool disable_break)
{
	//предложить поговорить с нами
	if(talk_partner->OfferTalk(this))
	{	
		StartTalk(talk_partner);

		if (CurrentGameUI()->TopInputReceiver())
		{
			CurrentGameUI()->TopInputReceiver()->HideDialog();
		}
		bool disableBreakDialog = EngineExternal().ClearSkyMode() ? disable_break : talk_partner->bDisableBreakDialog;
 		CurrentGameUI()->StartTalk(disableBreakDialog);
	}
}

void CActor::StartTalk (CInventoryOwner* talk_partner)
{
	VERIFY(talk_partner->cast_game_object());
	CInventoryOwner::StartTalk(talk_partner);
}

void CActor::NewPdaContact		(CInventoryOwner* pInvOwner)
{	
	if(!IsGameTypeSingle()) return;

	bool b_alive = !!pInvOwner->cast_game_object()->cast_entity_alive()->g_Alive();
	CurrentGameUI()->UIMainIngameWnd->AnimateContacts(b_alive);

	Level().MapManager().AddRelationLocation		( pInvOwner );
}

void CActor::LostPdaContact		(CInventoryOwner* pInvOwner)
{
	Level().MapManager().RemoveRelationLocation(pInvOwner);
}

void CActor::AddGameNews_deffered	 (GAME_NEWS_DATA& news_data, u32 delay)
{
	GAME_NEWS_DATA * d = new GAME_NEWS_DATA(news_data);
	//*d = news_data;
	m_defferedMessages.push_back( SDefNewsMsg() );
	m_defferedMessages.back().news_data = d;
	m_defferedMessages.back().time = Device.dwTimeGlobal+delay;
	std::sort(m_defferedMessages.begin(), m_defferedMessages.end() );
}

void CActor::UpdateDefferedMessages()
{
	while( m_defferedMessages.size() )
	{
		SDefNewsMsg& M = m_defferedMessages.back();
		if(M.time <=Device.dwTimeGlobal)
		{
			AddGameNews					(*M.news_data);		
			xr_delete					(M.news_data);
			m_defferedMessages.pop_back	();
		}else
			break;
	}
}

bool CActor::OnDialogSoundHandlerStart(CInventoryOwner *inv_owner, LPCSTR phrase)
{
	if (CAI_Trader* trader = inv_owner->cast_trader())
	{
		trader->dialog_sound_start(phrase);
		return true;
	}

	return false;
}

bool CActor::OnDialogSoundHandlerStop(CInventoryOwner *inv_owner)
{
	if (CAI_Trader* trader = inv_owner->cast_trader())
	{
		trader->dialog_sound_stop();
		return true;
	}

	return false;
}

#ifdef DEBUG
void CActor::DumpTasks()
{
	Level().GameTaskManager()->DumpTasks();
}
#endif // DEBUG