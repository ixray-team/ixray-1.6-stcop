#include "StdAfx.h"
#include "pch_script.h"
#include "Actor.h"
#include "UIGameSP.h"
#include "alife_registry_wrappers.h"
#include "map_manager.h"
#include "ui/UIMainIngameWnd.h"
#include "ui/UIPdaWnd.h"
#include "ui/UITalkWnd.h"
#include "encyclopedia_article.h"
#include "GametaskManager.h"
#include "InfoPortion.h"
#include "../xrScripts/script_callback_ex.h"
#include "game_cl_single.h"
#include "ui/UIPdaAux.h"
#include "pda_communication.h"

void CActor::AddEncyclopediaArticle(const CInfoPortion* info_portion) const
{
    VERIFY(info_portion);
    ARTICLE_VECTOR& article_vector = encyclopedia_registry->registry().objects();

    auto last_end = article_vector.end();
    auto B = article_vector.begin();
    auto E = last_end;

    for (ARTICLE_ID_VECTOR::const_iterator it = info_portion->ArticlesDisable().begin();
        it != info_portion->ArticlesDisable().end(); it++)
    {
        FindArticleByIDPred pred(*it);
        last_end = std::remove_if(B, last_end, pred);
    }
    article_vector.erase(last_end, E);


    for (ARTICLE_ID_VECTOR::const_iterator it = info_portion->Articles().begin();
        it != info_portion->Articles().end(); it++)
    {
        FindArticleByIDPred pred(*it);
        if (std::find_if(article_vector.begin(), article_vector.end(), pred) != article_vector.end()) continue;

        CEncyclopediaArticle article;

        article.Load(*it);

        article_vector.emplace_back(*it, Level().GetGameTime(), article.data()->articleType);
        const char* g, *n;
        int _atype = article.data()->articleType;
        g = *(article.data()->group);
        n = *(article.data()->name);
        callback(GameObject::eArticleInfo)(lua_game_object(), g, n, _atype);

        if (CurrentGameUI())
        {
            pda_section::part p = pda_section::encyclopedia;
            switch (article.data()->articleType)
            {
            case ARTICLE_DATA::eEncyclopediaArticle: p = pda_section::encyclopedia;
                break;
            case ARTICLE_DATA::eJournalArticle: p = pda_section::journal;
                break;
            case ARTICLE_DATA::eInfoArticle: p = pda_section::info;
                break;
            case ARTICLE_DATA::eTaskArticle: p = pda_section::quests;
                break;
            default: NODEFAULT;
            };
			CurrentGameUI()->PdaMenu()->PdaContentsChanged(p);
        }
        
        if (CurrentGameUI())
        {
            CurrentGameUI()->UpdatePda();
        }
    }

}

void CActor::AddGameTask(const CInfoPortion* info_portion) const
{
	VERIFY2(info_portion, "info_portion is nullptr");
	if (!info_portion)
	{
		Msg("! [%s] info_portion is nullptr!", __FUNCTION__);
		return;
	}
	const xr_vector<shared_str>& tasks = info_portion->GameTasks();
	if (tasks.empty())
		return;

	for (const shared_str& taskId : tasks)
	{
		Level().GameTaskManager()->GiveGameTaskToActor(taskId, 0);
	}
}

void  CActor::AddGameNews			 (GAME_NEWS_DATA& news_data)
{

	GAME_NEWS_VECTOR& news_vector	= game_news_registry->registry().objects();
	news_data.receive_time			= Level().GetGameTime();
	news_vector.push_back			(news_data);

	if ( CurrentGameUI() )
	{
		CurrentGameUI()->UIMainIngameWnd->ReceiveNews(&news_data);
		CurrentGameUI()->PdaMenu()->PdaContentsChanged(pda_section::news);
	}
}


bool CActor::OnReceiveInfo(shared_str info_id) const
{
	if(!CInventoryOwner::OnReceiveInfo(info_id))
		return false;

	CInfoPortion info_portion;
	info_portion.Load(info_id);

	AddEncyclopediaArticle(&info_portion);
	AddGameTask(&info_portion);

	callback(GameObject::eInventoryInfo)(lua_game_object(), *info_id);


	//только если находимся в режиме single
 	if(!CurrentGameUI()->TalkMenu) 
		return false;

	if(CurrentGameUI()->TalkMenu->IsActiveTalkUi())
	{
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
	}

	return true;
}


void CActor::OnDisableInfo(shared_str info_id) const
{
	CInventoryOwner::OnDisableInfo(info_id);

	if (!CurrentGameUI()->TalkMenu)
		return;

	//только если находимся в режиме single
	if(CurrentGameUI()->TalkMenu->IsActiveTalkUi())
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();
}

void  CActor::ReceivePhrase		(DIALOG_SHARED_PTR& phrase_dialog)
{
	//только если находимся в режиме single
	if (!CurrentGameUI()->TalkMenu)
		return;

	if(CurrentGameUI()->TalkMenu->IsActiveTalkUi())
		CurrentGameUI()->TalkMenu->NeedUpdateQuestions();

	CPhraseDialogManager::ReceivePhrase(phrase_dialog);
}

void CActor::UpdateAvailableDialogs(CPhraseDialogManager* partner)
{
	m_AvailableDialogs.clear();
	m_CheckedDialogs.clear();

	if (m_known_info_registry->registry().objects_ptr())
	{
		auto& infoPortionRegistry = *m_known_info_registry->registry().objects_ptr();
		for (const INFO_DATA& info_data : infoPortionRegistry)
		{
			//подгрузить кусочек информации с которым мы работаем
			CInfoPortion info_portion;
			info_portion.Load(info_data.info_id);

			const DIALOG_ID_VECTOR& names = info_portion.DialogNames();
			for (const shared_str& name : names)
				AddAvailableDialog(name.c_str(), partner);
		}
	}

	//добавить актерский диалог собеседника
	CInventoryOwner* pInvOwnerPartner = partner->cast_inventory_owner();
	VERIFY(pInvOwnerPartner);

	for (u32 i = 0; i < pInvOwnerPartner->CharacterInfo().ActorDialogs().size(); i++)
	{
		AddAvailableDialog(pInvOwnerPartner->CharacterInfo().ActorDialogs()[i], partner);
	}

	CPhraseDialogManager::UpdateAvailableDialogs(partner);
}

void CActor::TryToTalk()
{
	if (PdaCommunication_IsSessionActive())
	{
		return;
	}

	if (!IsTalking())
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
	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	bool renderHUD = noHUDonMaster ? g_SingleGameDifficulty == egdNovice : true;
	if (renderHUD)
	{
		CurrentGameUI()->UIMainIngameWnd->AnimateContacts(b_alive);
	}

	Level().MapManager().AddRelationLocation( pInvOwner );

	CurrentGameUI()->PdaMenu()->PdaContentsChanged(pda_section::contacts);
}

void CActor::LostPdaContact		(CInventoryOwner* pInvOwner)
{
	Level().MapManager().RemoveRelationLocation(pInvOwner);

	CurrentGameUI()->PdaMenu()->PdaContentsChanged(pda_section::contacts);
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

bool CActor::OnDialogSoundHandlerStart(CInventoryOwner *inv_owner, const char* phrase)
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