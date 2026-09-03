#include "StdAfx.h"
#include "UITalkWnd.h"
#include "UIPdaTalkHost.h"
#include "UITalkDialogWnd.h"
#include "UIPdaWnd.h"

#include "../Actor.h"
#include "../trade.h"
#include "UIGameSP.h"
#include "../PDA.h"
#include "../../xrServerEntities/character_info.h"
#include "../Level.h"

#include "../PhraseDialog.h"
#include "../PhraseDialogManager.h"
#include "../pda_communication.h"
#include "../PdaTalkDialogPolicy.h"
#include "../GametaskManager.h"

#include "../game_cl_base.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/CameraBase.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UI3tButton.h"

#include "EffectorDOF.h"
#include "ActorEffector.h"
#include "GamePersistent.h"

bool EnableTalkDof = true;

namespace
{
bool IsTalkDialogAllowedForContext(const DIALOG_SHARED_PTR& dialog, bool isPdaSession)
{
	return dialog && PdaTalkDialogPolicy().IsDialogAllowed(dialog->GetDialogID(), isPdaSession);
}
} // namespace

CUITalkWnd::CUITalkWnd()
{
	m_pActor				= nullptr;

	m_pOurInvOwner			= nullptr;
	m_pOthersInvOwner		= nullptr;
	m_pOurDialogManager		= nullptr;
	m_pOthersDialogManager	= nullptr;

	ToTopicMode				();

	const static Fvector4 talkDof = EngineExternal().GetTalkDof();
	m_TalkDof.set(talkDof);

	const static float fovScale = EngineExternal().GetTalkFovScale();
	m_talkFovScale = fovScale;
	clamp(m_talkFovScale, 0.2f, 1.0f);

	InitTalkWnd				();
	m_bNeedToUpdateQuestions = false;
	b_disable_break			= false;
	m_isPdaDialog			= false;
	m_pdaTalkHost			= new CUIPdaTalkHost();
	
	ActionRepeaters()->Register(this, kUI_UP);
	ActionRepeaters()->Register(this, kUI_DOWN);
	ActionRepeaters()->Register(this, kUI_SECONDARY_UP, 1, 50);
	ActionRepeaters()->Register(this, kUI_SECONDARY_DOWN, 1, 50);
}

CUITalkWnd::~CUITalkWnd()
{
	EndPdaEmbed();
	xr_delete(m_pdaTalkHost);
}

void CUITalkWnd::InitTalkWnd()
{
	inherited::SetWndRect(Frect().set(0, 0, UI_BASE_WIDTH, UI_BASE_HEIGHT));

	UITalkDialogWnd			= new CUITalkDialogWnd();
	UITalkDialogWnd->SetAutoDelete(true);
	AttachChild				(UITalkDialogWnd);
	UITalkDialogWnd->m_pParent = this;
	UITalkDialogWnd->InitTalkDialogWnd();
}

void CUITalkWnd::InitTalkDialog(bool skipLogClear)
{
	m_pActor = Actor();
	const bool isPdaSession = IsPdaMode() || IsEmbeddedInPda();

	if (!IsPdaMode() && m_pActor != nullptr && !m_pActor->IsTalking())
	{
		return;
	}

	if (!m_pActor)
	{
		if (isPdaSession)
		{
			StopPdaDialog();
		}
		return;
	}

	m_pOurInvOwner = m_pActor->cast_inventory_owner();
	m_pOthersInvOwner = m_pActor->GetTalkPartner();

	if (isPdaSession && !m_pOthersInvOwner)
	{
		m_pOthersInvOwner = PdaCommunication().GetSessionNpc();
		if (m_pOthersInvOwner)
		{
			m_pActor->SetTalkPartner(m_pOthersInvOwner);
			m_pOthersInvOwner->SetTalkPartner(m_pOurInvOwner);
		}
	}

	if (!m_pOurInvOwner || !m_pOthersInvOwner)
	{
		if (isPdaSession)
		{
			StopPdaDialog();
		}
		return;
	}

	if (!isPdaSession)
	{
		Level().GameTaskManager()->IssuePendingRewards();
	}

	m_pOurDialogManager = m_pOurInvOwner->cast_phrase_dialog_manager();
	m_pOthersDialogManager = m_pOthersInvOwner->cast_phrase_dialog_manager();

	if (!m_pOurDialogManager || !m_pOthersDialogManager)
	{
		if (isPdaSession)
		{
			StopPdaDialog();
		}
		return;
	}

	//имена собеседников
	if (UITalkDialogWnd->UIDialogFrameTop)
		UITalkDialogWnd->UIDialogFrameTop->SetTextIfNodeExist(m_pOthersInvOwner->Name());
	else if (UITalkDialogWnd->UIDialogFrame)
		UITalkDialogWnd->UIDialogFrame->UITitleText.SetTextIfNodeExist(m_pOthersInvOwner->Name());

	if (UITalkDialogWnd->UIDialogFrameBottom)
		UITalkDialogWnd->UIDialogFrameBottom->SetTextIfNodeExist(m_pOurInvOwner->Name());
	else if (UITalkDialogWnd->UIOurPhrasesFrame)
		UITalkDialogWnd->UIOurPhrasesFrame->UITitleText.SetTextIfNodeExist(m_pOurInvOwner->Name());

	UITalkDialogWnd->swapCharacterNames = !CurrentGameUI()->ActorMenu();
	if (UITalkDialogWnd->swapCharacterNames)
	{
		UITalkDialogWnd->UICharacterInfoRight.InitCharacter(m_pOurInvOwner);
		UITalkDialogWnd->UICharacterInfoLeft.InitCharacter(m_pOthersInvOwner);
	}
	else
	{
		UITalkDialogWnd->UICharacterInfoLeft.InitCharacter(m_pOurInvOwner);
		UITalkDialogWnd->UICharacterInfoRight.InitCharacter(m_pOthersInvOwner);
	}

	// Clear answer/question logs only for classic talk window path.
	// PDA path performs non-blocking clear before entering here.
	if (!skipLogClear)
	{
		UITalkDialogWnd->ClearAll();
	}
	UITalkDialogWnd->ResetQuestionSelection();

	// PDA uses the same phrase-dialog pipeline as face-to-face talk: clear any stale dialog, refresh actor-side
	// topics, then run InitOthersStartDialog (hello/start). The old ToTopicMode()-only path skipped
	// InitOthersStartDialog and never drove UITalkDialogWnd from PhraseDialogManager.
	if (isPdaSession)
	{
		ToTopicMode();
		m_pOurDialogManager->UpdateAvailableDialogs(m_pOthersDialogManager);
	}
	InitOthersStartDialog();
	NeedUpdateQuestions();
	Update();

	UITalkDialogWnd->mechanic_mode = m_pOthersInvOwner->SpecificCharacter().upgrade_mechanic();
	UITalkDialogWnd->SetOsoznanieMode(m_pOthersInvOwner->NeedOsoznanieMode());
	if (isPdaSession)
	{
		UITalkDialogWnd->Show(false, false);
	}
	else
	{
		UITalkDialogWnd->Show();
	}
	UITalkDialogWnd->UpdateButtonsLayout(b_disable_break, m_pOthersInvOwner->IsTradeEnabled());
}

void CUITalkWnd::InitOthersStartDialog()
{
	m_pOthersDialogManager->UpdateAvailableDialogs(m_pOurDialogManager);
	if(!m_pOthersDialogManager->AvailableDialogs().empty())
	{
		const bool isPdaSession = IsPdaMode();
		if (isPdaSession)
		{
			// Prefer dialogs explicitly marked for PDA, but keep legacy dialogs available.
			for (const DIALOG_SHARED_PTR& dialog : m_pOthersDialogManager->AvailableDialogs())
			{
				if (!IsTalkDialogAllowedForContext(dialog, true))
				{
					continue;
				}

				if (dialog->IsPdaAvailable())
				{
					m_pCurrentDialog = dialog;
					break;
				}
			}

			if (!m_pCurrentDialog)
			{
				for (const DIALOG_SHARED_PTR& dialog : m_pOthersDialogManager->AvailableDialogs())
				{
					if (IsTalkDialogAllowedForContext(dialog, true))
					{
						m_pCurrentDialog = dialog;
						break;
					}
				}
			}
		}
		else
		{
			for (const DIALOG_SHARED_PTR& dialog : m_pOthersDialogManager->AvailableDialogs())
			{
				if (IsTalkDialogAllowedForContext(dialog, false))
				{
					m_pCurrentDialog = dialog;
					break;
				}
			}
		}

		if (!m_pCurrentDialog)
		{
			ToTopicMode();
			return;
		}

		m_pOthersDialogManager->InitDialog(m_pOurDialogManager, m_pCurrentDialog);
		m_pCurrentDialog->SetTalkMode(isPdaSession ? ETalkMode::Pda : ETalkMode::Normal);
		
		//сказать фразу
		AddAnswer(m_pCurrentDialog->GetPhraseText("0"), m_pOthersInvOwner->NameReal());
		m_pOthersDialogManager->SayPhrase(m_pCurrentDialog, "0");

		//если диалог завершился, перейти в режим выбора темы
		if(!m_pCurrentDialog || m_pCurrentDialog->IsFinished()) ToTopicMode();
	}
}

void CUITalkWnd::NeedUpdateQuestions()
{
	m_bNeedToUpdateQuestions = true;
}

void CUITalkWnd::UpdateQuestions()
{
	UITalkDialogWnd->ClearQuestions();

	//если нет активного диалога, то
	//режима выбора темы
	if(!m_pCurrentDialog)
	{
		m_pOurDialogManager->UpdateAvailableDialogs(m_pOthersDialogManager);
		const bool isPdaSession = IsPdaMode();
		for(u32 i=0; i< m_pOurDialogManager->AvailableDialogs().size(); ++i)
		{
			const DIALOG_SHARED_PTR& phrase_dialog	= m_pOurDialogManager->AvailableDialogs()[i];
			if (!IsTalkDialogAllowedForContext(phrase_dialog, isPdaSession))
			{
				continue;
			}
			//if (phrase_dialog->GetPhraseCount() > 0)
			{
				SPhraseInfo phInfo;
				phInfo.sIconName = (phrase_dialog->GetPhrase("0"))->GetIconName();
				phInfo.bUseIconLtx = (phrase_dialog->GetPhrase("0"))->GetIconUsingLTX();
				phInfo.bFinalizer = (phrase_dialog->GetPhrase("0"))->IsFinalizer();

				AddQuestion(phrase_dialog->DialogCaption(), phrase_dialog->GetDialogID(), i, phInfo);
			}
		}
	}
	else
	{
		if(m_pCurrentDialog->IsWeSpeaking(m_pOurDialogManager))
		{
			//если в списке допустимых фраз только одна фраза пустышка, то просто
			//сказать (игрок сам не производит никаких действий)
			if( !m_pCurrentDialog->PhraseList().empty() && m_pCurrentDialog->allIsDummy() ){
				CPhrase* phrase = m_pCurrentDialog->PhraseList()[Random.randI(m_pCurrentDialog->PhraseList().size())];
				SayPhrase(phrase->GetID());
			};

			//выбор доступных фраз из активного диалога
			if( m_pCurrentDialog && !m_pCurrentDialog->allIsDummy() )
			{			
				int number = 0;
				for(PHRASE_VECTOR::const_iterator   it = m_pCurrentDialog->PhraseList().begin();
					it != m_pCurrentDialog->PhraseList().end();
					++it, ++number)
				{
					SPhraseInfo phInfo;
					CPhrase* phrase = *it;
					phInfo.bFinalizer = phrase->IsFinalizer();
					phInfo.sIconName = phrase->GetIconName();
					phInfo.bUseIconLtx = phrase->GetIconUsingLTX();
					AddQuestion(m_pCurrentDialog->GetPhraseText(phrase->GetID() ), phrase->GetID(), number, phInfo);
				}
			}
			else
				UpdateQuestions();
		}
	}
	m_bNeedToUpdateQuestions = false;

}

//////////////////////////////////////////////////////////////////////////

void CUITalkWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if(pWnd == UITalkDialogWnd && msg == TALK_DIALOG_TRADE_BUTTON_CLICKED)
	{
		SwitchToTrade();
	}
	else if(pWnd == UITalkDialogWnd && msg == TALK_DIALOG_UPGRADE_BUTTON_CLICKED)
	{
		SwitchToUpgrade();
	}
	else if(pWnd == UITalkDialogWnd && msg == TALK_DIALOG_QUESTION_CLICKED)
	{
		AskQuestion();
	}
	inherited::SendMessage(pWnd, msg, pData);
}

//////////////////////////////////////////////////////////////////////////
void UpdateCameraDirection(CGameObject* pTo, bool isFocus)
{
	if (!pTo)
		return;

	CInventoryOwner* io = pTo->cast_inventory_owner();
	if (!io)
		return;

	CActor* A = Actor();
	if (!A)
		return;

	CCameraBase* cam = A->cam_Active();
	if (!cam)
		return;

	Fvector target_pos;
	if (io->GetFocusingOnNpc())
	{
		if (IKinematics* pk = PKinematics(pTo->Visual()))
			pk->LL_GetBoneWorldPosition(pk->LL_BoneID("bip01_head"), pTo->XFORM(), target_pos);
		else
		{
			pTo->Center(target_pos);
			target_pos.y += pTo->Radius() * 0.5f;
		}
	}
	else
	{
		pTo->Center(target_pos);
		target_pos.y += pTo->Radius() * 0.5f;
	}

	Fvector target_dir;
	target_dir.sub(target_pos, cam->vPosition);
	target_dir.normalize();
	float p, h;
	target_dir.getHP(h, p);

	Fvector targ_angles = EulerYawPitchRollInertion({ cam->pitch , cam->yaw, 0.f }, { -p, -h, 0.f }, 0.5f, Device.fTimeDelta);
	cam->pitch = targ_angles.x;
	cam->yaw = targ_angles.y;
}

void CUITalkWnd::Update()
{
	if ((IsPdaMode() || IsEmbeddedInPda()) && !PdaCommunication_IsSessionActive())
	{
		StopPdaDialog();
		return;
	}

    //остановить разговор, если нужно
    if (!IsPdaMode() && g_actor && m_pActor && !m_pActor->IsTalking())
    {
        StopTalk();
    }
    else
    {
        CGameObject* pOurGO = m_pOurInvOwner != nullptr ? m_pOurInvOwner->cast_game_object() : nullptr;
        CGameObject* pOtherGO = m_pOthersInvOwner != nullptr ? m_pOthersInvOwner->cast_game_object() : nullptr;

        if (nullptr == pOurGO || nullptr == pOtherGO)
        {
            HideDialog();
            return;
        }
    }

	if (m_bNeedToUpdateQuestions)
	{
		UpdateQuestions();

		if (!UITalkDialogWnd->m_ClickedQuestionID.size() || !UITalkDialogWnd->HasQuestionWithID(UITalkDialogWnd->m_ClickedQuestionID))
			UITalkDialogWnd->SetFirstQuestionSelected();

		UITalkDialogWnd->ScrollSelectionIntoView();
	}
	UITalkDialogWnd->UpdateQuestionSelection();

	inherited::Update();
	// Remote PDA dialog: do not aim camera at NPC bones/visual (partner may be off-screen or not loaded into scene graph).
	if (!IsPdaMode() && m_pOthersInvOwner)
	{
		CGameObject* pOtherGO = m_pOthersInvOwner->cast_game_object();
		if (pOtherGO)
			UpdateCameraDirection(pOtherGO, m_pOthersInvOwner->GetFocusingOnNpc());
	}
	if (UITalkDialogWnd && m_pOthersInvOwner)
		UITalkDialogWnd->UpdateButtonsLayout(b_disable_break, m_pOthersInvOwner->IsTradeEnabled());

	if (playing_sound())
	{
		CGameObject* pOtherGO = m_pOthersInvOwner != nullptr ? m_pOthersInvOwner->cast_game_object() : nullptr;
		if (pOtherGO)
		{
			Fvector P = pOtherGO->Position();
			P.y += 1.8f;
			m_sound.set_position(P);
		}
	}
}

void CUITalkWnd::Draw()
{
	inherited::Draw				();
}

void CUITalkWnd::Show(bool status)
{
	if (status && !PdaCommunication_IsSessionActive() && (IsPdaMode() || IsEmbeddedInPda()))
	{
		StopPdaDialog();
	}

	inherited::Show					(status);
	if(status)
	{
		ActionRepeaters()->ResetAll(nullptr);
		InitTalkDialog				();

		if (m_pOthersInvOwner->GetFocusingOnNpc())
		{
			if (EnableTalkDof && !fsimilar(m_TalkDof.w, -1.0f))
			{
				m_pActor->Cameras().AddCamEffector(new CEffectorDOF(m_TalkDof, 0.0f));
			}

			g_fov = g_fov * m_talkFovScale;
		}
	}
	else
	{
		StopSnd						();
		UITalkDialogWnd->Hide		();

		if(m_pActor)
		{
			if (m_pOthersInvOwner->GetFocusingOnNpc())
			{
				g_fov = g_fov / m_talkFovScale;

				GamePersistent().RestoreEffectorDOF();
				m_pActor->Cameras().RemoveCamEffector(eCEDOF);
			}

			ToTopicMode					();

			if (m_pActor->IsTalking())
			{
				if (IsPdaMode())
				{
					PdaCommunication_Stop();
				}

				m_pActor->StopTalk();
			}

			m_pActor = nullptr;
			m_isPdaDialog = false;
		}
	}
}

bool CUITalkWnd::InitializeDialogForPda()
{
	if (!UITalkDialogWnd || !UITalkDialogWnd->TryClearAll())
	{
		return false;
	}

	InitTalkDialog(true);
	return m_pOthersInvOwner != nullptr && m_pOurDialogManager != nullptr && m_pOthersDialogManager != nullptr;
}

void CUITalkWnd::StopPdaDialog()
{
	if (PdaCommunication_IsSessionActive())
	{
		PdaCommunication_Stop();
	}

	if (UITalkDialogWnd && UITalkDialogWnd->IsShown())
	{
		UITalkDialogWnd->Hide();
	}

	EndPdaEmbed();

	m_isPdaDialog = false;
	m_bNeedToUpdateQuestions = false;
	ToTopicMode();

	m_pActor = nullptr;
	m_pOurInvOwner = nullptr;
	m_pOthersInvOwner = nullptr;
	m_pOurDialogManager = nullptr;
	m_pOthersDialogManager = nullptr;
}

bool CUITalkWnd::IsActiveTalkUi()
{
	return IsShown() || (IsPdaMode() && IsEmbeddedInPda());
}

void CUITalkWnd::BeginPdaEmbed(CUIPdaContactsWnd* contacts)
{
	if (!m_pdaTalkHost)
	{
		return;
	}

	m_pdaTalkHost->Begin(this, contacts);
}

void CUITalkWnd::EndPdaEmbed()
{
	if (!m_pdaTalkHost)
	{
		return;
	}

	m_pdaTalkHost->End(this);
}

bool CUITalkWnd::IsEmbeddedInPda() const
{
	return m_pdaTalkHost != nullptr && m_pdaTalkHost->IsActive();
}

bool  CUITalkWnd::TopicMode			() 
{
	return nullptr == m_pCurrentDialog.get();
}

void  CUITalkWnd::ToTopicMode		() 
{
	m_pCurrentDialog.reset();// = DIALOG_SHARED_PTR((CPhraseDialog*)nullptr);
}

void CUITalkWnd::AskQuestion()
{
	if(m_bNeedToUpdateQuestions) return;//quick dblclick:(
	shared_str					phrase_id;

	//игрок выбрал тему разговора
	if(TopicMode())
	{
		if (UITalkDialogWnd->m_ClickedQuestionID == "")
		{
			return;
		}

		if (!m_pOurDialogManager->HaveAvailableDialog(UITalkDialogWnd->m_ClickedQuestionID))
		{
			return;
		}

		const DIALOG_SHARED_PTR& selectedDialog = m_pOurDialogManager->GetDialogByID(UITalkDialogWnd->m_ClickedQuestionID);
		if (!IsTalkDialogAllowedForContext(selectedDialog, IsPdaMode()))
		{
			return;
		}

		m_pCurrentDialog = selectedDialog;
		
		m_pOurDialogManager->InitDialog(m_pOthersDialogManager, m_pCurrentDialog);
		const bool isPdaSession = IsPdaMode();
		m_pCurrentDialog->SetTalkMode(isPdaSession ? ETalkMode::Pda : ETalkMode::Normal);
		phrase_id = "0";
	}
	else
	{
		phrase_id = UITalkDialogWnd->m_ClickedQuestionID;
	}

	SayPhrase				(phrase_id);
	NeedUpdateQuestions		();
}

void CUITalkWnd::SayPhrase(const shared_str& phrase_id)
{
	AddAnswer(m_pCurrentDialog->GetPhraseText(phrase_id), m_pOurInvOwner->NameReal());
	m_pOurDialogManager->SayPhrase(m_pCurrentDialog, phrase_id);
	//если диалог завершился, перейти в режим выбора темы
	if(m_pCurrentDialog->IsFinished()) ToTopicMode();
}

void CUITalkWnd::AddQuestion(const shared_str& text, const shared_str& value, int number, SPhraseInfo phInfo)
{
	if(text.size() == 0)
		return;

	UITalkDialogWnd->AddQuestion(text.c_str(), value.c_str(), number, phInfo);
}

void CUITalkWnd::AddAnswer(const shared_str& text, const char* SpeakerName)
{
	//для пустой фразы вообще ничего не выводим
	if(text.size() == 0)
	{
		return;
	}
	PlaySnd			(text.c_str());

	bool i_am = (0 == xr_strcmp(SpeakerName, m_pOurInvOwner->NameReal()));
	UITalkDialogWnd->AddAnswer(SpeakerName, text.c_str(), i_am);
}

void CUITalkWnd::SwitchToTrade()
{
	if ( m_pOurInvOwner->IsTradeEnabled() && m_pOthersInvOwner->IsTradeEnabled() )
	{
		UITalkDialogWnd->Hide();
		CurrentGameUI()->StartTrade	(m_pOurInvOwner, m_pOthersInvOwner);
		StopSnd();
	}
}

void CUITalkWnd::SwitchToUpgrade()
{
	if (CurrentGameUI() && m_pOurInvOwner->IsTradeEnabled() && m_pOthersInvOwner->IsTradeEnabled())
	{
		UITalkDialogWnd->Hide();
		CurrentGameUI()->StartUpgrade(m_pOurInvOwner, m_pOthersInvOwner);
	}
}

bool CUITalkWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{

	if (keyboard_action==WINDOW_KEY_PRESSED)
	{
		if(is_binded(kUSE, dik) || is_binded(kQUIT, dik))
		{
			if (IsPdaMode() && is_binded(kQUIT, dik))
			{
				if (CurrentGameUI() && CurrentGameUI()->PdaMenu())
				{
					CurrentGameUI()->PdaMenu()->HideDialog();
				}
				else
				{
					StopTalk();
				}
				return true;
			}

			if(!b_disable_break)
			{
				HideDialog();
				return true;
			}
		}
		else if(is_binded(kSPRINT_TOGGLE, dik))
		{
            if (!m_pOthersInvOwner->NeedOsoznanieMode())
            {
                if (UITalkDialogWnd->mechanic_mode)
                    SwitchToUpgrade();
                else
                    SwitchToTrade();
                return true;
            }
		}
	}

	return inherited::OnKeyboardAction(dik,keyboard_action);
}

bool CUITalkWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action==WINDOW_KEY_PRESSED)
	{
		switch (get_binded_action(id, agUIGeneral))
		{
			case kUI_BACK:
			{
				if (IsPdaMode())
				{
					if (CurrentGameUI() && CurrentGameUI()->PdaMenu())
					{
						CurrentGameUI()->PdaMenu()->HideDialog();
					}
					else
					{
						StopTalk();
					}
					return true;
				}

				if (!b_disable_break)
				{
					HideDialog();
					return true;
				}
				else
				{
					// Exiting without picking a question is disabled
					// So we need to pick a question with finalizer mark (if there is one) and select it
					// In kbd mode this question(btn) has an accelerator key (ESC) tied to it
					return UITalkDialogWnd->TryClickFinalizerQuestion();
				}
			}
			case kUI_ACTION_2:
			{
				if (UITalkDialogWnd->mechanic_mode)
					SwitchToUpgrade();
				else
					SwitchToTrade();
				return true;
			}
			case kUI_UP:
			{
				if (!any_binded_key_for_action_pressed_c(kUI_DOWN))
					UITalkDialogWnd->OffsetQuestionSelection(false, true);
				ActionRepeaters()->SetActionStarted(this, kUI_UP);
				return true;
			}
			case kUI_DOWN:
			{
				if (!any_binded_key_for_action_pressed_c(kUI_UP))
					UITalkDialogWnd->OffsetQuestionSelection(true, true);
				ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
				return true;
			}
			case kUI_SECONDARY_DOWN:
			{
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_DOWN);
				return true;
			}
			case kUI_SECONDARY_UP:
			{
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_UP);
				return true;
			}
			case kUI_ACCEPT:
			{
				VERIFY(UITalkDialogWnd->m_ClickedQuestionID != "");
				if (UITalkDialogWnd->m_ClickedQuestionID != "")
				{
					SendMessage(UITalkDialogWnd, TALK_DIALOG_QUESTION_CLICKED);
				}
				return true;
			}
		}
	}

	return inherited::OnGamepadKeyAction(id,gamepad_action);
}

bool CUITalkWnd::OnGamepadKeyHold(int id)
{
	if (!IR_process()) 
		return false;

	switch (get_binded_action(id, agUIGeneral))
	{
		case kUI_SECONDARY_UP: 
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_UP) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_DOWN))
				UITalkDialogWnd->ScrollLogUp();
			return true;
		}
		case kUI_SECONDARY_DOWN:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_DOWN) && !any_binded_key_for_action_pressed_c(kUI_SECONDARY_UP))
				UITalkDialogWnd->ScrollLogDown();
			return true;
		}
		case kUI_UP:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP) && !any_binded_key_for_action_pressed_c(kUI_DOWN))
				UITalkDialogWnd->OffsetQuestionSelection(false, false);
			return true;
		}
		case kUI_DOWN:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN) && !any_binded_key_for_action_pressed_c(kUI_UP))
				UITalkDialogWnd->OffsetQuestionSelection(true, false);
			return true;
		}
	}

	return inherited::OnGamepadKeyHold(id);
}

void CUITalkWnd::PlaySnd(const char* text)
{
	u32 text_len = xr_strlen(text);
	
	// Very crude hack with check for maximum path size
	// Script result passes here not the text ID, but the fully localized variant
	if ( text_len == 0 || text_len >= _MAX_PATH)
	{
		return;
	}
	
	string_path	fn;
	
	const char* path = "characters_voice\\dialogs\\";
	const char* ext  = ".ogg";
	u32 tsize   = sizeof(fn) - xr_strlen(path) - xr_strlen(ext) - 1;
	if ( text_len > tsize )
	{
		text_len = tsize;
	}

	strncpy_s( fn, sizeof(fn), path, xr_strlen(path) );
	strncat(fn, text, text_len);
	strncat(fn, ext, xr_strlen(ext));

	//	strconcat( sizeof(fn), fn, "characters_voice\\dialogs\\", text2, ".ogg" );

	StopSnd();
	if (FS.exist(_game_sounds_, fn))
	{
		VERIFY(m_pActor);
		if (!m_pActor->OnDialogSoundHandlerStart(m_pOthersInvOwner, fn))
		{
			CGameObject* pOtherGO = m_pOthersInvOwner->cast_game_object();
			Fvector P = pOtherGO->Position();
			P.y += 1.8f;
			m_sound.create(fn, st_Effect, sg_SourceType);
			m_sound.play_at_pos(0, P);
		}
	}
}

void CUITalkWnd::StopSnd()
{
	if (m_pActor && m_pActor->OnDialogSoundHandlerStop(m_pOthersInvOwner)) return;

	if(m_sound.is_playing()) 
		m_sound.stop	();
}

void CUITalkWnd::AddIconedMessage(const char* caption, const char* text, const char* texture_name, const char* templ_name)
{
	UITalkDialogWnd->AddIconedAnswer(caption, text, texture_name, templ_name);
}

void CUITalkWnd::AddIconedMessage(const char* text, const char* texture_name, Frect texture_rect, const char* templ_name)
{
	UITalkDialogWnd->AddIconedAnswer(text, texture_name, texture_rect, templ_name);
}

void CUITalkWnd::StopTalk()
{
	if (m_pOthersInvOwner && m_pOthersInvOwner->NeedOsoznanieMode())
	{
		return;
	}

	if (PdaCommunication_IsSessionActive() || IsPdaMode() || IsEmbeddedInPda())
	{
		StopPdaDialog();
		return;
	}

	HideDialog();
}
