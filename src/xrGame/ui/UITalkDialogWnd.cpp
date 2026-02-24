#include "StdAfx.h"
#include "UITalkDialogWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UITalkWnd.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "../../xrEngine/string_table.h"
#include "../game_news.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_registry_wrappers.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"

#define				TALK_XML				"talk.xml"

using namespace InventoryUtilities;

CUITalkDialogWnd::CUITalkDialogWnd()
	: m_uiXml(nullptr),
	m_pParent(nullptr),
	mechanic_mode(false),
	m_ClickedQuestionID(""),
	UIDialogFrameTop(nullptr),
	UIDialogFrameBottom(nullptr),
	UIDialogFrame(nullptr),
	UIOurPhrasesFrame(nullptr),
	m_btn_pos(),
	UIToExitButton(nullptr),
	UIOurIcon(nullptr),
	UIOthersIcon(nullptr),
	UIQuestionsList(nullptr),
	UIAnswersList(nullptr),
	m_pNameTextFont(nullptr),
	m_iNameTextColor(0),
	m_uOurReplicsColor(0) {}

CUITalkDialogWnd::~CUITalkDialogWnd()
{
	xr_delete(m_uiXml);
}

void CUITalkDialogWnd::InitTalkDialogWnd()
{
	m_uiXml						= new CUIXml();
	m_uiXml->Load				(CONFIG_PATH, UI_PATH, TALK_XML);
	CUIXmlInit					ml_init;

	if (m_uiXml->NavigateToNode("main"))
	{
		CUIXmlInit::InitWindow(*m_uiXml, "main", 0, this);
	}
	else
	{
		SetWndPos(Fvector2().set(0, 0));
		SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));
	}

	if (m_uiXml->NavigateToNode("top_background"))
	{
		UIStaticTop = UIHelper::CreateStatic(*m_uiXml, "top_background", this);
	}
	if (m_uiXml->NavigateToNode("bottom_background"))
	{
		UIStaticBottom = UIHelper::CreateStatic(*m_uiXml, "bottom_background", this);
	}

	CUIXml xml_character;
	const char* charInfoProfile = "talk_character.xml";
	if (!xml_character.Load(CONFIG_PATH, UI_PATH, "talk_character.xml"))
		charInfoProfile = "trade_character.xml";

	if (m_uiXml->NavigateToNode("right_character_icon"))
	{
		UIOurIcon = UIHelper::CreateStatic(*m_uiXml, "right_character_icon", this);
		UIOurIcon->AttachChild(&UICharacterInfoLeft);
		UICharacterInfoLeft.InitCharacterInfo(Fvector2().set(0, 0), UIOurIcon->GetWndSize(), charInfoProfile);
	}

	if (m_uiXml->NavigateToNode("left_character_icon"))
	{
		UIOthersIcon = UIHelper::CreateStatic(*m_uiXml, "left_character_icon", this);
		UIOthersIcon->AttachChild(&UICharacterInfoRight);
		UICharacterInfoRight.InitCharacterInfo(Fvector2().set(0, 0), UIOthersIcon->GetWndSize(), charInfoProfile);
	}

	CUIWindow* answersParent = this;
	CUIWindow* questionsParent = this;

	// Фрейм с нащими фразами
	if (m_uiXml->NavigateToNode("frame_bottom"))
	{
		UIDialogFrameBottom = UIHelper::CreateStatic(*m_uiXml, "frame_bottom", this);
		questionsParent = UIDialogFrameBottom;
	}

	//основной фрейм диалога
	if (m_uiXml->NavigateToNode("frame_top"))
	{
		UIDialogFrameTop = UIHelper::CreateStatic(*m_uiXml, "frame_top", this);
		answersParent = UIDialogFrameTop;
	}
	if (m_uiXml->NavigateToNode("frame_line_window"))
	{
		//основной фрейм диалога
		UIDialogFrame = new CUIFrameLineWnd();
		AttachChild(UIDialogFrame);
		CUIXmlInit::InitFrameLine(*m_uiXml, "frame_line_window", 0, UIDialogFrame);
		answersParent = UIDialogFrame;

		// Фрейм с нащими фразами
		UIOurPhrasesFrame = new CUIFrameLineWnd();
		AttachChild(UIOurPhrasesFrame);
		CUIXmlInit::InitFrameLine(*m_uiXml, "frame_line_window", 1, UIOurPhrasesFrame);
		questionsParent = UIOurPhrasesFrame;
	}

	//Ответы
	UIAnswersList				= new CUIScrollView();
	UIAnswersList->SetAutoDelete(true);
	answersParent->AttachChild(UIAnswersList);
	CUIXmlInit::InitScrollView	(*m_uiXml, "answers_list", 0, UIAnswersList);
	UIAnswersList->SetWindowName("---UIAnswersList");

	//Вопросы
	UIQuestionsList				= new CUIScrollView();
	UIQuestionsList->SetAutoDelete(true);
	questionsParent->AttachChild(UIQuestionsList);
	CUIXmlInit::InitScrollView	(*m_uiXml, "questions_list", 0, UIQuestionsList);
	UIQuestionsList->SetWindowName("---UIQuestionsList");


	//кнопка перехода в режим торговли
	AttachChild					(&UIToTradeButton);
	CUIXmlInit::Init3tButton	(*m_uiXml, "button", 0, &UIToTradeButton);

	m_btn_pos[0] = UIToTradeButton.GetWndPos();

	if (m_uiXml->NavigateToNode("button_exit"))
	{
		UIToExitButton = UIHelper::Create3tButton(*m_uiXml, "button_exit", this);
		m_btn_pos[1] = UIToExitButton->GetWndPos();
		m_btn_pos[2].x = (m_btn_pos[0].x + m_btn_pos[1].x) / 2.0f;
		m_btn_pos[2].y = m_btn_pos[0].y;
	}
	else
	{
		m_btn_pos[1] = m_btn_pos[0];
		m_btn_pos[2] = m_btn_pos[0];
	}
	// шрифт для индикации имени персонажа в окне разговора
	CUIXmlInit::InitFont		(*m_uiXml, "font", 0, m_iNameTextColor, m_pNameTextFont);

	CGameFont * pFont			= nullptr;
	CUIXmlInit::InitFont		(*m_uiXml, "font", 1, m_uOurReplicsColor, pFont);


	SetWindowName				("----CUITalkDialogWnd");

	Register					(&UIToTradeButton);
	AddCallbackStr("question_item", LIST_ITEM_CLICKED, CUIWndCallback::void_function(this, &CUITalkDialogWnd::OnQuestionClicked));
	AddCallback(&UIToTradeButton, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITalkDialogWnd::OnTradeClicked));

	if (UIToExitButton)
	{
		AddCallback(UIToExitButton, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITalkDialogWnd::OnExitClicked));
	}

	m_gamepad_legend = UIHelper::CreateGamepadLegend(*m_uiXml, "gamepad_legend", this, false);
}

	
void CUITalkDialogWnd::Show()
{
    SendInfoToActor("ui_talk_show");
    SendInfoToLuaScripts("ui_talk_show");
    inherited::Show(true);
    inherited::Enable(true);

    ResetAll();
}

void CUITalkDialogWnd::Hide()
{
    SendInfoToActor("ui_talk_hide");
    SendInfoToLuaScripts("ui_talk_hide");
    inherited::Show(false);
    inherited::Enable(false);
    g_btnHint->Discard();
}

void CUITalkDialogWnd::OnQuestionClicked(CUIWindow* w, void*)
{
	m_ClickedQuestionID = ((CUIQuestionItem*)w)->m_s_value;
	GetMessageTarget()->SendMessage(this, TALK_DIALOG_QUESTION_CLICKED);
}

void CUITalkDialogWnd::OnExitClicked(CUIWindow* w, void*)
{
	m_pParent->StopTalk();
}

void CUITalkDialogWnd::OnTradeClicked(CUIWindow* w, void*)
{
	if ( mechanic_mode )
	{
		GetTop()->SendMessage(this, TALK_DIALOG_UPGRADE_BUTTON_CLICKED);
	}
	else
	{
		GetTop()->SendMessage(this, TALK_DIALOG_TRADE_BUTTON_CLICKED);
	}
}

void CUITalkDialogWnd::OnUpgradeClicked(CUIWindow* w, void*)
{
	GetTop()->SendMessage(this, TALK_DIALOG_UPGRADE_BUTTON_CLICKED);
}

void CUITalkDialogWnd::SetTradeMode()
{
	OnTradeClicked( &UIToTradeButton, 0 );
}

//пересылаем сообщение родительскому окну для обработки
//и фильтруем если оно пришло от нашего дочернего окна
void CUITalkDialogWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUITalkDialogWnd::ClearAll()
{
	UIAnswersList->Clear	();
	ClearQuestions			();
}

void CUITalkDialogWnd::ClearQuestions()
{
	UIQuestionsList->Clear();
}


void CUITalkDialogWnd::AddQuestion(LPCSTR str, LPCSTR value, int number, SPhraseInfo &phInfo)
{
	CUIQuestionItem* itm			= new CUIQuestionItem(m_uiXml,"question_item");
	itm->Init						(value, str, phInfo.bFinalizer);
	++number; //zero-based index

	string16 buff;
	xr_sprintf(buff, "%d.", number);
	float x_offset = 0.f;
	if (itm->m_num_text)
		itm->m_num_text->SetText(buff);
	if (number > 9)
	{
		x_offset += itm->m_fOffset;
	}
	if (number < 10)
	{
		itm->m_text->SetAccelerator(SDL_SCANCODE_Z + number, 0);
	}
	if (phInfo.bFinalizer)
	{
		itm->m_text->SetAccelerator		(kQUIT, 2);
		itm->m_text->SetAccelerator		(kUSE, 3);
	}
	if (phInfo.sIconName.size() > 1)
	{
		Fvector2 icon_size = itm->m_icon_size;
		itm->m_text->AddStatic();
		CUIStatic* pBtnStatic = itm->m_text->GetBtnStatic();
		pBtnStatic->SetWndPos(Fvector2().set(0.f, 0.f));
		if (!phInfo.bUseIconLtx)
		{
			pBtnStatic->InitTextureEx(phInfo.sIconName.c_str(), "hud\\default");
		}
		else
		{
			InventoryIconParams icons_struct = GetInventoryIconParams(phInfo.sIconName.c_str());
			pBtnStatic->GetUIStaticItem().SetShader(GetEquipmentIconsShader(icons_struct.icons_texture));
			float scaleIcon = icons_struct.scaleIcon;
			float x = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
			float y = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
			float width = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
			float height = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
			Frect tex_rect{ x, y, width, height };
			tex_rect.rb.add(tex_rect.lt);

			pBtnStatic->GetUIStaticItem().SetTextureRect(tex_rect);

			icon_size.x *= width / INV_GRID_WIDTH(scaleIcon);
		}
		pBtnStatic->SetWndPos(Fvector2().set(x_offset, 0.f));
		x_offset += icon_size.x + itm->m_fOffsetAfterIcon;
		pBtnStatic->SetWndSize(icon_size);
		pBtnStatic->SetStretchTexture(true);
	}
	itm->m_text->SetTextX			(x_offset);
	m_break_enabled = phInfo.bFinalizer;

	itm->SetWindowName				("question_item");
	UIQuestionsList->AddWindow		(itm, true);
	Register						(itm);
}


void CUITalkDialogWnd::AddAnswer(LPCSTR SpeakerName, LPCSTR str, bool bActor)
{
	CUIAnswerItem* itm				= new CUIAnswerItem(m_uiXml,bActor?"actor_answer_item":"other_answer_item");
	itm->Init						(str, SpeakerName);
	UIAnswersList->AddWindow		(itm, true);
	UIAnswersList->ScrollToEnd		();
	
	GAME_NEWS_DATA	news_data;
	news_data.news_caption = SpeakerName;

	xr_string res;
	res = "%c[250,255,232,208]#";
	res += str;
	res += "#";
	news_data.news_text	= res.c_str();

	news_data.m_type				= GAME_NEWS_DATA::eTalk;
	CUICharacterInfo* ci			= bActor ? &UICharacterInfoLeft : &UICharacterInfoRight; 
	if (swapCharacterNames)
		ci = bActor ? &UICharacterInfoRight : &UICharacterInfoLeft;

	news_data.texture_name			= ci->IconName();

	Frect emptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
	if (!news_data.tex_rect.cmp(emptyRect))
	{
		news_data.tex_rect = ci->UIIcon().GetUIStaticItem().GetTextureRect();
		news_data.tex_rect.x2 = news_data.tex_rect.width();
		news_data.tex_rect.y2 = news_data.tex_rect.height();
	}
	news_data.receive_time			= Level().GetGameTime();

	Actor()->game_news_registry->registry().objects().push_back(news_data);
}

void CUITalkDialogWnd::AddIconedAnswer(LPCSTR caption, LPCSTR text, LPCSTR texture_name, LPCSTR templ_name)
{
	CUIAnswerItemIconed* itm		= new CUIAnswerItemIconed(m_uiXml,templ_name);
	itm->Init						(text, caption, texture_name);
	UIAnswersList->AddWindow		(itm, true);
	UIAnswersList->ScrollToEnd		();
	
	GAME_NEWS_DATA	news_data;
	news_data.news_caption			= caption;
	news_data.news_text._set		( text );

	news_data.m_type				= GAME_NEWS_DATA::eTalk;
	news_data.texture_name			= texture_name;
	news_data.receive_time			= Level().GetGameTime();

	Actor()->game_news_registry->registry().objects().push_back(news_data);
}

void CUITalkDialogWnd::AddIconedAnswer(LPCSTR text, LPCSTR texture_name, Frect texture_rect, LPCSTR templ_name)
{
	CUIAnswerItemIconed* itm = new CUIAnswerItemIconed(m_uiXml, templ_name);
	itm->Init(text, texture_name, texture_rect);
	UIAnswersList->AddWindow(itm, true);
	UIAnswersList->ScrollToEnd();

	GAME_NEWS_DATA news_data;
	news_data.news_caption = "";
	news_data.news_text = text;
	news_data.tex_rect = texture_rect;

	news_data.m_type = GAME_NEWS_DATA::eTalk;
	news_data.texture_name = texture_name;
	news_data.receive_time = Level().GetGameTime();

	Actor()->game_news_registry->registry().objects().push_back(news_data);
}

void CUITalkDialogWnd::SetOsoznanieMode(bool b)
{
	if (UIOurIcon)
		UIOurIcon->Show(!b);

	if (UIOthersIcon)
		UIOthersIcon->Show(!b);

	UIAnswersList->Show	(!b);

	if (UIDialogFrameTop)
		UIDialogFrameTop->Show(!b);
	else if (UIDialogFrame)
		UIDialogFrame->Show(!b);

	UIToTradeButton.Show(!b && !pInput->GetControllerMode());
	if ( mechanic_mode )
	{
		UIToTradeButton.m_hint_text = "ui_st_upgrade_hint";
		UIToTradeButton.TextItemControl()->SetTextST( "ui_st_upgrade" );
	}
	else
	{
		UIToTradeButton.m_hint_text = "ui_st_trade_hint";
		UIToTradeButton.TextItemControl()->SetTextST( "ui_st_trade" );
	}
}

void CUITalkDialogWnd::UpdateButtonsLayout(bool b_disable_break, bool trade_enabled)
{
	m_trade_enabled = trade_enabled;
	UIToTradeButton.Show		(m_trade_enabled && !pInput->GetControllerMode());

	if (UIToExitButton)
	{
		UIToExitButton->Show(!b_disable_break && !pInput->GetControllerMode());

		if (UIToExitButton->IsShown() && UIToTradeButton.IsShown())
		{
			UIToTradeButton.SetWndPos(m_btn_pos[0]);
			UIToExitButton->SetWndPos(m_btn_pos[1]);
		}
		else if (UIToExitButton->IsShown())
		{
			UIToExitButton->SetWndPos(m_btn_pos[2]);
		}
		else if (UIToTradeButton.IsShown())
		{
			UIToTradeButton.SetWndPos(m_btn_pos[2]);
		}
	}
	UpdateGamepadLegend();
}

void CUIQuestionItem::SendMessage				(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

CUIQuestionItem::CUIQuestionItem(CUIXml* xml_doc, LPCSTR path)
{
	CUIXmlInit::InitWindow(*xml_doc, path, 0, this);

	m_min_height					= xml_doc->ReadAttribFlt(path,0,"min_height",15.0f);

	string512 str;

	m_icon_size.x					= xml_doc->ReadAttribFlt(path, 0, "icon_width", 15.0f);
	m_icon_size.y					= xml_doc->ReadAttribFlt(path, 0, "icon_height", 15.0f);
	m_fOffsetAfterIcon				= xml_doc->ReadAttribFlt(path, 0, "text_offset_after_icon", 3.0f);

	xr_strconcat					(str,path,":content_text");
	m_text							= UIHelper::Create3tButton(*xml_doc, str, this);

	m_fOffset						= xml_doc->ReadAttribFlt(str, 0, "offset", 0.f);

	Register						(m_text);
	AddCallback						(m_text,BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUIQuestionItem::OnTextClicked));

	xr_strconcat(str, path, ":num_text");
	if (xml_doc->NavigateToNode(str))
		m_num_text = UIHelper::CreateStatic(*xml_doc, str, this);
}

void CUIQuestionItem::Update()
{
	inherited::Update();
	if (m_num_text)
		m_num_text->Show(!pInput->GetControllerMode());
}

void CUIQuestionItem::Init			(LPCSTR val, LPCSTR text, bool isFinalizer)
{
	m_is_finalizer					= isFinalizer;
	m_s_value						= val;
	m_text->TextItemControl()->SetText(g_pStringTable->ParseStringFromScript(text).c_str());
	m_text->AdjustHeightToText		();
	float new_h						= std::max(m_min_height, m_text->GetWndPos().y+m_text->GetHeight());
	SetHeight						(new_h);
}

void	CUIQuestionItem::OnTextClicked(CUIWindow* w, void*)
{
	GetMessageTarget()->SendMessage(this, LIST_ITEM_CLICKED, (void*)this);
}


CUIAnswerItem::CUIAnswerItem			(CUIXml* xml_doc, LPCSTR path)
{
	CUIXmlInit::InitWindow(*xml_doc, path, 0, this);

	m_min_height					= xml_doc->ReadAttribFlt(path,0,"min_height",15.0f);
	m_bottom_footer					= xml_doc->ReadAttribFlt(path,0,"bottom_footer",0.0f);

	string512 str;

	xr_strconcat(str,path,":content_text");
	m_text = UIHelper::CreateStatic(*xml_doc, str, this);

	xr_strconcat(str,path,":name_caption");
	m_name = UIHelper::CreateStatic(*xml_doc, str, this);

	SetAutoDelete					(true);
}

void CUIAnswerItem::Init			(LPCSTR text, LPCSTR name)
{
	m_name->SetText					(TranslateName(name).c_str());
	m_text->SetText					(g_pStringTable->ParseStringFromScript(text).c_str());
	m_text->AdjustHeightToText		();
	float new_h						= std::max(m_min_height, m_text->GetWndPos().y+m_text->GetHeight());
	new_h							+= m_bottom_footer;
	SetHeight						(new_h);
}

CUIAnswerItemIconed::CUIAnswerItemIconed		(CUIXml* xml_doc, LPCSTR path)
:CUIAnswerItem(xml_doc, path)
{
	m_icon							= new CUIStatic();m_icon->SetAutoDelete(true);
	AttachChild						(m_icon);

	string512						str;
	CUIXmlInit						xml_init;

	xr_strconcat(str,path,":msg_icon");
	xml_init.InitStatic				(*xml_doc, str, 0, m_icon);
}

void CUIAnswerItemIconed::Init		(LPCSTR text, LPCSTR name, LPCSTR texture_name)
{
	xr_string res;
	res += g_pStringTable->ParseStringFromScript(name).c_str();
	res += "\\n %c[250,255,232,208]";
	res += g_pStringTable->ParseStringFromScript(text).c_str();

	inherited::Init					(res.c_str(), "");
	m_icon->InitTexture				(texture_name);
	m_icon->TextureOn				();
	m_icon->SetStretchTexture		(true);
}

void CUIAnswerItemIconed::Init(LPCSTR text, LPCSTR texture_name, Frect texture_rect)
{
	inherited::Init(text, "");
	m_icon->InitTexture(texture_name);
	Frect texture_rect_;

	texture_rect_.lt.set(texture_rect.x1, texture_rect.y1);
	texture_rect_.rb.set(texture_rect.x2, texture_rect.y2);
	texture_rect_.rb.add(texture_rect_.lt);
	m_icon->GetUIStaticItem().SetTextureRect(texture_rect_);
	m_icon->TextureOn();
	m_icon->SetStretchTexture(true);
}

// return true if we moved selection
bool CUITalkDialogWnd::OffsetQuestionSelection(bool next, bool bLoop)
{
	if (!UIQuestionsList)
		return false;

	WINDOW_LIST& questions = UIQuestionsList->Items();
	if (questions.empty())
	{
		m_ClickedQuestionID = "";
		return false;
	}

	CUIQuestionItem* pQuestion = GetQuestionItemByID(m_ClickedQuestionID);
	if (!pQuestion)
	{
		SetFirstQuestionSelected();
		return false;
	}

	WINDOW_LIST::iterator it = std::find(questions.begin(), questions.end(), pQuestion);
	if (next)
	{
		it++;
		if (it == questions.end())
		{
			if (bLoop)
				it = questions.begin();
			else
				return false;
		}
	}
	else
	{
		if (it == questions.begin())
		{
			if (bLoop)
				it = --questions.end();
			else
				return false;
		}
		else
			--it;
	}

	m_ClickedQuestionID = static_cast<CUIQuestionItem*>(*it)->m_s_value;

	UpdateQuestionSelection();

	ScrollSelectionIntoView();
	return true;
}

void CUITalkDialogWnd::ResetQuestionSelection()
{
	m_ClickedQuestionID = "";
	UpdateQuestionSelection();
}

void CUITalkDialogWnd::SetFirstQuestionSelected()
{
	WINDOW_LIST& questions = UIQuestionsList->Items();
	if (!questions.empty())
	{
		m_ClickedQuestionID = static_cast<CUIQuestionItem*>(*questions.begin())->m_s_value;
		UpdateQuestionSelection();
	}
}

void CUITalkDialogWnd::UpdateQuestionSelection()
{
	WINDOW_LIST& questions = UIQuestionsList->Items();
	for (WINDOW_LIST::iterator it = questions.begin(); it != questions.end(); ++it)
	{
		CUIQuestionItem* pQuestion = static_cast<CUIQuestionItem*>(*it);
		pQuestion->m_text->SetHighlighted(pQuestion->m_s_value == m_ClickedQuestionID && pInput->GetControllerMode());
	}
}

CUIQuestionItem*	CUITalkDialogWnd::GetQuestionItemByID(shared_str questionID)
{
	if (!UIQuestionsList)
		return NULL;

	WINDOW_LIST& questions = UIQuestionsList->Items();
	for (WINDOW_LIST::iterator it = questions.begin(); it != questions.end(); ++it)
	{
		CUIQuestionItem* pQuestion = static_cast<CUIQuestionItem*>(*it);
		if (pQuestion && pQuestion->m_s_value == questionID)
			return pQuestion;
	}
	return NULL;
}

bool CUITalkDialogWnd::HasQuestionWithID(shared_str questionID)
{
	return GetQuestionItemByID(questionID) != NULL;
}

void CUITalkDialogWnd::ScrollSelectionIntoView()
{
	if (m_ClickedQuestionID.size())
	{
		CUIQuestionItem* pQuestion = GetQuestionItemByID(m_ClickedQuestionID);
		if (pQuestion)
		{
			UIQuestionsList->ScrollToItem(pQuestion, iFloor(-UIQuestionsList->ScrollBar()->GetHeight()/2.0f + pQuestion->GetWndRect().height()/2.0f));
		}
	}
}

void CUITalkDialogWnd::ScrollLogUp()
{
	if (UIAnswersList)
	{
		CUIScrollBar* scrollbar = UIAnswersList->ScrollBar();
		if (scrollbar)
			scrollbar->TryScrollDec();
	}
}

void CUITalkDialogWnd::ScrollLogDown()
{
	if (UIAnswersList)
	{
		CUIScrollBar* scrollbar = UIAnswersList->ScrollBar();
		if (scrollbar)
			scrollbar->TryScrollInc();
	}
}

void CUITalkDialogWnd::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
	{
		return;
	}

	//UIInputLegend->AddItem("legend_ui_talk_replies", UIQuestionsList->Items().size() > 1);

	CUIWindow* tradeHint = m_gamepad_legend->FindChild("trade_hint");
	if (tradeHint)
	{
		tradeHint->Show(m_trade_enabled);
		if (tradeHint->ui_cast_static())
		{
			tradeHint->ui_cast_static()->SetTextST(mechanic_mode ? "ui_talk_open_upgrade" : "ui_talk_open_trade");
		}
	}
	CUIWindow* backHint = m_gamepad_legend->FindChild("back_hint");
	if (backHint)
	{
		backHint->Show(m_break_enabled);
	}
}

bool CUITalkDialogWnd::TryClickFinalizerQuestion()
{
	WINDOW_LIST& questions = UIQuestionsList->Items();
	for (WINDOW_LIST::iterator it = questions.begin(); it != questions.end(); ++it)
	{
		CUIQuestionItem* pQuestion = static_cast<CUIQuestionItem*>(*it);
		if (pQuestion && pQuestion->IsFinalizer())
		{
			m_ClickedQuestionID = pQuestion->m_s_value;
			GetMessageTarget()->SendMessage(this, TALK_DIALOG_QUESTION_CLICKED);
			return true;
		}
	}
	return false;
}