#include "pch_script.h"
#include "UITaskItem.h"
#include "UIXmlInit.h"
#include "UI3tButton.h"
#include "../gametask.h"
#include "../string_table.h"
#include "UIEventsWnd.h"
#include "UIEditBoxEx.h"
#include "UIEditBox.h"
#include "UIInventoryUtilities.h"
#include "../map_location.h"
#include "../map_manager.h"
#include "../level.h"
#include "../actor.h"
#include "../gametaskmanager.h"

CUITaskItem::CUITaskItem			(CUIEventsWnd* w)
:m_GameTask			(NULL),
m_TaskObjectiveIdx(u16(-1)),
m_EventsWnd(w),
CUIWindow()
{
}

CUITaskItem::~CUITaskItem			()
{}

void CUITaskItem::SetGameTask(CGameTask* gt, u16 obj_idx)				
{ 
	m_GameTask			= gt;
	m_TaskObjectiveIdx	= obj_idx;
	R_ASSERT(m_GameTask->m_Objectives.size() > m_TaskObjectiveIdx);
}

void CUITaskItem::SendMessage				(CUIWindow* pWnd, s16 msg, void* pData)
{
	inherited::SendMessage(pWnd, msg, pData);
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

SGameTaskObjective*	CUITaskItem::Objective	()	
{
	if (m_TaskObjectiveIdx > m_GameTask->m_Objectives.size())
	{
		Msg("! no objective(%d) for task item %s size=[%d]", m_TaskObjectiveIdx, m_GameTask->m_ID.c_str(), m_GameTask->m_Objectives.size());
		return NULL;
	}
	return &m_GameTask->m_Objectives[m_TaskObjectiveIdx];
}

bool CUITaskItem::OnMouseDown (int mouse_btn)
{
	return inherited::OnMouseDown(mouse_btn);
}

void CUITaskItem::Init				()
{
	SetWindowName					("job_item");
	Register						(this);
	AddCallbackStr						("job_item",BUTTON_CLICKED,CUIWndCallback::void_function(this,&CUITaskItem::OnItemClicked));
}

void CUITaskItem::OnItemClicked(CUIWindow*, void*)
{
	m_EventsWnd->ShowDescription						(GameTask(), ObjectiveIdx());
}


CUITaskRootItem::CUITaskRootItem	(CUIEventsWnd* w)
:inherited(w), m_can_update(false)
{
	Init();
}

CUITaskRootItem::~CUITaskRootItem		()
{}

void CUITaskRootItem::Init			()
{
	inherited::Init					();

	m_taskImage					= new CUIStatic();		m_taskImage->SetAutoDelete(true);			AttachChild(m_taskImage);
	m_captionStatic				= new CUIStatic();		m_captionStatic->SetAutoDelete(true);		AttachChild(m_captionStatic);
	m_remTimeStatic				= new CUIStatic();		m_remTimeStatic->SetAutoDelete(true);		AttachChild(m_remTimeStatic);

	m_switchDescriptionBtn		= new CUI3tButton();	m_switchDescriptionBtn->SetAutoDelete(true); AttachChild(m_switchDescriptionBtn);
	m_captionTime				= new CUI3tButton();	m_captionTime->SetAutoDelete(true);			AttachChild(m_captionTime);
	
	CUIXmlInit xml_init;
	CUIXml&						uiXml = m_EventsWnd->m_ui_task_item_xml;
	xml_init.InitWindow			(uiXml,"task_root_item",0,this);

	xml_init.InitStatic			(uiXml,	"task_root_item:image",			0,	m_taskImage);
	xml_init.InitStatic			(uiXml,	"task_root_item:caption",		0,	m_captionStatic);
	xml_init.InitStatic			(uiXml,	"task_root_item:caption_time",	0,	m_captionTime);
	xml_init.InitStatic			(uiXml,	"task_root_item:rem_time",		0,	m_remTimeStatic);
	
	xml_init.Init3tButton		(uiXml,"task_root_item:switch_description_btn",0,m_switchDescriptionBtn);

	m_switchDescriptionBtn->SetWindowName("m_switchDescriptionBtn");
	m_switchDescriptionBtn->SetMessageTarget(this);
	AddCallbackStr				("m_switchDescriptionBtn",BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUITaskRootItem::OnSwitchDescriptionClicked));
}


void CUITaskRootItem::SetGameTask(CGameTask* gt, u16 obj_idx)				
{
	inherited::SetGameTask(gt, obj_idx);

	CStringTable		stbl;
	SGameTaskObjective	*obj = &m_GameTask->m_Objectives[m_TaskObjectiveIdx];

	m_taskImage->InitTexture(*obj->icon_texture_file_name);

	Frect r							= obj->icon_rect;
	Frect texture_rect;

	texture_rect.lt.set					(r.x1, r.y1);
	texture_rect.rb.set					(r.x2, r.y2);
	texture_rect.rb.add					(texture_rect.lt);
	m_taskImage->SetTextureRect	(texture_rect);
	m_taskImage->SetStretchTexture	(true);

	m_captionStatic->TextItemControl()->SetText		(*stbl.translate(m_GameTask->m_Title));
	m_captionStatic->AdjustHeightToText	();
	
	xr_string	txt ="";
	txt			+= *(InventoryUtilities::GetDateAsString(gt->m_ReceiveTime, InventoryUtilities::edpDateToDay));
	txt			+= " ";
	txt			+= *(InventoryUtilities::GetTimeAsString(gt->m_ReceiveTime, InventoryUtilities::etpTimeToMinutes));

	m_captionTime->TextItemControl()->SetText(txt.c_str());
	m_captionTime->SetWndPos(m_captionTime->GetWndPos().x,m_captionStatic->GetWndPos().y+m_captionStatic->GetHeight()+3.0f);

	float h = _max	(m_taskImage->GetWndPos().y+m_taskImage->GetHeight(),m_captionTime->GetWndPos().y+m_captionTime->GetHeight());
	h	= _max(h,m_switchDescriptionBtn->GetWndPos().y+m_switchDescriptionBtn->GetHeight());
	SetHeight						(h);
	
	
	m_curr_descr_mode				= m_EventsWnd->GetDescriptionMode();
	if(m_curr_descr_mode)
		m_switchDescriptionBtn->InitTexture	("ui_icons_newPDA_showtext");
	else
		m_switchDescriptionBtn->InitTexture	("ui_icons_newPDA_showmap");

	m_remTimeStatic->Show			(	GameTask()->Objective(0).TaskState()==eTaskStateInProgress && 
										(GameTask()->m_ReceiveTime!=GameTask()->m_TimeToComplete) );
	
	if( m_remTimeStatic->IsShown() )
	{
		float _height	= GetWndSize().y;
		Fvector2 _pos	= m_captionTime->GetWndPos();
		_pos.y			+= m_captionTime->GetWndSize().y;
		_pos.x			= m_remTimeStatic->GetWndPos().x;

		m_remTimeStatic->SetWndPos(_pos);

		_height			= _max(_height, _pos.y+m_remTimeStatic->GetWndSize().y);
		SetHeight		(_height);
	}
}

void CUITaskRootItem::OnMapShown (bool state)
{
	m_switchDescriptionBtn->SetButtonState(state ? CUIButton::BUTTON_NORMAL : CUIButton::BUTTON_PUSHED);
}

void CUITaskRootItem::Update		()
{
	inherited::Update();
	if( m_curr_descr_mode	!= m_EventsWnd->GetDescriptionMode() ){
		m_curr_descr_mode				= m_EventsWnd->GetDescriptionMode();
		if(m_curr_descr_mode)
			m_switchDescriptionBtn->InitTexture	("ui_icons_newPDA_showtext");
		else
			m_switchDescriptionBtn->InitTexture	("ui_icons_newPDA_showmap");
	}

//	m_switchDescriptionBtn->SetButtonState(m_EventsWnd->GetDescriptionMode() ? CUIButton::BUTTON_NORMAL : CUIButton::BUTTON_PUSHED);
	
	if(m_remTimeStatic->IsShown())
	{
		string512									buff, buff2;
		InventoryUtilities::GetTimePeriodAsString	(buff, sizeof(buff), Level().GetGameTime(), GameTask()->m_TimeToComplete);
		xr_sprintf										(buff2,"%s %s", *CStringTable().translate("ui_st_time_remains"), buff);
		m_remTimeStatic->TextItemControl()->SetText					(buff2);
	
	}
}

bool CUITaskRootItem::OnDbClick	()
{
	return inherited::OnDbClick();
}

void CUITaskRootItem::OnSwitchDescriptionClicked	(CUIWindow*, void*)
{
	m_switchDescriptionBtn->SetButtonState(m_EventsWnd->GetDescriptionMode() ? CUIButton::BUTTON_PUSHED : CUIButton::BUTTON_NORMAL);

	m_EventsWnd->SetDescriptionMode						(!m_EventsWnd->GetDescriptionMode());
	OnItemClicked										(this, NULL);
	m_can_update = !m_can_update;
}

void CUITaskRootItem::MarkSelected (bool b)
{
}


CUITaskSubItem::CUITaskSubItem		(CUIEventsWnd* w)
:inherited(w)
{
	Init();
}

CUITaskSubItem::~CUITaskSubItem		()
{}

void CUITaskSubItem::Init			()
{
	inherited::Init					();
	CUIXml&						uiXml = m_EventsWnd->m_ui_task_item_xml;

	m_stateStatic			= new CUIStatic();		m_stateStatic->SetAutoDelete(true);				AttachChild(m_stateStatic);
	m_descriptionStatic		= new CUIStatic();		m_descriptionStatic->SetAutoDelete(true);		AttachChild(m_descriptionStatic);
	m_ActiveObjectiveStatic	= new CUIStatic();		m_ActiveObjectiveStatic->SetAutoDelete(true);	AttachChild(m_ActiveObjectiveStatic);
	m_showDescriptionBtn	= new CUI3tButton();	m_showDescriptionBtn->SetAutoDelete(true);		AttachChild(m_showDescriptionBtn);

	m_showDescriptionBtn->SetWindowName		("m_showDescriptionBtn");
	m_showDescriptionBtn->SetMessageTarget	(this);

	AddCallbackStr						("m_showDescriptionBtn",BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUITaskSubItem::OnShowDescriptionClicked));


	CUIXmlInit xml_init;
	xml_init.InitWindow				(uiXml,	"task_sub_item",							0,	this);
	xml_init.InitStatic				(uiXml,	"task_sub_item:state_image",				0,	m_stateStatic);
	xml_init.InitStatic				(uiXml,	"task_sub_item:description",				0,	m_descriptionStatic);
	xml_init.InitStatic				(uiXml,	"task_sub_item:active_objecttive_image",	0,	m_ActiveObjectiveStatic);
	xml_init.Init3tButton			(uiXml,	"task_sub_item:show_descr_btn",			0,	m_showDescriptionBtn);


	m_active_color					= xml_init.GetColor(uiXml, "task_sub_item:description:text_colors:active",		0, 0x00);
	m_failed_color					= xml_init.GetColor(uiXml, "task_sub_item:description:text_colors:failed",		0, 0x00);
	m_accomplished_color			= xml_init.GetColor(uiXml, "task_sub_item:description:text_colors:accomplished",0, 0x00);
	m_skiped_color					= xml_init.GetColor(uiXml, "task_sub_item:description:text_colors:skiped"		,0, 0x00);
}

void CUITaskSubItem::SetGameTask	(CGameTask* gt, u16 obj_idx)				
{
	inherited::SetGameTask			(gt, obj_idx);

	CStringTable		stbl;
	SGameTaskObjective	*obj = &m_GameTask->m_Objectives[m_TaskObjectiveIdx];

	m_descriptionStatic->TextItemControl()->SetText				(*stbl.translate(obj->description));
	m_descriptionStatic->AdjustHeightToText		();
	float h = _max(	m_ActiveObjectiveStatic->GetWndPos().y+m_ActiveObjectiveStatic->GetHeight(),
					m_descriptionStatic->GetWndPos().y+ m_descriptionStatic->GetHeight());
	SetHeight									(h);
	switch (obj->TaskState())
	{
//.		case eTaskUserDefined:
		case eTaskStateInProgress:
			m_stateStatic->InitTexture				("ui_icons_PDA_subtask_active");
			m_descriptionStatic->SetTextureColor		(m_active_color);
			break;
		case eTaskStateFail:
			m_stateStatic->InitTexture				("ui_icons_PDA_subtask_failed");
			m_descriptionStatic->SetTextureColor		(m_failed_color);
			break;
		case eTaskStateCompleted:
			m_stateStatic->InitTexture				("ui_icons_PDA_subtask_accomplished");
			m_descriptionStatic->SetTextureColor		(m_accomplished_color);
			break;
		case eTaskStateSkiped:
			m_stateStatic->InitTexture				("ui_icons_PDA_subtask_skiped");
			m_descriptionStatic->SetTextureColor		(m_skiped_color);
			break;
		default:
			NODEFAULT;
	};
}

void CUITaskSubItem::Update					()
{
	inherited::Update						();
	SGameTaskObjective	*obj				= &m_GameTask->m_Objectives[m_TaskObjectiveIdx];

	if (obj->visible_subtask == false)
	{
		Show									(false);
		m_ActiveObjectiveStatic->Show			(false);
		m_showDescriptionBtn->Show				(false);
		m_descriptionStatic->Show				(false);
	} else
	{
		Show									(true);
		m_descriptionStatic->Show				(true);
		bool bIsActive							= (Actor()->GameTaskManager().ActiveObjective() == obj); 
		m_ActiveObjectiveStatic->Show			(bIsActive);
		m_showDescriptionBtn->Show				(m_EventsWnd->ItemHasDescription(this));
	}
}

bool CUITaskSubItem::OnDbClick()
{
	SGameTaskObjective	*obj					= &m_GameTask->m_Objectives[m_TaskObjectiveIdx];
	if(obj->TaskState()!=eTaskStateInProgress)	return true;

	bool bIsActive								= (Actor()->GameTaskManager().ActiveObjective() == obj); 
	Actor()->GameTaskManager().SetActiveTask((!bIsActive)?m_GameTask->m_ID:"", m_TaskObjectiveIdx);
	return										true;
}

void CUITaskSubItem::OnActiveObjectiveClicked()
{
	m_EventsWnd->ShowDescription			(GameTask(), ObjectiveIdx());
}

void CUITaskSubItem::OnShowDescriptionClicked (CUIWindow*, void*)
{
	m_EventsWnd->ShowDescription						(GameTask(), ObjectiveIdx());
}

void CUITaskSubItem::OnMapShown (bool state)
{
}

void CUITaskSubItem::MarkSelected (bool b)
{
	m_showDescriptionBtn->SetButtonState		(b ? CUIButton::BUTTON_PUSHED : CUIButton::BUTTON_NORMAL);
}

/*
CUIUserTaskItem::CUIUserTaskItem(CUIEventsWnd* w)
:inherited(w)
{
	m_edtWnd = NULL;
	Init();
}

CUIUserTaskItem::~CUIUserTaskItem			()
{
	delete_data	(m_edtWnd);
}

void  CUIUserTaskItem::Init					()
{
	inherited::Init					();
	CUIXml&							uiXml = m_EventsWnd->m_ui_task_item_xml;

	m_image							= new CUIStatic();		m_image->SetAutoDelete(true);				AttachChild(m_image);

	m_descriptionStatic				= new CUIStatic();		m_descriptionStatic->SetAutoDelete(true);	AttachChild(m_descriptionStatic);

	m_captionStatic					= new CUIStatic();		m_captionStatic->SetAutoDelete(true);	AttachChild(m_captionStatic);
	

	m_showLocationBtn				= new CUI3tButton();	m_showLocationBtn->SetAutoDelete(true);		AttachChild(m_showLocationBtn);
	m_showLocationBtn->				SetWindowName("m_showLocationBtn");
	Register						(m_showLocationBtn);
	AddCallback						(m_showLocationBtn->WindowName(),BUTTON_CLICKED,boost::bind(&CUIUserTaskItem::OnShowLocationClicked,this));

	m_showPointerBtn				= new CUI3tButton();	m_showPointerBtn->SetAutoDelete(true);		AttachChild(m_showPointerBtn);
	m_showPointerBtn->				SetWindowName("m_showPointerBtn");
	Register						(m_showPointerBtn);
	AddCallback						(m_showPointerBtn->WindowName(),BUTTON_CLICKED,boost::bind(&CUIUserTaskItem::OnShowPointerClicked,this));


	m_editTextBtn					= new CUI3tButton();	m_editTextBtn->SetAutoDelete(true);		AttachChild(m_editTextBtn);
	m_editTextBtn->					SetWindowName("m_editTextBtn");
	Register						(m_editTextBtn);
	AddCallback						(m_editTextBtn->WindowName(), BUTTON_CLICKED,boost::bind(&CUIUserTaskItem::OnEditTextClicked,this));

	m_removeBtn						= new CUI3tButton();	m_removeBtn->SetAutoDelete(true);		AttachChild(m_removeBtn);
	m_removeBtn->					SetWindowName("m_removeBtn");
	Register						(m_removeBtn);
	AddCallback						(m_removeBtn->WindowName(), BUTTON_CLICKED,boost::bind(&CUIUserTaskItem::OnRemoveClicked,this));


	CUIXmlInit xml_init;
	xml_init.InitWindow				(uiXml,"task_user_item",0,this);

	xml_init.InitStatic				(uiXml,"task_user_item:image",0,m_image);
	xml_init.InitStatic				(uiXml,"task_user_item:description",0,m_descriptionStatic);
	xml_init.InitStatic				(uiXml,"task_user_item:caption",0,m_captionStatic);
	xml_init.Init3tButton			(uiXml,"task_user_item:location_btn",0,m_showLocationBtn);
	xml_init.Init3tButton			(uiXml,"task_user_item:show_pointer_btn",0,m_showPointerBtn);
	xml_init.Init3tButton			(uiXml,"task_user_item:edit_text_btn",0,m_editTextBtn);
	xml_init.Init3tButton			(uiXml,"task_user_item:remove_btn",0,m_removeBtn);

}

void CUIUserTaskItem::Update					()
{
	inherited::Update		();
	SGameTaskObjective	*obj				= &m_GameTask->m_Objectives[m_TaskObjectiveIdx];
	CMapLocation* ml						= obj->LinkedMapLocation();
	bool bHasLocation						= (NULL != ml);
	m_showPointerBtn->Show					(bHasLocation && ml->SpotEnabled());
	if(bHasLocation)
	{
		bool bPointer						= (Actor()->GameTaskManager().ActiveObjective() == obj);
		m_showPointerBtn->SetButtonState		(bPointer ? CUIButton::BUTTON_PUSHED : CUIButton::BUTTON_NORMAL);
//.		bool bShown							= m_GameTask->ShownLocations();
		bool bShown							= true;
		m_showLocationBtn->SetButtonState	(bShown ? CUIButton::BUTTON_PUSHED : CUIButton::BUTTON_NORMAL);
	}
}

void CUIUserTaskItem::SetGameTask				(CGameTask* gt, u16 obj_idx)
{
	inherited::SetGameTask			(gt, obj_idx);
	CStringTable		stbl;
	SGameTaskObjective	*obj = &m_GameTask->m_Objectives[m_TaskObjectiveIdx];

	m_image->InitTexture		(*obj->icon_texture_file_name);
	Frect r						= obj->icon_rect;
	Frect texture_rect;

	texture_rect.lt.set					(r.x1, r.y1);
	texture_rect.rb.set					(r.x2, r.y2);
	texture_rect.rb.add					(texture_rect.lt);
	m_image->SetTextureRect	(texture_rect);
	m_image->SetStretchTexture	(true);

	m_captionStatic->SetText					(*stbl.translate(gt->m_Title));
	m_captionStatic->AdjustHeightToText			();

	float h1 = _max( m_image->GetWndPos().y+m_image->GetHeight(),
					m_captionStatic->GetWndPos().y + m_captionStatic->GetHeight() );
	m_descriptionStatic->SetWndPos				(m_descriptionStatic->GetWndPos().x, h1+4.0f);

	m_descriptionStatic->SetText				(*stbl.translate(obj->description));
	m_descriptionStatic->AdjustHeightToText		();

	float h = _max(	m_image->GetWndPos().y+m_image->GetHeight(),
					m_descriptionStatic->GetWndPos().y+ m_descriptionStatic->GetHeight());

	SetHeight									(h+10.0f);
}

void CUIUserTaskItem::OnShowPointerClicked	()
{
	bool bPushed = m_showPointerBtn->GetCheck();
	if(bPushed)
		Actor()->GameTaskManager().SetActiveTask((bPushed)?m_GameTask->m_ID:"", m_TaskObjectiveIdx);
//.	m_GameTask->HighlightSpotOnMap			(m_TaskObjectiveIdx,bPushed);
}

void CUIUserTaskItem::OnShowLocationClicked	()
{
//.	bool bPushed = m_showLocationBtn->GetCheck	();
//.	m_GameTask->ShowLocations					(bPushed);
}

void CUIUserTaskItem::MarkSelected				(bool b)
{
}

void CUIUserTaskItem::OnDescriptionChanged		()
{
	Objective()->description = m_descriptionStatic->GetText();
}

void CUIUserTaskItem::OnEditTextClicked		()
{
	delete_data			(m_edtWnd);
	m_edtWnd			= new CUIUserTaskEditWnd(this);
	m_edtWnd->Start		();
}

void CUIUserTaskItem::OnRemoveClicked		()
{
	Level().MapManager().RemoveMapLocation(Objective()->LinkedMapLocation());
}


CUIUserTaskEditWnd::CUIUserTaskEditWnd		(CUIUserTaskItem* itm)
{
	m_userTask = itm;
	Init();
}

void CUIUserTaskEditWnd::SendMessage		(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIUserTaskEditWnd::Start()
{
	CStringTable stbl;

//.	m_userTask = itm;
	m_editCaption->SetText			(*stbl.translate(m_userTask->GameTask()->m_Title));
	m_editDescription->SetText		(*stbl.translate(m_userTask->Objective()->description));
	ShowDialog(true);
}

void CUIUserTaskEditWnd::OnOk			()
{
	m_userTask->GameTask()->m_Title			= m_editCaption->GetText();
	m_userTask->Objective()->description	= m_editDescription->GetText();
	m_userTask->Objective()->LinkedMapLocation	()->SetHint(m_editDescription->GetText());
	m_userTask->m_EventsWnd->Reload			();

	HideDialog();
}

void CUIUserTaskEditWnd::OnCancel				()
{
	GetHolder()->StartStopMenu(this, false);
}

void CUIUserTaskEditWnd::Init					()
{
	Hide					();
	CUIXml&					uiXml = m_userTask->m_EventsWnd->m_ui_task_item_xml;

	m_background		= new CUIFrameWindow();		m_background->SetAutoDelete(true);
	AttachChild			(m_background);
	
	m_btnOk				= new CUI3tButton();	m_btnOk->SetAutoDelete(true);		m_background->AttachChild(m_btnOk);
	m_btnOk->SetWindowName("m_btnOk");
	Register			(m_btnOk);
	AddCallback			(m_btnOk->WindowName(),BUTTON_CLICKED,boost::bind(&CUIUserTaskEditWnd::OnOk,this));

	m_btnCancel			= new CUI3tButton();	m_btnCancel->SetAutoDelete(true);	m_background->AttachChild(m_btnCancel);
	m_btnCancel->SetWindowName("m_btnCancel");
	Register			(m_btnCancel);
	AddCallback			(m_btnCancel->WindowName(),BUTTON_CLICKED,boost::bind(&CUIUserTaskEditWnd::OnCancel,this));

	m_editCaption		= new CUIEditBox();			m_editCaption->SetAutoDelete(true);		m_background->AttachChild(m_editCaption);
	m_editDescription	= new CUIEditBoxEx();		m_editDescription->SetAutoDelete(true); m_background->AttachChild(m_editDescription);


	CUIXmlInit xml_init;
	xml_init.InitWindow				(uiXml,"edit_user_item",0,								this);
	xml_init.InitFrameWindow		(uiXml,"edit_user_item:background",0,					m_background);
	xml_init.Init3tButton			(uiXml,"edit_user_item:background:ok_btn",0,			m_btnOk);
	xml_init.Init3tButton			(uiXml,"edit_user_item:background:cancel_btn",0,		m_btnCancel);
	xml_init.InitEditBox			(uiXml,"edit_user_item:background:edit_caption",0,		m_editCaption);
	xml_init.InitEditBoxEx			(uiXml,"edit_user_item:background:edit_description",0,	m_editDescription);

}
*/