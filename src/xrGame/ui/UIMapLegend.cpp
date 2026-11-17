////////////////////////////////////////////////////////////////////////////
//	Module 		: UIMapLegend.cpp
//	Created 	: 03.06.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Map Legend Wnd (PDA : Task) class impl
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UIMapLegend.h"
#include "PdaUiSound.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"

#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIHint.h"

#include "UIInventoryUtilities.h"
#include "../Level.h"
#include "../map_manager.h"

UIMapLegend::~UIMapLegend()
{
	xr_delete( m_list );
}

void UIMapLegend::init_from_xml( CUIXml& xml, const char* path )
{
	CUIXmlInit::InitWindow( xml, path, 0, this );

	XML_NODE*  stored_root = xml.GetLocalRoot();
	XML_NODE*  tmpl_root   = xml.NavigateToNode( path, 0 );
	xml.SetLocalRoot( tmpl_root );

	m_background = UIHelper::CreateFrameWindow( xml, "background_frame", this );
	m_caption    = UIHelper::CreateStatic( xml, "t_caption", this );
	m_btn_close  = UIHelper::Create3tButton( xml, "btn_close", this );

	if ( xml.NavigateToNode( "btn_toggle_all", 0 ) )
	{
		ToggleAllButton = UIHelper::Create3tButton( xml, "btn_toggle_all", this );
		ShowAllText = xml.ReadAttrib( "btn_toggle_all", 0, "text_show", "" );
		HideAllText = xml.ReadAttrib( "btn_toggle_all", 0, "text_hide", "" );
	}

	m_list = new CUIScrollView();
	AttachChild( m_list );
	CUIXmlInit::InitScrollView( xml, "legend_list", 0, m_list );

	const u32 HiddenAlpha = static_cast<u32>( std::clamp( xml.ReadAttribInt( "legend_list", 0, "hidden_alpha", 80 ), 0, 255 ) );

	UIMapLegendItem* list_item = nullptr;

	int cn = xml.GetNodesNum( "legend_list", 0, "item" );
	XML_NODE* root2 = xml.NavigateToNode( "legend_list", 0 );
	xml.SetLocalRoot( root2 );

	for ( int i = 0; i < cn ; ++i )
	{
		list_item = new UIMapLegendItem();
		list_item->SetOwner( this );
		list_item->SetHiddenAlpha( HiddenAlpha );
		list_item->init_from_xml( xml, i );
		m_list->AddWindow( list_item, true );
		Items.push_back( list_item );

		xml.SetLocalRoot( root2 );
	}
	xml.SetLocalRoot( stored_root );

	SyncFromFilter();
}

void UIMapLegend::Show( bool status )
{
	if ( status )
	{
		SyncFromFilter();
	}
	inherited::Show( status );
}

void UIMapLegend::SendMessage( CUIWindow* pWnd, s16 msg, void* pData )
{
	if ( pWnd == m_btn_close )
	{
		if ( msg == BUTTON_DOWN )
		{
			if (m_pUiSounds)
			{
				m_pUiSounds->Play(EPdaUiSound::PanelClose);
			}
			Show( false );
			return;
		}
	}
	if ( ToggleAllButton && pWnd == ToggleAllButton && msg == BUTTON_CLICKED )
	{
		if (m_pUiSounds)
		{
			m_pUiSounds->PlayFilterToggle();
		}
		SetAllCategoriesHidden( AreAllCategoriesVisible() );
		return;
	}
	inherited::SendMessage( pWnd, msg, pData );
}

void UIMapLegend::OnItemClicked(UIMapLegendItem& Item)
{
	if (!g_pGameLevel)
	{
		return;
	}

	if (m_pUiSounds)
	{
		m_pUiSounds->PlayFilterToggle();
	}
	Level().MapManager().SetSpotCategoryHidden(Item.GetCategory(), Item.IsSpotsVisible());
	SyncFromFilter();
}

void UIMapLegend::SyncFromFilter()
{
	if (g_pGameLevel)
	{
		const CMapManager& MapManager = Level().MapManager();
		for (UIMapLegendItem* Item : Items)
		{
			if (Item->IsToggleable())
			{
				Item->SetSpotsVisible(!MapManager.IsSpotCategoryHidden(Item->GetCategory()));
			}
		}
	}
	UpdateToggleAllText();
}

bool UIMapLegend::AreAllCategoriesVisible() const
{
	for (const UIMapLegendItem* Item : Items)
	{
		if (Item->IsToggleable() && !Item->IsSpotsVisible())
		{
			return false;
		}
	}
	return true;
}

void UIMapLegend::SetAllCategoriesHidden(bool Hidden)
{
	if (!g_pGameLevel)
	{
		return;
	}

	CMapManager& MapManager = Level().MapManager();
	for (const UIMapLegendItem* Item : Items)
	{
		if (Item->IsToggleable())
		{
			MapManager.SetSpotCategoryHidden(Item->GetCategory(), Hidden);
		}
	}
	SyncFromFilter();
}

void UIMapLegend::UpdateToggleAllText()
{
	if (!ToggleAllButton)
	{
		return;
	}

	const shared_str& Text = AreAllCategoriesVisible() ? HideAllText : ShowAllText;
	if (Text.size())
	{
		ToggleAllButton->TextItemControl()->SetTextST(Text.c_str());
	}
}

// =================================================================================================

UIMapLegendItem::UIMapLegendItem()
{

}

UIMapLegendItem::~UIMapLegendItem()
{

}

void UIMapLegendItem::init_from_xml( CUIXml& xml, int index )
{
	CUIXmlInit::InitWindow( xml, "item", index, this );

	XML_NODE* root3 = xml.NavigateToNode( "item", index );
	xml.SetLocalRoot( root3 );

	Category = xml.ReadAttrib( root3, "category", "" );
	if ( !Category.size() )
	{
		xr_string Patterns = xml.ReadAttrib( root3, "spots", "" );
		if ( xml.ReadAttribInt( root3, "user_spots", 0 ) != 0 )
		{
			if ( !Patterns.empty() )
			{
				Patterns += ",";
			}
			Patterns += CMapSpotFilter::UserSpotsToken;
		}

		if ( !Patterns.empty() )
		{
			const xr_string CategoryId = "legend:" + Patterns;
			Category = CategoryId.c_str();
			if ( g_pGameLevel )
			{
				Level().MapManager().RegisterSpotCategory( Category, Patterns.c_str() );
			}
		}
	}

	m_image[0] = UIHelper::CreateStatic( xml, "image",   this );

	if ( xml.NavigateToNode( "image_1" ) )
	{
		m_image[1] = UIHelper::CreateStatic( xml, "image_1", this );
	}
	if ( xml.NavigateToNode( "image_2" ) )
	{
		m_image[2] = UIHelper::CreateStatic( xml, "image_2", this );
	}
	if ( xml.NavigateToNode( "image_3" ) )
	{
		m_image[3] = UIHelper::CreateStatic( xml, "image_3", this );
	}

	m_text = UIHelper::CreateStatic( xml, "text_static", this );
	m_text->AdjustHeightToText();

	for ( int i = 0; i < 4; ++i )
	{
		if ( m_image[i] )
		{
			ImageColors[i] = m_image[i]->GetTextureColor();
		}
	}
	TextColor = m_text->GetTextColor();

	float h = m_text->GetWndPos().y + m_text->GetHeight();
	h = std::max( h, GetHeight() );
	SetHeight( h );
}

bool UIMapLegendItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (Owner && IsToggleable() && (mouse_action == WINDOW_LBUTTON_DOWN || mouse_action == WINDOW_LBUTTON_DB_CLICK))
	{
		Owner->OnItemClicked(*this);
		return true;
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}

void UIMapLegendItem::SetSpotsVisible(bool Visible)
{
	SpotsVisible = Visible;

	auto ApplyAlpha = [this](u32 Color)
	{
		if (SpotsVisible)
		{
			return Color;
		}
		return subst_alpha(Color, color_get_A(Color) * HiddenAlpha / 255);
	};

	for (int i = 0; i < 4; ++i)
	{
		if (m_image[i])
		{
			m_image[i]->SetTextureColor(ApplyAlpha(ImageColors[i]));
		}
	}
	if (m_text)
	{
		m_text->SetTextColor(ApplyAlpha(TextColor));
	}
}
