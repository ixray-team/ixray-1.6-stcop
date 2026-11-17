////////////////////////////////////////////////////////////////////////////
//	Module 		: UIMapLegend.h
//	Created 	: 03.06.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Map Legend Wnd (PDA : Task) class
////////////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIFrameWindow;
class CUIScrollView;
class CUIStatic;
class CUI3tButton;
class CUICheckButton;
class CUIFrameLineWnd;
class UIHint;
class CPdaUiSounds;
class UIMapLegendItem;

class UIMapLegend final : public CUIWindow
{
private:
	typedef CUIWindow	inherited;

public:
					UIMapLegend			() = default;
	virtual ~UIMapLegend		() override;

			void	init_from_xml		( CUIXml& xml, const char* path );
			void	SetUiSounds			(CPdaUiSounds* uiSounds) { m_pUiSounds = uiSounds; }

	virtual void	Show				( bool status ) override;
	virtual void	SendMessage			( CUIWindow* pWnd, s16 msg, void* pData ) override;

			void	OnItemClicked		(UIMapLegendItem& Item);
			void	SyncFromFilter		();

	virtual CUIWindow* ui_cast_window() override { return this; }

private:
			bool	AreAllCategoriesVisible	() const;
			void	SetAllCategoriesHidden	(bool Hidden);
			void	UpdateToggleAllText	();

private: // m_
	CUIFrameWindow*		m_background;
	CUIScrollView*		m_list;

	CUIStatic*			m_caption;
	CUI3tButton*		m_btn_close;
	CPdaUiSounds*		m_pUiSounds = nullptr;

	CUI3tButton*		ToggleAllButton = nullptr;
	shared_str			ShowAllText;
	shared_str			HideAllText;
	xr_vector<UIMapLegendItem*> Items;

}; // class UIMapLegend

// -------------------------------------------------------------------------------------------------

class UIMapLegendItem final : public CUIWindow
{
private:
	typedef CUIWindow	inherited;

public:
					UIMapLegendItem		();
	virtual ~UIMapLegendItem	() override;

			void	init_from_xml		( CUIXml& xml, int index );
//	virtual void	Update				();

	virtual bool	OnMouseAction		(float x, float y, EUIMessages mouse_action) override;

			void	SetOwner			(UIMapLegend* Legend) { Owner = Legend; }
			void	SetHiddenAlpha		(u32 Alpha) { HiddenAlpha = Alpha; }
			bool	IsToggleable		() const { return Category.size() != 0; }
			bool	IsSpotsVisible		() const { return SpotsVisible; }
			const shared_str& GetCategory	() const { return Category; }
			void	SetSpotsVisible		(bool Visible);

	virtual CUIWindow* ui_cast_window() override { return this; }

private: // m_
	CUIStatic*		m_image[4] = {};
	CUIStatic*		m_text = nullptr;

	UIMapLegend*	Owner = nullptr;
	shared_str		Category;
	bool			SpotsVisible = true;
	u32				HiddenAlpha = 80;
	u32				ImageColors[4] = {};
	u32				TextColor = 0;

};