#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "../../xrUI/xrUIXmlParser.h"

class CUIStatic;
struct GAME_NEWS_DATA;

// Split PDA logs (logs_list_news + logs_list_dialogs): optional per-column logs_item and stack layout.
// Nested template: logs_list_news/logs_item or logs_list_dialogs/logs_item.
// Stack nodes (split only): logs_itm_stack, logs_row_stack, logs_text_stack (sp_align, spacing).
class CUINewsItemWnd final : public CUIWindow
{
	using inherited = CUIWindow;

	enum class ELayout : u8
	{
		Legacy = 0,
		SingleStack = 1,
		NestedStack = 2,
	};

	CUIStatic* _uiDate = nullptr;
	CUIStatic* _uiCaption = nullptr;
	CUIStatic* _uiText = nullptr;
	CUIStatic* _uiImage = nullptr;
	CUIStatic* _dialogReplicaLine = nullptr;
	CUIStackPanel* _itemStack = nullptr;
	CUIStackPanel* _rowStack = nullptr;
	CUIStackPanel* _textStack = nullptr;
	ELayout _layout = ELayout::Legacy;
	float _itemStackSpacing = 0.f;
	float _rowStackSpacing = 0.f;
	float _textStackSpacing = 0.f;
	bool _legacyMode = false;
	bool _hasDialogReplicaLayout = false;

	void InitLegacyFromXml(CUIXml& uiXml);
	bool InitStackedFromXml(CUIXml& uiXml);
	void CreateNewsStatics(CUIXml& uiXml, CUIWindow* dateParent, CUIWindow* captionParent, CUIWindow* textParent, CUIWindow* imageParent);
	void UpdateStackedLayoutHeight();
	float ReadStackSpacing(CUIXml& uiXml, const char* path) const;
	void SetupLegacy(GAME_NEWS_DATA& newsData);
	void SetupStacked(GAME_NEWS_DATA& newsData);
	void ApplyNewsTexture(GAME_NEWS_DATA& newsData);

public:
	CUINewsItemWnd();
	~CUINewsItemWnd() override;

	void Init(CUIXml& uiXml, const char* startFrom, bool allowStackLayout);
	void Setup(GAME_NEWS_DATA& newsData);
	void Update() override {}
	CUIWindow* ui_cast_window() override { return this; }
};
