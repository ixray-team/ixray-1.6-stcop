#pragma once

#include "../../xrCore/xr_delegate.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrEngine/AI/alife_space.h"

class CUIXml;
class CUI3tButton;
class CUIStatic;

// Standalone calendar popup widget; owned and driven by CUILogsWnd.
class CUICalendar final :
    public CUIWindow,
    public CUIWndCallback
{
public:
    using inherited = CUIWindow;
    using DaySelectedCallback = xr_delegate<void(ALife::_TIME_ID)>;

    CUICalendar() = default;
    ~CUICalendar() override = default;

    bool InitFromXml(CUIXml& xml, CUIWindow* owner, CUI3tButton* anchorButton);
    void SetOnDaySelected(const DaySelectedCallback& callback);
    void UpdateState(ALife::_TIME_ID start, ALife::_TIME_ID selected, bool filterNews, bool filterTalk);
    void TogglePopup();
    void HidePopup() { Show(false); }
    bool HasUi() const { return !_cells.empty(); }

    void SendMessage(CUIWindow* window, s16 msg, void* data = nullptr) override;

    CUIWindow* ui_cast_window() override { return this; }

private:
    void BuildUi(CUIXml& xml);
    void Refresh();
    void SyncViewMonth();
    void RaisePopupToFront();
    void PositionNearAnchor();
    void ShiftMonth(int delta);
    void OnDay(CUIWindow* window, void* data);
    void OnMonthPrev(CUIWindow* window, void* data) { ShiftMonth(-1); }
    void OnMonthNext(CUIWindow* window, void* data) { ShiftMonth(1); }

    ALife::_TIME_ID NormalizeDay(ALife::_TIME_ID time) const;
    ALife::_TIME_ID DayToPeriod(u32 year, u32 month, u32 day) const;
    ALife::_TIME_ID MonthCaptionPeriod() const;
    bool IsAllowedDay(u32 year, u32 month, u32 day) const;
    bool CanShiftMonth(int delta) const;
    void EnableMonthControls();
    int CellDayForIndex(u32 cellIndex, u32 monthOffset, u32 daysInMonth) const;
    void ApplyCellAppearance(CUI3tButton* cell, u32 day, bool hasNews, bool enabled, bool isSelected);

    static constexpr u64 kDay2Ms = u64(24 * 60 * 60 * 1000);

    CUIWindow* _owner = nullptr;
    CUI3tButton* _anchor = nullptr;
    CUIStatic* _monthCaption = nullptr;
    CUI3tButton* _monthPrev = nullptr;
    CUI3tButton* _monthNext = nullptr;
    xr_vector<CUI3tButton*> _cells;

    DaySelectedCallback _onSelect;
    ALife::_TIME_ID _start = 0;
    ALife::_TIME_ID _selected = 0;
    ALife::_TIME_ID _current = 0;
    bool _filterNews = true;
    bool _filterTalk = true;
    u32 _viewYear = 0;
    u32 _viewMonth = 0;
    u32 _cols = 7;
    float _cellSize = 28.0f;
    float _gap = 2.0f;
    shared_str _cellTexture;
    bool _anchorToButton = true;
    bool _freeMonthNav = false;
    u32 _freeYearMin = 1986;
    u32 _freeYearMax = 2099;
};
