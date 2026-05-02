#include "StdAfx.h"
#include "CUICalendar.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrEngine/date_time.h"

#include "UIInventoryUtilities.h"

#include "../Actor.h"
#include "../game_news.h"
#include "../alife_registry_wrappers.h"
#include "../Level.h"

namespace
{
u32 MonthLength(u32 year, u32 month)
{
    static const u8 kDays[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (month < 1 || month > 12)
    {
        return 0;
    }
    u32 days = kDays[month - 1];
    if (month == 2 && ((year % 400 == 0) || ((year % 4 == 0) && (year % 100 != 0))))
    {
        ++days;
    }
    return days;
}

u32 MondayIndex(u32 year, u32 month, u32 day)
{
    if (month < 1 || month > 12 || day < 1)
    {
        return 0;
    }
    static const int kShift[12] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
    const u32 y = month < 3 ? year - 1 : year;
    return ((y + y / 4 - y / 100 + y / 400 + kShift[month - 1] + day - 1) % 7 + 6) % 7;
}

void ReadYmd(ALife::_TIME_ID time, u32& year, u32& month, u32& day)
{
    u32 h = 0;
    u32 min = 0;
    u32 s = 0;
    u32 ms = 0;
    split_time(time, year, month, day, h, min, s, ms);
}

void ReadYm(ALife::_TIME_ID time, u32& year, u32& month)
{
    u32 day = 0;
    u32 h = 0;
    u32 min = 0;
    u32 s = 0;
    u32 ms = 0;
    split_time(time, year, month, day, h, min, s, ms);
}

bool PassesFilter(const GAME_NEWS_DATA& item, bool news, bool talk)
{
    return (item.m_type == GAME_NEWS_DATA::eNews && news) || (item.m_type == GAME_NEWS_DATA::eTalk && talk);
}

int DaysBetween(u32 y0, u32 m0, u32 d0, u32 y1, u32 m1, u32 d1)
{
    int sign = 1;
    if (y0 > y1 || (y0 == y1 && m0 > m1) || (y0 == y1 && m0 == m1 && d0 > d1))
    {
        sign = -1;
        u32 t = y0;
        y0 = y1;
        y1 = t;
        t = m0;
        m0 = m1;
        m1 = t;
        t = d0;
        d0 = d1;
        d1 = t;
    }

    int days = 0;
    u32 y = y0;
    u32 m = m0;
    u32 d = d0;
    while (y < y1 || (y == y1 && m < m1) || (y == y1 && m == m1 && d < d1))
    {
        ++d;
        const u32 len = MonthLength(y, m);
        if (d > len)
        {
            d = 1;
            ++m;
            if (m > 12)
            {
                m = 1;
                ++y;
            }
        }
        ++days;
        if (days > 50000)
        {
            break;
        }
    }
    return days * sign;
}

void AddMonth(int& year, int& month, int delta)
{
    month += delta;
    while (month < 1)
    {
        month += 12;
        --year;
    }
    while (month > 12)
    {
        month -= 12;
        ++year;
    }
}

int CompareYm(u32 y0, u32 m0, u32 y1, u32 m1)
{
    if (y0 != y1)
    {
        return y0 < y1 ? -1 : 1;
    }
    if (m0 != m1)
    {
        return m0 < m1 ? -1 : 1;
    }
    return 0;
}

void AttachAutoStatics(CUIXml& xml, const char* path, CUIWindow* host)
{
    XML_NODE* stored = xml.GetLocalRoot();
    XML_NODE* node = xml.NavigateToNode(path, 0);
    if (!node)
    {
        return;
    }
    xml.SetLocalRoot(node);
    for (int i = 0, n = xml.GetNodesNum(path, 0, "auto_static"); i < n; ++i)
    {
        CUIStatic* label = new CUIStatic();
        label->SetAutoDelete(true);
        CUIXmlInit::InitStatic(xml, "auto_static", i, label);
        host->AttachChild(label);
    }
    xml.SetLocalRoot(stored);
}
}

bool CUICalendar::InitFromXml(CUIXml& xml, CUIWindow* owner, CUI3tButton* anchorButton)
{
    if (!owner || !anchorButton || !xml.NavigateToNode("calendar_popup", 0))
    {
        return false;
    }

    _owner = owner;
    _anchor = anchorButton;
    SetAutoDelete(true);
    const char* popupPath = "calendar_popup";
    CUIXmlInit::InitWindow(xml, popupPath, 0, this);
    _anchorToButton = xml.ReadAttribInt(popupPath, 0, "anchor_to_btn", 1) != 0;
    _freeMonthNav = xml.ReadAttribInt(popupPath, 0, "free_month_nav", 0) != 0;
    _freeYearMin = (u32)xml.ReadAttribInt(popupPath, 0, "free_month_year_min", 1986);
    _freeYearMax = (u32)xml.ReadAttribInt(popupPath, 0, "free_month_year_max", 2099);
    if (_freeYearMin > _freeYearMax)
    {
        const u32 t = _freeYearMin;
        _freeYearMin = _freeYearMax;
        _freeYearMax = t;
    }
    owner->AttachChild(this);
    Show(false);
    BuildUi(xml);
    return !_cells.empty();
}

void CUICalendar::SetOnDaySelected(const DaySelectedCallback& callback)
{
    _onSelect = callback;
}

void CUICalendar::UpdateState(ALife::_TIME_ID start, ALife::_TIME_ID selected, bool filterNews, bool filterTalk)
{
    _start = NormalizeDay(start);
    _selected = NormalizeDay(selected);
    _current = NormalizeDay(Level().GetGameTime());
    _filterNews = filterNews;
    _filterTalk = filterTalk;
    if (_cells.empty())
    {
        return;
    }
    SyncViewMonth();
    Refresh();
}

void CUICalendar::TogglePopup()
{
    if (_cells.empty())
    {
        return;
    }
    const bool show = !IsShown();
    Show(show);
    if (show)
    {
        RaisePopupToFront();
        SyncViewMonth();
        Refresh();
        EnableMonthControls();
        PositionNearAnchor();
    }
}

void CUICalendar::SendMessage(CUIWindow* window, s16 msg, void* data)
{
    inherited::SendMessage(window, msg, data);
    CUIWndCallback::OnEvent(window, msg, data);
}

void CUICalendar::BuildUi(CUIXml& xml)
{
    if (xml.NavigateToNode("calendar_popup:background", 0))
    {
        if (CUIFrameWindow* bg = UIHelper::CreateFrameWindow(xml, "calendar_popup:background", this, false))
        {
            bg->SetAutoDelete(true);
            bg->Enable(false);
            if (bg->UITitleText)
            {
                bg->UITitleText->Show(false);
                bg->UITitleText->Enable(false);
            }
        }
    }

    if (xml.NavigateToNode("calendar_popup:weekdays_row", 0))
    {
        CUIWindow* row = new CUIWindow();
        row->SetAutoDelete(true);
        CUIXmlInit::InitWindow(xml, "calendar_popup:weekdays_row", 0, row);
        AttachChild(row);
        AttachAutoStatics(xml, "calendar_popup:weekdays_row", row);
    }

    const char* hostPath = "calendar_popup:days_host";
    if (!xml.NavigateToNode(hostPath, 0))
    {
        return;
    }

    CUIWindow* host = new CUIWindow();
    host->SetAutoDelete(true);
    CUIXmlInit::InitWindow(xml, hostPath, 0, host);
    AttachChild(host);

    _cols = (u32)std::max(1, xml.ReadAttribInt(hostPath, 0, "columns", 7));
    const u32 rows = (u32)std::max(1, xml.ReadAttribInt(hostPath, 0, "rows", 6));
    _cellSize = xml.ReadAttribFlt(hostPath, 0, "cell_size", 28.0f);
    _gap = xml.ReadAttribFlt(hostPath, 0, "spacing", 2.0f);
    _cellTexture = xml.ReadAttrib(hostPath, 0, "cell_texture", "ui_pda_cal_day");

    const float step = _cellSize + _gap;
    const u32 cellCount = _cols * rows;
    _cells.reserve(cellCount);
    for (u32 i = 0; i < cellCount; ++i)
    {
        CUI3tButton* cell = new CUI3tButton();
        cell->SetAutoDelete(true);
        cell->InitButton(Fvector2().set(0.0f, 0.0f), Fvector2().set(_cellSize, _cellSize));
        if (_cellTexture.size())
        {
            cell->InitTexture(_cellTexture.c_str(), false);
            cell->SetStretchTexture(true);
        }
        cell->SetWndPos(Fvector2().set((i % _cols) * step, (i / _cols) * step));
        cell->SetWndSize(Fvector2().set(_cellSize, _cellSize));
        host->AttachChild(cell);
        Register(cell);
        cell->SetMessageTarget(this);
        AddCallback(cell, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUICalendar::OnDay));
        _cells.push_back(cell);
    }

    const char* stepperPath = "calendar_popup:month_stepper";
    if (!xml.NavigateToNode(stepperPath, 0))
    {
        return;
    }

    const float stepperX = xml.ReadAttribFlt(stepperPath, 0, "x", 0.0f);
    const float stepperY = xml.ReadAttribFlt(stepperPath, 0, "y", 0.0f);

    string_path captionPath = {};
    xr_strconcat(captionPath, stepperPath, ":month_caption");
    if (xml.NavigateToNode(captionPath, 0))
    {
        _monthCaption = UIHelper::CreateStatic(xml, captionPath, this, false);
        if (_monthCaption)
        {
            Fvector2 pos = _monthCaption->GetWndPos();
            pos.x += stepperX;
            pos.y += stepperY;
            _monthCaption->SetWndPos(pos);
            _monthCaption->Enable(false);
        }
    }

    auto attachMonthButtonLambda = [&](const char* nodeName, void (CUICalendar::*handler)(CUIWindow*, void*), CUI3tButton*& out)
    {
        string_path path = {};
        xr_strconcat(path, stepperPath, ":", nodeName);
        if (!xml.NavigateToNode(path, 0))
        {
            return;
        }
        out = UIHelper::Create3tButton(xml, path, this);
        if (!out)
        {
            return;
        }
        Fvector2 pos = out->GetWndPos();
        pos.x += stepperX;
        pos.y += stepperY;
        out->SetWndPos(pos);
        Register(out);
        out->SetMessageTarget(this);
        AddCallback(out, BUTTON_CLICKED, CUIWndCallback::void_function(this, handler));
    };

    attachMonthButtonLambda("btn_prev_month", &CUICalendar::OnMonthPrev, _monthPrev);
    attachMonthButtonLambda("btn_next_month", &CUICalendar::OnMonthNext, _monthNext);
}

void CUICalendar::SyncViewMonth()
{
    const ALife::_TIME_ID viewTime = _selected ? _selected : _current;
    u32 day = 0;
    ReadYmd(viewTime, _viewYear, _viewMonth, day);
}

void CUICalendar::RaisePopupToFront()
{
    if (!_owner || !_owner->IsChild(this))
    {
        return;
    }
    SetAutoDelete(false);
    _owner->DetachChild(this);
    _owner->AttachChild(this);
    SetAutoDelete(true);
}

void CUICalendar::EnableMonthControls()
{
    if (_monthPrev)
    {
        _monthPrev->Show(true);
        _monthPrev->Enable(CanShiftMonth(-1));
    }
    if (_monthNext)
    {
        _monthNext->Show(true);
        _monthNext->Enable(CanShiftMonth(1));
    }
    if (_monthCaption)
    {
        _monthCaption->Show(true);
    }
}

void CUICalendar::ApplyCellAppearance(CUI3tButton* cell, u32 day, bool hasNews, bool enabled, bool isSelected)
{
    string32 text;
    xr_sprintf(text, hasNews ? "%u*" : "%u", day);
    cell->SetText(text);
    cell->SetStateTextColor(
        enabled ? (isSelected ? color_argb(255, 255, 220, 120) : color_argb(255, 200, 200, 200)) : color_argb(255, 90, 90, 90),
        S_Enabled);
    cell->SetStateTextColor(color_argb(255, 90, 90, 90), S_Disabled);
}

int CUICalendar::CellDayForIndex(u32 cellIndex, u32 monthOffset, u32 daysInMonth) const
{
    const int day = (int)cellIndex - (int)monthOffset + 1;
    if (day < 1 || (u32)day > daysInMonth)
    {
        return 0;
    }
    return day;
}

void CUICalendar::Refresh()
{
    if (_cells.empty())
    {
        return;
    }

    if (_viewMonth < 1 || _viewMonth > 12)
    {
        const ALife::_TIME_ID fallback = _selected ? _selected : _current;
        if (fallback)
        {
            SyncViewMonth();
        }
        if (_viewMonth < 1 || _viewMonth > 12)
        {
            return;
        }
    }

    u32 selY = 0;
    u32 selM = 0;
    u32 selD = 0;
    ReadYmd(_selected, selY, selM, selD);

    if (_monthCaption)
    {
        _monthCaption->SetText(InventoryUtilities::GetDateAsString(
            MonthCaptionPeriod(), InventoryUtilities::edpDateToMonth).c_str());
    }

    const u32 len = MonthLength(_viewYear, _viewMonth);
    const u32 offset = MondayIndex(_viewYear, _viewMonth, 1);

    xr_vector<u8> marks(len + 1, 0);
    if (CActor* actor = Actor())
    {
        for (GAME_NEWS_DATA& item : actor->game_news_registry->registry().objects())
        {
            if (!PassesFilter(item, _filterNews, _filterTalk))
            {
                continue;
            }
            u32 y = 0;
            u32 m = 0;
            u32 d = 0;
            ReadYmd(item.receive_time, y, m, d);
            if (y == _viewYear && m == _viewMonth && d <= len)
            {
                marks[d] = 1;
            }
        }
    }

    for (u32 i = 0; i < _cells.size(); ++i)
    {
        CUI3tButton* cell = _cells[i];
        const int day = CellDayForIndex(i, offset, len);
        if (!day)
        {
            cell->Show(false);
            cell->Enable(false);
            continue;
        }

        const u32 d = (u32)day;
        const bool enabled = IsAllowedDay(_viewYear, _viewMonth, d);
        const bool isSelected = selY == _viewYear && selM == _viewMonth && selD == d;
        cell->Show(true);
        cell->Enable(enabled);
        ApplyCellAppearance(cell, d, marks[d] != 0, enabled, isSelected);
    }

    EnableMonthControls();
}

void CUICalendar::PositionNearAnchor()
{
    if (!_anchorToButton || !_owner || !_anchor)
    {
        return;
    }

    Frect anchor = {};
    Frect owner = {};
    _anchor->GetAbsoluteRect(anchor);
    _owner->GetAbsoluteRect(owner);

    Fvector2 pos;
    pos.x = anchor.x1 - owner.x1;
    pos.y = anchor.y2 - owner.y1 + 4.0f;
    pos.x = clampr(pos.x, 0.0f, std::max(0.0f, _owner->GetWidth() - GetWidth()));
    pos.y = clampr(pos.y, 0.0f, std::max(0.0f, _owner->GetHeight() - GetHeight()));
    SetWndPos(pos);
}

bool CUICalendar::CanShiftMonth(int delta) const
{
    if (_viewMonth < 1 || _viewMonth > 12)
    {
        return false;
    }

    int year = (int)_viewYear;
    int month = (int)_viewMonth;
    AddMonth(year, month, delta);
    if (year < 1)
    {
        return false;
    }

    if (_freeMonthNav)
    {
        return (u32)year >= _freeYearMin && (u32)year <= _freeYearMax;
    }

    u32 startY = 0;
    u32 startM = 0;
    u32 curY = 0;
    u32 curM = 0;
    ReadYm(_start, startY, startM);
    ReadYm(_current, curY, curM);

    if (CompareYm((u32)year, (u32)month, startY, startM) < 0)
    {
        return false;
    }
    if (CompareYm((u32)year, (u32)month, curY, curM) > 0)
    {
        return false;
    }
    return true;
}

void CUICalendar::ShiftMonth(int delta)
{
    if (!CanShiftMonth(delta))
    {
        return;
    }

    int year = (int)_viewYear;
    int month = (int)_viewMonth;
    AddMonth(year, month, delta);
    if (year < 1)
    {
        return;
    }
    _viewYear = (u32)year;
    _viewMonth = (u32)month;
    Refresh();
}

void CUICalendar::OnDay(CUIWindow* window, void* data)
{
    const u32 len = MonthLength(_viewYear, _viewMonth);
    const u32 offset = MondayIndex(_viewYear, _viewMonth, 1);

    for (u32 i = 0; i < _cells.size(); ++i)
    {
        if (_cells[i] != window)
        {
            continue;
        }
        const int day = CellDayForIndex(i, offset, len);
        if (!day)
        {
            return;
        }
        const u32 d = (u32)day;
        if (!IsAllowedDay(_viewYear, _viewMonth, d))
        {
            return;
        }
        const ALife::_TIME_ID period = DayToPeriod(_viewYear, _viewMonth, d);
        _selected = period;
        if (_onSelect)
        {
            _onSelect(period);
        }
        HidePopup();
        Refresh();
        return;
    }
}

ALife::_TIME_ID CUICalendar::NormalizeDay(ALife::_TIME_ID time) const
{
    time -= time % kDay2Ms;
    return time;
}

ALife::_TIME_ID CUICalendar::DayToPeriod(u32 year, u32 month, u32 day) const
{
    u32 startY = 0;
    u32 startM = 0;
    u32 startD = 0;
    ReadYmd(_start, startY, startM, startD);
    const int offset = DaysBetween(startY, startM, startD, year, month, day);
    ALife::_TIME_ID t = NormalizeDay(_start);
    return t + (s64)offset * (s64)kDay2Ms;
}

ALife::_TIME_ID CUICalendar::MonthCaptionPeriod() const
{
    if (_freeMonthNav)
    {
        return generate_time(_viewYear, _viewMonth, 1, 0, 0, 0);
    }
    return DayToPeriod(_viewYear, _viewMonth, 1);
}

bool CUICalendar::IsAllowedDay(u32 year, u32 month, u32 day) const
{
    const ALife::_TIME_ID period = DayToPeriod(year, month, day);
    return period >= NormalizeDay(_start) && period <= NormalizeDay(_current);
}
