////////////////////////////////////////////////////////////////////////////
//	Module 		: ui_drop_amount.h
//	Created 	: 25.07.2025
//	Author		: St4lker0k765, Desert_Cliff (21.02.2026)
//	Description : Implementation for custom amount of items for drop
////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "ui_drop_amount.h"
#include "UIActorMenu.h"
#include "UIGameCustom.h"
#include "UIInventoryWnd.h"
#include "UICarBodyWnd.h"
#include "inventory_item.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UITrackBar.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrCore/xr_ini.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"

CUIItemDropAmountWnd::CUIItemDropAmountWnd()
    : _background(nullptr)
    , _staticPicture(nullptr)
    , _staticText(nullptr)
    , _buttonYes(nullptr)
    , _buttonNo(nullptr)
    , _trackBar(nullptr)
    , _staticValueMin(nullptr)
    , _staticValueMax(nullptr)
    , _titleText(nullptr)
    , _itemNameText(nullptr)
    , _weightText(nullptr)
    , _editAmount(nullptr)
    , _btnDec(nullptr)
    , _btnInc(nullptr)
    , _btnHalf(nullptr)
    , _btnAll(nullptr)
    , _btnCancel(nullptr)
    , _btnAccept(nullptr)
    , _dropMode(eModeDrop)
    , _extendedLayout(false)
    , _simpleDropMode(false)
    , _currentAmount(1)
    , _maxAmount(1)
    , _pItem(nullptr)
{
    m_bWorkInPause = true;
}

CUIItemDropAmountWnd::~CUIItemDropAmountWnd()
{
    ActionRepeaters()->UnregisterOwner(this);
}

void CUIItemDropAmountWnd::InitDropAmount()
{
    CUIXml uiXml;
    uiXml.Load(CONFIG_PATH, UI_PATH, "custom_drop_amount.xml");

    SetWndPos(Fvector2().set(0, 0));
    SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));

    LPCSTR base = nullptr;
    if (uiXml.NavigateToNode("split_item", 0))
    {
        _extendedLayout = true;
        base = "split_item";
    }
    else
    {
        _extendedLayout = false;
        base = "custom_drop";
    }

    _background = UIHelper::CreateStatic(uiXml, base, this);
    string512 str;

    if (_extendedLayout)
    {
        xr_strconcat(str, base, ":item_title");
        if (uiXml.NavigateToNode(str, 0))
            _titleText = UIHelper::CreateStatic(uiXml, str, _background);

        xr_strconcat(str, base, ":item_name");
        if (uiXml.NavigateToNode(str, 0))
            _itemNameText = UIHelper::CreateStatic(uiXml, str, _background);

        xr_strconcat(str, base, ":weight_text");
        if (uiXml.NavigateToNode(str, 0))
            _weightText = UIHelper::CreateStatic(uiXml, str, _background);

        xr_strconcat(str, base, ":edit_amount");
        if (uiXml.NavigateToNode(str, 0))
        {
            _editAmount = UIHelper::CreateEditBox(uiXml, str, _background);
            Register(_editAmount);
            AddCallback(_editAmount, EDIT_TEXT_COMMIT, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnEditCommit));
        }

        xr_strconcat(str, base, ":button_decrement");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnDec = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnDec);
            AddCallback(_btnDec, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnDecClicked));
        }

        xr_strconcat(str, base, ":button_increment");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnInc = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnInc);
            AddCallback(_btnInc, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnIncClicked));
        }
    }
    else
    {
        xr_strconcat(str, base, ":picture");
        if (uiXml.NavigateToNode(str, 0))
            _staticPicture = UIHelper::CreateStatic(uiXml, str, _background);

        xr_strconcat(str, base, ":text_hint");
        _staticText = UIHelper::CreateStatic(uiXml, str, _background);

        xr_strconcat(str, base, ":button_yes");
        _buttonYes = UIHelper::Create3tButton(uiXml, str, _background);
        Register(_buttonYes);
        AddCallback(_buttonYes, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnYesClicked));

        xr_strconcat(str, base, ":button_no");
        _buttonNo = UIHelper::Create3tButton(uiXml, str, _background);
        Register(_buttonNo);
        AddCallback(_buttonNo, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnNoClicked));
    }

    xr_strconcat(str, base, ":trackbar");
    _trackBar = UIHelper::CreateTrackBar(uiXml, str, _background);
    _trackBar->SetCurrentID(0);
    _trackBar->SaveBackUpOptValue();
    if (_extendedLayout)
    {
        Register(_trackBar);
        AddCallback(_trackBar, TRACK_VALUE_CHANGED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnTrackChanged));
        AddCallback(_trackBar, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnTrackChanged));
    }

    xr_strconcat(str, base, ":value_min");
    if (uiXml.NavigateToNode(str, 0))
        _staticValueMin = UIHelper::CreateStatic(uiXml, str, _background);

    xr_strconcat(str, base, ":value_max");
    if (uiXml.NavigateToNode(str, 0))
        _staticValueMax = UIHelper::CreateStatic(uiXml, str, _background);

    if (_extendedLayout)
    {
        xr_strconcat(str, base, ":button_half");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnHalf = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnHalf);
            AddCallback(_btnHalf, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnHalfClicked));
        }

        xr_strconcat(str, base, ":button_all");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnAll = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnAll);
            AddCallback(_btnAll, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnAllClicked));
        }

        xr_strconcat(str, base, ":button_cancel");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnCancel = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnCancel);
            AddCallback(_btnCancel, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnCancelClicked));
        }

        xr_strconcat(str, base, ":button_accept");
        if (uiXml.NavigateToNode(str, 0))
        {
            _btnAccept = UIHelper::Create3tButton(uiXml, str, _background);
            Register(_btnAccept);
            AddCallback(_btnAccept, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnAcceptClicked));
        }
    }

    xr_strconcat(str, base, ":gamepad_legend");
    _gamepadLegend = UIHelper::CreateGamepadLegend(uiXml, str, this, false);

    ActionRepeaters()->Register(this, kUI_LEFT);
    ActionRepeaters()->Register(this, kUI_RIGHT);
}

void CUIItemDropAmountWnd::RecalculateLayout(CInventoryItem* pItem)
{
    // WTF?
}

void CUIItemDropAmountWnd::ShowDropAmount(u32 max, EDropMode mode, CInventoryItem* pItem)
{
    _simpleDropMode = true;
    _maxAmount = (int)max;
    _pItem = pItem;
    _currentAmount = 1;

    Enable(true);
    ShowDialog(false);

    _trackBar->SetOptIBounds(1, max + 1);
    _trackBar->SetIValue(1);
    _trackBar->UndoOptValue();
    _currentAmount = _trackBar->GetIValue();

    _dropMode = mode;

    if (_titleText && _extendedLayout)
    {
        _titleText->SetTextST("st_drop_amount_title");
    }

    if (_staticText)
    {
        string256 hintStr;
        xr_sprintf(hintStr, "st_custom_drop_hint_%d", _dropMode);
        _staticText->SetTextST(hintStr);
        if (_staticText->IsColorAnimationPresent())
            _staticText->ResetColorAnimation();
    }
    else if (_itemNameText && _extendedLayout)
    {
        if (_pItem)
        {
            shared_str invName = pSettings->line_exist(_pItem->m_section_id.c_str(), "inv_name")
                ? pSettings->r_string(_pItem->m_section_id.c_str(), "inv_name")
                : _pItem->m_section_id;
            _itemNameText->SetTextST(invName.c_str());
        }
        else
        {
            string256 hintStr;
            xr_sprintf(hintStr, "st_custom_drop_hint_%d", _dropMode);
            _itemNameText->SetTextST(hintStr);
        }
        if (_itemNameText->IsColorAnimationPresent())
            _itemNameText->ResetColorAnimation();
    }

    if (_editAmount && _extendedLayout)
    {
        _editAmount->ClearText();
        SyncValueToEdit();
    }

    if (_staticValueMax)
    {
        string32 cnt;
        xr_sprintf(cnt, "%d", max + 1);
        _staticValueMax->SetText(cnt);
    }

    UpdateWeightText();
}

void CUIItemDropAmountWnd::Show(CInventoryItem* pItem, int maxAmount, std::function<void(int)> callback)
{
    if (!_extendedLayout || !callback)
        return;

    _simpleDropMode = false;
    _pItem = pItem;
    _maxAmount = maxAmount;
    _callback = std::move(callback);
    _currentAmount = 1;

    Enable(true);
    ShowDialog(false);

    _trackBar->SetOptIBounds(1, maxAmount + 1);
    _trackBar->SetIValue(1);
    _trackBar->SaveBackUpOptValue();

    if (_itemNameText && pItem)
    {
        shared_str invName = pSettings->line_exist(pItem->m_section_id.c_str(), "inv_name")
            ? pSettings->r_string(pItem->m_section_id.c_str(), "inv_name")
            : pItem->m_section_id;
        _itemNameText->SetTextST(invName.c_str());
        if (_itemNameText->IsColorAnimationPresent())
            _itemNameText->ResetColorAnimation();
    }

    if (_editAmount)
        _editAmount->ClearText();

    UpdateWeightText();

    if (_staticValueMax)
    {
        string32 cnt;
        xr_sprintf(cnt, "%d", maxAmount + 1);
        _staticValueMax->SetText(cnt);
    }

    RecalculateLayout(pItem);
}

void CUIItemDropAmountWnd::SyncValueToEdit()
{
    if (_editAmount)
    {
        string32 buf;
        xr_sprintf(buf, "%d", _currentAmount);
        _editAmount->SetText(buf);
    }
    UpdateWeightText();
}

void CUIItemDropAmountWnd::SyncEditToValue()
{
    if (!_editAmount)
        return;
    LPCSTR text = _editAmount->GetText();
    int val = 1;
    if (text && xr_strlen(text) > 0)
        val = atoi(text);
    clamp(val, 1, _maxAmount + 1);
    _currentAmount = val;
    _trackBar->SetIValue(val);
    UpdateWeightText();
}

void CUIItemDropAmountWnd::UpdateWeightText()
{
    if (!_weightText || !_pItem)
        return;
    float weight = _pItem->Weight() * _currentAmount;
    LPCSTR weightLabel = g_pStringTable->translate("st_weight").c_str();
    LPCSTR kgLabel = g_pStringTable->translate("st_kg").c_str();
    string128 buf;
    xr_sprintf(buf, "%s: %.2f %s", weightLabel, weight, kgLabel);
    _weightText->SetText(buf);
}

void CUIItemDropAmountWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
    CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIItemDropAmountWnd::PerformDrop()
{
    int amount = _extendedLayout ? _currentAmount : _trackBar->GetIValue();
    if (amount < 1)
        amount = 1;

    switch (_dropMode)
    {
    case eModeDrop:
    {
        if (CurrentGameUI()->ActorMenu())
            CurrentGameUI()->ActorMenu()->DropAllCurrentItem((u32)amount);
        else
            CurrentGameUI()->InventoryWnd()->DropAllCurrentItem((u32)amount);
        break;
    }
    case eModeMove:
    {
        if (CurrentGameUI()->ActorMenu())
            CurrentGameUI()->ActorMenu()->MoveAllCurrentItem((u32)amount);
        else
            CurrentGameUI()->CarBodyWnd()->MoveAllCurrentItem((u32)amount);
        break;
    }
    case eModeTake:
    {
        if (CurrentGameUI()->ActorMenu())
            CurrentGameUI()->ActorMenu()->TakeAllCurrentItem((u32)amount);
        else
            CurrentGameUI()->CarBodyWnd()->TakeAllCurrentItem((u32)amount);
        break;
    }
    }
}

void CUIItemDropAmountWnd::OnBtnYesClicked(CUIWindow* w, void* d)
{
    PerformDrop();
    HideDialog();
}

void CUIItemDropAmountWnd::OnBtnNoClicked(CUIWindow* w, void* d)
{
    HideDialog();
}

void CUIItemDropAmountWnd::OnBtnAcceptClicked(CUIWindow* w, void* d)
{
    if (_editAmount && _extendedLayout)
        SyncEditToValue();
    if (_simpleDropMode)
    {
        PerformDrop();
        HideDialog();
        return;
    }
    SyncEditToValue();
    const int amount = _currentAmount;
    std::function<void(int)> callback = std::move(_callback);
    _callback = nullptr;

    HideDialog();

    if (callback)
        callback(amount);
}

void CUIItemDropAmountWnd::OnBtnCancelClicked(CUIWindow* w, void* d)
{
    HideDialog();
}

void CUIItemDropAmountWnd::OnBtnDecClicked(CUIWindow* w, void* d)
{
    _currentAmount--;
    clamp(_currentAmount, 1, _maxAmount + 1);
    _trackBar->SetIValue(_currentAmount);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnIncClicked(CUIWindow* w, void* d)
{
    _currentAmount++;
    clamp(_currentAmount, 1, _maxAmount + 1);
    _trackBar->SetIValue(_currentAmount);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnHalfClicked(CUIWindow* w, void* d)
{
    _currentAmount = (_maxAmount + 2) / 2;
    clamp(_currentAmount, 1, _maxAmount + 1);
    _trackBar->SetIValue(_currentAmount);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnAllClicked(CUIWindow* w, void* d)
{
    _currentAmount = _maxAmount + 1;
    _trackBar->SetIValue(_currentAmount);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnEditCommit(CUIWindow* w, void* d)
{
    SyncEditToValue();
}

void CUIItemDropAmountWnd::OnTrackChanged(CUIWindow* w, void* d)
{
    _currentAmount = _trackBar->GetIValue();
    SyncValueToEdit();
}

bool CUIItemDropAmountWnd::OnKeyboardAction(int dik, EUIMessages keyboardAction)
{
    if (_extendedLayout && !_simpleDropMode)
    {
        if (keyboardAction == WINDOW_KEY_PRESSED)
        {
            if (dik == SDL_SCANCODE_RETURN || dik == SDL_SCANCODE_KP_ENTER)
            {
                OnBtnAcceptClicked(nullptr, nullptr);
                return true;
            }
            if (dik == SDL_SCANCODE_ESCAPE)
            {
                OnBtnCancelClicked(nullptr, nullptr);
                return true;
            }
        }
    }

    if (is_binded(kUSE, dik) || is_binded(kINVENTORY, dik) || is_binded(kQUIT, dik))
    {
        if (WINDOW_KEY_PRESSED == keyboardAction)
            HideDialog();
        return true;
    }

    if (CUIDialogWnd::OnKeyboardAction(dik, keyboardAction))
        return true;

    return false;
}

bool CUIItemDropAmountWnd::OnGamepadKeyAction(int id, EUIMessages gamepadAction)
{
    if (gamepadAction == WINDOW_KEY_PRESSED)
    {
        switch (get_binded_action(id, agUIGeneral))
        {
            case kUI_ACCEPT:
            {
                OnBtnAcceptClicked(this, nullptr);
                return true;
            }
            case kUI_BACK:
            {
                OnBtnCancelClicked(this, nullptr);
                return true;
            }
            case kUI_ACTION_1:
            {
                OnBtnHalfClicked(this, nullptr);
                return true;
            }
            case kUI_ACTION_2:
            {
                OnBtnAllClicked(this, nullptr);
                return true;
            }
            case kUI_LEFT:
            {
                ActionRepeaters()->SetActionStarted(this, kUI_LEFT);
                if (!any_binded_key_for_action_pressed_c(kUI_RIGHT))
                    _trackBar->StepLeft();
                return true;
            }
            case kUI_RIGHT:
            {
                ActionRepeaters()->SetActionStarted(this, kUI_RIGHT);
                if (!any_binded_key_for_action_pressed_c(kUI_LEFT))
                    _trackBar->StepRight();
                return true;
            }
        }
    }

    return CUIDialogWnd::OnGamepadKeyAction(id, gamepadAction);
}

bool CUIItemDropAmountWnd::OnGamepadKeyHold(int id)
{
    switch (get_binded_action(id, agUIGeneral))
    {
        case kUI_LEFT:
        {
            if (ActionRepeaters()->CanRepeatActionNow(this, kUI_LEFT) && !any_binded_key_for_action_pressed_c(kUI_RIGHT))
                _trackBar->StepLeft();
            return true;
        }
        case kUI_RIGHT:
        {
            if (ActionRepeaters()->CanRepeatActionNow(this, kUI_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_LEFT))
                _trackBar->StepRight();
            return true;
        }
    }

    return CUIDialogWnd::OnGamepadKeyHold(id);
}

void CUIItemDropAmountWnd::Update()
{
    CUIDialogWnd::Update();

    if (_btnAccept)
        _btnAccept->Show(!pInput->GetControllerMode());

    if (_btnCancel)
        _btnCancel->Show(!pInput->GetControllerMode());

    if (_btnHalf)
        _btnHalf->Show(!pInput->GetControllerMode());

    if (_btnAll)
        _btnAll->Show(!pInput->GetControllerMode());
}
