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
#include "../Inventory.h"
#include "WeaponAmmo.h"
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
#include "UITalkWnd.h"
#include "UITradeWnd.h"

namespace
{
const char* GetTitleStringIdForMode(CUIItemDropAmountWnd::EDropMode mode)
{
    switch (mode)
    {
    case CUIItemDropAmountWnd::eModeDrop:
        return "st_drop_amount_title";
    case CUIItemDropAmountWnd::eModeMove:
        return "st_move_amount";
    case CUIItemDropAmountWnd::eModeTake:
        return "st_take_amount";
    case CUIItemDropAmountWnd::eModeToOffer:
        return "st_move_to_offer_amount";
    case CUIItemDropAmountWnd::eModeFromOffer:
        return "st_remove_from_offer_amount";
    case CUIItemDropAmountWnd::eModeToCart:
        return "st_move_to_cart_amount";
    case CUIItemDropAmountWnd::eModeFromCart:
        return "st_remove_from_cart_amount";
    default:
        return "st_drop_amount_title";
    }
}

int GetMaxSelectable(int maxAmount)
{
    return maxAmount + 1;
}

void ConfigureAmountTrackBar(CUITrackBar* trackBar, int maxSelectable, int amount)
{
    if (trackBar == nullptr)
    {
        return;
    }

    trackBar->SetTrackBarMode(eTrackBarModeFloat);
    trackBar->SetNumOfSigns(0);
    trackBar->SetStep(1.0f);
    trackBar->SetOptFBounds(0.0f, static_cast<float>(maxSelectable));
    clamp(amount, 1, maxSelectable);
    trackBar->SetFValue(static_cast<float>(amount));
    trackBar->SaveBackUpOptValue();
}

int ReadAmountFromTrackBar(CUITrackBar* trackBar, int maxSelectable)
{
    if (trackBar == nullptr)
    {
        return 1;
    }

    int amount = iFloor(trackBar->GetFValue() + 0.5f);
    clamp(amount, 1, maxSelectable);
    return amount;
}

void WriteAmountToTrackBar(CUITrackBar* trackBar, int amount, int maxSelectable)
{
    if (trackBar == nullptr)
    {
        return;
    }

    clamp(amount, 1, maxSelectable);
    trackBar->SetFValue(static_cast<float>(amount));
}

void UpdateItemNameText(CUIStatic* itemNameText, CUICellItem* cellItem, CInventoryItem* item, int amount)
{
	if (itemNameText == nullptr || cellItem == nullptr || item == nullptr)
    {
        return;
    }

    shared_str invName = pSettings->line_exist(item->m_section_id.c_str(), "inv_name") ?
        pSettings->r_string(item->m_section_id.c_str(), "inv_name") :
        item->m_section_id;

    CWeaponAmmo* weaponAmmo = item->cast_weapon_ammo();
    if (weaponAmmo != nullptr)
    {
		s32 count = weaponAmmo->m_boxCurr;
        for (u32 i = 1; i < amount; i++)
        {
			CUICellItem* itm = cellItem->Child(i-1);
			CInventoryItem* iitm = (PIItem)itm->m_pData;
			count += iitm->cast_weapon_ammo()->m_boxCurr;
        }

        string256 ammoNameText;
        xr_sprintf(ammoNameText, "%s (%d)", *g_pStringTable->translate(invName.c_str()),
            count);
        itemNameText->SetText(ammoNameText);
    }
    else
    {
        itemNameText->SetTextST(invName.c_str());
    }
}
}

CUIItemDropAmountWnd::CUIItemDropAmountWnd()
{
    m_bWorkInPause = true;
}

CUIItemDropAmountWnd::~CUIItemDropAmountWnd()
{
    ActionRepeaters()->UnregisterOwner(this);
}

void CUIItemDropAmountWnd::InitDropAmount(CUIXml& uiXml)
{
    SetWndPos(Fvector2().set(0, 0));
    SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));

    const char* base = "split_item";
    XML_NODE* stored_root = uiXml.GetLocalRoot();
    XML_NODE* base_node = uiXml.NavigateToNode(base, 0);
    if (!base_node)
    {
        Msg("! CUIItemDropAmountWnd::InitDropAmount: <%s> node missing in [%s]", base, uiXml.m_xml_file_name);
        R_ASSERT4(base_node != nullptr, "split_item node not found in UI XML", base, uiXml.m_xml_file_name);
        return;
    }
    _background = UIHelper::CreateStatic(uiXml, base, this);
    uiXml.SetLocalRoot(base_node);

    _titleText = UIHelper::CreateStatic(uiXml, "item_title", _background);
    _itemNameText = UIHelper::CreateStatic(uiXml, "item_name", _background);
    _weightText = UIHelper::CreateStatic(uiXml, "weight_text", _background);

    _editAmount = UIHelper::CreateEditBox(uiXml, "edit_amount", _background);
    Register(_editAmount);
    AddCallback(_editAmount, EDIT_TEXT_COMMIT, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnEditCommit));

    _btnDec = UIHelper::Create3tButton(uiXml, "button_decrement", _background);
    Register(_btnDec);
    AddCallback(_btnDec, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnDecClicked));

    _btnInc = UIHelper::Create3tButton(uiXml, "button_increment", _background);
    Register(_btnInc);
    AddCallback(_btnInc, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnIncClicked));

    _trackBar = UIHelper::CreateTrackBar(uiXml, "trackbar", _background);
    _trackBar->SetTrackBarMode(eTrackBarModeFloat);
    _trackBar->SetNumOfSigns(0);
    _trackBar->SetStep(1.0f);
    Register(_trackBar);
    AddCallback(_trackBar, TRACK_VALUE_CHANGED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnTrackChanged));
    AddCallback(_trackBar, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnTrackChanged));

    _staticValueMin = UIHelper::CreateStatic(uiXml, "value_min", _background);
    _staticValueMax = UIHelper::CreateStatic(uiXml, "value_max", _background);

    _btnHalf = UIHelper::Create3tButton(uiXml, "button_half", _background);
    Register(_btnHalf);
    AddCallback(_btnHalf, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnHalfClicked));

    _btnAll = UIHelper::Create3tButton(uiXml, "button_all", _background);
    Register(_btnAll);
    AddCallback(_btnAll, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnAllClicked));

    _btnCancel = UIHelper::Create3tButton(uiXml, "button_cancel", _background);
    Register(_btnCancel);
    AddCallback(_btnCancel, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnCancelClicked));

    _btnAccept = UIHelper::Create3tButton(uiXml, "button_accept", _background);
    Register(_btnAccept);
    AddCallback(_btnAccept, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIItemDropAmountWnd::OnBtnAcceptClicked));

    _gamepadLegend = UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

    uiXml.SetLocalRoot(stored_root);
    ActionRepeaters()->Register(this, kUI_LEFT);
    ActionRepeaters()->Register(this, kUI_RIGHT);
}

void CUIItemDropAmountWnd::RecalculateLayout(CInventoryItem* pItem)
{
    // WTF?
}

void CUIItemDropAmountWnd::ShowDropAmount(u32 max, EDropMode mode, CInventoryItem* pItem, CUICellItem* pCellItem)
{
    if (_trackBar == nullptr)
    {
        return;
    }

    _simpleDropMode = true;
    _maxAmount = (int)max;
    _pItem = pItem;
	_pCellItem = pCellItem;

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount = maxSelectable / 2;
    if (_currentAmount < 1)
    {
        _currentAmount = 1;
    }

    Enable(true);
    ShowDialog(false);

    ConfigureAmountTrackBar(_trackBar, maxSelectable, _currentAmount);
    _currentAmount = ReadAmountFromTrackBar(_trackBar, maxSelectable);

    _dropMode = mode;

    _titleText->SetTextST(GetTitleStringIdForMode(_dropMode));

    if (_pItem)
    {
		UpdateItemNameText(_itemNameText, _pCellItem, _pItem, _currentAmount);
    }
    else
    {
        string256 hintStr;
        xr_sprintf(hintStr, "st_custom_drop_hint_%d", _dropMode);
        _itemNameText->SetTextST(hintStr);
    }
    if (_itemNameText->IsColorAnimationPresent())
        _itemNameText->ResetColorAnimation();

    _editAmount->ClearText();
    SyncValueToEdit();

    string32 cnt;
    xr_sprintf(cnt, "%d", maxSelectable);
    _staticValueMax->SetText(cnt);
    UpdateWeightText();
}

void CUIItemDropAmountWnd::Show(CInventoryItem* pItem, CUICellItem* pCellItem, int maxAmount, std::function<void(int)> callback)
{
    if (!callback)
    {
        return;
    }

    if (_trackBar == nullptr)
    {
        return;
    }

    _simpleDropMode = false;
    _pItem = pItem;
	_pCellItem = pCellItem;
	_maxAmount = maxAmount;
    _callback = std::move(callback);

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount = maxSelectable / 2;
    if (_currentAmount < 1)
    {
        _currentAmount = 1;
    }

    Enable(true);
    ShowDialog(false);

    ConfigureAmountTrackBar(_trackBar, maxSelectable, _currentAmount);
    _currentAmount = ReadAmountFromTrackBar(_trackBar, maxSelectable);

    if (pItem)
    {
		UpdateItemNameText(_itemNameText, pCellItem, pItem, _currentAmount);
        if (_itemNameText->IsColorAnimationPresent())
        {
            _itemNameText->ResetColorAnimation();
        }
    }

    if (_editAmount)
        _editAmount->ClearText();

    UpdateWeightText();

    if (_staticValueMax)
    {
        string32 cnt;
        xr_sprintf(cnt, "%d", maxSelectable);
        _staticValueMax->SetText(cnt);
    }

    RecalculateLayout(pItem);
}

void CUIItemDropAmountWnd::SyncValueToEdit()
{
    string32 buf;
    xr_sprintf(buf, "%d", _currentAmount);
    _editAmount->SetText(buf);

    if (_pItem)
    {
		UpdateItemNameText(_itemNameText, _pCellItem, _pItem, _currentAmount);
    }

    UpdateWeightText();
}

void CUIItemDropAmountWnd::SyncEditToValue()
{
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    const char* text = _editAmount->GetText();
    int val = 1;
    if (text && xr_strlen(text) > 0)
        val = atoi(text);
    clamp(val, 1, maxSelectable);
    _currentAmount = val;
    WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    UpdateWeightText();
}

void CUIItemDropAmountWnd::UpdateWeightText()
{
    if (!_pItem)
        return;

    float itemWeight = _pItem->m_pInventory ? _pItem->m_pInventory->CalcItemWeight(_pItem) : _pItem->Weight();
    float weight = itemWeight;

    for (u32 i = 1; i < _currentAmount; i++)
	{
		CUICellItem* itm = _pCellItem->Child(i - 1);
		CInventoryItem* iitm = (PIItem)itm->m_pData;
		weight += iitm->m_pInventory ? iitm->m_pInventory->CalcItemWeight(iitm) : iitm->Weight();
	}

    const char* weightLabel = g_pStringTable->translate("st_weight").c_str();
    const char* kgLabel = g_pStringTable->translate("st_kg").c_str();

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
    int amount = _currentAmount;
    if (amount < 1)
        amount = 1;

    switch (_dropMode)
    {
    case eModeDrop:
    {
		CurrentGameUI()->GetInventoryMenu()->DropAllCurrentItem((u32)amount);
        break;
    }
    case eModeMove:
    {
		CurrentGameUI()->GetCarbodyMenu()->MoveAllCurrentItem((u32)amount);
        break;
    }
    case eModeTake:
    {
		CurrentGameUI()->GetCarbodyMenu()->TakeAllCurrentItem((u32)amount);
        break;
    }
    case eModeFromOffer:
    {
		CurrentGameUI()->GetTradeMenu()->ToBagAll((u32)amount);
        break;
    }
    case eModeToOffer:
    {
		CurrentGameUI()->GetTradeMenu()->ToActorTradeAll((u32)amount);
        break;
    }
    case eModeToCart:
    {
        CurrentGameUI()->GetTradeMenu()->ToPartnerTradeAll((u32)amount);
        break;
    }
    case eModeFromCart:
    {
		CurrentGameUI()->GetTradeMenu()->ToPartnerTradeBagAll((u32)amount);
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
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount--;
    clamp(_currentAmount, 1, maxSelectable);
    WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnIncClicked(CUIWindow* w, void* d)
{
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount++;
    clamp(_currentAmount, 1, maxSelectable);
    WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnHalfClicked(CUIWindow* w, void* d)
{
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount = maxSelectable / 2;
    clamp(_currentAmount, 1, maxSelectable);
    WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnBtnAllClicked(CUIWindow* w, void* d)
{
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount = maxSelectable;
    WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    SyncValueToEdit();
}

void CUIItemDropAmountWnd::OnEditCommit(CUIWindow* w, void* d)
{
    SyncEditToValue();
}

void CUIItemDropAmountWnd::OnTrackChanged(CUIWindow* w, void* d)
{
    if (_trackBar == nullptr)
    {
        return;
    }

    const int maxSelectable = GetMaxSelectable(_maxAmount);
    _currentAmount = ReadAmountFromTrackBar(_trackBar, maxSelectable);
    // Keep thumb inside selectable range (float track allows 0..max for linear mapping).
    if (iFloor(_trackBar->GetFValue() + 0.5f) != _currentAmount)
    {
        WriteAmountToTrackBar(_trackBar, _currentAmount, maxSelectable);
    }
    SyncValueToEdit();
}

bool CUIItemDropAmountWnd::OnKeyboardAction(int dik, EUIMessages keyboardAction)
{
    if (!_simpleDropMode)
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
    if (_trackBar == nullptr)
    {
        return CUIDialogWnd::OnGamepadKeyAction(id, gamepadAction);
    }

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
    if (_trackBar == nullptr)
    {
        return CUIDialogWnd::OnGamepadKeyHold(id);
    }

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

    if (_btnAccept == nullptr)
    {
        return;
    }

    _btnAccept->Show(!pInput->GetControllerMode());
    _btnCancel->Show(!pInput->GetControllerMode());
    _btnHalf->Show(!pInput->GetControllerMode());
    _btnAll->Show(!pInput->GetControllerMode());
}
