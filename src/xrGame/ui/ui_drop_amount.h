////////////////////////////////////////////////////////////////////////////
//	Module 		: ui_drop_amount.h
//	Created 	: 25.07.2025
//	Author		: St4lker0k765, Desert_Cliff (21.02.2026)
//	Description : Implementation for custom amount of items for drop
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include <functional>

class CUIStatic;
class CUIWindow;
class CUITrackBar;
class CUI3tButton;
class CUIEditBox;
class CInventoryItem;
class CUIGamepadLegend;

class CUIItemDropAmountWnd final :
    public CUIDialogWnd,
    public CUIWndCallback
{
public:
    enum EDropMode
    {
        eModeDrop,
        eModeMove,
        eModeTake,
        eModeToOffer,
        eModeFromOffer,
        eModeToCart,
        eModeFromCart,
    };

    CUIItemDropAmountWnd();
    ~CUIItemDropAmountWnd();

    bool HasInitializedLayout() const { return _trackBar != nullptr; }

    void InitDropAmount(CUIXml& uiXml);
    void ShowDropAmount(u32 max, EDropMode mode, CInventoryItem* pItem = nullptr);
    void Show(CInventoryItem* pItem, int maxAmount, std::function<void(int)> callback);

    void SendMessage(CUIWindow* pWnd, s16 msg, void* pData = nullptr) override;
    bool OnKeyboardAction(int dik, EUIMessages keyboardAction) override;
    CUIWindow* ui_cast_window() override { return this; }
    bool OnGamepadKeyAction(int id, EUIMessages gamepadAction) override;
    bool OnGamepadKeyHold(int id) override;
    void Update() override;

private:
    CUIStatic* _background = nullptr;
    CUITrackBar* _trackBar = nullptr;
    CUIStatic* _staticValueMin = nullptr;
    CUIStatic* _staticValueMax = nullptr;

    CUIStatic* _titleText = nullptr;
    CUIStatic* _itemNameText = nullptr;
    CUIStatic* _weightText = nullptr;
    CUIEditBox* _editAmount = nullptr;
    CUI3tButton* _btnDec = nullptr;
    CUI3tButton* _btnInc = nullptr;
    CUI3tButton* _btnHalf = nullptr;
    CUI3tButton* _btnAll = nullptr;
    CUI3tButton* _btnCancel = nullptr;
    CUI3tButton* _btnAccept = nullptr;
    CUIGamepadLegend* _gamepadLegend = nullptr;

    EDropMode _dropMode = eModeDrop;
    bool _simpleDropMode = false;
    int _currentAmount = 1;
    int _maxAmount = 1;
    CInventoryItem* _pItem = nullptr;
    std::function<void(int)> _callback;

    void PerformDrop();
    void RecalculateLayout(CInventoryItem* pItem);
    void SyncValueToEdit();
    void SyncEditToValue();
    void UpdateWeightText();
    void OnBtnYesClicked(CUIWindow* w, void* d);
    void OnBtnNoClicked(CUIWindow* w, void* d);
    void OnBtnAcceptClicked(CUIWindow* w, void* d);
    void OnBtnCancelClicked(CUIWindow* w, void* d);
    void OnBtnDecClicked(CUIWindow* w, void* d);
    void OnBtnIncClicked(CUIWindow* w, void* d);
    void OnBtnHalfClicked(CUIWindow* w, void* d);
    void OnBtnAllClicked(CUIWindow* w, void* d);
    void OnEditCommit(CUIWindow* w, void* d);
    void OnTrackChanged(CUIWindow* w, void* d);
};
