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
    };

    CUIItemDropAmountWnd();
    ~CUIItemDropAmountWnd();

    void InitDropAmount(CUIXml& uiXml);
    void ShowDropAmount(u32 max, EDropMode mode, CInventoryItem* pItem = nullptr);
    void Show(CInventoryItem* pItem, int maxAmount, std::function<void(int)> callback);
    bool IsExtendedLayout() const { return _extendedLayout; }

    void SendMessage(CUIWindow* pWnd, s16 msg, void* pData = nullptr) override;
    bool OnKeyboardAction(int dik, EUIMessages keyboardAction) override;
    CUIWindow* ui_cast_window() override { return this; }
    bool OnGamepadKeyAction(int id, EUIMessages gamepadAction) override;
    bool OnGamepadKeyHold(int id) override;
    void Update() override;

private:
    CUIStatic* _background;
    CUIStatic* _staticPicture;
    CUIStatic* _staticText;
    CUI3tButton* _buttonYes;
    CUI3tButton* _buttonNo;
    CUITrackBar* _trackBar;
    CUIStatic* _staticValueMin;
    CUIStatic* _staticValueMax;

    CUIStatic* _titleText;
    CUIStatic* _itemNameText;
    CUIStatic* _weightText;
    CUIEditBox* _editAmount;
    CUI3tButton* _btnDec;
    CUI3tButton* _btnInc;
    CUI3tButton* _btnHalf;
    CUI3tButton* _btnAll;
    CUI3tButton* _btnCancel;
    CUI3tButton* _btnAccept;
    CUIGamepadLegend* _gamepadLegend = nullptr;

    EDropMode _dropMode;
    bool _extendedLayout;
    bool _simpleDropMode;
    int _currentAmount;
    int _maxAmount;
    CInventoryItem* _pItem;
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
