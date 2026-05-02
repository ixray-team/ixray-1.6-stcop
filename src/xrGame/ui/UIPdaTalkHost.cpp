#include "StdAfx.h"
#include "UIPdaTalkHost.h"

#include "UIPdaContactsWnd.h"
#include "UITalkDialogWnd.h"
#include "UITalkWnd.h"

#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollView.h"

CUIPdaTalkHost::CUIPdaTalkHost() :
    _contacts(nullptr),
    _savedTalkDlgPos(),
    _savedTalkDlgSize()
{
}

CUIPdaTalkHost::~CUIPdaTalkHost()
{
}

bool CUIPdaTalkHost::Begin(CUITalkWnd* talkWnd, CUIPdaContactsWnd* contacts)
{
    CUITalkDialogWnd* dialogWnd = talkWnd ? talkWnd->UITalkDialogWnd : nullptr;
    CUIFrameWindow* rightFrame = contacts ? contacts->GetRightFrame() : nullptr;

    if (!talkWnd || !contacts || !rightFrame || !dialogWnd)
    {
        return false;
    }

    if (IsActive())
    {
        return _contacts == contacts;
    }

    _savedTalkDlgPos = dialogWnd->GetWndPos();
    _savedTalkDlgSize = dialogWnd->GetWndSize();

    // DetachChild deletes auto-delete children; this dialog is reparented, not destroyed.
    dialogWnd->SetAutoDelete(false);
    talkWnd->DetachChild(dialogWnd);

    rightFrame->AttachChild(dialogWnd);
    dialogWnd->ReloadDialogLayout(true, contacts);

    CUIScrollView* details = contacts->GetDetailsScroll();
    if (details)
    {
        details->Show(false);
    }

    if (dialogWnd->HasPdaDialogLayout())
    {
        dialogWnd->SetWndPos(Fvector2().set(0.0f, 0.0f));
        dialogWnd->SetWndSize(rightFrame->GetWndSize());
    }
    else if (details)
    {
        dialogWnd->SetWndPos(details->GetWndPos());
        dialogWnd->SetWndSize(details->GetWndSize());
    }
    else
    {
        dialogWnd->SetWndPos(Fvector2().set(0.0f, 0.0f));
        dialogWnd->SetWndSize(rightFrame->GetWndSize());
    }

    dialogWnd->ShowForPdaEmbed();
    dialogWnd->SetMessageTarget(talkWnd);

    _contacts = contacts;
    _dialogOnRightFrame = true;
    return true;
}

void CUIPdaTalkHost::End(CUITalkWnd* talkWnd)
{
    if (!IsActive())
    {
        return;
    }

    CUITalkDialogWnd* dialogWnd = talkWnd ? talkWnd->UITalkDialogWnd : nullptr;
    if (!talkWnd || !dialogWnd)
    {
        _contacts = nullptr;
        _dialogOnRightFrame = false;
        return;
    }

    CUIFrameWindow* rightFrame = _contacts->GetRightFrame();
    if (_dialogOnRightFrame && rightFrame && dialogWnd)
    {
        dialogWnd->SetAutoDelete(false);
        if (!rightFrame->TryDetachChild(dialogWnd))
        {
            _contacts = nullptr;
            _dialogOnRightFrame = false;
            return;
        }
        _dialogOnRightFrame = false;
    }

    CUIScrollView* details = _contacts->GetDetailsScroll();
    if (details)
    {
        details->Show(true);
    }

    dialogWnd->ReloadDialogLayout(false, nullptr);

    dialogWnd->SetAutoDelete(false);
    if (dialogWnd->GetParent() != talkWnd)
    {
        talkWnd->AttachChild(dialogWnd);
    }

    dialogWnd->SetWndPos(_savedTalkDlgPos);
    dialogWnd->SetWndSize(_savedTalkDlgSize);
    dialogWnd->SetAutoDelete(true);
    dialogWnd->SetMessageTarget(nullptr);

    _contacts = nullptr;
    _dialogOnRightFrame = false;
}
