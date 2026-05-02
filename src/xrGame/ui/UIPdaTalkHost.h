#pragma once

#include "../../xrCore/_vector2.h"

class CUIPdaContactsWnd;
class CUITalkWnd;

class CUIPdaTalkHost final
{
public:
    CUIPdaTalkHost();
    ~CUIPdaTalkHost();

    bool Begin(CUITalkWnd* talkWnd, CUIPdaContactsWnd* contacts);
    void End(CUITalkWnd* talkWnd);
    bool IsActive() const { return _contacts != nullptr; }

private:
    CUIPdaContactsWnd* _contacts = nullptr;
    bool _dialogOnRightFrame = false;
    Fvector2 _savedTalkDlgPos;
    Fvector2 _savedTalkDlgSize;
};
