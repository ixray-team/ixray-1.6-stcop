#pragma once

#include "../../xrUI/Widgets/UIWindow.h"

namespace UINavigationOwnership
{
    inline void ReparentOwned(CUIWindow* newParent, CUIWindow* child)
    {
        VERIFY(newParent);
        VERIFY(child);
        if (!newParent || !child)
        {
            return;
        }

        if (child->GetParent() == newParent)
        {
            child->SetAutoDelete(true);
            return;
        }

        child->SetAutoDelete(false);
        if (CUIWindow* parent = child->GetParent())
        {
            parent->DetachChild(child);
        }

        newParent->AttachChild(child);
        child->SetAutoDelete(true);
    }

    inline bool IsOwnedChild(CUIWindow* parent, CUIWindow* child)
    {
        return parent && child && child->GetParent() == parent && child->IsAutoDelete();
    }

    inline void DetachForManualDelete(CUIWindow* child)
    {
        if (!child)
        {
            return;
        }

        child->SetAutoDelete(false);
        if (CUIWindow* parent = child->GetParent())
        {
            parent->DetachChild(child);
        }
    }
}
