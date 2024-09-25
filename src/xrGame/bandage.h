#pragma once
#include "eatable_item_object.h"

class CBandage : public CEatableItemObject {
public:
    CBandage();
    virtual ~CBandage();

    virtual bool CanUseItem() const override;
    virtual shared_str GetUseString() const override;
};