#pragma once

class IRenderVisual;

enum class EAnimNotifyType
{
    give_info,
    disable_info,
    lua_functor
};

class IAnimNotify
{
public:
    virtual ~IAnimNotify() = default;

    virtual void Construct(CInifile ini, LPCSTR sect) = 0;
    virtual void Execute(IRenderVisual* visual, u16 bone_id) = 0;
};

struct IAnimNotifyMessage
{
    IAnimNotify* notify;
    IRenderVisual* render_visual;
    u16 bone_id;
};