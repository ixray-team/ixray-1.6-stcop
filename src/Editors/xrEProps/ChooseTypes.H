#ifndef ChooseTypesH
#define ChooseTypesH

#include "ChooseTypesHelper.H"

struct SChooseItem
{
    SChooseItem() = default;

	shared_str				name;
	shared_str				hint;
    SChooseItem	(const char* nm, const char* ht):name(nm),hint(ht){}
};

class IRHISurface;

using ChooseItemVec = xr_vector<SChooseItem>;
using ChooseItemVecIt = ChooseItemVec::iterator;

// typedef
typedef xr_delegate<void(ChooseItemVec&,void*)>	   	TOnChooseFillItems;
typedef xr_delegate<void(SChooseItem*, PropItemVec&)>	TOnChooseSelectItem;
typedef xr_delegate<void(const char*, HDC, const Irect&)> 	TOnDrawThumbnail;
typedef xr_delegate<void()>						   	TOnChooseClose;

using TGetTexture = xr_delegate<void(const char*, IRHISurface*&)>;

typedef void (*TOnChooseFillEvents)();

struct SChooseEvents{
	enum{
    	flAnimated		= (1<<0),
        flClearTexture  = (1<<1)
    };
	shared_str			caption;
    TOnChooseFillItems	on_fill;
    TOnChooseSelectItem	on_sel;
    TOnDrawThumbnail    on_thm;
    TGetTexture         on_get_texture;
    TOnChooseClose	    on_close;
    Flags32				flags;

    SChooseEvents	(){caption="Select Item";flags.zero();}
    SChooseEvents	(const char* capt, TOnChooseFillItems f, TOnChooseSelectItem s, TGetTexture t, TOnChooseClose c, u32 fl)
    {
    	Set				(capt,f,s,t,c,fl);
    }
    void				Set				(const char* capt, TOnChooseFillItems f, TOnChooseSelectItem s, TGetTexture t, TOnChooseClose c, u32 fl)
    {
        caption			= capt;
        on_fill			= f;
        on_sel			= s;
       // on_thm			= t;
        on_get_texture	= t;
        on_close		= c;
        flags.assign	(fl);
    }
};

#define NONE_CAPTION "<none>"

#endif
