//---------------------------------------------------------------------------
#pragma once

//---------------------------------------------------------------------------
class UIItemListForm;

class ListItem
{
	friend class		CListHelper;
    friend class		UIItemListForm;
    shared_str		  	key;
	shared_str		  	prefix = "";
	shared_str		  	icon = "";
    int					type;
	void*				item;
public:                        
    typedef fastdelegate::FastDelegate<void(ListItem*)> TOnListItemFocused;   
    typedef fastdelegate::FastDelegate<void(ListItem*)> TOnClick;
    TOnClick			OnClickEvent;
    TOnListItemFocused	OnItemFocused;
  //  TOnDrawThumbnail	OnDrawThumbnail;
public:
    UIItemListForm* Parent;
    int 				tag;
    LPVOID				m_Object;
    int					icon_index;
    u32					prop_color;
    bool selected;
public:
    enum{
    	flShowCB		= (1<<0),
    	flCBChecked		= (1<<1),
		flDrawThumbnail	= (1<<2),
		flDrawCanvas	= (1<<3),
        flSorted		= (1<<4),
        flHidden		= (1<<5),
    };
    Flags32				m_Flags;
public:
						ListItem		(int _type):type(_type), selected(false),prop_color(0),item(0),key(0),tag(0),icon_index(-1),/*OnDrawThumbnail(0),*/OnItemFocused(0),m_Object(0){m_Flags.zero();}
	virtual 			~ListItem		(){};
    void				SetName			(const char* _key){key=_key;}
	void				SetPrefix		(const char* _prefix){prefix = _prefix;}
	void				SetIconStr		(const char* _icon){icon = _icon;}

    IC void				Visible			(bool val){m_Flags.set(flHidden, !val);}
    IC bool				Visible			() const{ return !m_Flags.test(flHidden);}
    IC int				Type			(){return type;}
	IC void*			Item			(){return item;}
	IC const char*			Key				(){return *key;}
	IC const char*			Prefix			(){return *prefix;}
	IC const char*			Icon			(){return *icon;}
    IC void				SetIcon			(int index){icon_index=index;}
};

using ListItemsVec = xr_vector<ListItem*>;
using ListItemsIt = ListItemsVec::iterator;
//---------------------------------------------------------------------------




