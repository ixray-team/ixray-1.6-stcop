//---------------------------------------------------------------------------
#ifndef ItemListHelperH
#define ItemListHelperH


//---------------------------------------------------------------------------
class CListHelper: public IListHelper{
//------------------------------------------------------------------------------
public:
    virtual ListItem* 		 	FindItem		(ListItemsVec& items,	const char* key);
 	virtual bool 				NameAfterEdit	(ListItem* sender, const char* value, shared_str& edit_val);
public:
	virtual ListItem*			CreateItem		(ListItemsVec& items, const char* key, int type, u32 item_flags=0, void* object=0);
};
//---------------------------------------------------------------------------
#endif
