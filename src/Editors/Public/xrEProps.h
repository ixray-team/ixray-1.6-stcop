#ifndef xrEPropsH
#define xrEPropsH

#ifndef XREPROPS_API
#	define XREPROPS_API __declspec(dllimport)
#endif

#include "PropertiesListTypes.h"
#include "ItemListTypes.h"
#include "xrEPropsTypes.h"

//typedef fastdelegate::FastDelegate<void(const char*, const char*)>	    TOnItemClone;
//typedef fastdelegate::FastDelegate<void(const char*)>	            TOnItemCreate;

//typedef fastdelegate::FastDelegate<void(ListItemsVec&)> 			TOnILItemsFocused;
typedef fastdelegate::FastDelegate<void()>							TOnILCloseEvent;
//typedef fastdelegate::FastDelegate<void(const char*, const char*, EItemType)>	TOnItemRename;
//typedef fastdelegate::FastDelegate<void(const char*, EItemType)>	        TOnItemRemove;
//typedef fastdelegate::FastDelegate<void()>							TOnItemAfterRemove;
typedef fastdelegate::FastDelegate<void()> 							TOnCloseEvent;
typedef fastdelegate::FastDelegate<void()>		  					TOnModifiedEvent;
//typedef fastdelegate::FastDelegate<void(ListItem*)>			        TOnILItemFocused;

#define DECLARE_XR_DELEGATE(name, retval, ...) using T##name = fastdelegate::FastDelegate<retval(__VA_ARGS__)>;

	

//------------------------------------------------------------------------------
// Prepare Key
//------------------------------------------------------------------------------
IC xr_string FolderAppend	(const char* val)
{
	xr_string	tmp 	= (val&&val[0])?val:"";
    if (val&&val[0])tmp	+= "\\";
    return 	tmp;
}
IC shared_str PrepareKey	(const char* pref, 	const char* key)
{
    R_ASSERT	(key);
	return 		shared_str((FolderAppend(pref)+key).c_str());
}
IC shared_str PrepareKey (const char* pref0, 	const char* pref1,	const char* key)
{
    R_ASSERT	(key);
    return 		shared_str((FolderAppend(pref0)+FolderAppend(pref1)+key).c_str());
}
IC shared_str PrepareKey (const char* pref0,	const char* pref1, 	const char* pref2,	const char* key)
{
    R_ASSERT	(key);
    return 		shared_str((FolderAppend(pref0)+FolderAppend(pref1)+FolderAppend(pref2)+key).c_str());
}
//------------------------------------------------------------------------------
// Properties
//------------------------------------------------------------------------------
class XREPROPS_API CPropHelper
{
	PropItem*				CreateItem			(PropItemVec& items, const shared_str& key, EPropType type, u32 item_flags = 0);
	PropValue*				AppendValue			(PropItemVec& items, const shared_str& key, PropValue* val, EPropType type, u32 item_flags = 0);

	template <class T>
	T*						Append				(PropItemVec& items, const shared_str& key, T* val, EPropType type)
	{
		return static_cast<T*>(AppendValue(items, key, val, type));
	}

	void					NameDraw			(PropValue* sender, xr_string& draw_val);

public:
	PropItem*				FindItem			(PropItemVec& items, shared_str key, EPropType type = PROP_UNDEF);

	// Angle edit: the value is stored in radians and shown in degrees. Also used by the script properties helper
	bool					FvectorRDOnAfterEdit(PropValue* sender, Fvector& edit_val);
	void					FvectorRDOnBeforeEdit(PropValue* sender, Fvector& edit_val);
	void					FvectorRDOnDraw		(PropValue* sender, xr_string& draw_val);
	bool					floatRDOnAfterEdit	(PropValue* sender, float& edit_val);
	void					floatRDOnBeforeEdit	(PropValue* sender, float& edit_val);
	void					floatRDOnDraw		(PropValue* sender, xr_string& draw_val);
	// Name edit: the last part of the path, checked against the list items
	void					NameBeforeEdit		(PropValue* sender, shared_str& edit_val);
	bool					NameAfterEdit		(PropValue* sender, shared_str& edit_val);

	// s8, s16, s32, u8, u16, u32
	template <class T>
	NumericValue<T>*		CreateNumeric		(PropItemVec& items, shared_str key, T* val, std::type_identity_t<T> mn = 0, std::type_identity_t<T> mx = 100, std::type_identity_t<T> inc = 1)
	{
		static_assert(std::is_same_v<T, s8> || std::is_same_v<T, s16> || std::is_same_v<T, s32> || std::is_same_v<T, u8> || std::is_same_v<T, u16> || std::is_same_v<T, u32>, "CreateNumeric takes s8, s16, s32, u8, u16, u32; CreateFloat for float");
		return Append(items, key, new NumericValue<T>(val, mn, mx, inc, 0), PROP_NUMERIC);
	}
	// Flags8, Flags16, Flags32
	template <class T>
	FlagValue<T>*			CreateFlag			(PropItemVec& items, shared_str key, T* val, typename T::TYPE mask, const char* c0 = 0, const char* c1 = 0, u32 flags = 0)
	{
		return Append(items, key, new FlagValue<T>(val, mask, c0, c1, flags), PROP_FLAG);
	}
	// u8, u16, u32
	template <class T>
	TokenValue<T>*			CreateToken			(PropItemVec& items, shared_str key, T* val, xr_token* token)
	{
		return Append(items, key, new TokenValue<T>(val, token), PROP_TOKEN);
	}
	// u8, u16, u32
	template <class T>
	RTokenValue<T>*			CreateRToken		(PropItemVec& items, shared_str key, T* val, xr_rtoken* token, u32 t_cnt)
	{
		return Append(items, key, new RTokenValue<T>(val, token, t_cnt), PROP_RTOKEN);
	}

	CaptionValue*			CreateCaption		(PropItemVec& items, shared_str key, shared_str val);
	CanvasValue*			CreateCanvas		(PropItemVec& items, shared_str key, shared_str val, int height);
	ButtonValue*			CreateButton		(PropItemVec& items, shared_str key, shared_str val, u32 flags, ButtonValue::TOnBtnClick onclick = 0);
	ChooseValue*			CreateChoose		(PropItemVec& items, shared_str key, shared_str* val, u32 mode, const char* path = 0, void* fill_param = 0, u32 sub_item_count = 1, u32 choose_flags = cfAllowNone);
	MultiChooseValue*		CreateChooseTexture	(PropItemVec& items, shared_str key);
	FloatValue*				CreateFloat			(PropItemVec& items, shared_str key, float* val, float mn = 0.f, float mx = 1.f, float inc = 0.01f, int decim = 2);
	BOOLValue*				CreateBOOL			(PropItemVec& items, shared_str key, int* val);
	BoolValue*				CreateBool			(PropItemVec& items, shared_str key, bool* val);
	VectorValue*			CreateVector		(PropItemVec& items, shared_str key, Fvector* val, float mn = 0.f, float mx = 1.f, float inc = 0.01f, int decim = 2);
	RListValue*				CreateRList			(PropItemVec& items, shared_str key, shared_str* val, shared_str* lst, u32 cnt);
	U32Value*				CreateColor			(PropItemVec& items, shared_str key, u32* val);
	ColorValue*				CreateFColor		(PropItemVec& items, shared_str key, Fcolor* val);
	VectorValue*			CreateVColor		(PropItemVec& items, shared_str key, Fvector* val);
	RTextValue*				CreateRText			(PropItemVec& items, shared_str key, shared_str* val);
	STextValue*				CreateSText			(PropItemVec& items, shared_str key, xr_string* val);
	WaveValue*				CreateWave			(PropItemVec& items, shared_str key, WaveForm* val);
	FloatValue*				CreateTime			(PropItemVec& items, shared_str key, float* val, float mn = 0.f, float mx = 86400.f);
	ShortcutValue*			CreateShortcut		(PropItemVec& items, shared_str key, xr_shortcut* val);
	FloatValue*				CreateAngle			(PropItemVec& items, shared_str key, float* val, float mn = flt_min, float mx = flt_max, float inc = 0.01f, int decim = 2);
	VectorValue*			CreateAngle3		(PropItemVec& items, shared_str key, Fvector* val, float mn = flt_min, float mx = flt_max, float inc = 0.01f, int decim = 2);
	RTextValue*				CreateName			(PropItemVec& items, shared_str key, shared_str* val, ListItem* owner);
	RTextValue*				CreateNameCB		(PropItemVec& items, shared_str key, shared_str* val, TOnDrawTextEvent = 0, RTextValue::TOnBeforeEditEvent = 0, RTextValue::TOnAfterEditEvent = 0);
	GameTypeValue*			CreateGameType		(PropItemVec& items, shared_str key, GameTypeChooser* val);
	TokenValueSH*			CreateTokenSH		(PropItemVec& items, shared_str key, u32* val, const TokenValueSH::Item* lst, u32 cnt);
	// C string values
	CListValue*				CreateCList			(PropItemVec& items, shared_str key, char* val, u32 sz, xr_string* lst, u32 cnt);
	CTextValue*				CreateTexture		(PropItemVec& items, shared_str key, char* val, u32 sz);
};
//---------------------------------------------------------------------------

extern XREPROPS_API CPropHelper& PHelper();
//---------------------------------------------------------------------------

//------------------------------------------------------------------------------
// List
//------------------------------------------------------------------------------
class XREPROPS_API IListHelper
{
public:
    virtual ListItem* 				FindItem			(ListItemsVec& items,	const char* key)=0;
 	virtual bool 					NameAfterEdit		(ListItem* sender, const char* value, shared_str& edit_val)=0;
public:
	virtual ListItem*				CreateItem			(ListItemsVec& items, const char* key, int type, u32 item_flags=0, void* object=0)=0;
};

XREPROPS_API IListHelper& LHelper ();

#endif
 