#include "stdafx.h"

template<typename T>
inline bool MixedNumeric(PropItem* item, bool& change)
{
	change = false;
	NumericValue<T>* V = dynamic_cast<NumericValue<T>*>(item->GetFrontValue());
	if (!V)					return false;
	T value = *V->value;
	item->BeforeEdit<NumericValue<T>, T>(value);

	if (item->AfterEdit< NumericValue<T>, T>(value))
	{
		change = item->ApplyValue< NumericValue<T>, T>(value);
	}
	return true;
}
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
template <class T>
BOOL MixedFlag(PropItem* prop, bool& change)
{
	FlagValue<_flags<T> >* V = dynamic_cast<FlagValue<_flags<T> >*>(prop->GetFrontValue());
	if (!V)					return FALSE;
	_flags<T> new_val = V->GetValue();

	prop->BeforeEdit<FlagValue<_flags<T> >, _flags<T> >(new_val);
	if (prop->AfterEdit<FlagValue<_flags<T> >, _flags<T> >(new_val))
		change = prop->ApplyValue<FlagValue<_flags<T> >, _flags<T> >(new_val);
	return TRUE;
}
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
template <class T>
BOOL MixedToken(PropItem* prop, bool& change)
{
	TokenValue<T>* V = dynamic_cast<TokenValue<T>*>(prop->GetFrontValue());
	if (!V)					return FALSE;
	T edit_value = V->GetValue();
	prop->BeforeEdit<TokenValue<T>, T>(edit_value);
	if (prop->AfterEdit<TokenValue<T>, T>(edit_value))
		change = prop->ApplyValue<TokenValue<T>, T>(edit_value);
	return TRUE;
}
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
template <class T>
BOOL MixedRToken(PropItem* prop, bool& change)
{
	RTokenValue<T>* V = dynamic_cast<RTokenValue<T>*>(prop->GetFrontValue());
	if (!V)					return FALSE;
	T edit_value = V->GetValue();
	prop->BeforeEdit<RTokenValue<T>, T>(edit_value);
	if (prop->AfterEdit<RTokenValue<T>, T>(edit_value))
		change = prop->ApplyValue<RTokenValue<T>, T>(edit_value);
	return TRUE;
}
void UIPropertiesItem::RemoveMixed()
{
	EPropType type = PItem->Type();
	bool change = false;
	switch (type)
	{
	case PROP_SHORTCUT:
	{
		ShortcutValue* V = dynamic_cast<ShortcutValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		xr_shortcut val = *V->value;
		if (V->ApplyValue(val))change = true;
	}
	break;
	case PROP_CHOOSE:
	{

		ChooseValue* V = dynamic_cast<ChooseValue*>(PItem->GetFrontValue()); VERIFY(V);
		shared_str	edit_val = V->GetValue();
		if (!edit_val.size()) 	edit_val = V->m_StartPath;
		PItem->BeforeEdit<ChooseValue, shared_str>(edit_val);
		if (PItem->AfterEdit<ChooseValue, shared_str>(edit_val))
			if (PItem->ApplyValue<ChooseValue, shared_str>(edit_val))
			{
				change = true;
			}
	}
	break;
	case PROP_NUMERIC:
	{
		if (!MixedNumeric<u32>(PItem, change))
			if (!MixedNumeric<float>(PItem, change))
				if (!MixedNumeric<u8>(PItem, change))
					if (!MixedNumeric<s8>(PItem, change))
						if (!MixedNumeric<u16>(PItem, change))
							if (!MixedNumeric<u16>(PItem, change))
								if (!MixedNumeric<s32>(PItem, change))
									R_ASSERT(false);
	}
	break;
	case PROP_BOOLEAN:
	{
		BOOLValue* V = dynamic_cast<BOOLValue*>(PItem->GetFrontValue()); VERIFY(V);
		BOOL val = V->GetValue();
		PItem->BeforeEdit<BOOLValue, BOOL>(val);

		if (PItem->AfterEdit<BOOLValue, BOOL>(val))
			if (PItem->ApplyValue<BOOLValue, BOOL>(val))
			{
				change = true;
			}
	}
	break;
	case PROP_FLAG:
	{
		if (!MixedFlag<u8>(PItem, change))
			if (!MixedFlag<u16>(PItem, change))
				if (!MixedFlag<u32>(PItem, change))
					R_ASSERT(false);
	}
	break;
	case PROP_VECTOR:
	{
		VectorValue* V = dynamic_cast<VectorValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		Fvector edit_val = V->GetValue();

		PItem->BeforeEdit<VectorValue, Fvector>(edit_val);
		if (PItem->AfterEdit<VectorValue, Fvector>(edit_val))
			if (PItem->ApplyValue<VectorValue, Fvector>(edit_val))
			{
				change = true;
			}
	}
	break;
	case PROP_TOKEN:
	{
		if (!MixedToken<u8>(PItem, change))
			if (!MixedToken<u16>(PItem, change))
				if (!MixedToken<u32>(PItem, change))
					R_ASSERT(false);
	}
	break;
	case PROP_RTOKEN:
	{
		if (!MixedRToken<u8>(PItem, change))
			if (!MixedRToken<u16>(PItem, change))
				if (!MixedRToken<u32>(PItem, change))
					R_ASSERT(false);
	}
		break;
	case PROP_RLIST:
	{
		RListValue* V = dynamic_cast<RListValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		LPCSTR edit_value = V->value ? V->value->c_str() : 0;
		int index = 0;
		const char* InTokens[256];
		int i = 0;
		for (; i < V->item_count; i++)
		{
			if (V->items[i] == edit_value)
			{
				index = i;
			}
		}
		if (PItem->AfterEdit<RListValue, shared_str>(V->items[index]))
			if (PItem->ApplyValue<RListValue, shared_str>(V->items[index]))change = true;
	}
		break;
	case PROP_COLOR:
	{
		U32Value* V = dynamic_cast<U32Value*>(PItem->GetFrontValue()); R_ASSERT(V);
		u32 edit_val = V->GetValue();

		PItem->BeforeEdit<U32Value, u32>(edit_val);
		if (PItem->AfterEdit<U32Value, u32>(edit_val))
			if (PItem->ApplyValue<U32Value, u32>(edit_val))
			{
				change = true;
				
			}
	}
	break;
	case PROP_FCOLOR:
	{
		ColorValue* V = dynamic_cast<ColorValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		Fcolor edit_val = V->GetValue();

		PItem->BeforeEdit<ColorValue, Fcolor>(edit_val);
			if (PItem->AfterEdit<ColorValue, Fcolor>(edit_val))
				if (PItem->ApplyValue<ColorValue, Fcolor>(edit_val)) {
					change = true;
				}
	}
		break;
	case PROP_VCOLOR:
	{
		VectorValue* V = dynamic_cast<VectorValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		Fvector edit_val = V->GetValue();

		PItem->BeforeEdit<VectorValue, Fvector>(edit_val);
			if (PItem->AfterEdit<VectorValue, Fvector>(edit_val))
				if (PItem->ApplyValue<VectorValue, Fvector>(edit_val))
				{
					change = true;
				}
	}
		break;
	case PROP_RTEXT:
	case PROP_STEXT:
	case PROP_CTEXT:
	{
		CTextValue* V1 = dynamic_cast<CTextValue*>(PropertiesFrom->m_EditTextValue->GetFrontValue());
		if (V1)
		{
			xr_string out = PItem->GetDrawText();
			if (PropertiesFrom->m_EditTextValue->AfterEdit<CTextValue, xr_string>(out))
			{
				if (PropertiesFrom->m_EditTextValue->ApplyValue<CTextValue, LPCSTR>(out.c_str()))
				{
					change = true;
				}
			}
		}
		else
		{
			RTextValue* V2 = dynamic_cast<RTextValue*>(PropertiesFrom->m_EditTextValue->GetFrontValue());
			if (V2)
			{
				shared_str out = PItem->GetDrawText().c_str();
				if (PropertiesFrom->m_EditTextValue->AfterEdit<RTextValue, shared_str>(out))
				{
					if (PropertiesFrom->m_EditTextValue->ApplyValue<RTextValue, shared_str>(out))
					{
						change = true;
					}
				}
			}
			else
			{
				STextValue* V3 = dynamic_cast<STextValue*>(PropertiesFrom->m_EditTextValue->GetFrontValue());
				if (V3)
				{
					xr_string out = PItem->GetDrawText();;
					if (PropertiesFrom->m_EditTextValue->AfterEdit<STextValue, xr_string>(out))
					{
						if (PropertiesFrom->m_EditTextValue->ApplyValue<STextValue, xr_string>(out))
						{
							change = true;
						}
					}
				}
				else
				{
					R_ASSERT(false);
				}
			}
		}
	}
		break;
	case PROP_CLIST:
	{
		CListValue* V = dynamic_cast<CListValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		LPCSTR edit_value = V->value;
		int index = 0;
		int i = 0;
		for (; i < V->item_count; i++)
		{
			if (V->items[i] == edit_value)
			{
				index = i;
			}
		}
		if (PItem->AfterEdit<CListValue, xr_string>(V->items[index]))
			if (PItem->ApplyValue<CListValue, LPCSTR>(V->items[index].c_str()))change = true;
	}
		break;
	case PROP_SH_TOKEN:
	{
		TokenValueSH* V = dynamic_cast<TokenValueSH*>(PItem->GetFrontValue()); R_ASSERT(V);
		u32 edit_value = V->GetValue();
		PItem->BeforeEdit<TokenValueSH, u32>(edit_value);
		if (PItem->AfterEdit<TokenValueSH, u32>(edit_value))
			if (PItem->ApplyValue<TokenValueSH, u32>(edit_value))
			{
				change = true;
			}
	}
		break;
	case PROP_TEXTURE2:
	{
		CTextValue* T = dynamic_cast<CTextValue*>(PItem->GetFrontValue()); R_ASSERT(T);
		xr_string edit_val = T->GetValue();
		PItem->BeforeEdit<CTextValue, xr_string>(edit_val);

		if (PItem->AfterEdit<CTextValue, xr_string>(edit_val))
		{
			if (PItem->ApplyValue<CTextValue, LPCSTR>(edit_val.c_str()))
			{
				change = true;
			}
		}
	}
		break;
	case PROP_GAMETYPE:
	{
		GameTypeValue* V = dynamic_cast<GameTypeValue*>(PItem->GetFrontValue()); R_ASSERT(V);
		ImGui::Text(PItem->GetDrawText().c_str());
		auto value = V->GetValue();
		PItem->BeforeEdit<GameTypeValue, GameTypeChooser>(value);
		if (PItem->AfterEdit<GameTypeValue, GameTypeChooser>(value))
		{
			if (PItem->ApplyValue<GameTypeValue, GameTypeChooser>(value))
			{
				change = true;
			}
		}
	}
		break;
	default:
		return;
		break;
	}
	if (change)
	{
		PropertiesFrom->Modified();
	}
	else
	{
		PropertiesFrom->Modified();
		PItem->m_Flags.set(PropItem::flIgnoreMixed, 1);
	}
}

