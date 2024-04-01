#pragma once
#include "../EditorRender/stdafx.h"

#include "XrUI.h"
#include "ItemListTypes.h"

#define EditorTextureID IDirect3DBaseTexture9* 


using TOnItemClone = fastdelegate::FastDelegate2<LPCSTR, LPCSTR>;
using TOnItemCreate = fastdelegate::FastDelegate1<LPCSTR>;

using TOnILCloseEvent = fastdelegate::FastDelegate0<>;
using TOnItemRename = fastdelegate::FastDelegate3<LPCSTR, LPCSTR, EItemType>;
using TOnItemRemove = fastdelegate::FastDelegate2<LPCSTR, EItemType>;
using TOnItemAfterRemove = fastdelegate::FastDelegate0<>;
using TOnCloseEvent = fastdelegate::FastDelegate0<>;
using TOnModifiedEvent = fastdelegate::FastDelegate0<>;
using TOnILItemFocused = fastdelegate::FastDelegate1<ListItem*>;
using TOnILItemsFocused = fastdelegate::FastDelegate1<ListItemsVec&>;

#include "xrEProps.h"

#include "Tree\Base\UITreeItem.h"
#include "Tree\Choose\UIChooseFormItem.h"
#include "Tree\Choose\UIChooseForm.h"
#include "Tree\ItemList\UIItemListFormItem.h"
#include "Tree\ItemList\UIItemListForm.h"
#include "Tree\Properties\UIPropertiesItem.h"
#include "Tree\Properties\UIPropertiesForm.h"
