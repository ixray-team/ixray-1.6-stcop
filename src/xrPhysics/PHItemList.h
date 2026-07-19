#pragma once

/*
 * FX: »нтрузивный список на основе вектора с пулом свободных слотов
 *
 * ¬место св¤зного списка используем вектор указателей.
 * ”даленные элементы помечаютс¤ nullptr и их индексы сохран¤ютс¤ в пуле свободных слотов дл¤ переиспользовани¤.
 */

#define DECLARE_PHLIST_ITEM(class_name)             \
	friend class CPHItemList<class_name>;           \
	friend class CPHItemList<class_name>::iterator; \
	u32 PhListIndex;								\
	u32 PhListVersion;

#define DECLARE_PHSTACK_ITEM(class_name)   \
	DECLARE_PHLIST_ITEM(class_name)        \
	friend class CPHItemStack<class_name>; \
	u16 StackPos;

template <class T>
class CPHItemList
{
private:
	struct SItemSlot
	{
		T* Ptr;
		u32 NextFree;

		SItemSlot()
			: Ptr(nullptr), NextFree(u32(-1)) {}
	};

	xr_vector<SItemSlot> Items;
	u32 FreeHead;
	u16 Size;

	bool IsValidIndex(u32 idx) const
	{
		return idx < Items.size() && Items[idx].Ptr != nullptr;
	}

public:
	class iterator
	{
		CPHItemList<T>* List;
		u32 Idx;

		void Advance()
		{
			if (!List || Idx == u32(-1))
			{
				Idx = u32(-1);
				return;
			}

			// »щем следующий не-nullptr слот
			u32 size = (u32)List->Items.size();
			while (Idx < size && List->Items[Idx].Ptr == nullptr)
			{
				++Idx;
			}

			if (Idx >= size)
			{
				Idx = u32(-1);
			}
		}

	public:
		iterator()
			: List(nullptr), Idx(u32(-1)) {}

		iterator(CPHItemList<T>* list, u32 idx = 0)
			: List(list), Idx(idx)
		{
			if (List && Idx != u32(-1))
			{
				Advance();
			}
		}

		iterator& operator++()
		{
			if (List && Idx != u32(-1))
			{
				++Idx;
				Advance();
			}
			return *this;
		}

		iterator operator++(int)
		{
			iterator temp = *this;
			++(*this);
			return temp;
		}

		T* operator*() const
		{
			if (List && IsValid())
			{
				return List->Items[Idx].Ptr;
			}
			return nullptr;
		}

		T* operator->() const
		{
			return **this;
		}

		bool operator==(const iterator& right) const
		{
			if (List != right.List)
			{
				return false;
			}
			if (Idx == u32(-1) && right.Idx == u32(-1))
			{
				return true;
			}
			return Idx == right.Idx;
		}

		bool operator!=(const iterator& right) const
		{
			return !(*this == right);
		}

		bool IsValid() const
		{
			return List && Idx != u32(-1) &&
				   Idx < List->Items.size() &&
				   List->Items[Idx].Ptr != nullptr;
		}

		operator T*() const
		{
			return **this;
		}

		u32 GetIndex() const { return Idx; }
	};

	CPHItemList()
		: FreeHead(u32(-1)), Size(0)
	{
		Reserve(1024);
	}

	~CPHItemList() = default;

	void Reserve(size_t count)
	{
		Items.reserve(count);
	}

	u16 Count() const
	{
		return Size;
	}

	bool IsEmpty() const
	{
		return Size == 0;
	}

	void PushBack(T* item)
	{
		if (!item)
		{
			return;
		}

		u32 idx;

		if (FreeHead != u32(-1))
		{
			idx = FreeHead;
			FreeHead = Items[idx].NextFree;
			Items[idx].Ptr = item;
		}
		else
		{
			idx = (u32)Items.size();
			Items.emplace_back();
			Items[idx].Ptr = item;
		}

		item->PhListIndex = idx;
		item->PhListVersion = 1;

		++Size;
	}

	void Erase(iterator& it)
	{
		if (!it.IsValid())
		{
			return;
		}

		u32 idx = it.GetIndex();
		T* item = Items[idx].Ptr;

		if (item)
		{
			Items[idx].Ptr = nullptr;
			Items[idx].NextFree = FreeHead;
			FreeHead = idx;

			item->PhListIndex = u32(-1);
			++item->PhListVersion;

			--Size;
		}

		it = end();
	}

	void Erase(T* item)
	{
		if (!item || item->PhListIndex >= Items.size())
		{
			return;
		}

		u32 idx = item->PhListIndex;
		if (Items[idx].Ptr == item)
		{
			Items[idx].Ptr = nullptr;
			Items[idx].NextFree = FreeHead;
			FreeHead = idx;

			item->PhListIndex = u32(-1);
			++item->PhListVersion;

			--Size;
		}
	}

	void MoveItems(CPHItemList<T>& InputList)
	{
		if (InputList.IsEmpty())
		{
			return;
		}

		Reserve(Items.size() + InputList.Size);

		for (T* MovedItem : InputList)
		{
			if (MovedItem)
			{
				PushBack(MovedItem);
			}
		}

		InputList.Empty();
	}

	void Empty()
	{
		Items.clear();
		FreeHead = u32(-1);
		Size = 0;
	}

	void Compact()
	{
		if (FreeHead == u32(-1))
		{
			return;
		}

		xr_vector<SItemSlot> compacted;
		compacted.reserve(Size);

		for (auto& slot : Items)
		{
			if (slot.Ptr)
			{
				compacted.push_back(slot);
			}
		}

		for (u32 i = 0; i < compacted.size(); ++i)
		{
			if (compacted[i].Ptr)
			{
				compacted[i].Ptr->PhListIndex = i;
			}
		}

		Items.swap(compacted);
		FreeHead = u32(-1);
	}

	iterator begin()
	{
		return iterator(this, 0);
	}

	iterator end()
	{
		return iterator(this, u32(-1));
	}

	iterator begin() const
	{
		return iterator(const_cast<CPHItemList<T>*>(this), 0);
	}

	iterator end() const
	{
		return iterator(const_cast<CPHItemList<T>*>(this), u32(-1));
	}

	T* operator[](u32 idx) const
	{
		if (idx < Items.size() && Items[idx].Ptr)
		{
			return Items[idx].Ptr;
		}
		return nullptr;
	}

	const xr_vector<SItemSlot>& GetItems() const
	{
		return Items;
	}
};

#define DEFINE_PHITEM_LIST(T, N, I) \
	typedef CPHItemList<T> N;       \
	typedef CPHItemList<T>::iterator I;