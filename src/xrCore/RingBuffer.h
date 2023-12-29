//Giperion May 2018
//[EUREKA] 3.6

// Allowed for XRay Oxygen
#pragma once
#include <type_traits>

///Buffer for storing last BufferSize items pushed. Good option for history
template<typename StoredType, std::size_t  BufferSize = 10>
class RingBuffer
{
public:
	std::size_t Position = 0;

	RingBuffer()
	{
		//if stored type is fundamental, we must clear all with zero
		constexpr bool isFundamental = std::is_fundamental<StoredType>::value;
		if constexpr (isFundamental)
		{
			memset(&Buffer[0], 0, sizeof(StoredType) * BufferSize);
		}
		//otherwise, the default constructor will be called, clearing all without us
	}

	void Write(StoredType Value) {
		Buffer[(Position++ % BufferSize)] = Value;
	}

	//Get first element of this container. 
	unsigned int GetHead() const
	{
		return PosHead;
	}

	//Get current end of this container
	unsigned int GetTail() const
	{
		if (GetHead() == 0)
			return BufferSize - 1;

		if (GetHead() == (BufferSize - 1))
			return 0;

		return GetHead() - 1;
	}

	unsigned int GetSize() const
	{
		return BufferSize;
	}

	void MoveHead(unsigned int DeltaPos)
	{
		PosHead += DeltaPos;
		PosHead %= BufferSize;
	}

	const StoredType& Get(unsigned int Pos) const
	{
		return Buffer[Pos];
	}

	const StoredType& GetLooped(unsigned int Pos) const
	{
		return Buffer[Pos % GetSize()];
	}

	StoredType& Get(unsigned int Pos)
	{
		return Buffer[Pos];
	}

	void Push(StoredType&& Elem)
	{
		if (PosHead == 0)
		{
			PosHead = BufferSize - 1;
		}
		else
		{
			--PosHead;
		}
		Buffer[PosHead] = Elem;
	}

	void Push(StoredType& Elem)
	{
		if (PosHead == 0)
		{
			PosHead = BufferSize - 1;
		}
		else
		{
			--PosHead;
		}
		Buffer[PosHead] = Elem;
	}

	// write elements to storage, but don't move head pointer. So after this the head will point to first copied element
	bool WriteFromHeadNoMove(const StoredType* pElems, unsigned int ElemsCount)
	{
		if (ElemsCount >= BufferSize) return false;

		// don't use Push. Push works great for single item, for thing like history and etc. 
		// But for large arrays it's better have classic forward (like) data chain.
		unsigned int RemainElems = ElemsCount;

		if (int FirstChunkSize = BufferSize - GetHead())
		{
			unsigned int CopySize = FirstChunkSize >= ElemsCount ? ElemsCount : FirstChunkSize;
			memcpy(&Buffer[PosHead], pElems, CopySize * sizeof(StoredType));
			RemainElems = FirstChunkSize >= ElemsCount ? 0 : ElemsCount - FirstChunkSize;
		}

		if (RemainElems > 0)
		{
			memcpy(&Buffer[0], pElems + (ElemsCount - RemainElems), RemainElems * sizeof(StoredType));
		}

		return true;
	}

	template<typename Functor>
	void ForEachElementFromHead(Functor& FuncElem) const
	{
		unsigned int TailPos = GetTail();

		unsigned int Pos = GetHead();
		while (Pos != TailPos)
		{
			FuncElem(Get(Pos));

			++Pos;
			Pos %= BufferSize;
		}
	}

private:
	unsigned int PosHead = 0;
	StoredType Buffer[BufferSize];
};