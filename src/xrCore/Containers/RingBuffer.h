//Giperion May 2018
//[EUREKA] 3.6

#pragma once
#include <algorithm>
#include <type_traits>

template<typename StoredType, std::size_t BufferSize = 10>
class RingBuffer
{
public:
	std::size_t Position = 0;

	RingBuffer()
	{
		constexpr bool isFundamental = std::is_fundamental<StoredType>::value;
		if constexpr (isFundamental)
		{
			memset(&Buffer[0], 0, sizeof(StoredType) * BufferSize);
		}
	}

	void Write(StoredType Value)
	{
		Buffer[(Position++ % BufferSize)] = Value;
	}

	unsigned int GetHead() const
	{
		return PosHead;
	}

	unsigned int GetTail() const
	{
		if (GetHead() == 0)
			return BufferSize - 1;

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

	bool WriteFromHeadNoMove(const StoredType* pElems, unsigned int ElemsCount)
	{
		if (ElemsCount >= BufferSize) return false;

		size_t RemainElems = ElemsCount;
		if (const size_t FirstChunkSize = BufferSize - GetHead())
		{
			const size_t CopySize = FirstChunkSize >= ElemsCount ? ElemsCount : FirstChunkSize;
			std::copy_n(pElems, CopySize, &Buffer[PosHead]);
			RemainElems = FirstChunkSize >= ElemsCount ? 0 : ElemsCount - FirstChunkSize;
		}

		if (RemainElems > 0)
		{
			std::copy_n(pElems + (ElemsCount - RemainElems), RemainElems, &Buffer[0]);
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