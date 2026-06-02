#include "stdafx.h"
#include "TShaderDefinesContainer.h"

TShaderDefinesContainer::TShaderDefinesContainer() :
	CRC32(CRC32_INITIAL)
{
	Defines.reserve(16);
}

TShaderDefinesContainer::TShaderDefinesContainer(const TShaderDefinesContainer& Right)
{
	Copy(Right);
}

TShaderDefinesContainer::TShaderDefinesContainer(TShaderDefinesContainer&& Right)
{
	Swap(Right);
}

void TShaderDefinesContainer::Copy(const TShaderDefinesContainer& Right)
{
	CRC32 = Right.CRC32;
	Defines = Right.Defines;
}

void TShaderDefinesContainer::Swap(TShaderDefinesContainer& Right)
{
	std::swap(CRC32, Right.CRC32);
	Defines.swap(Right.Defines);
}

TShaderDefinesContainer& TShaderDefinesContainer::operator=(TShaderDefinesContainer&& Right)
{
	Swap(Right);
	return *this;
}

TShaderDefinesContainer& TShaderDefinesContainer::operator=(const TShaderDefinesContainer& Right)
{
	Copy(Right);
	return *this;
}

void TShaderDefinesContainer::Add(shared_str Define)
{
	auto Item = std::lower_bound
	(
		Defines.begin(), Defines.end(), Define,
		[](const shared_str& left, const shared_str& right)
		{
			return left._get()->dwCRC < right._get()->dwCRC;
		}
	);

	Defines.insert(Item, Define);
}

void TShaderDefinesContainer::UpdateCRC32()
{
	CRC32 = CRC32_INITIAL;

	for (const shared_str& Define : Defines)
	{
		CRC32 = crc32(Define.c_str(), Define.size(), CRC32);
	}
}

bool TShaderDefinesContainer::operator<(const TShaderDefinesContainer& Right) const
{
	if (Defines.size() != Right.Defines.size())
	{
		return Defines.size() < Right.Defines.size();
	}
	
	if (CRC32 != Right.CRC32)
	{
		return CRC32 < Right.CRC32;
	}
	
	for (u32 i = 0; i < Defines.size(); i++)
	{
		if (Defines[i] != Right.Defines[i])
		{
			return Defines[i] < Right.Defines[i];
		}
	}

	return false;
}

bool TShaderDefinesContainer::operator==(const TShaderDefinesContainer& Right) const
{
	if (CRC32 != Right.CRC32)
	{
		return false;
	}

	if (Defines.size() != Right.Defines.size())
	{
		return false;
	}

	for (u32 i = 0; i < Defines.size(); i++)
	{
		if (Defines[i] != Right.Defines[i])
		{
			return false;
		}
	}

	return true;
}

bool TShaderDefinesContainer::operator!=(const TShaderDefinesContainer& Right) const
{
	return !(this->operator==(Right));
}
