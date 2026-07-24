#include "stdafx.h"
#include "TiramisuShaderDefinesContainer.h"

TiramisuShaderDefinesContainer::TiramisuShaderDefinesContainer() :
	CRC32(CRC32_INITIAL)
{
	Defines.reserve(16);
}

TiramisuShaderDefinesContainer::TiramisuShaderDefinesContainer(const TiramisuShaderDefinesContainer& Right)
{
	Copy(Right);
}

TiramisuShaderDefinesContainer::TiramisuShaderDefinesContainer(TiramisuShaderDefinesContainer&& Right)
{
	Swap(Right);
}

void TiramisuShaderDefinesContainer::Copy(const TiramisuShaderDefinesContainer& Right)
{
	CRC32 = Right.CRC32;
	Defines = Right.Defines;
}

void TiramisuShaderDefinesContainer::Swap(TiramisuShaderDefinesContainer& Right)
{
	std::swap(CRC32, Right.CRC32);
	Defines.swap(Right.Defines);
}

TiramisuShaderDefinesContainer& TiramisuShaderDefinesContainer::operator=(TiramisuShaderDefinesContainer&& Right)
{
	Swap(Right);
	return *this;
}

TiramisuShaderDefinesContainer& TiramisuShaderDefinesContainer::operator=(const TiramisuShaderDefinesContainer& Right)
{
	Copy(Right);
	return *this;
}

void TiramisuShaderDefinesContainer::Add(shared_str Define)
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

void TiramisuShaderDefinesContainer::UpdateCRC32()
{
	CRC32 = CRC32_INITIAL;

	for (const shared_str& Define : Defines)
	{
		CRC32 = crc32(Define.c_str(), Define.size(), CRC32);
	}
}

bool TiramisuShaderDefinesContainer::operator<(const TiramisuShaderDefinesContainer& Right) const
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

bool TiramisuShaderDefinesContainer::operator==(const TiramisuShaderDefinesContainer& Right) const
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

bool TiramisuShaderDefinesContainer::operator!=(const TiramisuShaderDefinesContainer& Right) const
{
	return !(this->operator==(Right));
}
