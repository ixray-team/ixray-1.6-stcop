#include "stdafx.h"
#include "XRayShaderDefinesContainer.h"

XRayShaderDefinesContainer::XRayShaderDefinesContainer() :
	CRC32(CRC32_INITIAL)
{
	Defines.reserve(16);
}

XRayShaderDefinesContainer::XRayShaderDefinesContainer(const XRayShaderDefinesContainer& Right)
{
	Copy(Right);
}

XRayShaderDefinesContainer::XRayShaderDefinesContainer(XRayShaderDefinesContainer&& Right)
{
	Swap(Right);
}

void XRayShaderDefinesContainer::Copy(const XRayShaderDefinesContainer& Right)
{
	CRC32 = Right.CRC32;
	Defines = Right.Defines;
}

void XRayShaderDefinesContainer::Swap(XRayShaderDefinesContainer& Right)
{
	std::swap(CRC32, Right.CRC32);
	Defines.swap(Right.Defines);
}

XRayShaderDefinesContainer& XRayShaderDefinesContainer::operator=(XRayShaderDefinesContainer&& Right)
{
	Swap(Right);
	return *this;
}

XRayShaderDefinesContainer& XRayShaderDefinesContainer::operator=(const XRayShaderDefinesContainer& Right)
{
	Copy(Right);
	return *this;
}

void XRayShaderDefinesContainer::Add(shared_str Define)
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

void XRayShaderDefinesContainer::UpdateCRC32()
{
	CRC32 = CRC32_INITIAL;

	for (const shared_str& Define : Defines)
	{
		CRC32 = crc32(Define.c_str(), Define.size(), CRC32);
	}
}

bool XRayShaderDefinesContainer::operator<(const XRayShaderDefinesContainer& Right) const
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

bool XRayShaderDefinesContainer::operator==(const XRayShaderDefinesContainer& Right) const
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

bool XRayShaderDefinesContainer::operator!=(const XRayShaderDefinesContainer& Right) const
{
	return !(this->operator==(Right));
}
