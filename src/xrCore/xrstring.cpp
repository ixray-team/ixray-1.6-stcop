#include "stdafx.h"
#pragma hdrstop

#include "xrstring.h"

#include "FS_impl.h"

xr_vector<xr_string> xr_string::Split(char splitCh) const
{
	xr_vector<xr_string> Result;

	u32 SubStrBeginCursor = 0;
	u32 Len = 0;

	u32 StrCursor = 0;
	for (; StrCursor < size(); ++StrCursor)
	{
		if (at(StrCursor) == splitCh)
		{
			if ((StrCursor - SubStrBeginCursor) > 0)
			{
				Len = StrCursor - SubStrBeginCursor;
				Result.emplace_back(xr_string(&at(SubStrBeginCursor), Len));
				SubStrBeginCursor = StrCursor + 1;
			}
			else
			{
				Result.emplace_back("");
				SubStrBeginCursor = StrCursor + 1;
			}
		}
	}

	if (StrCursor > SubStrBeginCursor)
	{
		Len = StrCursor - SubStrBeginCursor;
		Result.emplace_back(xr_string(&at(SubStrBeginCursor), Len));
	}
	return Result;
}

xr_string::xr_string(LPCSTR Str, u32 Size)
	: Super(Str, Size)
{
}

xr_string::xr_string(Super&& other)
	: Super(other)
{
}

xr_string::xr_string(LPCSTR Str)
	: Super(Str)
{
}

xr_string& xr_string::operator=(LPCSTR Str) 
{
	Super::operator=(Str);
	return *this;
}

xr_string& xr_string::operator=(const Super& other) 
{
	Super::operator=(other);
	return *this;
}

xr_vector<xr_string> xr_string::Split(u32 NumberOfSplits, ...) const
{
	xr_vector<xr_string> intermediateTokens;
	xr_vector<xr_string> Result;

	va_list args;
	va_start(args, NumberOfSplits);

	for (u32 i = 0; i < NumberOfSplits; ++i)
	{
		char splitCh = va_arg(args, char);

		//special case for first try
		if (i == 0)
		{
			Result = Split(splitCh);
		}

		for (xr_string& str : Result)
		{
			xr_vector<xr_string> TokenStrResult = str.Split(splitCh);
			intermediateTokens.insert(intermediateTokens.end(), TokenStrResult.begin(), TokenStrResult.end());
		}

		if (!intermediateTokens.empty())
		{
			Result.clear();
			Result.insert(Result.begin(), intermediateTokens.begin(), intermediateTokens.end());
			intermediateTokens.clear();
		}
	}

	va_end(args);

	return Result;
}


xr_string xr_string::RemoveWhitespaces() const 
{
	size_t Size = size();
	if (Size == 0) return xr_string();

	xr_string Result;
	Result.reserve(Size);

	const char* OrigStr = data();

	for (size_t i = 0; i < Size; ++i)
	{
		if (OrigStr[i] != ' ')
		{
			Result.push_back(OrigStr[i]);
		}
	}

	return Result;
}

bool xr_string::StartWith(const xr_string& Other) const
{
	return StartWith(Other.data(), Other.size());
}


bool xr_string::StartWith(LPCSTR Str) const 
{
	u32 StrLen = xr_strlen(Str);
	return StartWith(Str, (int)StrLen);
}

bool xr_string::StartWith(LPCSTR Str, size_t Size) const 
{
	size_t OurSize = size();

	//String is greater then our, we can't success
	if (OurSize < Size) return false;

	const char* OurStr = data();

	for (int i = 0; i < Size; ++i)
	{
		if (OurStr[i] != Str[i])
		{
			return false;
		}
	}

	return true;
}

bool xr_string::Contains(const xr_string& SubStr) const
{
	return find(SubStr) != npos;
}

bool xr_string::EqualWithCaseInsensitive(const xr_string& SubStr) const
{
	xr_string Left = *this;
	xr_string Right = SubStr;

	xr_strlwr(Left);
	xr_strlwr(Right);

	return Left == Right;
}

xr_string xr_string::ToString(int Value) 
{
	string64 buf = { 0 };
	itoa(Value, &buf[0], 10);

	return std::move(xr_string(buf));
}

xr_string xr_string::ToString(unsigned int Value)
{
	string64 buf = { 0 };
	sprintf(buf, "%u", Value);

	return std::move(xr_string(buf));
}

xr_string xr_string::ToString(float Value) 
{
	string64 buf = { 0 };
	sprintf(buf, "%.3f", Value);

	xr_string buff = buf;

	size_t StartPos = buff.find('.');

	bool NeedClear = true;

	for (size_t Iter = StartPos + 1; Iter < buff.length(); Iter++)
	{
		if (buff[Iter] != '0')
		{
			NeedClear = false;
			break;
		}
	}

	if (NeedClear)
	{
		return buff.substr(0, StartPos);
	}

	return buff;
}

xr_string xr_string::ToString(double Value) 
{
	string64 buf = { 0 };
	sprintf(buf, "%f", Value);

	xr_string buff = buf;

	size_t StartPos = buff.find('.');

	bool NeedClear = true;

	for (size_t Iter = StartPos + 1; Iter < buff.length(); Iter++)
	{
		if (buff[Iter] != '0')
		{
			NeedClear = false;
			break;
		}
	}

	if (NeedClear)
	{
		return buff.substr(0, StartPos);
	}

	return buff;
}

xr_string xr_string::Join(xrStringVector::iterator beginIter, xrStringVector::iterator endIter, const char delimeter /*= '\0'*/) 
{
	xr_string Result;
	xrStringVector::iterator cursorIter = beginIter;

	while (cursorIter != endIter)
	{
		Result.append(*cursorIter);
		if (delimeter != '\0')
		{
			Result.push_back(delimeter);
		}
		cursorIter++;
	}

	if (delimeter != '\0')
	{
		Result.erase(Result.end() - 1);
	}

	return Result;
}
