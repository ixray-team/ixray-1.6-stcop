#pragma once
#include "StdAfx.h"
#include "pch_script.h"

class CFFxCrypto final
{
private:
	
public:
	CFFxCrypto();
	virtual	~CFFxCrypto();
	
	LPCSTR CRC64(LPCSTR input);

	DECLARE_SCRIPT_REGISTER_FUNCTION;
};
