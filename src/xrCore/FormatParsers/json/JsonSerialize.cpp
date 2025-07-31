#include "stdafx.h"
#include "JsonSerialize.h"

#include <fstream>

CJsonSerializer::CJsonSerializer(shared_str FileName)
{
	FS.update_path(jfn, "$app_data_root$", *FileName);

	if (std::filesystem::exists(jfn))
	{
		std::ifstream f(jfn);
		f >> JSONData;
	}
}

CJsonSerializer::~CJsonSerializer()
{
	std::ofstream o(jfn);
	o << JSONData;
}