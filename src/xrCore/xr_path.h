///////////////////////////////////////////////////////////
// FX: я устал по кд преобразовывать типы туда <-> обратно
///////////////////////////////////////////////////////////

#pragma once

// std::filesystem::path с поддержкой xr_string.
/// !!!ИСПОЛЬЗОВАТЬ синонемичный xr_path
class XRCORE_API CFilePath: 
	public std::filesystem::path
{
	using inherited = std::filesystem::path;

public:
	CFilePath() :
		inherited() {};

	CFilePath(const xr_string& Str) :
		inherited(Str.c_str()) {};
	
	CFilePath(const char* Str) :
		inherited(Str) {};

	CFilePath(const std::filesystem::directory_entry& Str) :
		inherited(Str.path()) {};

	CFilePath(const std::filesystem::path& Str) :
		inherited(Str) {};

	[[nodiscard]] xr_string xstring() const;
	[[nodiscard]] xr_string xfilename() const;

public:
	[[nodiscard]] inherited string() const = delete;
	[[nodiscard]] inherited filename() const = delete;

public:
	XRCORE_API friend CFilePath operator/(const CFilePath& _Left, const CFilePath& _Right);
	operator xr_string() const;

	CFilePath& operator=(const CFilePath& Right) = default;
	CFilePath& operator=(const xr_string& Right);
	CFilePath& operator=(const char* Right);

public:
	static bool exists(const CFilePath& Path);

private:
	IC inherited _parent_class() const
	{
		return *this;
	};
};

using xr_path = CFilePath;
using xr_dir_entry = std::filesystem::directory_entry;
using xr_dir_iter = std::filesystem::directory_iterator;
using xr_dir_recursive_iter = std::filesystem::recursive_directory_iterator;