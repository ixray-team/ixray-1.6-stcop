#pragma once
#include <fswatcher/filewatch.hpp>

class CContentView:
	public XrUI
{
	struct DragDropData 
	{
		xr_string FileName;
	};

	struct IconData 
	{
		ref_texture Icon;
		bool UseButtonColor = false;
	};

	struct HintItem
	{
		xr_string Name;
		ImVec2 Pos;
		bool Active = false;
	};

	struct FileOptData
	{
		std::filesystem::path File;
		bool IsDir = false;
		shared_str ISESect;
	};

public:
	CContentView();
	virtual void Draw() override;
	void DrawHeader();
	void FindFile();
	void DrawISEDir(size_t& HorBtnIter, const size_t IterCount);
	void DrawRootDir(size_t& HorBtnIter, const size_t& IterCount, xr_string& NextDir);
	void DrawOtherDir(size_t& HorBtnIter, const size_t IterCount, xr_string& NextDir);

	void RescanISEDirectory(const xr_string& path);
	void RescanDirectory();
	virtual void Init();
	virtual void Destroy();
	virtual void ResetBegin();
	virtual void ResetEnd();

private:
	bool DrawItem(const FileOptData& FilePath, size_t& HorBtnIter, const size_t IterCount);
	bool DrawContext(const std::filesystem::path& Path) const;
	bool Contains();
	IconData& GetTexture(const xr_string& IconPath);

private:

	HintItem CurrentItemHint;

	DragDropData Data;
	xr_vector<FileOptData> Files;
	filewatch::FileWatch<std::string>* WatcherPtr;

	ref_texture MenuIcon;

	xr_string CurrentDir;
	xr_string RootDir;
	xr_string LogsDir;
	ImVec2 BtnSize = { 64, 64 };

	xr_hash_map<xr_string, IconData> Icons;

	bool IsDelWatcher = false;
	bool IsSpawnElement = false;
	xr_string ISEPath;
	string32 FindStr = {};

	volatile xr_atomic_bool LockFiles = false;
};