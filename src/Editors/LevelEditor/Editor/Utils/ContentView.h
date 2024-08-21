#pragma once

class CContentView:
	public XrUI
{
	struct DragDropData 
	{
		xr_string FileName;
	};

	DragDropData Data;
public:
	CContentView();
	virtual void Draw() override;

	bool DrawItem(const xr_string& FilePath, size_t& HorBtnIter, const size_t IterCount);

private:
	ref_texture& GetTexture(const xr_string& IconPath);

private:
	xr_string CurrentDir;
	xr_string RootDir;
	xr_string LogsDir;
	ImVec2 BtnSize = { 64, 64 };

	xr_hash_map<xr_string, ref_texture> Icons;
};