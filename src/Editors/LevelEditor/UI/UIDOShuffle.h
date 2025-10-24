#pragma once
class EDetailManager;
class EImageThumbnail;
class UIDOOneColor;
class UIDOShuffle :
	public IEditorWnd
{
	friend UIDOOneColor;
public:
	UIDOShuffle();
	virtual ~UIDOShuffle();
	virtual void Draw();

public:
	static bool GetResult();
	static void Update();
	static void Show(EDetailManager* DM);
	static void LoadFromStream(xr_string& fname);

private:
	static UIDOShuffle* Form;

private:
	EDetailManager* DM;

	UIPropertiesForm* m_Props;
	xr_vector<UIDOOneColor*> m_color_indices;

	EImageThumbnail* m_Thm;
	ref_texture m_TextureNull;
	ref_texture m_Texture;
	ref_texture m_RealTexture;
	ref_texture m_MaskTexture;

	U8Vec Pixels;
	xr_vector<xr_string> m_list;
	int m_list_selected;
	bool bModif;
	bool m_ChooseObject;

private:
	void FillData(bool ReloadTex);
	void OnItemFocused(const char* name);
	bool FindItem(const char* name);

	void ClearIndexForms();
	bool ApplyChanges(bool msg=true);
};