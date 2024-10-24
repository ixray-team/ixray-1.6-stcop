//---------------------------------------------------------------------------
#ifndef LogFormH
#define LogFormH
//---------------------------------------------------------------------------

class UILogForm 
{
public:
	static void AddMessage( const xr_string& msg);
	static void Show();
	static void SetActive();
	static void Hide();
	static void Update();
	static void Destroy();
	
private:
	static xr_vector<xr_string>*List;
	static xr_vector<xr_string>* GetList();
	static xr_vector<bool*> SelectedItems;
	static bool bAutoScroll;
	static string_path m_Filter;
	static string_path m_Exec;
};
//---------------------------------------------------------------------------
#endif
