#include "xrCore.h"
#define SCRIPTS_API
#include "FormatParsers/LevelCForm/CFormIO.h"

static const char* h_str =
"The following keys are supported / required:\n"
"-? or -h   == this help\n"
"-f<NAME>   == compile level in gamedata/levels/<NAME>/\n"
"-o         == modify build options\n"
"-s         == build game spawn data\n"
"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char* h_str);

//string_path INI_FILE;

//extern LPCSTR LEVEL_GRAPH_NAME;

//extern LPCSTR GAME_CONFIG;

extern void clear_temp_folder();
extern void	xrCompiler(LPCSTR name, bool draft_mode, bool pure_covers, LPCSTR out_name);
extern void	verify_level_graph(LPCSTR name, bool verbose);

#include "ConverterUI.h"

void StartupConv()
{
	SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
	
	// Load project
	//FS.update_path(INI_FILE, "$game_config$", GAME_CONFIG);

	for (const auto& [Name, Selected] : CFormConverter::GetConverterSettings().Files)
	{
		if (!Selected)
			continue;

		string4096 name;
		strcpy(name, Name.data());

		if (xr_strlen(name))
			xr_strcat(name, "\\");

		//string_path prjName;
		//prjName[0] = 0;
		xr_stack_string_path prjName;
		//bool can_use_name = false;

		if (xr_strlen(name) < sizeof(string_path))
		{
			//can_use_name = true;
			FS.update_path(prjName, "$game_levels$", name);
		}

		prjName.append("level");

		auto CForm = XRay::CForm::Read(prjName.c_str());

		switch (CFormConverter::GetConverterSettings().LC_CformType)
		{
		case CFormVersions::Vanilla:
			{
				xr_vector<Fvector> Verts;
				xr_vector<CDB::TRI> Tris;
				CForm->GetStaticGeom(Verts, Tris);
				
				XRay::CForm::CFormatVanilla TargetCForm;
				TargetCForm.AddStaticGeom(Verts, Tris);
				XRay::CForm::Write(prjName.c_str(), TargetCForm);
				break;
			}
		case CFormVersions::VanillaChunked:
			{
				xr_vector<Fvector> Verts;
				xr_vector<CDB::TRI> Tris;
				CForm->GetStaticGeom(Verts, Tris);

				size_t mem_bytes = Tris.size()*sizeof(CDB::TRI) + Verts.size()*sizeof(Fvector);
				u32 Number = (mem_bytes / (1024ull*1024ull)) / CFormConverter::GetConverterSettings().LC_CFormChunkSize;
				if (!Number)
				{
					XRay::CForm::CFormatVanilla TargetCForm;
					TargetCForm.AddStaticGeom(Verts, Tris);
					XRay::CForm::Write(prjName.c_str(), TargetCForm);
				} else
				{
					XRay::CForm::CFormatVanillaChunked TargetCForm(Number+1);
					TargetCForm.AddStaticGeom(Verts, Tris);
					XRay::CForm::Write(prjName.c_str(), TargetCForm);
				}
				break;
			}
		default:
			{
				FATAL("Invalid target CForm type!");
			}
		}
	}
}