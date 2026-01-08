#include "xrCore.h"
#define SCRIPTS_API
#include "../../xrEngine/_d3d_extensions.h"
#include "../xrLC/vbm.h"
#include "FormatParsers/LevelCForm/CFormIO.h"
#include "FormatParsers/LevelGeom/GeomIO.h"

SWIContainer g_SWI, x_SWI;
VBContainer g_VB, x_VB;
IBContainer g_IB, x_IB;

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

		if (CFormConverter::GetConverterSettings().CForm)
		{
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

		if (CFormConverter::GetConverterSettings().Geom)
		{
			auto func = [&](xr_string_view Ext)
			{
				auto Geom = XRay::Geom::Read(prjName, Ext);

				if (!I_ASSERT(Geom))
				{
					return;
				}
				
				xr_vector<u8> VB, IB, SWI;

				auto read_func = [&](xr_vector<u8>& Buff, IReaderBase& Data)
				{
					Buff.resize(Data.length());
					Data.r(Buff.data(), Buff.size());
				};
				read_func(VB, Geom->GetVBData());
				read_func(IB, Geom->GetIBData());
				read_func(SWI, Geom->GetSWIData());

				Geom.reset();
				
				xr_unique_ptr<XRay::Geom::IFormat> FormatPtr = nullptr;
				switch (CFormConverter::GetConverterSettings().LC_GeomType)
				{
				case GeomVanillaType::Vanilla:
					{
						FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
						break;
					}
				case GeomVanillaType::Chunked:
					{
						size_t mem_bytes = VB.size() + IB.size() + SWI.size();
						u32 Number = (mem_bytes/(1024ull*1024ull))/CFormConverter::GetConverterSettings().LC_GeomChunkSize;
						if (!Number)
						{
							FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
						} else
						{
							FormatPtr.reset(new XRay::Geom::CGeomVanillaChunkedFormat(Number+1));
						}
						break;
					}
				default:
					{
						FATAL("Invalid Geom type!");
					}
				}
				IVERIFY(FormatPtr);

				FormatPtr->AddVBData(VB);
				FormatPtr->AddIBData(IB);
				FormatPtr->AddSWIData(SWI);

				Write(prjName, Ext, *FormatPtr);
			};
			func(".geom");
			func(".geomx");
		}
	}
}