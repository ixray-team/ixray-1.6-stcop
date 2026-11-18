#include "stdafx.h"
#include "UITerrainTool.h"
#include "IconsFontAwesome7.h"
#include "../../Editor/Tools/Terrain/ESceneTerrainTools.h"

namespace
{
	const u32 g_TerrainRes[] = {65, 129, 257, 513, 1025, 2049};
	const char* g_TerrainResNames[] = {"65 x 65", "129 x 129", "257 x 257", "513 x 513", "1025 x 1025", "2049 x 2049"};
	constexpr int g_TerrainResCount = int(sizeof(g_TerrainRes) / sizeof(g_TerrainRes[0]));

	const char* g_ModeIcons[ESceneTerrainTool::bmCount] =
	{
		ICON_FA_ARROW_UP,
		ICON_FA_ARROW_DOWN,
		ICON_FA_WATER,
		ICON_FA_RULER_VERTICAL,
		ICON_FA_DICE,
		ICON_FA_ERASER,
		ICON_FA_PALETTE,
	};

	const char* g_ModeHints[ESceneTerrainTool::bmCount] =
	{
		"Raise terrain.\nCtrl+LMB: lower",
		"Lower terrain.\nCtrl+LMB: raise",
		"Smooth terrain",
		"Flatten to the target height.\nAlt+LMB: pick target height",
		"Add fractal noise.\nCtrl+LMB: subtract noise",
		"Cut holes.\nCtrl+LMB: restore holes to the target height",
		"Paint the selected material layer into the terrain mask and / or the surface (color map).\nCtrl+LMB: erase painting (back to rules / roads)",
	};

	const u32 g_BakeSizes[] = {512, 1024, 2048, 4096};
	const char* g_BakeSizeNames[] = {"512", "1024", "2048", "4096"};

	const char* g_PreviewNames[] = {"Height", "Surface Texture", "Material Mask"};
	const char* g_LayerChannel[] = {"R", "G", "B", "A"};

	void Hint(const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
		{
			ImGui::SetTooltip("%s", Text);
		}
	}
}

UITerrainTool::UITerrainTool()
{
	m_CreateRes = 1;
	m_CreateHeight = 0.5f;
	m_FillHeight = 0.5f;
	m_SmoothPasses = 1;
	m_ResizeRes = 1;
	m_BakeSize = 1;
}

UITerrainTool::~UITerrainTool()
{
}

void UITerrainTool::Draw()
{
	const float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;
	CTerrain* Active = tool->GetActiveTerrain();

	if (XRay::ImGui::BeginDarkChild("TerrainToolsBorder", {0, 0}, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		// Create
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Create Heightmap"))
		{
			ImGui::SetNextItemWidth(-0.01f);
			ImGui::Combo("##Resolution", &m_CreateRes, g_TerrainResNames, g_TerrainResCount);
			Hint("Heightmap resolution (vertices per side)");

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##BaseHeight", &m_CreateHeight, 0.01f, 1.f, "Base Height: %.2f");

			if (XRay::ImGui::Button(ICON_FA_MOUNTAIN " Create", {-0.01f, 0}))
			{
				const u32 Res = g_TerrainRes[m_CreateRes];
				tool->CreateTerrain("terrain", Res, Res, m_CreateHeight);
			}

			XRay::ImGui::EndExpand();
		}

		// Sculpt
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Sculpt"))
		{
			const bool InSculpt = (LTools && LTools->GetSubTarget() == ESceneTerrainTool::estTerrainSculpt);

			bool SculptState = InSculpt;
			if (XRay::ImGui::ToggleButton(InSculpt ? ICON_FA_PAINTBRUSH " Sculpt Mode: ON" : ICON_FA_PAINTBRUSH " Sculpt Mode: OFF", &SculptState, {-0.01f, 0}))
			{
				if (SculptState)
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_TERRAIN, ESceneTerrainTool::estTerrainSculpt);
					ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
				}
				else
				{
					ExecCommand(COMMAND_CHANGE_TARGET, OBJCLASS_TERRAIN);
				}
			}

			XRay::ImGui::Separator();

			// Brush modes: rows of four
			const float ColW = (ImGui::GetContentRegionAvail().x - ItemSpacingX * 3) / 4;
			for (int i = 0; i < ESceneTerrainTool::bmCount; ++i)
			{
				const auto Mode = ESceneTerrainTool::ETerrainBrushMode(i);
				bool Selected = (tool->m_BrushMode == Mode);
				xr_string Label = xr_string(g_ModeIcons[i]) + " " + ESceneTerrainTool::BrushModeName(Mode);
				if (XRay::ImGui::ToggleButton(Label.c_str(), &Selected, {ColW, 0}))
				{
					tool->m_BrushMode = Mode;
				}
				Hint(g_ModeHints[i]);
				if (i % 4 != 3 && i + 1 < ESceneTerrainTool::bmCount)
				{
					ImGui::SameLine(0, ItemSpacingX);
				}
			}

			// Material layer for the Paint brush
			if (tool->m_BrushMode == ESceneTerrainTool::bmPaint)
			{
				ImGui::Spacing();
				ImGui::TextDisabled("Paint material:");
				if (!Active)
				{
					ImGui::TextDisabled("Select a terrain object");
				}
				else
				{
					auto& Mats = Active->GetMask().Materials;
					if (Mats.empty())
					{
						Active->EnsureMask();
					}
					const float MatW = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
					for (u32 i = 0; i < u32(Mats.size()); ++i)
					{
						auto& M = Mats[i];
						M.EnsureDetails();
						const u32 Ch = std::min(M.Channel, XRay::Editor::TerrainMask::CHANNEL_COUNT - 1);
						const u32 Avg = M.Details[Ch].AvgColor;

						xr_string Label = xr_string(g_LayerChannel[Ch]) + ": " + (M.Name.size() ? *M.Name : "?");
						ImGui::PushID(int(i));
						bool Sel = (tool->m_PaintLayer == i);
						ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(color_get_R(Avg) / 255.f, color_get_G(Avg) / 255.f, color_get_B(Avg) / 255.f, 1.f));
						ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 2.f);
						if (XRay::ImGui::ToggleButton(Label.c_str(), &Sel, {MatW, 0}))
						{
							tool->m_PaintLayer = i;
						}
						ImGui::PopStyleVar();
						ImGui::PopStyleColor();
						xr_string Tip = xr_string("Shader: ") + (M.Shader.size() ? *M.Shader : "-") + "\nGame Mtl: " + (M.GameMtl.size() ? *M.GameMtl : "-") + "\nRendered detail: " + (M.Details[Ch].Texture.size() ? *M.Details[Ch].Texture : "?");
						Hint(Tip.c_str());
						ImGui::PopID();
						if (i % 2 == 0 && i + 1 < u32(Mats.size()))
						{
							ImGui::SameLine(0, ItemSpacingX);
						}
					}
				}
				ImGui::TextDisabled("Materials: Properties > Material Mask > Materials");
				ImGui::Checkbox("Mask", &tool->m_PaintMask);
				Hint("Assign the material to the terrain (shader + game material + mask channel)");
				ImGui::SameLine();
				ImGui::Checkbox("Surface", &tool->m_PaintSurface);
				Hint("Paint the surface (color map) as well");

				if (tool->m_PaintSurface)
				{
					static const char* SourceNames[] = {"Material detail", "Solid color"};
					ImGui::SetNextItemWidth(-0.01f);
					ImGui::Combo("##SurfaceSource", &tool->m_SurfaceSource, SourceNames, 2);
					Hint("What goes into the surface:\nLayer texture - the paint layer texture tiled in world space\nSolid color - the color below");
					if (tool->m_SurfaceSource == 1)
					{
						ImGui::SetNextItemWidth(-0.01f);
						ImGui::ColorEdit3("##SurfaceColor", (float*)&tool->m_SurfaceColor);
					}
				}

				ImGui::TextDisabled("Layer textures: Properties > Material Mask");
			}

			ImGui::Spacing();

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##Radius", &tool->m_BrushSize, 0.5f, 500.f, "Radius: %.1f m", ImGuiSliderFlags_Logarithmic);
			Hint("Mouse wheel");

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##Strength", &tool->m_BrushStrength, 0.0005f, 0.2f, "Strength: %.4f", ImGuiSliderFlags_Logarithmic);
			Hint("Ctrl + mouse wheel");

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##Hardness", &tool->m_BrushHardness, 0.f, 1.f, "Hardness: %.2f");
			Hint("Alt + mouse wheel. 0 = soft falloff, 1 = hard edge");

			ImGui::SetNextItemWidth(-0.01f);
			ImGui::SliderFloat("##Spacing", &tool->m_BrushSpacing, 0.05f, 1.f, "Spacing: %.2f");
			Hint("Distance between brush stamps along the stroke, fraction of the radius");

			if (tool->m_BrushMode == ESceneTerrainTool::bmNoise)
			{
				ImGui::SetNextItemWidth(-0.01f);
				ImGui::SliderFloat("##NoiseScale", &tool->m_NoiseScale, 1.f, 256.f, "Noise Size: %.1f m", ImGuiSliderFlags_Logarithmic);
			}

			if (tool->m_BrushMode == ESceneTerrainTool::bmFlatten || tool->m_BrushMode == ESceneTerrainTool::bmHole)
			{
				ImGui::SetNextItemWidth(-0.01f);
				ImGui::SliderFloat("##Target", &tool->m_FlattenTarget, 0.001f, 1.f, "Target Height: %.3f");
				Hint("Alt+LMB on the terrain: pick height under the cursor");
				ImGui::Checkbox("Pick target at stroke start", &tool->m_FlattenPickOnStroke);
			}

			ImGui::Checkbox("Invert brush (Ctrl)", &tool->m_BrushInvert);

			if (InSculpt)
			{
				ImGui::Spacing();
				ImGui::TextDisabled("LMB paint, Ctrl invert, Alt pick height");
				ImGui::TextDisabled("Wheel radius, Ctrl strength, Alt hardness");
				if (CTerrain* Hover = tool->GetHoverTerrain(); Hover && tool->m_BrushActive)
				{
					Fvector Local;
					Hover->_ITransform().transform_tiny(Local, tool->m_BrushPos);
					float Gx, Gz;
					Hover->GetHeightmap().LocalToGrid(Local, Gx, Gz);
					ImGui::TextDisabled("Cursor: [%d, %d] h=%.3f  y=%.2f m", iFloor(Gx + 0.5f), iFloor(Gz + 0.5f), Hover->GetHeightmap().Sample(Gx, Gz), tool->m_BrushPos.y);
				}
			}

			XRay::ImGui::EndExpand();
		}

		// Whole-map operations
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Heightmap"))
		{
			if (!Active)
			{
				ImGui::TextDisabled("Select a terrain object");
			}
			else
			{
				auto& H = Active->GetHeightmap();
				ImGui::Text("%s: %u x %u", Active->GetName(), H.Width, H.Height);
				ImGui::TextDisabled("Height range: %.3f .. %.3f", H.MinH, H.MaxH);

				const float HalfW = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

				int PreviewMode = int(Active->GetPreviewMode());
				ImGui::SetNextItemWidth(HalfW);
				if (ImGui::Combo("##PreviewMode", &PreviewMode, g_PreviewNames, 3))
				{
					Active->SetPreviewMode(u32(PreviewMode));
				}
				Hint("Heightmap preview coloring:\nHeight - height tint\nSurface Texture - the terrain color map\nMaterial Mask - mask layers (rules + painting + roads)");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Reload Textures", {-0.01f, 0}))
				{
					Active->ReloadPreviewTexture();
					for (auto& M : Active->GetMask().Materials)
					{
						M.ReloadDetails();
					}
					Active->MarkMaskDirty();
				}
				Hint("Re-read the textures after re-importing them");

				ImGui::SetNextItemWidth(HalfW);
				ImGui::SliderFloat("##FillHeight", &m_FillHeight, 0.001f, 1.f, "%.3f");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Fill", {-0.01f, 0}))
				{
					tool->OpFill(Active, m_FillHeight);
				}
				Hint("Set every non-hole sample to this height");

				ImGui::SetNextItemWidth(HalfW);
				ImGui::SliderInt("##SmoothPasses", &m_SmoothPasses, 1, 32, "Passes: %d");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Smooth All", {-0.01f, 0}))
				{
					tool->OpSmooth(Active, m_SmoothPasses);
				}

				if (XRay::ImGui::Button("Normalize", {HalfW, 0}))
				{
					tool->OpNormalize(Active);
				}
				Hint("Stretch the height range to [0, 1]");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Invert", {-0.01f, 0}))
				{
					tool->OpInvert(Active);
				}

				if (XRay::ImGui::Button("Fill Holes", {HalfW, 0}))
				{
					tool->OpFillHoles(Active);
				}
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button(ICON_FA_MOUNTAIN " Update Mesh", {-0.01f, 0}))
				{
					tool->OpApplyMesh(Active);
				}
				Hint("Rebuild the scene mesh from the heightmap now");

				ImGui::SetNextItemWidth(HalfW);
				ImGui::Combo("##ResizeRes", &m_ResizeRes, g_TerrainResNames, g_TerrainResCount);
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Resample", {-0.01f, 0}))
				{
					tool->OpResize(Active, g_TerrainRes[m_ResizeRes]);
				}
				Hint("Change resolution keeping the shape (bilinear)");

				XRay::ImGui::Separator();

				if (XRay::ImGui::Button(ICON_FA_FILE_ARROW_DOWN " Import R16", {HalfW, 0}))
				{
					xr_string FileName;
					if (EFS.GetOpenName("$server_data_root$", FileName, false, nullptr, -1, "*.r16"))
					{
						tool->OpImportR16(Active, FileName.c_str());
					}
				}
				Hint("Square 16-bit RAW heightmap");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button(ICON_FA_FILE_ARROW_UP " Export R16", {-0.01f, 0}))
				{
					xr_string FileName = Active->GetName();
					if (EFS.GetSaveName("$server_data_root$", FileName, nullptr, -1, "*.r16"))
					{
						tool->OpExportR16(Active, FileName.c_str());
					}
				}
			}

			XRay::ImGui::EndExpand();
		}

		// Material mask: rules + painting + roads -> <texture>.tga and <texture>_mask.tga
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Material Mask"))
		{
			if (!Active)
			{
				ImGui::TextDisabled("Select a terrain object");
			}
			else
			{
				const float HalfW = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

				if (XRay::ImGui::Button("Show Mask", {HalfW, 0}))
				{
					Active->SetPreviewMode(XRay::Editor::HeightmapUtils::SHeightMap::pmColors);
				}
				Hint("Preview the mask on the heightmap");
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button("Clear Mask Paint", {-0.01f, 0}))
				{
					tool->OpClearPaint(Active);
				}
				Hint("Remove hand painting from the mask, keep rules and roads");

				if (XRay::ImGui::Button("Clear Surface Paint", {-0.01f, 0}))
				{
					tool->OpClearSurfacePaint(Active);
				}
				Hint("Remove hand painting from the surface (color map)");

				ImGui::SetNextItemWidth(HalfW);
				ImGui::Combo("##BakeSize", &m_BakeSize, g_BakeSizeNames, 4);
				ImGui::SameLine(0, ItemSpacingX);
				if (XRay::ImGui::Button(ICON_FA_FILL_DRIP " Bake & Apply", {-0.01f, 0}))
				{
					tool->OpBakeMask(Active, g_BakeSizes[m_BakeSize]);
					ExecCommand(COMMAND_UPDATE_PROPERTIES);
				}
				Hint("Bake the mask (rules + painting + roads) and the color map into the texture library,\nbuild the game textures and assign the color map to this terrain.\nThe terrain shader reads <texture>_mask by itself.");

				const xr_string Base = Active->MaskBaseName();
				ImGui::TextDisabled("%s + %s_mask", Base.c_str(), Base.c_str());
				XRay::ImGui::Separator();
				if (XRay::ImGui::Button("Check Seams", {-0.01f, 0}))
				{
					Active->EnsureMask();
					Active->GetMask().CollectSeams(m_Seams);
					m_SeamsChecked = true;
				}
				Hint("Touching materials must share transition details:\nA.shader[B.channel] == B.shader[B.channel] and vice versa.\nIncompatible borders are red in the Material Mask preview.");
				if (m_SeamsChecked)
				{
					auto& Mats = Active->GetMask().Materials;
					if (m_Seams.empty())
					{
						ImGui::TextDisabled("No seams");
					}
					for (const auto& Seam : m_Seams)
					{
						if (Seam.A >= Mats.size() || Seam.B >= Mats.size())
						{
							continue;
						}
						ImGui::TextColored(ImVec4(1.f, 0.45f, 0.4f, 1.f), "%s - %s: %u edges", *Mats[Seam.A].Name, *Mats[Seam.B].Name, Seam.Count);
						ImGui::TextWrapped("  %s", Seam.Why.c_str());
					}
				}
				ImGui::TextDisabled("Rules and materials: Properties > Material Mask");
				ImGui::TextDisabled("Road materials: road Properties > Terrain Mask");
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar();
		XRay::ImGui::EndDarkChild();
	}
}
