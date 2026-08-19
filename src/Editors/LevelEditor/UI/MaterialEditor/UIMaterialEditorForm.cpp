#include "stdafx.h"
#include "UIMaterialEditorForm.h"

#include "../../../xrEUI/ImNodeEditor/imnodes.h"

#include <algorithm>
#include <cctype>
#include <ranges>

namespace
{
using Tiramisu::Editor::FMaterialDependencyChange;
using Tiramisu::Editor::FMaterialEditorOperationResult;

xr_string Lower(xr_string_view Value)
{
	xr_string Result(Value);
	std::ranges::transform(Result, Result.begin(), [](const unsigned char Character)
						   { return static_cast<char>(std::tolower(Character)); });
	return Result;
}

std::filesystem::path NormalizeEditorPath(const std::filesystem::path& Path)
{
	if (Path.empty())
	{
		return {};
	}
	std::error_code Error;
	std::filesystem::path Result = std::filesystem::weakly_canonical(Path, Error);
	if (Error)
	{
		Error.clear();
		Result = std::filesystem::absolute(Path, Error);
	}
	return (Error ? Path : Result).lexically_normal();
}

bool SameEditorPath(const std::filesystem::path& Left, const std::filesystem::path& Right)
{
	if (Left.empty() || Right.empty())
	{
		return false;
	}
#if defined(_WIN32)
	return Lower(NormalizeEditorPath(Left).generic_string()) ==
		   Lower(NormalizeEditorPath(Right).generic_string());
#else
	return NormalizeEditorPath(Left) == NormalizeEditorPath(Right);
#endif
}

bool IsMaterialAssetPath(const std::filesystem::path& Path)
{
	const xr_string Name = Lower(Path.filename().string());
	return Name.ends_with(".material.json") ||
		   Name.ends_with(".material-instance.json");
}

template <size_t Size>
void SetTextBuffer(xr_array<char, Size>& Buffer, const xr_string_view Value)
{
	Buffer.fill('\0');
	const size_t Count = std::min(Value.size(), Buffer.size() - 1);
	std::ranges::copy_n(Value.begin(), Count, Buffer.begin());
}

bool HasMaterialExtension(const xr_string_view Path)
{
	return Lower(Path).ends_with(".material.json");
}

bool IsRecoveryNewer(const std::filesystem::path& RecoveryPath, const std::filesystem::path& OriginalPath)
{
	std::error_code Error;
	if (!std::filesystem::exists(RecoveryPath, Error) || Error)
	{
		return false;
	}
	const auto RecoveryTime = std::filesystem::last_write_time(RecoveryPath, Error);
	if (Error)
	{
		return false;
	}
	const auto OriginalTime = std::filesystem::last_write_time(OriginalPath, Error);
	return !Error && RecoveryTime >= OriginalTime;
}

FMaterialGraphCompileResult BuildMaterialImplementation(
	const FMaterialAsset& Material,
	FMaterialStaticParameterSet StaticParameters = {}
)
{
	if (Material.Implementation.Type == EMaterialImplementationType::Graph)
	{
		FMaterialGraphCompileOptions Options;
		Options.Parameters = Material.Parameters;
		Options.StaticParameters = std::move(StaticParameters);
		for (const FMaterialParameterDefinition& Parameter : Material.Parameters)
		{
			if (Parameter.IsStatic() &&
				!Options.StaticParameters.contains(Parameter.Id))
			{
				Options.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
			}
		}
		return CompileMaterialGraph(Material.Implementation.Graph, Options);
	}

	FMaterialGraphCompileResult Result;
	if (Material.Implementation.Source.empty())
	{
		Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, "editor.hlsl_source_missing", "HLSL material implementation source is empty.", {}, {}});
		return Result;
	}

	xr_string RelativePath = Material.Implementation.Source;
	std::ranges::replace(RelativePath, '/', '\\');
	if (!Lower(RelativePath).starts_with("r5\\"))
	{
		RelativePath = "r5\\" + RelativePath;
	}
	IReader* Reader = FS.r_open("$game_shaders$", RelativePath.c_str());
	if (!Reader)
	{
		Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, "editor.hlsl_source_open_failed", "Cannot open HLSL implementation '" + RelativePath + "'.", {}, {}});
		return Result;
	}
	Result.GeneratedHlsl.assign(static_cast<const char*>(Reader->pointer()), static_cast<size_t>(Reader->length()));
	FS.r_close(Reader);
	return Result;
}

FMaterialValue DefaultParameterValue(const EMaterialParameterType Type)
{
	switch (Type)
	{
		case EMaterialParameterType::Float2:
			return FFloat2{0.0f, 0.0f};
		case EMaterialParameterType::Float3:
			return FFloat3{0.0f, 0.0f, 0.0f};
		case EMaterialParameterType::Float4:
		case EMaterialParameterType::Color:
			return FFloat4{0.0f, 0.0f, 0.0f, 1.0f};
		case EMaterialParameterType::Texture2D:
		case EMaterialParameterType::TextureCube:
			return xr_string{};
		case EMaterialParameterType::SamplerPreset:
			return xr_string{"linear_wrap"};
		case EMaterialParameterType::StaticBool:
			return false;
		case EMaterialParameterType::StaticEnum:
			return s32{0};
		default:
			return 0.0f;
	}
}

ImU32 PinColor(const EMaterialValueType Type)
{
	switch (Type)
	{
		case EMaterialValueType::Float1:
			return IM_COL32(90, 190, 110, 255);
		case EMaterialValueType::Float2:
			return IM_COL32(90, 165, 210, 255);
		case EMaterialValueType::Float3:
			return IM_COL32(100, 125, 230, 255);
		case EMaterialValueType::Float4:
			return IM_COL32(175, 105, 230, 255);
		case EMaterialValueType::Bool:
			return IM_COL32(210, 85, 85, 255);
		case EMaterialValueType::Texture2D:
		case EMaterialValueType::TextureCube:
			return IM_COL32(220, 155, 70, 255);
		default:
			return IM_COL32(160, 160, 160, 255);
	}
}

bool DrawMaterialFloatTypeCombo(
	const char* Label,
	EMaterialValueType& Type
)
{
	bool Changed = false;
	const xr_string_view Preview = ToString(Type);
	if (ImGui::BeginCombo(Label, Preview.data()))
	{
		constexpr xr_array Types = {
			EMaterialValueType::Float1,
			EMaterialValueType::Float2,
			EMaterialValueType::Float3,
			EMaterialValueType::Float4,
		};
		for (const EMaterialValueType Candidate : Types)
		{
			const bool Selected = Type == Candidate;
			const xr_string_view Name = ToString(Candidate);
			if (ImGui::Selectable(Name.data(), Selected))
			{
				Type = Candidate;
				Changed = true;
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}
	return Changed;
}

ImNodesPinShape PinShape(const EMaterialValueType Type)
{
	if (Type == EMaterialValueType::Texture2D || Type == EMaterialValueType::TextureCube)
	{
		return ImNodesPinShape_QuadFilled;
	}
	if (Type == EMaterialValueType::Bool)
	{
		return ImNodesPinShape_TriangleFilled;
	}
	return ImNodesPinShape_CircleFilled;
}

const char* SeverityName(const EMaterialDiagnosticSeverity Severity)
{
	switch (Severity)
	{
		case EMaterialDiagnosticSeverity::Info:
			return "Info";
		case EMaterialDiagnosticSeverity::Warning:
			return "Warning";
		default:
			return "Error";
	}
}
} // namespace

UIMaterialEditorForm::UIMaterialEditorForm()
{
	ImNodes::SetImGuiContext(ImGui::GetCurrentContext());
	NodesContext = ImNodes::CreateContext();
	SyncMaterialDrafts();
	Compile();
	NextAutosaveTime = ImGui::GetTime() + 30.0;
	NextDependencyPollTime = ImGui::GetTime() + 0.5;
	bOpen = false;
}

UIMaterialEditorForm::~UIMaterialEditorForm()
{
	ReleasePreview();
	if (NodesContext)
	{
		ImNodes::SetCurrentContext(NodesContext);
		ImNodes::DestroyContext(NodesContext);
		ImNodes::SetCurrentContext(nullptr);
		NodesContext = nullptr;
	}
}

void UIMaterialEditorForm::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImNodes::SetCurrentContext(NodesContext);
	ImGui::SetNextWindowSize(ImVec2(1280.0f, 760.0f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("Material Editor", &bOpen, ImGuiWindowFlags_MenuBar))
	{
		ImGui::End();
		return;
	}

	DrawToolbar();
	TickAutosave();
	TickDependencyWatcher();

	const float OutputWidth = 360.0f;
	ImGui::BeginChild("##MaterialPalette", ImVec2(230.0f, 0.0f), true);
	DrawPalette();
	ImGui::EndChild();

	ImGui::SameLine();
	const float GraphWidth = std::max(320.0f, ImGui::GetContentRegionAvail().x - OutputWidth - ImGui::GetStyle().ItemSpacing.x);
	ImGui::BeginChild("##MaterialGraph", ImVec2(GraphWidth, 0.0f), true, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);
	DrawGraph();
	ImGui::EndChild();

	ImGui::SameLine();
	ImGui::BeginChild("##MaterialOutput", ImVec2(0.0f, 0.0f), true);
	DrawOutputPanel();
	ImGui::EndChild();

	ImGui::End();
}

void UIMaterialEditorForm::DrawToolbar()
{
	if (ImGui::BeginMenuBar())
	{
		ImGui::BeginDisabled(Document.IsDirty());
		if (ImGui::MenuItem("New"))
		{
			Document.NewMaterial();
			ResetPresentationState();
			Compile();
			AutosaveStatus.clear();
			NextAutosaveTime = ImGui::GetTime() + 30.0;
		}
		if (ImGui::MenuItem("Open...", "Ctrl+O"))
		{
			OpenMaterial();
		}
		if (ImGui::MenuItem("Recover Autosave..."))
		{
			OpenAutosave();
		}
		ImGui::EndDisabled();

		if (ImGui::MenuItem("Save", "Ctrl+S"))
		{
			SaveMaterial(false);
		}
		if (ImGui::MenuItem("Save As...", "Ctrl+Shift+S"))
		{
			SaveMaterial(true);
		}

		ImGui::Separator();
		if (ImGui::MenuItem("Copy", "Ctrl+C"))
		{
			CopySelection();
		}
		if (ImGui::MenuItem("Paste", "Ctrl+V"))
		{
			PasteClipboard();
		}

		ImGui::BeginDisabled(!Document.CanUndo());
		if (ImGui::MenuItem("Undo", "Ctrl+Z"))
		{
			Document.Undo();
			ResetPresentationState();
			Compile();
		}
		ImGui::EndDisabled();

		ImGui::BeginDisabled(!Document.CanRedo());
		if (ImGui::MenuItem("Redo", "Ctrl+Y"))
		{
			Document.Redo();
			ResetPresentationState();
			Compile();
		}
		ImGui::EndDisabled();

		if (ImGui::MenuItem("Compile", "F7"))
		{
			Compile();
		}

		if (ImGui::MenuItem("Auto Reload Dependencies", nullptr, &AutoReloadDependencies) && AutoReloadDependencies)
		{
			RefreshDependencyWatch();
		}
		if (!PendingDependencyChanges.empty())
		{
			if (ImGui::MenuItem("Reload External Changes (discard local edits)"))
			{
				ApplyDependencyChanges(std::move(PendingDependencyChanges), true);
			}
			if (ImGui::MenuItem("Keep Local Edits"))
			{
				PendingDependencyChanges.clear();
				DependencyStatus = "External changes acknowledged; local edits kept";
				RefreshDependencyWatch();
			}
		}

		ImGui::Separator();
		ImGui::TextUnformatted(Document.IsDirty() ? "Modified" : "Saved");
		if (!AutosaveStatus.empty())
		{
			ImGui::SameLine();
			ImGui::TextDisabled("| %s", AutosaveStatus.c_str());
		}
		if (!DependencyStatus.empty())
		{
			ImGui::SameLine();
			ImGui::TextDisabled("| %s", DependencyStatus.c_str());
		}
		ImGui::EndMenuBar();
	}

	if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_Z, false) && Document.Undo())
	{
		ResetPresentationState();
		Compile();
	}
	if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_Y, false) && Document.Redo())
	{
		ResetPresentationState();
		Compile();
	}
	if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_S, false))
	{
		SaveMaterial(ImGui::GetIO().KeyShift);
	}
	if (ImGui::GetIO().KeyCtrl && !ImGui::GetIO().WantTextInput &&
		ImGui::IsKeyPressed(ImGuiKey_C, false))
	{
		CopySelection();
	}
	if (ImGui::GetIO().KeyCtrl && !ImGui::GetIO().WantTextInput &&
		ImGui::IsKeyPressed(ImGuiKey_V, false))
	{
		PasteClipboard();
	}
	if (ImGui::GetIO().KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_O, false) &&
		!Document.IsDirty())
	{
		OpenMaterial();
	}
	if (ImGui::IsKeyPressed(ImGuiKey_F7, false))
	{
		Compile();
	}
}

void UIMaterialEditorForm::TickAutosave()
{
	const double Now = ImGui::GetTime();
	if (Now < NextAutosaveTime)
	{
		return;
	}
	NextAutosaveTime = Now + 30.0;

	bool SavedAny = false;
	bool Failed = false;
	if (Document.IsDirty())
	{
		const std::filesystem::path RecoveryPath = MaterialRecoveryPath();
		std::error_code Error;
		if (!RecoveryPath.parent_path().empty())
		{
			std::filesystem::create_directories(RecoveryPath.parent_path(), Error);
		}
		if (Error)
		{
			AutosaveStatus = "Material autosave directory failed";
			Failed = true;
		}
		else
		{
			FMaterialEditorOperationResult Result =
				Document.SaveRecoveryFile(RecoveryPath);
			if (!Result.Succeeded())
			{
				AutosaveStatus = "Material autosave failed";
				Failed = true;
				SetDiagnostics(std::move(Result.Diagnostics));
			}
			else
			{
				SavedAny = true;
			}
		}
	}

	if (SavedAny && !Failed)
	{
		AutosaveStatus = "Autosaved recovery";
	}
}

void UIMaterialEditorForm::RefreshDependencyWatch()
{
	xr_vector<std::filesystem::path> Dependencies;
	const auto Add = [&Dependencies](const std::filesystem::path& Path)
	{
		if (!Path.empty())
		{
			Dependencies.push_back(Path);
		}
	};

	Add(std::filesystem::path(Document.GetMaterial().SourcePath.c_str()));
	const FMaterialAsset& ActiveMaterial = Document.GetMaterial();
	string_path ShaderRoot{};
	FS.update_path(ShaderRoot, "$game_shaders$", "r5\\");
	const auto AddShaderDependency = [&Add, &ShaderRoot](xr_string Reference)
	{
		if (Reference.empty())
		{
			return;
		}
		std::ranges::replace(Reference, '\\', '/');
		if (Lower(Reference).starts_with("r5/"))
		{
			Reference.erase(0, 3);
		}
		const std::filesystem::path Path{Reference.c_str()};
		Add(Path.is_absolute() ? Path : std::filesystem::path{ShaderRoot} / Path);
	};
	AddShaderDependency(ActiveMaterial.HlslTemplate);
	if (ActiveMaterial.Implementation.Type == EMaterialImplementationType::Hlsl)
	{
		AddShaderDependency(ActiveMaterial.Implementation.Source);
	}
	for (const xr_string& Dependency : ActiveMaterial.Dependencies)
	{
		AddShaderDependency(Dependency);
	}

	DependencyWatcher.Reset(Dependencies);
	NextDependencyPollTime = ImGui::GetTime() + 0.5;
}

void UIMaterialEditorForm::TickDependencyWatcher()
{
	if (!AutoReloadDependencies || !PendingDependencyChanges.empty())
	{
		return;
	}
	const double Now = ImGui::GetTime();
	if (Now < NextDependencyPollTime)
	{
		return;
	}
	NextDependencyPollTime = Now + 0.5;

	xr_vector<FMaterialDependencyChange> Changes = DependencyWatcher.Poll();
	if (!Changes.empty())
	{
		ApplyDependencyChanges(std::move(Changes), false);
	}
}

void UIMaterialEditorForm::ApplyDependencyChanges(
	xr_vector<FMaterialDependencyChange> Changes,
	const bool AllowDirtyReload
)
{
	if (Changes.empty())
	{
		return;
	}

	const std::filesystem::path MaterialPath(
		Document.GetMaterial().SourcePath.c_str()
	);
	const bool MaterialChanged = std::ranges::any_of(
		Changes,
		[&MaterialPath](const FMaterialDependencyChange& Change)
		{
			return SameEditorPath(Change.Path, MaterialPath);
		}
	);

	if (!AllowDirtyReload && MaterialChanged && Document.IsDirty())
	{
		PendingDependencyChanges = std::move(Changes);
		DependencyStatus = "External changes pending (local edits are protected)";
		Diagnostics.push_back({EMaterialDiagnosticSeverity::Warning, "editor.external_change_conflict", "A source asset changed on disk while the editor document has local edits. "
																										"Choose whether to reload or keep the local version.",
							   {},
							   {}});
		return;
	}

	DependencyStatus.clear();
	bool ReloadedMaterial = false;
	bool ShaderDependencyChanged = false;
	for (const FMaterialDependencyChange& Change : Changes)
	{
		ShaderDependencyChanged |= !IsMaterialAssetPath(Change.Path);
	}

	if (MaterialChanged && !MaterialPath.empty())
	{
		FMaterialEditorOperationResult Result =
			Document.OpenMaterialFile(MaterialPath);
		if (!Result.Succeeded())
		{
			SetDiagnostics(std::move(Result.Diagnostics));
			DependencyStatus = "External master reload failed; last good document kept";
		}
		else
		{
			ResetPresentationState();
			ReloadedMaterial = true;
		}
	}
	if (ReloadedMaterial || ShaderDependencyChanged)
	{
		Compile();
	}

	PendingDependencyChanges.clear();
	if (DependencyStatus.empty())
	{
		DependencyStatus = "Reloaded " + std::to_string(Changes.size()) +
						   (Changes.size() == 1
								? " changed dependency"
								: " changed dependencies");
	}
	RefreshDependencyWatch();
}

void UIMaterialEditorForm::DrawPalette()
{
	ImGui::TextUnformatted("Node Palette");
	if (Document.GetMaterial().Implementation.Type != EMaterialImplementationType::Graph)
	{
		ImGui::Separator();
		ImGui::TextWrapped("This master uses a hand-written HLSL implementation. "
						   "Graph nodes are read-only until implementation conversion is supported.");
		return;
	}
	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##MaterialNodeSearch", "Search nodes...", Search.data(), Search.size());
	ImGui::Separator();

	const xr_string Filter = Lower(Search.data());
	xr_string_view LastCategory;
	for (const FMaterialNodeDefinition& Definition : GetMaterialNodeDefinitions())
	{
		if (Definition.Type == "material_output")
		{
			continue;
		}
		if (Definition.Type == "constant")
		{
			struct FConstantPaletteEntry
			{
				xr_string_view Label;
				xr_string_view SearchAliases;
				EMaterialValueType ValueType;
			};

			constexpr xr_array ConstantEntries = {
				FConstantPaletteEntry{
					"Constant (float)",
					"constant scalar float constant1",
					EMaterialValueType::Float1
				},
				FConstantPaletteEntry{
					"Constant2 (float2)",
					"constant2 vector2 float2",
					EMaterialValueType::Float2
				},
				FConstantPaletteEntry{
					"Constant3 (float3)",
					"constant3 vector3 float3 color rgb",
					EMaterialValueType::Float3
				},
				FConstantPaletteEntry{
					"Constant4 (float4)",
					"constant4 vector4 float4 color rgba",
					EMaterialValueType::Float4
				},
			};

			bool HasVisibleEntry = false;
			for (const FConstantPaletteEntry& Entry : ConstantEntries)
			{
				if (Filter.empty() || Lower(Entry.SearchAliases).find(Filter) != xr_string::npos)
				{
					HasVisibleEntry = true;
					break;
				}
			}
			if (!HasVisibleEntry)
			{
				continue;
			}

			if (LastCategory != Definition.Category)
			{
				if (!LastCategory.empty())
				{
					ImGui::Spacing();
				}
				ImGui::TextDisabled(
					"%.*s",
					static_cast<int>(Definition.Category.size()),
					Definition.Category.data()
				);
				LastCategory = Definition.Category;
			}

			for (const FConstantPaletteEntry& Entry : ConstantEntries)
			{
				if (!Filter.empty() &&
					Lower(Entry.SearchAliases).find(Filter) == xr_string::npos)
				{
					continue;
				}
				const xr_string Label = xr_string(Entry.Label) + "##constant_" +
					xr_string(ToString(Entry.ValueType));
				if (ImGui::Button(Label.c_str(), ImVec2(-1.0f, 0.0f)))
				{
					AddNode(Definition.Type, Entry.ValueType);
				}
			}
			continue;
		}
		if (Definition.Type == "make_vector" ||
			Definition.Type == "break_vector" ||
			Definition.Type == "swizzle")
		{
			struct FVectorPaletteEntry
			{
				xr_string Label;
				xr_string SearchAliases;
				EMaterialValueType ValueType;
			};

			const xr_string OperationName = Definition.Type == "make_vector"
				? "Make"
				: Definition.Type == "break_vector" ? "Break" : "Swizzle";
			const xr_array VectorEntries = {
				FVectorPaletteEntry{
					OperationName + " Float2",
					Lower(OperationName + " vector2 float2"),
					EMaterialValueType::Float2
				},
				FVectorPaletteEntry{
					OperationName + " Float3",
					Lower(OperationName + " vector3 float3 rgb"),
					EMaterialValueType::Float3
				},
				FVectorPaletteEntry{
					OperationName + " Float4",
					Lower(OperationName + " vector4 float4 rgba"),
					EMaterialValueType::Float4
				},
			};

			const bool HasVisibleEntry = std::ranges::any_of(
				VectorEntries,
				[&Filter](const FVectorPaletteEntry& Entry)
				{
					return Filter.empty() ||
						Entry.SearchAliases.find(Filter) != xr_string::npos;
				}
			);
			if (!HasVisibleEntry)
			{
				continue;
			}

			if (LastCategory != Definition.Category)
			{
				if (!LastCategory.empty())
				{
					ImGui::Spacing();
				}
				ImGui::TextDisabled(
					"%.*s",
					static_cast<int>(Definition.Category.size()),
					Definition.Category.data()
				);
				LastCategory = Definition.Category;
			}

			for (const FVectorPaletteEntry& Entry : VectorEntries)
			{
				if (!Filter.empty() &&
					Entry.SearchAliases.find(Filter) == xr_string::npos)
				{
					continue;
				}
				const xr_string Label = Entry.Label + "##" +
					xr_string(Definition.Type) + "_" +
					xr_string(ToString(Entry.ValueType));
				if (ImGui::Button(Label.c_str(), ImVec2(-1.0f, 0.0f)))
				{
					AddNode(Definition.Type, Entry.ValueType);
				}
			}
			continue;
		}

		const xr_string SearchText = Lower(
			xr_string(Definition.DisplayName) + " " +
			xr_string(Definition.Type) + " " +
			xr_string(Definition.Category)
		);
		if (!Filter.empty() && SearchText.find(Filter) == xr_string::npos)
		{
			continue;
		}

		if (LastCategory != Definition.Category)
		{
			if (!LastCategory.empty())
			{
				ImGui::Spacing();
			}
			ImGui::TextDisabled("%.*s", static_cast<int>(Definition.Category.size()), Definition.Category.data());
			LastCategory = Definition.Category;
		}

		const xr_string Label = xr_string(Definition.DisplayName) + "##" + xr_string(Definition.Type);
		if (ImGui::Button(Label.c_str(), ImVec2(-1.0f, 0.0f)))
		{
			AddNode(Definition.Type);
		}
	}
}

void UIMaterialEditorForm::DrawGraph()
{
	ImNodes::BeginNodeEditor();

	const FMaterialGraph& Graph = Document.GetGraph();
	for (const FMaterialGraphNode& Node : Graph.Nodes)
	{
		const int NodeUiId = UiId(Node.Id.Value);
		if (PositionedNodes.emplace(NodeUiId).second)
		{
			ImNodes::SetNodeGridSpacePos(NodeUiId, ImVec2(Node.EditorPosition[0], Node.EditorPosition[1]));
		}

		ImNodes::BeginNode(NodeUiId);
		ImNodes::BeginNodeTitleBar();
		const FMaterialNodeDefinition* Definition = FindMaterialNodeDefinition(Node.Type);
		if (Node.Type == "constant" && !Node.Pins.empty())
		{
			switch (Node.Pins.front().Type)
			{
				case EMaterialValueType::Float2:
					ImGui::TextUnformatted("Constant2");
					break;
				case EMaterialValueType::Float3:
					ImGui::TextUnformatted("Constant3");
					break;
				case EMaterialValueType::Float4:
					ImGui::TextUnformatted("Constant4");
					break;
				default:
					ImGui::TextUnformatted("Constant");
					break;
			}
		}
		else
		{
			xr_string Title = Definition
				? xr_string(Definition->DisplayName)
				: Node.Type;
			if (Node.Type == "make_vector" ||
				Node.Type == "break_vector" ||
				Node.Type == "swizzle")
			{
				const auto VectorPin = std::ranges::find_if(
					Node.Pins,
					[](const FMaterialGraphPin& Pin)
					{
						return Pin.Type >= EMaterialValueType::Float2 &&
							Pin.Type <= EMaterialValueType::Float4;
					}
				);
				if (VectorPin != Node.Pins.end())
				{
					const xr_string OperationName = Node.Type == "make_vector"
						? "Make "
						: Node.Type == "break_vector" ? "Break " : "Swizzle ";
					Title = OperationName + xr_string(ToString(VectorPin->Type));
				}
			}
			ImGui::TextUnformatted(Title.c_str());
		}
		ImNodes::EndNodeTitleBar();

		DrawNodeProperties(Node);

		for (const FMaterialGraphPin& Pin : Node.Pins)
		{
			ImNodes::PushColorStyle(ImNodesCol_Pin, PinColor(Pin.Type));
			const int PinUiId = UiId(Pin.Id.Value);
			if (Pin.Direction == EMaterialPinDirection::Input)
			{
				ImNodes::BeginInputAttribute(PinUiId, PinShape(Pin.Type));
			}
			else
			{
				ImNodes::BeginOutputAttribute(PinUiId, PinShape(Pin.Type));
			}

			ImGui::Text("%s [%.*s]", Pin.Name.c_str(), static_cast<int>(ToString(Pin.Type).size()), ToString(Pin.Type).data());

			if (Pin.Direction == EMaterialPinDirection::Input)
			{
				ImNodes::EndInputAttribute();
			}
			else
			{
				ImNodes::EndOutputAttribute();
			}
			ImNodes::PopColorStyle();
		}
		ImNodes::EndNode();
	}

	for (const FMaterialGraphLink& Link : Graph.Links)
	{
		ImNodes::Link(UiId(Link.Id), UiId(Link.FromPin.Value), UiId(Link.ToPin.Value));
	}

	ImNodes::MiniMap(0.18f, ImNodesMiniMapLocation_BottomRight);
	ImNodes::EndNodeEditor();

	int StartedPin = 0;
	int EndedPin = 0;
	if (ImNodes::IsLinkCreated(&StartedPin, &EndedPin))
	{
		const FMaterialGraphPin* From = PinFromUiId(StartedPin);
		const FMaterialGraphPin* To = PinFromUiId(EndedPin);
		if (From && To)
		{
			if (From->Direction == EMaterialPinDirection::Input)
			{
				std::swap(From, To);
				std::swap(StartedPin, EndedPin);
			}
			FMaterialEditorOperationResult Result = Document.Connect(
				MakeStableId(), From->Id, To->Id
			);
			if (Result.Succeeded())
			{
				Compile();
			}
			else
			{
				SetDiagnostics(std::move(Result.Diagnostics));
			}
		}
	}

	int DestroyedLink = 0;
	if (ImNodes::IsLinkDestroyed(&DestroyedLink))
	{
		const auto Stable = UiToStable.find(DestroyedLink);
		if (Stable != UiToStable.end())
		{
			FMaterialEditorOperationResult Result = Document.Disconnect(Stable->second);
			if (Result.Succeeded())
			{
				Compile();
			}
			else
			{
				SetDiagnostics(std::move(Result.Diagnostics));
			}
		}
	}

	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) &&
		ImGui::IsKeyPressed(ImGuiKey_Delete, false))
	{
		DeleteSelection();
	}

	for (const FMaterialGraphNode& Node : Document.GetGraph().Nodes)
	{
		const ImVec2 Position = ImNodes::GetNodeGridSpacePos(UiId(Node.Id.Value));
		Document.SetNodePosition(Node.Id, {Position.x, Position.y}, false);
	}
}

void UIMaterialEditorForm::DrawOutputPanel()
{
	if (!ImGui::BeginTabBar("##MaterialEditorOutputTabs"))
	{
		return;
	}

	if (ImGui::BeginTabItem("Details"))
	{
		DrawDetailsPanel();
		ImGui::EndTabItem();
	}

	if (ImGui::BeginTabItem("Preview"))
	{
		DrawPreviewPanel();
		ImGui::EndTabItem();
	}

	if (ImGui::BeginTabItem("Diagnostics"))
	{
		if (Diagnostics.empty())
		{
			ImGui::TextColored(ImVec4(0.4f, 0.9f, 0.5f, 1.0f), "Compile succeeded");
		}
		for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
		{
			const ImVec4 Color = Diagnostic.Severity == EMaterialDiagnosticSeverity::Error
									 ? ImVec4(1.0f, 0.35f, 0.3f, 1.0f)
								 : Diagnostic.Severity == EMaterialDiagnosticSeverity::Warning
									 ? ImVec4(1.0f, 0.75f, 0.25f, 1.0f)
									 : ImVec4(0.5f, 0.75f, 1.0f, 1.0f);
			ImGui::TextColored(Color, "%s: %s", SeverityName(Diagnostic.Severity), Diagnostic.Code.c_str());
			ImGui::TextWrapped("%s", Diagnostic.Message.c_str());
			if (Diagnostic.Node.IsValid())
			{
				ImGui::TextDisabled("Node: %s", Diagnostic.Node.Value.c_str());
				ImGui::SameLine();
				if (ImGui::SmallButton(("Focus##" + Diagnostic.Node.Value +
										Diagnostic.Code)
										   .c_str()))
				{
					const int NodeId = UiId(Diagnostic.Node.Value);
					ImNodes::ClearNodeSelection();
					ImNodes::SelectNode(NodeId);
					ImNodes::EditorContextMoveToNode(NodeId);
				}
			}
			ImGui::Separator();
		}
		ImGui::EndTabItem();
	}

	if (ImGui::BeginTabItem("Generated HLSL"))
	{
		const xr_string& Hlsl = CompileResult.GeneratedHlsl;
		ImGui::InputTextMultiline("##GeneratedMaterialHlsl", Hlsl.empty() ? const_cast<char*>("") : const_cast<char*>(Hlsl.c_str()), Hlsl.empty() ? 1 : Hlsl.size() + 1, ImVec2(-1.0f, -1.0f), ImGuiInputTextFlags_ReadOnly);
		ImGui::EndTabItem();
	}

	if (ImGui::BeginTabItem("Material JSON"))
	{
		const xr_string Json = Document.SerializeMaterial();
		ImGui::InputTextMultiline("##MaterialAssetJson", const_cast<char*>(Json.c_str()), Json.size() + 1, ImVec2(-1.0f, -1.0f), ImGuiInputTextFlags_ReadOnly);
		ImGui::EndTabItem();
	}
	ImGui::EndTabBar();
}

void UIMaterialEditorForm::DrawPreviewPanel()
{
	constexpr const char* PrimitiveNames[] = {"Sphere", "Cube", "Plane"};
	constexpr const char* EnvironmentNames[] = {"Studio", "Neutral", "Outdoor"};

	int Primitive = static_cast<int>(PreviewPrimitive);
	ImGui::SetNextItemWidth(120.0f);
	if (ImGui::Combo("Primitive", &Primitive, PrimitiveNames, static_cast<int>(std::size(PrimitiveNames))))
	{
		PreviewPrimitive = static_cast<EMaterialPreviewPrimitive>(Primitive);
		PreviewSourceDirty = true;
	}

	ImGui::SetNextItemWidth(120.0f);
	if (ImGui::Combo("Environment", &PreviewEnvironment, EnvironmentNames, static_cast<int>(std::size(EnvironmentNames))))
	{
		PreviewSourceDirty = true;
	}

	IMaterialPreviewRenderer& ActiveRenderer = GetMaterialPreviewRenderer();
	if (PreviewRenderer && PreviewRenderer != &ActiveRenderer)
	{
		ReleasePreview();
	}

	if (!ActiveRenderer.IsAvailable())
	{
		const FMaterialPreviewFrame Frame = ActiveRenderer.GetPreviewFrame({});
		ImGui::Separator();
		ImGui::TextWrapped("%.*s", static_cast<int>(Frame.Diagnostic.size()), Frame.Diagnostic.data());
		ImGui::TextDisabled("Start LevelEditor with the Tiramisu editor backend to enable live preview.");
		return;
	}

	if (!PreviewHandle.IsValid())
	{
		PreviewRenderer = &ActiveRenderer;
		PreviewHandle = PreviewRenderer->CreatePreview();
		PreviewSourceDirty = true;
	}
	if (!PreviewHandle.IsValid())
	{
		ImGui::TextWrapped("The Tiramisu backend could not allocate a material preview handle.");
		return;
	}

	const FMaterialAsset& PreviewMaterial = Document.GetMaterial();
	const xr_string MaterialJson = SerializeMaterialAssetJson(PreviewMaterial);
	const xr_string InstanceJson;
	if (MaterialJson != SubmittedMaterialJson ||
		InstanceJson != SubmittedInstanceJson)
	{
		PreviewSourceDirty = true;
	}

	if (PreviewSourceDirty)
	{
		FMaterialGraphCompileResult PreviewImplementation =
			BuildMaterialImplementation(PreviewMaterial);
		if (!PreviewImplementation.Diagnostics.empty())
		{
			Diagnostics = PreviewImplementation.Diagnostics;
		}

		SubmittedMaterialJson = MaterialJson;
		SubmittedInstanceJson = InstanceJson;
		SubmittedHlsl = std::move(PreviewImplementation.GeneratedHlsl);

		FMaterialPreviewSource Source;
		Source.MaterialAssetId = PreviewMaterial.Id.Value;
		Source.MaterialJson = SubmittedMaterialJson;
		Source.MaterialInstanceJson = SubmittedInstanceJson;
		Source.GeneratedHlsl = SubmittedHlsl;
		Source.Environment = EnvironmentNames[PreviewEnvironment];
		Source.Primitive = PreviewPrimitive;
		Source.Revision = ++PreviewRevision;
		PreviewRenderer->UpdatePreview(PreviewHandle, Source);
		PreviewSourceDirty = false;
	}

	ImGui::Separator();
	const ImVec2 Available = ImGui::GetContentRegionAvail();
	const u32 Width = static_cast<u32>(std::max(1.0f, Available.x));
	const u32 Height = static_cast<u32>(std::max(1.0f, Available.y - 24.0f));
	if (Width != PreviewWidth || Height != PreviewHeight)
	{
		PreviewWidth = Width;
		PreviewHeight = Height;
		PreviewRenderer->ResizePreview(PreviewHandle, Width, Height);
	}

	PreviewRenderer->RenderPreview(PreviewHandle, ImGui::GetIO().DeltaTime);
	const FMaterialPreviewFrame Frame = PreviewRenderer->GetPreviewFrame(PreviewHandle);
	switch (Frame.State)
	{
		case EMaterialPreviewState::Compiling:
			ImGui::TextDisabled("Compiling revision %llu...", static_cast<unsigned long long>(PreviewRevision));
			if (Frame.UsingLastGoodPipeline)
			{
				ImGui::TextDisabled("Last good revision %llu remains active", static_cast<unsigned long long>(Frame.AcceptedRevision));
			}
			break;
		case EMaterialPreviewState::Ready:
			ImGui::TextDisabled("Ready (revision %llu)", static_cast<unsigned long long>(Frame.AcceptedRevision));
			break;
		case EMaterialPreviewState::Error:
			ImGui::TextColored(ImVec4(1.0f, 0.35f, 0.3f, 1.0f), "Preview compilation failed");
			if (Frame.UsingLastGoodPipeline)
			{
				ImGui::TextColored(ImVec4(1.0f, 0.75f, 0.25f, 1.0f), "Last good revision %llu remains active", static_cast<unsigned long long>(Frame.AcceptedRevision));
			}
			break;
		default:
			ImGui::TextDisabled("Preview unavailable");
			break;
	}

	if (!Frame.Diagnostic.empty())
	{
		ImGui::TextWrapped("%.*s", static_cast<int>(Frame.Diagnostic.size()), Frame.Diagnostic.data());
	}
	if (Frame.PipelineKey != 0)
	{
		ImGui::TextDisabled("Pipeline: %016llx | %.*s | %.*s | %.*s", static_cast<unsigned long long>(Frame.PipelineKey), static_cast<int>(Frame.Backend.size()), Frame.Backend.data(), static_cast<int>(Frame.RenderPass.size()), Frame.RenderPass.data(), static_cast<int>(Frame.VertexFactory.size()), Frame.VertexFactory.data());
	}
	if (Frame.Surface.IsValid())
	{
		ImGui::Image(Frame.Surface.ImGuiTextureId, ImVec2(static_cast<float>(Width), static_cast<float>(Height)));
	}
}

void UIMaterialEditorForm::DrawNodeProperties(const FMaterialGraphNode& Node)
{
	if (Node.Type == "custom_hlsl")
	{
		DrawCustomHlslSignature(Node);
	}
	for (const FMaterialNodePropertyDefinition& Definition :
		 GetMaterialNodePropertyDefinitions(Node.Type))
	{
		const auto Current = Node.Properties.find(xr_string(Definition.Name));
		if (Current == Node.Properties.end())
		{
			continue;
		}

		const xr_string DraftKey = Node.Id.Value + "|" + xr_string(Definition.Name);
		ImGui::PushID(Definition.Name.data());
		ImGui::TextDisabled("%.*s", static_cast<int>(Definition.DisplayName.size()), Definition.DisplayName.data());

		if (Definition.Kind == EMaterialNodePropertyKind::ParameterId)
		{
			const xr_string* CurrentId = std::get_if<xr_string>(&Current->second);
			const FMaterialParameterDefinition* CurrentParameter = CurrentId && !CurrentId->empty()
																	   ? Document.GetMaterial().FindParameter({*CurrentId})
																	   : nullptr;
			const char* Preview = CurrentParameter
									  ? (CurrentParameter->DisplayName.empty() ? CurrentParameter->Name.c_str()
																			   : CurrentParameter->DisplayName.c_str())
									  : (CurrentId && !CurrentId->empty() ? CurrentId->c_str() : "Unassigned");

			ImGui::SetNextItemWidth(180.0f);
			if (ImGui::BeginCombo("##Parameter", Preview))
			{
				if (ImGui::Selectable("Unassigned", !CurrentId || CurrentId->empty()))
				{
					FMaterialEditorOperationResult Result = Document.SetNodeProperty(
						Node.Id, Definition.Name, xr_string{}
					);
					if (Result.Succeeded())
					{
						Compile();
					}
					else
					{
						SetDiagnostics(std::move(Result.Diagnostics));
					}
				}

				const auto Output = std::ranges::find_if(Node.Pins, [](const FMaterialGraphPin& Pin)
														 { return Pin.Direction == EMaterialPinDirection::Output; });
				for (const FMaterialParameterDefinition& Parameter :
					 Document.GetMaterial().Parameters)
				{
					bool Compatible = false;
					if (Node.Type == "parameter")
					{
						Compatible = !Parameter.IsStatic() && Output != Node.Pins.end() &&
									 ToValueType(Parameter.Type) == Output->Type;
					}
					else if (Node.Type == "static_switch")
					{
						Compatible = Parameter.Type == EMaterialParameterType::StaticBool;
					}
					else if (Node.Type == "texture_sample")
					{
						Compatible = Parameter.Type == EMaterialParameterType::Texture2D;
					}
					if (!Compatible)
					{
						continue;
					}

					const bool Selected = CurrentId && *CurrentId == Parameter.Id.Value;
					const xr_string Label = (Parameter.DisplayName.empty()
												 ? Parameter.Name
												 : Parameter.DisplayName) +
											"##" + Parameter.Id.Value;
					if (ImGui::Selectable(Label.c_str(), Selected))
					{
						FMaterialEditorOperationResult Result = Document.SetNodeProperty(
							Node.Id, Definition.Name, Parameter.Id.Value
						);
						if (Result.Succeeded())
						{
							Compile();
						}
						else
						{
							SetDiagnostics(std::move(Result.Diagnostics));
						}
					}
					if (Selected)
					{
						ImGui::SetItemDefaultFocus();
					}
				}
				ImGui::EndCombo();
			}
			if (Node.Type == "texture_sample")
			{
				ImGui::TextDisabled("Texture link overrides this parameter");
			}
		}
		else if (Definition.Kind == EMaterialNodePropertyKind::HlslExpression ||
			Definition.Kind == EMaterialNodePropertyKind::String)
		{
			const xr_string* Code = std::get_if<xr_string>(&Current->second);
			auto [Draft, Inserted] = NodeStringDrafts.try_emplace(DraftKey);
			if (Inserted)
			{
				SetTextBuffer(Draft->second, Code ? xr_string_view{*Code} : xr_string_view{});
			}
			ImGui::SetNextItemWidth(240.0f);
			if (Definition.Kind == EMaterialNodePropertyKind::HlslExpression)
			{
				ImGui::InputTextMultiline(
					"##Code",
					Draft->second.data(),
					Draft->second.size(),
					ImVec2(240.0f, 72.0f)
				);
			}
			else
			{
				ImGui::InputText(
					"##Text",
					Draft->second.data(),
					Draft->second.size()
				);
			}
			if (ImGui::IsItemDeactivatedAfterEdit())
			{
				FMaterialEditorOperationResult Result = Document.SetNodeProperty(
					Node.Id, Definition.Name, xr_string{Draft->second.data()}
				);
				if (Result.Succeeded())
				{
					Compile();
				}
				else
				{
					SetDiagnostics(std::move(Result.Diagnostics));
					SetTextBuffer(
						Draft->second,
						std::get<xr_string>(Current->second)
					);
				}
			}
		}
		else
		{
			FMaterialValue& Draft = NodePropertyDrafts.try_emplace(
														  DraftKey, Current->second
			)
										.first->second;
			bool Commit = false;
			bool Active = false;
			ImGui::SetNextItemWidth(150.0f);
			if (float* Value = std::get_if<float>(&Draft))
			{
				ImGui::DragFloat("##Value", Value, 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
				Active = ImGui::IsItemActive();
			}
			else if (FFloat2* Value = std::get_if<FFloat2>(&Draft))
			{
				ImGui::DragFloat2("##Value", Value->data(), 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
				Active = ImGui::IsItemActive();
			}
			else if (FFloat3* Value = std::get_if<FFloat3>(&Draft))
			{
				ImGui::DragFloat3("##Value", Value->data(), 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
				Active = ImGui::IsItemActive();
			}
			else if (FFloat4* Value = std::get_if<FFloat4>(&Draft))
			{
				ImGui::DragFloat4("##Value", Value->data(), 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
				Active = ImGui::IsItemActive();
			}
			else if (bool* Value = std::get_if<bool>(&Draft))
			{
				Commit = ImGui::Checkbox("##Value", Value);
			}
			else if (s32* Value = std::get_if<s32>(&Draft))
			{
				int EditorValue = *Value;
				if (ImGui::DragInt("##Value", &EditorValue, 1.0f))
				{
					*Value = EditorValue;
				}
				Commit = ImGui::IsItemDeactivatedAfterEdit();
				Active = ImGui::IsItemActive();
			}

			if (Commit)
			{
				FMaterialEditorOperationResult Result = Document.SetNodeProperty(
					Node.Id, Definition.Name, Draft
				);
				if (Result.Succeeded())
				{
					Compile();
				}
				else
				{
					SetDiagnostics(std::move(Result.Diagnostics));
				}
			}
			else if (!Active && Draft != Current->second)
			{
				Draft = Current->second;
			}
		}
		ImGui::PopID();
	}
}

void UIMaterialEditorForm::DrawCustomHlslSignature(
	const FMaterialGraphNode& Node
)
{
	auto [DraftIterator, Inserted] =
		CustomHlslSignatureDrafts.try_emplace(Node.Id.Value);
	FCustomHlslSignatureDraft& Draft = DraftIterator->second;
	if (Inserted)
	{
		for (const FMaterialGraphPin& Pin : Node.Pins)
		{
			if (Pin.Direction == EMaterialPinDirection::Input)
			{
				FCustomHlslInputDraft Input;
				SetTextBuffer(Input.Name, Pin.Name);
				Input.Type = Pin.Type;
				Draft.Inputs.push_back(std::move(Input));
			}
			else if (Pin.Name == "Result")
			{
				Draft.OutputType = Pin.Type;
			}
		}
	}

	ImGui::TextDisabled("Signature");
	ImGui::SetNextItemWidth(150.0f);
	(void)DrawMaterialFloatTypeCombo(
		"Result Type##CustomHlsl",
		Draft.OutputType
	);
	ImGui::TextDisabled("Inputs");
	xr_optional<size_t> RemoveIndex;
	for (size_t Index = 0; Index < Draft.Inputs.size(); ++Index)
	{
		FCustomHlslInputDraft& Input = Draft.Inputs[Index];
		ImGui::PushID(static_cast<int>(Index));
		ImGui::SetNextItemWidth(105.0f);
		ImGui::InputText("##InputName", Input.Name.data(), Input.Name.size());
		ImGui::SameLine();
		ImGui::SetNextItemWidth(85.0f);
		(void)DrawMaterialFloatTypeCombo("##InputType", Input.Type);
		ImGui::SameLine();
		if (ImGui::SmallButton("-"))
		{
			RemoveIndex = Index;
		}
		ImGui::PopID();
	}
	if (RemoveIndex)
	{
		Draft.Inputs.erase(Draft.Inputs.begin() + *RemoveIndex);
	}

	if (Draft.Inputs.size() < 16 && ImGui::SmallButton("+ Input"))
	{
		xr_set<xr_string> ExistingNames;
		for (const FCustomHlslInputDraft& Input : Draft.Inputs)
		{
			ExistingNames.emplace(Input.Name.data());
		}
		u32 Suffix = static_cast<u32>(Draft.Inputs.size());
		xr_string Name;
		do
		{
			Name = "Input" + std::to_string(Suffix++);
		}
		while (ExistingNames.contains(Name));

		FCustomHlslInputDraft Input;
		SetTextBuffer(Input.Name, Name);
		Draft.Inputs.push_back(std::move(Input));
	}
	ImGui::SameLine();
	if (ImGui::SmallButton("Apply Signature"))
	{
		xr_vector<FMaterialCustomHlslInputDefinition> Inputs;
		Inputs.reserve(Draft.Inputs.size());
		for (const FCustomHlslInputDraft& Input : Draft.Inputs)
		{
			Inputs.push_back({Input.Name.data(), Input.Type});
		}
		FMaterialEditorOperationResult Result =
			Document.SetCustomHlslSignature(
				Node.Id,
				Inputs,
				Draft.OutputType
			);
		if (Result.Succeeded())
		{
			Compile();
		}
		else
		{
			SetDiagnostics(std::move(Result.Diagnostics));
		}
	}
	ImGui::TextDisabled(
		"Use {InputName} in the expression; Parameter nodes connect to these pins."
	);
	ImGui::TextDisabled(
		"Renaming or changing a pin type removes incompatible links."
	);
	ImGui::Separator();
}

void UIMaterialEditorForm::DrawDetailsPanel()
{
	const FMaterialAsset& Asset = Document.GetMaterial();
	ImGui::TextDisabled("GUID");
	ImGui::TextWrapped("%s", Asset.Id.Value.c_str());
	ImGui::TextDisabled("Source");
	ImGui::TextWrapped("%s", Asset.SourcePath.empty() ? "Unsaved" : Asset.SourcePath.c_str());
	ImGui::Separator();

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputText("Name", MaterialNameDraft.data(), MaterialNameDraft.size());
	if (ImGui::IsItemDeactivatedAfterEdit() &&
		Document.SetMaterialName(MaterialNameDraft.data()))
	{
		Compile();
	}

	constexpr EMaterialDomain Domains[] = {
		EMaterialDomain::Surface, EMaterialDomain::Decal, EMaterialDomain::UI, EMaterialDomain::PostProcess
	};
	if (ImGui::BeginCombo("Domain", ToString(Asset.Domain).data()))
	{
		for (const EMaterialDomain Domain : Domains)
		{
			const bool Selected = Asset.Domain == Domain;
			if (ImGui::Selectable(ToString(Domain).data(), Selected) &&
				Document.SetMaterialDomain(Domain))
			{
				Compile();
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}

	constexpr EMaterialBlendMode BlendModes[] = {
		EMaterialBlendMode::Opaque, EMaterialBlendMode::Masked, EMaterialBlendMode::Translucent, EMaterialBlendMode::Additive, EMaterialBlendMode::Modulate
	};
	if (ImGui::BeginCombo("Blend Mode", ToString(Asset.BlendMode).data()))
	{
		for (const EMaterialBlendMode BlendMode : BlendModes)
		{
			const bool Selected = Asset.BlendMode == BlendMode;
			if (ImGui::Selectable(ToString(BlendMode).data(), Selected) &&
				Document.SetMaterialBlendMode(BlendMode))
			{
				Compile();
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}

	constexpr EMaterialShadingModel ShadingModels[] = {
		EMaterialShadingModel::DefaultLit, EMaterialShadingModel::Unlit, EMaterialShadingModel::Foliage, EMaterialShadingModel::Hair
	};
	if (ImGui::BeginCombo("Shading Model", ToString(Asset.ShadingModel).data()))
	{
		for (const EMaterialShadingModel ShadingModel : ShadingModels)
		{
			const bool Selected = Asset.ShadingModel == ShadingModel;
			if (ImGui::Selectable(ToString(ShadingModel).data(), Selected) &&
				Document.SetMaterialShadingModel(ShadingModel))
			{
				Compile();
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}

	bool TwoSided = Asset.TwoSided;
	if (ImGui::Checkbox("Two Sided", &TwoSided) &&
		Document.SetMaterialTwoSided(TwoSided))
	{
		Compile();
	}

	ImGui::Text("Implementation: %s", Asset.Implementation.Type == EMaterialImplementationType::Graph ? "Graph" : "HLSL");
	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputText("HLSL Template", MaterialTemplateDraft.data(), MaterialTemplateDraft.size());
	if (ImGui::IsItemDeactivatedAfterEdit() &&
		Document.SetMaterialHlslTemplate(MaterialTemplateDraft.data()))
	{
		Compile();
	}

	ImGui::Separator();
	ImGui::Text("Parameters (%zu)", Asset.Parameters.size());
	const FMaterialPermutationStatistics Permutations =
		CalculateMaterialPermutationStatistics(Asset.Parameters);
	if (Permutations.Overflow)
	{
		ImGui::Text("Permutations: overflow (> %llu)", static_cast<unsigned long long>(Permutations.PermutationCount));
	}
	else
	{
		ImGui::Text("Permutations: %s%llu", Permutations.Exact ? "" : ">= ", static_cast<unsigned long long>(Permutations.PermutationCount));
	}
	ImGui::TextDisabled("Static Bool: %zu, Static Enum: %zu%s", Permutations.StaticBoolParameters, Permutations.StaticEnumParameters, Permutations.Exact ? "" : " (enum range incomplete)");
	constexpr EMaterialParameterType ParameterTypes[] = {
		EMaterialParameterType::Scalar, EMaterialParameterType::Float2, EMaterialParameterType::Float3, EMaterialParameterType::Float4, EMaterialParameterType::Color, EMaterialParameterType::Texture2D, EMaterialParameterType::TextureCube, EMaterialParameterType::SamplerPreset, EMaterialParameterType::StaticBool, EMaterialParameterType::StaticEnum
	};
	if (ImGui::BeginCombo("Add Parameter", "Select type..."))
	{
		for (const EMaterialParameterType Type : ParameterTypes)
		{
			if (!ImGui::Selectable(ToString(Type).data()))
			{
				continue;
			}
			FMaterialParameterDefinition Definition;
			Definition.Id.Value = GenerateMaterialGuid();
			Definition.Name = "New_" + xr_string(ToString(Type));
			Definition.DisplayName = Definition.Name;
			Definition.Type = Type;
			Definition.DefaultValue = DefaultParameterValue(Type);
			FMaterialEditorOperationResult Result =
				Document.AddParameter(std::move(Definition));
			if (Result.Succeeded())
			{
				Compile();
			}
			else
			{
				SetDiagnostics(std::move(Result.Diagnostics));
			}
		}
		ImGui::EndCombo();
	}

	xr_optional<FMaterialParameterId> ParameterToRemove;
	for (const FMaterialParameterDefinition& Parameter : Asset.Parameters)
	{
		if (DrawParameterEditor(Parameter))
		{
			ParameterToRemove = Parameter.Id;
		}
	}
	if (ParameterToRemove)
	{
		FMaterialEditorOperationResult Result =
			Document.RemoveParameter(*ParameterToRemove);
		if (Result.Succeeded())
		{
			ParameterDrafts.erase(ParameterToRemove->Value);
			Compile();
		}
		else
		{
			SetDiagnostics(std::move(Result.Diagnostics));
		}
	}
}

bool UIMaterialEditorForm::DrawParameterEditor(
	const FMaterialParameterDefinition& Parameter
)
{
	ImGui::PushID(Parameter.Id.Value.c_str());
	FParameterEditorDraft& Draft = GetParameterDraft(Parameter);
	const xr_string Header = (Parameter.DisplayName.empty()
								  ? Parameter.Name
								  : Parameter.DisplayName) +
							 "##Parameter";
	if (ImGui::CollapsingHeader(Header.c_str()))
	{
		ImGui::TextDisabled("GUID: %s", Parameter.Id.Value.c_str());

		constexpr EMaterialParameterType Types[] = {
			EMaterialParameterType::Scalar, EMaterialParameterType::Float2, EMaterialParameterType::Float3, EMaterialParameterType::Float4, EMaterialParameterType::Color, EMaterialParameterType::Texture2D, EMaterialParameterType::TextureCube, EMaterialParameterType::SamplerPreset, EMaterialParameterType::StaticBool, EMaterialParameterType::StaticEnum
		};
		if (ImGui::BeginCombo("Type", ToString(Draft.Type).data()))
		{
			for (const EMaterialParameterType Type : Types)
			{
				const bool Selected = Draft.Type == Type;
				if (ImGui::Selectable(ToString(Type).data(), Selected) && !Selected)
				{
					Draft.Type = Type;
					Draft.DefaultValue = DefaultParameterValue(Type);
					if (const xr_string* Text = std::get_if<xr_string>(&Draft.DefaultValue))
					{
						SetTextBuffer(Draft.DefaultText, *Text);
					}
					if (!CommitParameterDraft(Parameter.Id))
					{
						SyncParameterDraft(Draft, Parameter);
					}
				}
				if (Selected)
				{
					ImGui::SetItemDefaultFocus();
				}
			}
			ImGui::EndCombo();
		}

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputText("Name", Draft.Name.data(), Draft.Name.size());
		if (ImGui::IsItemDeactivatedAfterEdit())
		{
			CommitParameterDraft(Parameter.Id);
		}

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputText("Display Name", Draft.DisplayName.data(), Draft.DisplayName.size());
		if (ImGui::IsItemDeactivatedAfterEdit())
		{
			CommitParameterDraft(Parameter.Id);
		}

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputText("Category", Draft.Category.data(), Draft.Category.size());
		if (ImGui::IsItemDeactivatedAfterEdit())
		{
			CommitParameterDraft(Parameter.Id);
		}

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputTextMultiline("Description", Draft.Description.data(), Draft.Description.size(), ImVec2(-1.0f, 54.0f));
		if (ImGui::IsItemDeactivatedAfterEdit())
		{
			CommitParameterDraft(Parameter.Id);
		}

		ImGui::TextDisabled("Default");
		bool CommitDefault = false;
		if (float* Value = std::get_if<float>(&Draft.DefaultValue))
		{
			float Minimum = Draft.HasMinimum ? Draft.Minimum : 0.0f;
			float Maximum = Draft.HasMaximum ? Draft.Maximum : 0.0f;
			if (!Draft.HasMinimum || !Draft.HasMaximum || Minimum >= Maximum)
			{
				Minimum = Maximum = 0.0f;
			}
			ImGui::SetNextItemWidth(-1.0f);
			ImGui::DragFloat("##Default", Value, 0.01f, Minimum, Maximum);
			CommitDefault = ImGui::IsItemDeactivatedAfterEdit();
		}
		else if (FFloat2* Value = std::get_if<FFloat2>(&Draft.DefaultValue))
		{
			ImGui::SetNextItemWidth(-1.0f);
			ImGui::DragFloat2("##Default", Value->data(), 0.01f);
			CommitDefault = ImGui::IsItemDeactivatedAfterEdit();
		}
		else if (FFloat3* Value = std::get_if<FFloat3>(&Draft.DefaultValue))
		{
			ImGui::SetNextItemWidth(-1.0f);
			ImGui::DragFloat3("##Default", Value->data(), 0.01f);
			CommitDefault = ImGui::IsItemDeactivatedAfterEdit();
		}
		else if (FFloat4* Value = std::get_if<FFloat4>(&Draft.DefaultValue))
		{
			ImGui::SetNextItemWidth(-1.0f);
			if (Draft.Type == EMaterialParameterType::Color)
			{
				ImGui::ColorEdit4("##Default", Value->data());
			}
			else
			{
				ImGui::DragFloat4("##Default", Value->data(), 0.01f);
			}
			CommitDefault = ImGui::IsItemDeactivatedAfterEdit();
		}
		else if (bool* Value = std::get_if<bool>(&Draft.DefaultValue))
		{
			CommitDefault = ImGui::Checkbox("##Default", Value);
		}
		else if (s32* Value = std::get_if<s32>(&Draft.DefaultValue))
		{
			int EditorValue = *Value;
			ImGui::SetNextItemWidth(-1.0f);
			if (ImGui::DragInt("##Default", &EditorValue, 1.0f))
			{
				*Value = EditorValue;
			}
			CommitDefault = ImGui::IsItemDeactivatedAfterEdit();
		}
		else if (std::get_if<xr_string>(&Draft.DefaultValue))
		{
			ImGui::SetNextItemWidth(-1.0f);
			ImGui::InputText("##Default", Draft.DefaultText.data(), Draft.DefaultText.size());
			if (ImGui::IsItemDeactivatedAfterEdit())
			{
				Draft.DefaultValue = xr_string{Draft.DefaultText.data()};
				CommitDefault = true;
			}
		}
		if (CommitDefault)
		{
			CommitParameterDraft(Parameter.Id);
		}

		const bool SupportsRange = Draft.Type == EMaterialParameterType::Scalar ||
								   Draft.Type == EMaterialParameterType::StaticEnum;
		if (SupportsRange)
		{
			if (ImGui::Checkbox("Minimum", &Draft.HasMinimum))
			{
				CommitParameterDraft(Parameter.Id);
			}
			if (Draft.HasMinimum)
			{
				ImGui::SameLine();
				ImGui::SetNextItemWidth(100.0f);
				ImGui::DragFloat("##MinimumValue", &Draft.Minimum, 0.01f);
				if (ImGui::IsItemDeactivatedAfterEdit())
				{
					CommitParameterDraft(Parameter.Id);
				}
			}
			if (ImGui::Checkbox("Maximum", &Draft.HasMaximum))
			{
				CommitParameterDraft(Parameter.Id);
			}
			if (Draft.HasMaximum)
			{
				ImGui::SameLine();
				ImGui::SetNextItemWidth(100.0f);
				ImGui::DragFloat("##MaximumValue", &Draft.Maximum, 0.01f);
				if (ImGui::IsItemDeactivatedAfterEdit())
				{
					CommitParameterDraft(Parameter.Id);
				}
			}
		}
	}

	bool Remove = false;
	if (!Parameter.IsStatic() || Parameter.Type == EMaterialParameterType::StaticBool)
	{
		if (ImGui::SmallButton("Add Node"))
		{
			const bool StaticSwitch = Parameter.IsStatic();
			const FMaterialNodeId NodeId{MakeStableId()};
			FMaterialEditorOperationResult Added = Document.AddNode(
				StaticSwitch ? "static_switch" : "parameter",
				NodeId,
				{},
				StaticSwitch
					? EMaterialValueType::Float1
					: ToValueType(Parameter.Type)
			);
			if (Added.Succeeded())
			{
				FMaterialEditorOperationResult Assigned = Document.SetNodeProperty(
					NodeId, "parameter_id", Parameter.Id.Value
				);
				if (Assigned.Succeeded())
				{
					Compile();
				}
				else
				{
					SetDiagnostics(std::move(Assigned.Diagnostics));
				}
			}
			else
			{
				SetDiagnostics(std::move(Added.Diagnostics));
			}
		}
		ImGui::SameLine();
	}
	if (ImGui::SmallButton("Remove"))
	{
		Remove = true;
	}
	ImGui::Separator();
	ImGui::PopID();
	return Remove;
}

UIMaterialEditorForm::FParameterEditorDraft& UIMaterialEditorForm::GetParameterDraft(
	const FMaterialParameterDefinition& Parameter
)
{
	auto [Draft, Inserted] = ParameterDrafts.try_emplace(Parameter.Id.Value);
	if (Inserted)
	{
		SyncParameterDraft(Draft->second, Parameter);
	}
	return Draft->second;
}

void UIMaterialEditorForm::SyncParameterDraft(FParameterEditorDraft& Draft, const FMaterialParameterDefinition& Parameter)
{
	SetTextBuffer(Draft.Name, Parameter.Name);
	SetTextBuffer(Draft.DisplayName, Parameter.DisplayName);
	SetTextBuffer(Draft.Category, Parameter.Category);
	SetTextBuffer(Draft.Description, Parameter.Description);
	Draft.Type = Parameter.Type;
	Draft.DefaultValue = Parameter.DefaultValue;
	if (const xr_string* Text = std::get_if<xr_string>(&Draft.DefaultValue))
	{
		SetTextBuffer(Draft.DefaultText, *Text);
	}
	else
	{
		Draft.DefaultText.fill('\0');
	}
	Draft.HasMinimum = Parameter.Minimum.has_value();
	Draft.HasMaximum = Parameter.Maximum.has_value();
	Draft.Minimum = Parameter.Minimum.value_or(0.0f);
	Draft.Maximum = Parameter.Maximum.value_or(1.0f);
}

bool UIMaterialEditorForm::CommitParameterDraft(
	const FMaterialParameterId& ParameterId
)
{
	const FMaterialParameterDefinition* Current =
		Document.GetMaterial().FindParameter(ParameterId);
	const auto Draft = ParameterDrafts.find(ParameterId.Value);
	if (!Current || Draft == ParameterDrafts.end())
	{
		return false;
	}

	FMaterialParameterDefinition Updated = *Current;
	Updated.Name = Draft->second.Name.data();
	Updated.DisplayName = Draft->second.DisplayName.data();
	Updated.Category = Draft->second.Category.data();
	Updated.Description = Draft->second.Description.data();
	Updated.Type = Draft->second.Type;
	Updated.DefaultValue = Draft->second.DefaultValue;
	Updated.Minimum = Draft->second.HasMinimum
						  ? xr_optional<float>{Draft->second.Minimum}
						  : std::nullopt;
	Updated.Maximum = Draft->second.HasMaximum
						  ? xr_optional<float>{Draft->second.Maximum}
						  : std::nullopt;

	FMaterialEditorOperationResult Result = Document.UpdateParameter(
		ParameterId, std::move(Updated)
	);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return false;
	}
	Compile();
	return true;
}

void UIMaterialEditorForm::Compile()
{
	CompileResult = BuildMaterialImplementation(Document.GetMaterial());
	Diagnostics = CompileResult.Diagnostics;
	PreviewSourceDirty = true;
	RefreshDependencyWatch();
}

void UIMaterialEditorForm::ReleasePreview()
{
	if (PreviewRenderer && PreviewHandle.IsValid())
	{
		PreviewRenderer->DestroyPreview(PreviewHandle);
	}
	PreviewRenderer = nullptr;
	PreviewHandle = {};
	PreviewWidth = 0;
	PreviewHeight = 0;
	PreviewSourceDirty = true;
	SubmittedMaterialJson.clear();
	SubmittedInstanceJson.clear();
	SubmittedHlsl.clear();
}

void UIMaterialEditorForm::AddNode(
	const xr_string_view Type,
	const EMaterialValueType ValueType
)
{
	const float Offset = static_cast<float>((Document.GetGraph().Nodes.size() % 7) * 28);
	FMaterialEditorOperationResult Result = Document.AddNode(
		Type,
		{MakeStableId()},
		{80.0f + Offset, 80.0f + Offset},
		ValueType
	);
	if (Result.Succeeded())
	{
		Compile();
	}
	else
	{
		SetDiagnostics(std::move(Result.Diagnostics));
	}
}

void UIMaterialEditorForm::DeleteSelection()
{
	xr_vector<int> Links(static_cast<size_t>(ImNodes::NumSelectedLinks()));
	if (!Links.empty())
	{
		ImNodes::GetSelectedLinks(Links.data());
	}
	for (const int LinkUiId : Links)
	{
		const auto Stable = UiToStable.find(LinkUiId);
		if (Stable != UiToStable.end())
		{
			Document.Disconnect(Stable->second);
		}
	}

	xr_vector<int> Nodes(static_cast<size_t>(ImNodes::NumSelectedNodes()));
	if (!Nodes.empty())
	{
		ImNodes::GetSelectedNodes(Nodes.data());
	}
	for (const int NodeUiId : Nodes)
	{
		const auto Stable = UiToStable.find(NodeUiId);
		if (Stable == UiToStable.end())
		{
			continue;
		}
		FMaterialEditorOperationResult Result = Document.RemoveNode({Stable->second});
		if (!Result.Succeeded())
		{
			SetDiagnostics(std::move(Result.Diagnostics));
		}
	}
	NodePropertyDrafts.clear();
	NodeStringDrafts.clear();
	CustomHlslSignatureDrafts.clear();
	Compile();
}

void UIMaterialEditorForm::CopySelection()
{
	xr_vector<int> SelectedUiIds(
		static_cast<size_t>(ImNodes::NumSelectedNodes())
	);
	if (!SelectedUiIds.empty())
	{
		ImNodes::GetSelectedNodes(SelectedUiIds.data());
	}

	xr_vector<FMaterialNodeId> SelectedNodeIds;
	SelectedNodeIds.reserve(SelectedUiIds.size());
	for (const int UiNodeId : SelectedUiIds)
	{
		const auto Stable = UiToStable.find(UiNodeId);
		if (Stable != UiToStable.end())
		{
			SelectedNodeIds.push_back({Stable->second});
		}
	}

	xr_string ClipboardJson;
	FMaterialEditorOperationResult Result = Document.CopyNodes(
		SelectedNodeIds, ClipboardJson
	);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}
	ImGui::SetClipboardText(ClipboardJson.c_str());
}

void UIMaterialEditorForm::PasteClipboard()
{
	const char* ClipboardText = ImGui::GetClipboardText();
	if (!ClipboardText || !*ClipboardText)
	{
		return;
	}

	xr_vector<FMaterialNodeId> PastedNodeIds;
	FMaterialEditorOperationResult Result = Document.PasteNodes(
		ClipboardText, {32.0f, 32.0f}, PastedNodeIds
	);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}

	ImNodes::ClearNodeSelection();
	for (const FMaterialNodeId& NodeId : PastedNodeIds)
	{
		ImNodes::SelectNode(UiId(NodeId.Value));
	}
	Compile();
}

void UIMaterialEditorForm::OpenMaterial()
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_data$", "render_materials");
	xr_string Path;
	if (!EFS.GetOpenName("$game_data$", Path, false, MaterialRoot, -1, "*.material.json"))
	{
		return;
	}

	const std::filesystem::path MaterialPath{Path.c_str()};
	std::filesystem::path RecoveryPath = MaterialPath;
	RecoveryPath += ".autosave";
	bool RestoreRecovery = false;
	if (IsRecoveryNewer(RecoveryPath, MaterialPath))
	{
		RestoreRecovery = ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "A newer Material Editor autosave exists. Restore it?") == mrYes;
	}

	FMaterialEditorOperationResult Result = RestoreRecovery
												? Document.OpenRecoveryFile(RecoveryPath, MaterialPath)
												: Document.OpenMaterialFile(MaterialPath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}

	xr_vector<FMaterialDiagnostic> LoadDiagnostics = std::move(Result.Diagnostics);
	ResetPresentationState();
	Compile();
	Diagnostics.insert(Diagnostics.begin(), LoadDiagnostics.begin(), LoadDiagnostics.end());
	AutosaveStatus = RestoreRecovery ? "Recovered autosave (unsaved)" : xr_string{};
	NextAutosaveTime = ImGui::GetTime() + 30.0;
}

bool UIMaterialEditorForm::OpenMaterialFile(
	const std::filesystem::path& MaterialPath
)
{
	const std::filesystem::path CurrentPath(
		Document.GetMaterial().SourcePath.c_str()
	);
	if (Document.IsDirty() && !SameEditorPath(CurrentPath, MaterialPath))
	{
		Show();
		AutosaveStatus =
			"Save or discard the active material before opening another one";
		return false;
	}

	std::filesystem::path RecoveryPath = MaterialPath;
	RecoveryPath += ".autosave";
	const bool RestoreRecovery = IsRecoveryNewer(
		RecoveryPath,
		MaterialPath
	) && ELog.DlgMsg(
		mtConfirmation,
		mbYes | mbNo,
		"A newer Material Editor autosave exists. Restore it?"
	) == mrYes;
	FMaterialEditorOperationResult Result = RestoreRecovery
		? Document.OpenRecoveryFile(RecoveryPath, MaterialPath)
		: Document.OpenMaterialFile(MaterialPath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		Show();
		return false;
	}

	xr_vector<FMaterialDiagnostic> LoadDiagnostics =
		std::move(Result.Diagnostics);
	ResetPresentationState();
	Compile();
	Diagnostics.insert(
		Diagnostics.begin(),
		LoadDiagnostics.begin(),
		LoadDiagnostics.end()
	);
	AutosaveStatus = RestoreRecovery
		? "Recovered autosave (unsaved)"
		: xr_string{};
	NextAutosaveTime = ImGui::GetTime() + 30.0;
	Show();
	return true;
}

bool UIMaterialEditorForm::CreateMaterialFile(
	const std::filesystem::path& MaterialPath
)
{
	if (Document.IsDirty())
	{
		Show();
		AutosaveStatus =
			"Save or discard the active material before creating another one";
		return false;
	}
	Document.NewMaterial();
	FMaterialEditorOperationResult Result =
		Document.SaveMaterialFile(MaterialPath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		Show();
		return false;
	}
	ResetPresentationState();
	Compile();
	Diagnostics.insert(
		Diagnostics.begin(),
		Result.Diagnostics.begin(),
		Result.Diagnostics.end()
	);
	Show();
	return true;
}

void UIMaterialEditorForm::OpenAutosave()
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_data$", "render_materials");
	xr_string Path;
	if (!EFS.GetOpenName("$game_data$", Path, false, MaterialRoot, -1, "*.autosave"))
	{
		return;
	}

	const std::filesystem::path RecoveryPath{Path.c_str()};
	std::filesystem::path OriginalPath;
	xr_string OriginalText = RecoveryPath.string();
	if (OriginalText.ends_with(".autosave"))
	{
		OriginalText.resize(OriginalText.size() - xr_string_view{".autosave"}.size());
		OriginalPath = OriginalText.c_str();
		if (RecoveryPath.parent_path().filename() == ".autosave" ||
			!std::filesystem::exists(OriginalPath))
		{
			OriginalPath.clear();
		}
	}

	FMaterialEditorOperationResult Result =
		Document.OpenRecoveryFile(RecoveryPath, OriginalPath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}

	xr_vector<FMaterialDiagnostic> LoadDiagnostics =
		std::move(Result.Diagnostics);
	ResetPresentationState();
	Compile();
	Diagnostics.insert(Diagnostics.begin(), LoadDiagnostics.begin(), LoadDiagnostics.end());
	AutosaveStatus = "Recovered autosave (unsaved)";
	NextAutosaveTime = ImGui::GetTime() + 30.0;
}

void UIMaterialEditorForm::SaveMaterial(const bool SaveAs)
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_data$", "render_materials");
	xr_string Path = Document.GetMaterial().SourcePath.c_str();
	if (SaveAs || Path.empty())
	{
		if (!EFS.GetSaveName("$game_data$", Path, MaterialRoot, -1, "*.material.json"))
		{
			return;
		}
		if (!HasMaterialExtension(Path))
		{
			Path += ".material.json";
		}
	}

	const std::filesystem::path PreviousRecovery = MaterialRecoveryPath();
	FMaterialEditorOperationResult Result = Document.SaveMaterialFile(Path.c_str());
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}

	xr_vector<FMaterialDiagnostic> SaveDiagnostics = std::move(Result.Diagnostics);
	RemoveRecoveryFile(PreviousRecovery);
	RemoveRecoveryFile(MaterialRecoveryPath());
	AutosaveStatus = "Saved; recovery cleared";
	NextAutosaveTime = ImGui::GetTime() + 30.0;
	Compile();
	Diagnostics.insert(Diagnostics.begin(), SaveDiagnostics.begin(), SaveDiagnostics.end());
}

std::filesystem::path UIMaterialEditorForm::MaterialRecoveryPath() const
{
	if (!Document.GetMaterial().SourcePath.empty())
	{
		std::filesystem::path Result{Document.GetMaterial().SourcePath.c_str()};
		Result += ".autosave";
		return Result;
	}

	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_data$", "render_materials");
	return std::filesystem::path{MaterialRoot} / ".autosave" /
		   (Document.GetMaterial().Id.Value + ".material.json.autosave").c_str();
}

void UIMaterialEditorForm::RemoveRecoveryFile(
	const std::filesystem::path& Path
)
{
	if (Path.empty())
	{
		return;
	}
	std::error_code Ignored;
	std::filesystem::remove(Path, Ignored);
}

void UIMaterialEditorForm::ResetPresentationState()
{
	StableToUi.clear();
	UiToStable.clear();
	PositionedNodes.clear();
	NodePropertyDrafts.clear();
	NodeStringDrafts.clear();
	CustomHlslSignatureDrafts.clear();
	ParameterDrafts.clear();
	SyncMaterialDrafts();
}

void UIMaterialEditorForm::SyncMaterialDrafts()
{
	SetTextBuffer(MaterialNameDraft, Document.GetMaterial().Name);
	SetTextBuffer(MaterialTemplateDraft, Document.GetMaterial().HlslTemplate);
}

int UIMaterialEditorForm::UiId(const xr_string_view StableId)
{
	if (const auto Existing = StableToUi.find(xr_string(StableId)); Existing != StableToUi.end())
	{
		return Existing->second;
	}

	u32 Hash = 2166136261u;
	for (const unsigned char Character : StableId)
	{
		Hash ^= Character;
		Hash *= 16777619u;
	}
	int Candidate = static_cast<int>(Hash & 0x7fffffffu);
	if (Candidate == 0)
	{
		Candidate = 1;
	}
	while (UiToStable.contains(Candidate))
	{
		Candidate = Candidate == 0x7fffffff ? 1 : Candidate + 1;
	}

	xr_string Stable(StableId);
	StableToUi.emplace(Stable, Candidate);
	UiToStable.emplace(Candidate, std::move(Stable));
	return Candidate;
}

xr_string UIMaterialEditorForm::MakeStableId()
{
	return GenerateMaterialGuid();
}

const FMaterialGraphPin* UIMaterialEditorForm::PinFromUiId(const int Id) const
{
	const auto Stable = UiToStable.find(Id);
	if (Stable == UiToStable.end())
	{
		return nullptr;
	}
	return FindMaterialGraphPin(Document.GetGraph(), {Stable->second});
}

void UIMaterialEditorForm::SetDiagnostics(xr_vector<FMaterialDiagnostic> InDiagnostics)
{
	Diagnostics = std::move(InDiagnostics);
}
