#include "stdafx.h"
#include "UIMaterialInstanceEditorForm.h"

#include <MaterialEditorDocument.h>
#include <MaterialGraph.h>
#include <MaterialInstanceParentResolver.h>

#include <algorithm>
#include <cctype>
#include <ranges>

namespace
{
using Tiramisu::Editor::FMaterialEditorOperationResult;

template <size_t Size>
void SetTextBuffer(
	xr_array<char, Size>& Buffer,
	const xr_string_view Value
)
{
	Buffer.fill('\0');
	const size_t Count = std::min(Value.size(), Buffer.size() - 1);
	std::ranges::copy_n(Value.begin(), Count, Buffer.begin());
}

xr_string Lower(xr_string_view Value)
{
	xr_string Result(Value);
	std::ranges::transform(
		Result,
		Result.begin(),
		[](const unsigned char Character)
		{
			return static_cast<char>(std::tolower(Character));
		}
	);
	return Result;
}

bool IsRecoveryNewer(
	const std::filesystem::path& RecoveryPath,
	const std::filesystem::path& OriginalPath
)
{
	std::error_code Error;
	if (!std::filesystem::exists(RecoveryPath, Error) || Error)
	{
		return false;
	}
	const auto RecoveryTime =
		std::filesystem::last_write_time(RecoveryPath, Error);
	if (Error)
	{
		return false;
	}
	const auto OriginalTime =
		std::filesystem::last_write_time(OriginalPath, Error);
	return !Error && RecoveryTime >= OriginalTime;
}

FMaterialGraphCompileResult BuildMaterialImplementation(
	const FMaterialAsset& Material,
	FMaterialStaticParameterSet StaticParameters
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
				Options.StaticParameters.emplace(
					Parameter.Id,
					Parameter.DefaultValue
				);
			}
		}
		return CompileMaterialGraph(Material.Implementation.Graph, Options);
	}

	FMaterialGraphCompileResult Result;
	if (Material.Implementation.Source.empty())
	{
		Result.Diagnostics.push_back({
			EMaterialDiagnosticSeverity::Error,
			"editor.hlsl_source_missing",
			"HLSL material implementation source is empty.",
			{},
			{}
		});
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
		Result.Diagnostics.push_back({
			EMaterialDiagnosticSeverity::Error,
			"editor.hlsl_source_open_failed",
			"Cannot open HLSL implementation '" + RelativePath + "'.",
			{},
			{}
		});
		return Result;
	}
	Result.GeneratedHlsl.assign(
		static_cast<const char*>(Reader->pointer()),
		static_cast<size_t>(Reader->length())
	);
	FS.r_close(Reader);
	return Result;
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

UIMaterialInstanceEditorForm::UIMaterialInstanceEditorForm()
{
	SyncDrafts();
	NextAutosaveTime = ImGui::GetTime() + 30.0;
	NextDependencyPollTime = ImGui::GetTime() + 0.5;
	bOpen = false;
}

UIMaterialInstanceEditorForm::~UIMaterialInstanceEditorForm()
{
	ReleasePreview();
}

void UIMaterialInstanceEditorForm::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImGui::SetNextWindowSize(ImVec2(980.0f, 720.0f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin(
		"Material Instance Editor",
		&bOpen,
		ImGuiWindowFlags_MenuBar
	))
	{
		ImGui::End();
		return;
	}

	DrawToolbar();
	TickAutosave();
	TickDependencies();

	const float InspectorWidth = 430.0f;
	ImGui::BeginChild(
		"##MaterialInstanceInspector",
		ImVec2(InspectorWidth, 0.0f),
		true
	);
	DrawInstanceDetails();
	ImGui::Separator();
	DrawOverrides();
	ImGui::EndChild();

	ImGui::SameLine();
	ImGui::BeginChild("##MaterialInstanceOutput", ImVec2(0.0f, 0.0f), true);
	if (ImGui::BeginTabBar("##MaterialInstanceTabs"))
	{
		if (ImGui::BeginTabItem("Preview"))
		{
			DrawPreview();
			ImGui::EndTabItem();
		}
		if (ImGui::BeginTabItem("Diagnostics"))
		{
			DrawDiagnostics();
			ImGui::EndTabItem();
		}
		if (ImGui::BeginTabItem("Instance JSON"))
		{
			DrawJson();
			ImGui::EndTabItem();
		}
		ImGui::EndTabBar();
	}
	ImGui::EndChild();
	ImGui::End();
}

void UIMaterialInstanceEditorForm::DrawToolbar()
{
	if (ImGui::BeginMenuBar())
	{
		ImGui::BeginDisabled(Document.IsDirty());
		if (ImGui::MenuItem("New"))
		{
			NewInstance();
		}
		if (ImGui::MenuItem("Open...", "Ctrl+O"))
		{
			OpenInstance();
		}
		if (ImGui::MenuItem("Recover Autosave..."))
		{
			OpenAutosave();
		}
		ImGui::EndDisabled();

		if (ImGui::MenuItem("Save", "Ctrl+S"))
		{
			SaveInstance(false);
		}
		if (ImGui::MenuItem("Save As...", "Ctrl+Shift+S"))
		{
			SaveInstance(true);
		}

		ImGui::Separator();
		ImGui::BeginDisabled(!Document.CanUndo());
		if (ImGui::MenuItem("Undo", "Ctrl+Z"))
		{
			Document.Undo();
			SyncDrafts();
			(void)ResolveParent();
		}
		ImGui::EndDisabled();
		ImGui::BeginDisabled(!Document.CanRedo());
		if (ImGui::MenuItem("Redo", "Ctrl+Y"))
		{
			Document.Redo();
			SyncDrafts();
			(void)ResolveParent();
		}
		ImGui::EndDisabled();

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

	const ImGuiIO& Io = ImGui::GetIO();
	if (Io.KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_S, false))
	{
		SaveInstance(Io.KeyShift);
	}
	if (Io.KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_O, false) &&
		!Document.IsDirty())
	{
		OpenInstance();
	}
	if (Io.KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_Z, false) &&
		Document.Undo())
	{
		SyncDrafts();
		(void)ResolveParent();
	}
	if (Io.KeyCtrl && ImGui::IsKeyPressed(ImGuiKey_Y, false) &&
		Document.Redo())
	{
		SyncDrafts();
		(void)ResolveParent();
	}
}

void UIMaterialInstanceEditorForm::DrawInstanceDetails()
{
	const FMaterialInstanceAsset& Instance = Document.GetInstance();
	ImGui::TextDisabled("GUID");
	ImGui::TextWrapped("%s", Instance.Id.Value.c_str());
	ImGui::TextDisabled("Source");
	ImGui::TextWrapped(
		"%s",
		Instance.SourcePath.empty() ? "Unsaved" : Instance.SourcePath.c_str()
	);
	ImGui::Separator();

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputText("Name", NameDraft.data(), NameDraft.size());
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		Document.SetName(NameDraft.data());
	}

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputText("Parent", ParentDraft.data(), ParentDraft.size());
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		FMaterialEditorOperationResult Result =
			Document.SetParent(ParentDraft.data());
		if (!Result.Succeeded())
		{
			SetDiagnostics(std::move(Result.Diagnostics));
			SyncDrafts();
		}
		else
		{
			(void)ResolveParent();
		}
	}
	if (ImGui::Button("Choose Parent Asset..."))
	{
		ChooseParent();
	}

	const xr_optional<FMaterialAsset>& Parent = Document.GetParentMaterial();
	if (!Parent)
	{
		ImGui::TextWrapped(
			"Choose a master material before editing typed overrides."
		);
		return;
	}
	ImGui::Text("Master: %s", Parent->Name.c_str());
	ImGui::TextDisabled(
		"Domain=%.*s, Blend=%.*s, Shading=%.*s",
		static_cast<int>(ToString(Parent->Domain).size()),
		ToString(Parent->Domain).data(),
		static_cast<int>(ToString(Parent->BlendMode).size()),
		ToString(Parent->BlendMode).data(),
		static_cast<int>(ToString(Parent->ShadingModel).size()),
		ToString(Parent->ShadingModel).data()
	);
}

void UIMaterialInstanceEditorForm::DrawOverrides()
{
	const xr_optional<FMaterialAsset>& Parent = Document.GetParentMaterial();
	if (!Parent)
	{
		return;
	}

	const FMaterialInstanceAsset& Instance = Document.GetInstance();
	ImGui::Text("Parameter Overrides (%zu)", Parent->Parameters.size());
	for (const FMaterialParameterDefinition& Parameter : Parent->Parameters)
	{
		ImGui::PushID(Parameter.Id.Value.c_str());
		const FMaterialParameterMap& Overrides = Parameter.IsStatic()
			? Instance.StaticOverrides
			: Instance.Overrides;
		bool Enabled = Overrides.contains(Parameter.Id);
		if (ImGui::Checkbox("##Override", &Enabled))
		{
			const FMaterialValue* Inherited = Document.GetInheritedValue(
				Parameter.Id,
				Parameter.IsStatic()
			);
			FMaterialEditorOperationResult Result = Enabled
				? Document.SetOverride(
					Parameter.Id,
					Inherited ? *Inherited : Parameter.DefaultValue,
					Parameter.IsStatic()
				)
				: Document.RemoveOverride(Parameter.Id);
			if (!Result.Succeeded())
			{
				SetDiagnostics(std::move(Result.Diagnostics));
			}
			OverrideDrafts.erase(Parameter.Id.Value);
			StringDrafts.erase(Parameter.Id.Value);
			PreviewDirty = true;
		}
		ImGui::SameLine();
		ImGui::Text(
			"%s [%.*s]%s",
			Parameter.DisplayName.empty()
				? Parameter.Name.c_str()
				: Parameter.DisplayName.c_str(),
			static_cast<int>(ToString(Parameter.Type).size()),
			ToString(Parameter.Type).data(),
			Parameter.IsStatic() ? " (permutation)" : ""
		);

		const FMaterialParameterMap& CurrentOverrides = Parameter.IsStatic()
			? Instance.StaticOverrides
			: Instance.Overrides;
		const auto Current = CurrentOverrides.find(Parameter.Id);
		if (Current != CurrentOverrides.end())
		{
			FMaterialValue& Draft = OverrideDrafts.try_emplace(
				Parameter.Id.Value,
				Current->second
			).first->second;
			bool Commit = false;
			ImGui::SetNextItemWidth(-1.0f);
			if (float* Value = std::get_if<float>(&Draft))
			{
				ImGui::DragFloat(
					"##Value",
					Value,
					0.01f,
					Parameter.Minimum.value_or(0.0f),
					Parameter.Maximum.value_or(0.0f)
				);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
			}
			else if (FFloat2* Value = std::get_if<FFloat2>(&Draft))
			{
				ImGui::DragFloat2("##Value", Value->data(), 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
			}
			else if (FFloat3* Value = std::get_if<FFloat3>(&Draft))
			{
				ImGui::DragFloat3("##Value", Value->data(), 0.01f);
				Commit = ImGui::IsItemDeactivatedAfterEdit();
			}
			else if (FFloat4* Value = std::get_if<FFloat4>(&Draft))
			{
				if (Parameter.Type == EMaterialParameterType::Color)
				{
					ImGui::ColorEdit4("##Value", Value->data());
				}
				else
				{
					ImGui::DragFloat4("##Value", Value->data(), 0.01f);
				}
				Commit = ImGui::IsItemDeactivatedAfterEdit();
			}
			else if (bool* Value = std::get_if<bool>(&Draft))
			{
				Commit = ImGui::Checkbox("Value", Value);
			}
			else if (s32* Value = std::get_if<s32>(&Draft))
			{
				int EditorValue = *Value;
				if (ImGui::DragInt("##Value", &EditorValue, 1.0f))
				{
					*Value = EditorValue;
				}
				Commit = ImGui::IsItemDeactivatedAfterEdit();
			}
			else if (const xr_string* Value = std::get_if<xr_string>(&Draft))
			{
				auto [Text, Inserted] =
					StringDrafts.try_emplace(Parameter.Id.Value);
				if (Inserted)
				{
					SetTextBuffer(Text->second, *Value);
				}
				ImGui::InputText(
					"##Value",
					Text->second.data(),
					Text->second.size()
				);
				if (ImGui::IsItemDeactivatedAfterEdit())
				{
					Draft = xr_string{Text->second.data()};
					Commit = true;
				}
			}

			if (Commit)
			{
				FMaterialEditorOperationResult Result = Document.SetOverride(
					Parameter.Id,
					Draft,
					Parameter.IsStatic()
				);
				if (!Result.Succeeded())
				{
					SetDiagnostics(std::move(Result.Diagnostics));
				}
				PreviewDirty = true;
			}
		}
		ImGui::Separator();
		ImGui::PopID();
	}
}

void UIMaterialInstanceEditorForm::DrawPreview()
{
	constexpr const char* PrimitiveNames[] = {"Sphere", "Cube", "Plane"};
	constexpr const char* EnvironmentNames[] = {
		"Studio",
		"Neutral",
		"Outdoor"
	};

	int Primitive = static_cast<int>(PreviewPrimitive);
	ImGui::SetNextItemWidth(120.0f);
	if (ImGui::Combo(
		"Primitive",
		&Primitive,
		PrimitiveNames,
		static_cast<int>(std::size(PrimitiveNames))
	))
	{
		PreviewPrimitive = static_cast<EMaterialPreviewPrimitive>(Primitive);
		PreviewDirty = true;
	}
	ImGui::SetNextItemWidth(120.0f);
	if (ImGui::Combo(
		"Environment",
		&PreviewEnvironment,
		EnvironmentNames,
		static_cast<int>(std::size(EnvironmentNames))
	))
	{
		PreviewDirty = true;
	}

	const xr_optional<FMaterialAsset>& Parent = Document.GetParentMaterial();
	if (!Parent)
	{
		ImGui::TextWrapped("Parent master is not resolved.");
		return;
	}

	IMaterialPreviewRenderer& ActiveRenderer = GetMaterialPreviewRenderer();
	if (PreviewRenderer && PreviewRenderer != &ActiveRenderer)
	{
		ReleasePreview();
	}
	if (!ActiveRenderer.IsAvailable())
	{
		const FMaterialPreviewFrame Frame = ActiveRenderer.GetPreviewFrame({});
		ImGui::TextWrapped(
			"%.*s",
			static_cast<int>(Frame.Diagnostic.size()),
			Frame.Diagnostic.data()
		);
		return;
	}
	if (!PreviewHandle.IsValid())
	{
		PreviewRenderer = &ActiveRenderer;
		PreviewHandle = PreviewRenderer->CreatePreview();
		PreviewDirty = true;
	}
	if (!PreviewHandle.IsValid())
	{
		ImGui::TextWrapped("Cannot allocate material instance preview.");
		return;
	}

	const xr_string MaterialJson = SerializeMaterialAssetJson(*Parent);
	const xr_string InstanceJson = Document.SerializeFlattenedInstance();
	PreviewDirty |= MaterialJson != SubmittedMaterialJson ||
		InstanceJson != SubmittedInstanceJson;
	if (PreviewDirty)
	{
		FMaterialGraphCompileResult Implementation = BuildMaterialImplementation(
			*Parent,
			Document.GetEffectiveStaticParameters()
		);
		if (!Implementation.Diagnostics.empty())
		{
			Diagnostics = Implementation.Diagnostics;
		}
		SubmittedMaterialJson = MaterialJson;
		SubmittedInstanceJson = InstanceJson;
		SubmittedHlsl = std::move(Implementation.GeneratedHlsl);

		FMaterialPreviewSource Source;
		Source.MaterialAssetId = Document.GetInstance().Id.Value;
		Source.MaterialJson = SubmittedMaterialJson;
		Source.MaterialInstanceJson = SubmittedInstanceJson;
		Source.GeneratedHlsl = SubmittedHlsl;
		Source.Environment = EnvironmentNames[PreviewEnvironment];
		Source.Primitive = PreviewPrimitive;
		Source.Revision = ++PreviewRevision;
		PreviewRenderer->UpdatePreview(PreviewHandle, Source);
		PreviewDirty = false;
	}

	ImGui::Separator();
	const ImVec2 Available = ImGui::GetContentRegionAvail();
	const u32 Width = static_cast<u32>(std::max(1.0f, Available.x));
	const u32 Height = static_cast<u32>(
		std::max(1.0f, Available.y - 24.0f)
	);
	if (Width != PreviewWidth || Height != PreviewHeight)
	{
		PreviewWidth = Width;
		PreviewHeight = Height;
		PreviewRenderer->ResizePreview(PreviewHandle, Width, Height);
	}
	PreviewRenderer->RenderPreview(PreviewHandle, ImGui::GetIO().DeltaTime);
	const FMaterialPreviewFrame Frame =
		PreviewRenderer->GetPreviewFrame(PreviewHandle);
	if (!Frame.Diagnostic.empty())
	{
		ImGui::TextWrapped(
			"%.*s",
			static_cast<int>(Frame.Diagnostic.size()),
			Frame.Diagnostic.data()
		);
	}
	if (Frame.Surface.IsValid())
	{
		ImGui::Image(
			Frame.Surface.ImGuiTextureId,
			ImVec2(static_cast<float>(Width), static_cast<float>(Height))
		);
	}
}

void UIMaterialInstanceEditorForm::DrawDiagnostics()
{
	if (Diagnostics.empty())
	{
		ImGui::TextColored(
			ImVec4(0.4f, 0.9f, 0.5f, 1.0f),
			"Instance is valid"
		);
	}
	for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
	{
		const ImVec4 Color =
			Diagnostic.Severity == EMaterialDiagnosticSeverity::Error
			? ImVec4(1.0f, 0.35f, 0.3f, 1.0f)
			: Diagnostic.Severity == EMaterialDiagnosticSeverity::Warning
			? ImVec4(1.0f, 0.75f, 0.25f, 1.0f)
			: ImVec4(0.5f, 0.75f, 1.0f, 1.0f);
		ImGui::TextColored(
			Color,
			"%s: %s",
			SeverityName(Diagnostic.Severity),
			Diagnostic.Code.c_str()
		);
		ImGui::TextWrapped("%s", Diagnostic.Message.c_str());
		ImGui::Separator();
	}
}

void UIMaterialInstanceEditorForm::DrawJson()
{
	const xr_string Json = Document.SerializeInstance();
	ImGui::InputTextMultiline(
		"##MaterialInstanceJson",
		const_cast<char*>(Json.c_str()),
		Json.size() + 1,
		ImVec2(-1.0f, -1.0f),
		ImGuiInputTextFlags_ReadOnly
	);
}

void UIMaterialInstanceEditorForm::TickAutosave()
{
	const double Now = ImGui::GetTime();
	if (Now < NextAutosaveTime)
	{
		return;
	}
	NextAutosaveTime = Now + 30.0;
	if (!Document.IsDirty())
	{
		return;
	}

	const std::filesystem::path Path = RecoveryPath();
	std::error_code Error;
	std::filesystem::create_directories(Path.parent_path(), Error);
	if (Error)
	{
		AutosaveStatus = "Autosave directory failed";
		return;
	}
	FMaterialEditorOperationResult Result = Document.SaveRecoveryFile(Path);
	if (!Result.Succeeded())
	{
		AutosaveStatus = "Autosave failed";
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}
	AutosaveStatus = "Autosaved recovery";
}

void UIMaterialInstanceEditorForm::TickDependencies()
{
	const double Now = ImGui::GetTime();
	if (Now < NextDependencyPollTime)
	{
		return;
	}
	NextDependencyPollTime = Now + 0.5;
	const auto Changes = DependencyWatcher.Poll();
	if (Changes.empty())
	{
		return;
	}
	if (Document.IsDirty())
	{
		DependencyStatus = "External changes pending; local edits are protected";
		return;
	}

	const std::filesystem::path Source(
		Document.GetInstance().SourcePath.c_str()
	);
	if (!Source.empty())
	{
		FMaterialEditorOperationResult Result = Document.OpenInstanceFile(Source);
		if (!Result.Succeeded())
		{
			SetDiagnostics(std::move(Result.Diagnostics));
			DependencyStatus = "External reload failed";
			return;
		}
	}
	SyncDrafts();
	(void)ResolveParent();
	DependencyStatus = "External dependencies reloaded";
}

void UIMaterialInstanceEditorForm::RefreshDependencies()
{
	xr_vector<std::filesystem::path> Dependencies = ParentDependencies;
	const std::filesystem::path Source(
		Document.GetInstance().SourcePath.c_str()
	);
	if (!Source.empty())
	{
		Dependencies.push_back(Source);
	}
	DependencyWatcher.Reset(Dependencies);
	NextDependencyPollTime = ImGui::GetTime() + 0.5;
}

void UIMaterialInstanceEditorForm::NewInstance()
{
	Document.NewInstance();
	Diagnostics.clear();
	ParentDependencies.clear();
	SyncDrafts();
	RefreshDependencies();
	PreviewDirty = true;
}

void UIMaterialInstanceEditorForm::OpenInstance()
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	xr_string Path;
	if (EFS.GetOpenName(
		"$game_data$",
		Path,
		false,
		MaterialRoot,
		-1,
		"*.material-instance.json"
	))
	{
		(void)OpenInstanceFile(Path.c_str());
	}
}

void UIMaterialInstanceEditorForm::OpenAutosave()
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	xr_string Path;
	if (!EFS.GetOpenName(
		"$game_data$",
		Path,
		false,
		MaterialRoot,
		-1,
		"*.material-instance.json.autosave"
	))
	{
		return;
	}
	FMaterialEditorOperationResult Result = Document.OpenRecoveryFile(Path.c_str());
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}
	SyncDrafts();
	(void)ResolveParent();
	AutosaveStatus = "Recovered autosave (unsaved)";
}

void UIMaterialInstanceEditorForm::SaveInstance(const bool SaveAs)
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	xr_string Path = Document.GetInstance().SourcePath.c_str();
	if (SaveAs || Path.empty())
	{
		if (!EFS.GetSaveName(
			"$game_data$",
			Path,
			MaterialRoot,
			-1,
			"*.material-instance.json"
		))
		{
			return;
		}
		if (!Lower(Path).ends_with(".material-instance.json"))
		{
			Path += ".material-instance.json";
		}
	}

	const std::filesystem::path OldRecovery = RecoveryPath();
	FMaterialEditorOperationResult Result = Document.SaveInstanceFile(Path.c_str());
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return;
	}
	std::error_code Ignored;
	std::filesystem::remove(OldRecovery, Ignored);
	std::filesystem::remove(RecoveryPath(), Ignored);
	AutosaveStatus = "Saved; recovery cleared";
	SetDiagnostics(std::move(Result.Diagnostics));
	RefreshDependencies();
}

void UIMaterialInstanceEditorForm::ChooseParent()
{
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	xr_string Path;
	if (EFS.GetOpenName(
		"$game_data$",
		Path,
		false,
		MaterialRoot,
		-1,
		"*.material.json;*.material-instance.json"
	))
	{
		(void)SetParentFromFile(Path.c_str());
	}
}

bool UIMaterialInstanceEditorForm::SetParentFromFile(
	const std::filesystem::path& ParentPath
)
{
	FMaterialEditorOperationResult Result;
	xr_string ParentReference;
	xr_optional<FMaterialAsset> Master;
	if (Lower(ParentPath.filename().string()).ends_with(
		".material-instance.json"
	))
	{
		Tiramisu::Editor::TiramisuMaterialInstanceEditorDocument ParentDocument;
		Result = ParentDocument.OpenInstanceFile(ParentPath);
		if (Result.Succeeded())
		{
			ParentReference = ParentDocument.GetInstance().Id.Value;
		}
	}
	else
	{
		Tiramisu::Editor::TiramisuMaterialEditorDocument ParentDocument;
		Result = ParentDocument.OpenMaterialFile(ParentPath);
		if (Result.Succeeded())
		{
			ParentReference = ParentDocument.GetMaterial().Id.Value;
			Master = ParentDocument.GetMaterial();
		}
	}
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		return false;
	}

	Result = Document.SetParent(std::move(ParentReference));
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		SyncDrafts();
		return false;
	}
	if (Master)
	{
		Document.SetParentMaterial(std::move(*Master));
		ParentDependencies = {ParentPath};
		SetDiagnostics(std::move(Result.Diagnostics));
	}
	else if (!ResolveParent())
	{
		return false;
	}
	SyncDrafts();
	RefreshDependencies();
	PreviewDirty = true;
	return true;
}

bool UIMaterialInstanceEditorForm::ResolveParent()
{
	if (Document.GetInstance().Parent.empty())
	{
		Document.ClearParentMaterial();
		ParentDependencies.clear();
		RefreshDependencies();
		PreviewDirty = true;
		return false;
	}

	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	auto Resolution = Tiramisu::Editor::ResolveMaterialInstanceParent(
		MaterialRoot,
		Document.GetInstance()
	);
	bool Succeeded = Resolution.Succeeded();
	xr_vector<FMaterialDiagnostic> ResolutionDiagnostics =
		std::move(Resolution.Diagnostics);
	if (Succeeded)
	{
		ParentDependencies = std::move(Resolution.AssetDependencies);
		FMaterialEditorOperationResult Applied = Document.SetParentResolution(
			std::move(Resolution.Master),
			std::move(Resolution.Parent)
		);
		Succeeded = Applied.Succeeded();
		ResolutionDiagnostics.insert(
			ResolutionDiagnostics.end(),
			Applied.Diagnostics.begin(),
			Applied.Diagnostics.end()
		);
	}
	SetDiagnostics(std::move(ResolutionDiagnostics));
	OverrideDrafts.clear();
	StringDrafts.clear();
	RefreshDependencies();
	PreviewDirty = true;
	return Succeeded;
}

bool UIMaterialInstanceEditorForm::OpenInstanceFile(
	const std::filesystem::path& InstancePath
)
{
	const std::filesystem::path CurrentPath(
		Document.GetInstance().SourcePath.c_str()
	);
	if (Document.IsDirty() && CurrentPath != InstancePath)
	{
		Show();
		AutosaveStatus =
			"Save or discard the active instance before opening another one";
		return false;
	}

	std::filesystem::path AutosavePath = InstancePath;
	AutosavePath += ".autosave";
	const bool Restore = IsRecoveryNewer(AutosavePath, InstancePath) &&
		ELog.DlgMsg(
			mtConfirmation,
			mbYes | mbNo,
			"A newer Material Instance autosave exists. Restore it?"
		) == mrYes;
	FMaterialEditorOperationResult Result = Restore
		? Document.OpenRecoveryFile(AutosavePath, InstancePath)
		: Document.OpenInstanceFile(InstancePath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		Show();
		return false;
	}
	SetDiagnostics(std::move(Result.Diagnostics));
	SyncDrafts();
	(void)ResolveParent();
	AutosaveStatus = Restore ? "Recovered autosave (unsaved)" : xr_string{};
	NextAutosaveTime = ImGui::GetTime() + 30.0;
	Show();
	return true;
}

bool UIMaterialInstanceEditorForm::CreateInstanceFromMaster(
	const std::filesystem::path& MasterPath,
	const std::filesystem::path& InstancePath
)
{
	if (Document.IsDirty())
	{
		Show();
		AutosaveStatus =
			"Save or discard the active instance before creating another one";
		return false;
	}
	Document.NewInstance();
	if (!SetParentFromFile(MasterPath))
	{
		Show();
		return false;
	}
	FMaterialEditorOperationResult Result =
		Document.SaveInstanceFile(InstancePath);
	if (!Result.Succeeded())
	{
		SetDiagnostics(std::move(Result.Diagnostics));
		Show();
		return false;
	}
	SetDiagnostics(std::move(Result.Diagnostics));
	SyncDrafts();
	RefreshDependencies();
	PreviewDirty = true;
	Show();
	return true;
}

void UIMaterialInstanceEditorForm::SyncDrafts()
{
	SetTextBuffer(NameDraft, Document.GetInstance().Name);
	SetTextBuffer(ParentDraft, Document.GetInstance().Parent);
	OverrideDrafts.clear();
	StringDrafts.clear();
}

void UIMaterialInstanceEditorForm::ReleasePreview()
{
	if (PreviewRenderer && PreviewHandle.IsValid())
	{
		PreviewRenderer->DestroyPreview(PreviewHandle);
	}
	PreviewRenderer = nullptr;
	PreviewHandle = {};
	PreviewWidth = 0;
	PreviewHeight = 0;
	PreviewDirty = true;
	SubmittedMaterialJson.clear();
	SubmittedInstanceJson.clear();
	SubmittedHlsl.clear();
}

void UIMaterialInstanceEditorForm::SetDiagnostics(
	xr_vector<FMaterialDiagnostic> InDiagnostics
)
{
	Diagnostics = std::move(InDiagnostics);
}

std::filesystem::path UIMaterialInstanceEditorForm::RecoveryPath() const
{
	if (!Document.GetInstance().SourcePath.empty())
	{
		std::filesystem::path Result(
			Document.GetInstance().SourcePath.c_str()
		);
		Result += ".autosave";
		return Result;
	}
	string_path MaterialRoot{};
	FS.update_path(MaterialRoot, "$game_render_materials$", "");
	return std::filesystem::path(MaterialRoot) / ".autosave" /
		(Document.GetInstance().Id.Value +
		 ".material-instance.json.autosave").c_str();
}
