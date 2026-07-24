#include "stdafx.h"
#include "imgui_internal.h"
#include "../Renderer/Tiramisu/TiramisuEditorNativeScene.h"

namespace
{
template <size_t Size>
void CopyDraft(xr_array<char, Size>& Target, const xr_string_view Source)
{
	Target.fill('\0');
	const size_t Count = std::min(Source.size(), Size - 1);
	std::copy_n(Source.data(), Count, Target.data());
}

struct FNativeMaterialSlotDraft
{
	u32 MaterialSlot = 0;
	xr_array<char, 512> Material = {};
	bool HasOverride = false;
	bool TwoSided = false;
};

struct FNativeDetailsUiState
{
	u64 Revision = 0;
	xr_string ComponentId;
	xr_array<char, 256> Name = {};
	xr_array<float, 3> Position = {};
	bool Visible = true;
	bool PositionTransaction = false;
	FEditorNativeSceneComponentDetails Details;
	xr_vector<FNativeMaterialSlotDraft> MaterialSlots;
};

struct FNativeBulkMaterialSlotDraft
{
	u32 MaterialSlot = 0;
	xr_array<char, 512> Material = {};
	bool MaterialMixed = false;
	bool TwoSided = false;
	bool TwoSidedMixed = false;
	bool TwoSidedEdited = false;
};

struct FNativeBulkDetailsUiState
{
	u64 Revision = 0;
	FEditorNativeSceneBulkMaterialDetails Details;
	xr_vector<FNativeBulkMaterialSlotDraft> MaterialSlots;
};

struct FNativeLightDetailsUiState
{
	u64 Revision = 0;
	xr_string LightId;
	xr_array<char, 256> Name = {};
	FEditorNativeSceneLightDetails Details;
	bool PropertyTransaction = false;
};

FNativeDetailsUiState& GetNativeDetailsUiState()
{
	static FNativeDetailsUiState State;
	return State;
}

FNativeBulkDetailsUiState& GetNativeBulkDetailsUiState()
{
	static FNativeBulkDetailsUiState State;
	return State;
}

FNativeLightDetailsUiState& GetNativeLightDetailsUiState()
{
	static FNativeLightDetailsUiState State;
	return State;
}

void RefreshNativeDetailsUiState(
	const TiramisuEditorNativeSceneDocument& Document,
	const FEditorNativeSceneComponentDetails& Details)
{
	FNativeDetailsUiState& State = GetNativeDetailsUiState();
	State.Revision = Document.GetRevision();
	State.ComponentId = Details.Id;
	CopyDraft(State.Name, Details.Name);
	State.Position = Details.Position;
	State.Visible = Details.Visible;
	State.Details = Details;
	State.MaterialSlots.clear();
	State.MaterialSlots.reserve(Details.MaterialSlots.size());
	for (const FEditorNativeSceneMaterialSlotDetails& Slot :
		Details.MaterialSlots)
	{
		FNativeMaterialSlotDraft Draft;
		Draft.MaterialSlot = Slot.MaterialSlot;
		Draft.HasOverride = Slot.HasOverride;
		Draft.TwoSided = Slot.HasOverride
			? Slot.OverrideTwoSided : Slot.BaseTwoSided;
		CopyDraft(Draft.Material, Slot.HasOverride
			? Slot.OverrideMaterial : Slot.BaseMaterial);
		State.MaterialSlots.push_back(std::move(Draft));
	}
}

void RefreshNativeBulkDetailsUiState(
	const TiramisuEditorNativeSceneDocument& Document,
	const FEditorNativeSceneBulkMaterialDetails& Details)
{
	FNativeBulkDetailsUiState& State = GetNativeBulkDetailsUiState();
	State.Revision = Document.GetRevision();
	State.Details = Details;
	State.MaterialSlots.clear();
	State.MaterialSlots.reserve(Details.MaterialSlots.size());
	for (const FEditorNativeSceneBulkMaterialSlotDetails& Slot :
		Details.MaterialSlots)
	{
		FNativeBulkMaterialSlotDraft Draft;
		Draft.MaterialSlot = Slot.MaterialSlot;
		if (Slot.OverrideCount == Slot.ComponentCount &&
			!Slot.OverrideMaterialMixed)
		{
			CopyDraft(Draft.Material, Slot.OverrideMaterial);
		}
		else if (Slot.OverrideCount == 0 &&
			!Slot.BaseMaterialMixed)
		{
			CopyDraft(Draft.Material, Slot.BaseMaterial);
		}
		else
		{
			Draft.MaterialMixed = true;
		}

		if (Slot.OverrideCount == Slot.ComponentCount &&
			!Slot.OverrideTwoSidedMixed)
		{
			Draft.TwoSided = Slot.OverrideTwoSided;
		}
		else if (Slot.OverrideCount == 0 &&
			!Slot.BaseTwoSidedMixed)
		{
			Draft.TwoSided = Slot.BaseTwoSided;
		}
		else
		{
			Draft.TwoSided = Slot.OverrideCount != 0
				? Slot.OverrideTwoSided : Slot.BaseTwoSided;
			Draft.TwoSidedMixed = true;
		}
		State.MaterialSlots.push_back(std::move(Draft));
	}
}

void RefreshNativeLightDetailsUiState(
	const TiramisuEditorNativeSceneDocument& Document,
	const FEditorNativeSceneLightDetails& Details)
{
	FNativeLightDetailsUiState& State =
		GetNativeLightDetailsUiState();
	State.Revision = Document.GetRevision();
	State.LightId = Details.Id;
	CopyDraft(State.Name, Details.Name);
	State.Details = Details;
}

void ReportNativeDetailsError(const xr_string& Diagnostic)
{
	Msg("! Native scene details: %s", Diagnostic.c_str());
	UI->SetStatus("Native scene property change failed. See log.");
}

void DrawNativeBulkMaterialProperties(
	TiramisuEditorNativeSceneDocument& Document)
{
	const xr_optional<FEditorNativeSceneBulkMaterialDetails> Details =
		Document.GetSelectedComponentsMaterialDetails();
	if (!Details)
	{
		ImGui::TextDisabled("Selected components are unresolved.");
		return;
	}

	FNativeBulkDetailsUiState& State =
		GetNativeBulkDetailsUiState();
	if (State.Revision != Document.GetRevision() ||
		State.Details.ComponentCount != Details->ComponentCount)
	{
		RefreshNativeBulkDetailsUiState(Document, *Details);
	}

	ImGui::TextDisabled("%zu components selected. Material edits apply "
		"to all.", State.Details.ComponentCount);
	ImGui::SeparatorText("Common material slots");
	if (State.Details.MaterialSlots.empty())
	{
		ImGui::TextDisabled(
			"The selection has no common material slots.");
		return;
	}

	const bool Editable = Document.IsEditableRenderScene();
	ImGui::BeginDisabled(!Editable);
	for (size_t Index = 0;
		Index < State.Details.MaterialSlots.size(); ++Index)
	{
		const FEditorNativeSceneBulkMaterialSlotDetails& Slot =
			State.Details.MaterialSlots[Index];
		FNativeBulkMaterialSlotDraft& Draft =
			State.MaterialSlots[Index];
		ImGui::PushID(static_cast<int>(Slot.MaterialSlot));
		const xr_string Header =
			xr_string(std::to_string(Slot.MaterialSlot)) + ": " +
			(Slot.NameMixed ? "<multiple slot names>" : Slot.Name);
		if (ImGui::CollapsingHeader(Header.c_str(),
				ImGuiTreeNodeFlags_DefaultOpen))
		{
			ImGui::TextWrapped("Base: %s",
				Slot.BaseMaterialMixed
					? "<multiple materials>"
					: Slot.BaseMaterial.c_str());
			if (Slot.OverrideCount == 0)
			{
				ImGui::TextDisabled("Overrides: none");
			}
			else if (Slot.OverrideCount == Slot.ComponentCount)
			{
				ImGui::TextDisabled("Overrides: all%s",
					Slot.OverrideMaterialMixed
						? " (mixed values)" : "");
			}
			else
			{
				ImGui::TextDisabled("Overrides: %zu of %zu",
					Slot.OverrideCount, Slot.ComponentCount);
			}

			const char* MaterialHint = Draft.MaterialMixed
				? "<enter one material for all selected>"
				: "Material/MaterialInstance GUID or path";
			if (ImGui::InputTextWithHint("Material", MaterialHint,
					Draft.Material.data(), Draft.Material.size()))
			{
				Draft.MaterialMixed = false;
			}

			ImGui::PushItemFlag(
				ImGuiItemFlags_MixedValue, Draft.TwoSidedMixed);
			if (ImGui::Checkbox("Two sided", &Draft.TwoSided))
			{
				Draft.TwoSidedMixed = false;
				Draft.TwoSidedEdited = true;
			}
			ImGui::PopItemFlag();

			if (ImGui::Button("Apply override to all"))
			{
				xr_string Diagnostic;
				const xr_optional<bool> TwoSided =
					Draft.TwoSidedEdited
					? xr_optional<bool>(Draft.TwoSided)
					: std::nullopt;
				if (!Document.SetSelectedComponentsMaterialOverride(
						Draft.MaterialSlot, Draft.Material.data(),
						TwoSided, Diagnostic))
				{
					ReportNativeDetailsError(Diagnostic);
				}
				State.Revision = 0;
			}
			ImGui::SameLine();
			ImGui::BeginDisabled(Slot.OverrideCount == 0);
			if (ImGui::Button("Clear overrides"))
			{
				xr_string Diagnostic;
				if (!Document.ClearSelectedMaterialOverride(
						Draft.MaterialSlot, Diagnostic))
				{
					ReportNativeDetailsError(Diagnostic);
				}
				State.Revision = 0;
			}
			ImGui::EndDisabled();
			if (Draft.TwoSidedMixed && !Draft.TwoSidedEdited)
			{
				ImGui::TextDisabled("Mixed Two sided values are "
					"preserved until the checkbox is changed.");
			}
		}
		ImGui::PopID();
	}
	ImGui::EndDisabled();
}

void DrawNativeLightProperties(
	TiramisuEditorNativeSceneDocument& Document,
	const FEditorNativeSceneLightDetails& Details)
{
	FNativeLightDetailsUiState& State =
		GetNativeLightDetailsUiState();
	if (State.LightId != Details.Id)
	{
		if (State.PropertyTransaction)
			(void)Document.EndEditTransaction();
		State.PropertyTransaction = false;
		RefreshNativeLightDetailsUiState(Document, Details);
	}
	else if (!State.PropertyTransaction &&
		State.Revision != Document.GetRevision())
	{
		RefreshNativeLightDetailsUiState(Document, Details);
	}

	const auto Apply = [&]()
	{
		State.Details.Name = State.Name.data();
		xr_string Diagnostic;
		if (!Document.SetSelectedLightDetails(
				State.Details, Diagnostic))
		{
			ReportNativeDetailsError(Diagnostic);
			State.Revision = 0;
			return false;
		}
		State.Revision = Document.GetRevision();
		return true;
	};
	const auto BeginContinuousEdit = [&]()
	{
		if (ImGui::IsItemActivated() &&
			!State.PropertyTransaction)
		{
			State.PropertyTransaction =
				Document.BeginEditTransaction();
		}
	};
	const auto EndContinuousEdit = [&]()
	{
		if (ImGui::IsItemDeactivated() &&
			State.PropertyTransaction)
		{
			(void)Document.EndEditTransaction();
			State.PropertyTransaction = false;
			State.Revision = 0;
		}
	};

	const bool Editable = Document.IsEditableRenderScene();
	ImGui::BeginDisabled(!Editable);
	if (ImGui::InputText("Name", State.Name.data(), State.Name.size(),
			ImGuiInputTextFlags_EnterReturnsTrue))
	{
		(void)Apply();
		State.Revision = 0;
	}
	if (ImGui::Checkbox("Visible", &State.Details.Visible))
	{
		(void)Apply();
		State.Revision = 0;
	}
	if (ImGui::Checkbox(
			"Cast shadows", &State.Details.CastShadows))
	{
		(void)Apply();
		State.Revision = 0;
	}

	const char* LightType = "Point";
	switch (State.Details.Type)
	{
	case Tiramisu::Scene::ELightType::Directional:
		LightType = "Directional";
		break;
	case Tiramisu::Scene::ELightType::Point:
		LightType = "Point";
		break;
	case Tiramisu::Scene::ELightType::Spot:
		LightType = "Spot";
		break;
	}
	if (ImGui::BeginCombo("Type", LightType))
	{
		const auto TypeItem =
			[&](const char* Label,
				const Tiramisu::Scene::ELightType Type)
			{
				const bool Selected = State.Details.Type == Type;
				if (ImGui::Selectable(Label, Selected))
				{
					State.Details.Type = Type;
					(void)Apply();
					State.Revision = 0;
				}
				if (Selected)
					ImGui::SetItemDefaultFocus();
			};
		TypeItem("Directional",
			Tiramisu::Scene::ELightType::Directional);
		TypeItem("Point", Tiramisu::Scene::ELightType::Point);
		TypeItem("Spot", Tiramisu::Scene::ELightType::Spot);
		ImGui::EndCombo();
	}

	const bool PositionChanged = ImGui::DragFloat3(
		"Position", State.Details.Position.data(), 0.01f);
	BeginContinuousEdit();
	if (PositionChanged)
		(void)Apply();
	EndContinuousEdit();

	const bool ColorChanged = ImGui::ColorEdit3(
		"Color", State.Details.Color.data(),
		ImGuiColorEditFlags_Float | ImGuiColorEditFlags_HDR);
	BeginContinuousEdit();
	if (ColorChanged)
		(void)Apply();
	EndContinuousEdit();

	const bool IntensityChanged = ImGui::DragFloat(
		"Intensity", &State.Details.Intensity, 0.05f,
		0.0f, 1000000.0f, "%.3f",
		ImGuiSliderFlags_AlwaysClamp);
	BeginContinuousEdit();
	if (IntensityChanged)
		(void)Apply();
	EndContinuousEdit();

	if (State.Details.Type !=
		Tiramisu::Scene::ELightType::Directional)
	{
		const bool RangeChanged = ImGui::DragFloat(
			"Range", &State.Details.Range, 0.1f,
			0.001f, 1000000.0f, "%.3f",
			ImGuiSliderFlags_AlwaysClamp);
		BeginContinuousEdit();
		if (RangeChanged)
			(void)Apply();
		EndContinuousEdit();
	}
	if (State.Details.Type == Tiramisu::Scene::ELightType::Spot)
	{
		bool ConeChanged = ImGui::DragFloat(
			"Inner cone", &State.Details.InnerConeAngleDegrees,
			0.1f, 0.0f, 89.0f, "%.2f deg",
			ImGuiSliderFlags_AlwaysClamp);
		BeginContinuousEdit();
		if (ConeChanged)
		{
			State.Details.InnerConeAngleDegrees = std::min(
				State.Details.InnerConeAngleDegrees,
				State.Details.OuterConeAngleDegrees);
			(void)Apply();
		}
		EndContinuousEdit();

		ConeChanged = ImGui::DragFloat(
			"Outer cone", &State.Details.OuterConeAngleDegrees,
			0.1f, 0.01f, 89.9f, "%.2f deg",
			ImGuiSliderFlags_AlwaysClamp);
		BeginContinuousEdit();
		if (ConeChanged)
		{
			State.Details.OuterConeAngleDegrees = std::max(
				State.Details.OuterConeAngleDegrees,
				State.Details.InnerConeAngleDegrees);
			(void)Apply();
		}
		EndContinuousEdit();
	}
	ImGui::EndDisabled();
	ImGui::TextDisabled(
		"Native renderer-neutral LightComponent (RenderScene v2).");
}

void DrawNativeProperties()
{
	TiramisuEditorNativeSceneDocument& Document =
		GetEditorNativeSceneDocument();
	const std::filesystem::path& SourcePath = Document.GetSourcePath();
	ImGui::TextUnformatted("Native RenderScene");
	ImGui::TextDisabled("%s", SourcePath.empty()
		? "Unsaved" : SourcePath.generic_string().c_str());
	ImGui::Separator();

	if (Document.GetSelectionCount() == 0)
	{
		ImGui::TextDisabled("Select a native scene object.");
		return;
	}
	if (Document.GetSelectionCount() != 1)
	{
		DrawNativeBulkMaterialProperties(Document);
		return;
	}
	const xr_optional<FEditorNativeSceneLightDetails> LightDetails =
		Document.GetSingleSelectedLightDetails();
	if (LightDetails)
	{
		DrawNativeLightProperties(Document, *LightDetails);
		return;
	}
	const xr_optional<FEditorNativeSceneComponentDetails> Details =
		Document.GetSingleSelectedComponentDetails();
	if (!Details)
	{
		ImGui::TextDisabled("Selected component is unresolved.");
		return;
	}

	FNativeDetailsUiState& State = GetNativeDetailsUiState();
	if (State.ComponentId != Details->Id)
	{
		if (State.PositionTransaction)
			(void)Document.EndEditTransaction();
		State.PositionTransaction = false;
		RefreshNativeDetailsUiState(Document, *Details);
	}
	else if (!State.PositionTransaction &&
		State.Revision != Document.GetRevision())
	{
		RefreshNativeDetailsUiState(Document, *Details);
	}

	const bool Editable = Document.IsEditableRenderScene();
	ImGui::BeginDisabled(!Editable);
	if (ImGui::InputText("Name", State.Name.data(), State.Name.size(),
			ImGuiInputTextFlags_EnterReturnsTrue))
	{
		xr_string Diagnostic;
		if (!Document.SetSelectedComponentName(State.Name.data(),
				Diagnostic))
		{
			ReportNativeDetailsError(Diagnostic);
		}
		State.Revision = 0;
	}
	if (ImGui::Checkbox("Visible", &State.Visible))
	{
		if (!Document.SetSelectedComponentVisibility(State.Visible))
			ReportNativeDetailsError(
				"Cannot update component visibility.");
		State.Revision = 0;
	}

	const bool PositionChanged = ImGui::DragFloat3(
		"Position", State.Position.data(), 0.01f);
	if (ImGui::IsItemActivated() && !State.PositionTransaction)
	{
		State.PositionTransaction = Document.BeginEditTransaction();
	}
	if (PositionChanged)
	{
		if (!Document.SetSelectedComponentPosition(State.Position))
			ReportNativeDetailsError("Cannot update component position.");
		State.Revision = Document.GetRevision();
	}
	if (ImGui::IsItemDeactivated() && State.PositionTransaction)
	{
		(void)Document.EndEditTransaction();
		State.PositionTransaction = false;
		State.Revision = 0;
	}

	ImGui::TextDisabled("StaticMesh: %s",
		State.Details.StaticMesh.c_str());
	ImGui::SeparatorText("Material overrides");
	for (size_t Index = 0;
		Index < State.Details.MaterialSlots.size(); ++Index)
	{
		const FEditorNativeSceneMaterialSlotDetails& Slot =
			State.Details.MaterialSlots[Index];
		FNativeMaterialSlotDraft& Draft = State.MaterialSlots[Index];
		ImGui::PushID(static_cast<int>(Slot.MaterialSlot));
		const xr_string Header =
			xr_string(std::to_string(Slot.MaterialSlot)) + ": " + Slot.Name;
		if (ImGui::CollapsingHeader(Header.c_str(),
				ImGuiTreeNodeFlags_DefaultOpen))
		{
			ImGui::TextWrapped("Base: %s", Slot.BaseMaterial.c_str());
			if (ImGui::Checkbox("Override", &Draft.HasOverride))
			{
				xr_string Diagnostic;
				bool Changed = false;
				if (Draft.HasOverride)
				{
					if (Draft.Material[0] == '\0')
						CopyDraft(Draft.Material, Slot.BaseMaterial);
					Changed = Document.SetSelectedMaterialOverride(
						Draft.MaterialSlot, Draft.Material.data(),
						Draft.TwoSided, Diagnostic);
				}
				else
				{
					Changed = Document.ClearSelectedMaterialOverride(
						Draft.MaterialSlot, Diagnostic);
				}
				if (!Changed)
					ReportNativeDetailsError(Diagnostic);
				State.Revision = 0;
			}
			if (Draft.HasOverride)
			{
				bool Apply = ImGui::InputText("Material",
					Draft.Material.data(), Draft.Material.size(),
					ImGuiInputTextFlags_EnterReturnsTrue);
				Apply |= ImGui::Checkbox(
					"Two sided", &Draft.TwoSided);
				if (Apply)
				{
					xr_string Diagnostic;
					if (!Document.SetSelectedMaterialOverride(
							Draft.MaterialSlot, Draft.Material.data(),
							Draft.TwoSided, Diagnostic))
					{
						ReportNativeDetailsError(Diagnostic);
					}
					State.Revision = 0;
				}
				ImGui::TextDisabled(
					"Enter a Material/MaterialInstance GUID or path.");
			}
		}
		ImGui::PopID();
	}
	ImGui::EndDisabled();
	if (!Editable)
		ImGui::TextDisabled("StaticMesh preview is read-only.");
}
} // namespace

UILPropertiesForm::UILPropertiesForm()
{
}

UILPropertiesForm::~UILPropertiesForm()
{
}

void UILPropertiesForm::Draw()
{
	if (bOpen)
	{
		if (ImGui::Begin("Properties", &bOpen))
		{
			if (XRay::ImGui::BeginDarkChild("WorldPropertiesBorder"))
			{
				if (GetEditorNativeSceneDocument().IsOpen())
				{
					DrawNativeProperties();
				}
				else if (PropUpdateIsCompleted)
				{
					LTools->GetProperties()->Draw();
				}
				else
				{
					ImGui::Text("Async loading...");
				}

				XRay::ImGui::EndDarkChild();
			}
		}
		ImGui::End();
	}
}
