#include "stdafx.h"
#include "ReferenceReplacer.h"

UIReferenceReplacer::UIReferenceReplacer()
{
}

UIReferenceReplacer::~UIReferenceReplacer()
{
	SceneReferences.clear();
}

void UIReferenceReplacer::Draw()
{
	if (ImGui::Begin("References Replacer", &bOpen, ImGuiWindowFlags_AlwaysAutoResize))
	{
		bool HasItemInScene = false;

		ImGui::Text("Scene References: ");
		ImGui::SetNextItemWidth(340);
		if (ImGui::BeginListBox("##Scene References"))
		{
			bool HasChars = xr_strlen(InputFile);

			for (const xr_string& Ref : SceneReferences)
			{
				if (HasChars)
				{
					if (!Ref.Contains(InputFile))
						continue;

					if (Ref.EqualWithCaseInsensitive(InputFile))
					{
						HasItemInScene = true;
					}
				}
				
				static bool SelectState = false;
				if (ImGui::Selectable(Ref.c_str(), &SelectState))
				{
					xr_strcpy(InputFile, Ref.c_str());
				}

				SelectState = false;
			}

			ImGui::EndListBox();
		}

		ImGui::Text("Old: ");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(302);
		ImGui::InputText("##inputRP", InputFile, sizeof(InputFile));

		ImGui::Text("New: ");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(246);
		ImGui::InputText("##OutputRP", OutputFile, sizeof(OutputFile));
		ImGui::SameLine();

		if (ImGui::Button("Select"))
		{
			UIChooseForm::SelectItem(smObject, 1, InputFile, 0, 0, 0, 0, 0);
			Selected = true;
		}

		ImGui::BeginDisabled(!HasItemInScene);
		if (ImGui::Button("Replace"))
		{
			ESceneObjectTool* ObjectToolPtr = static_cast<ESceneObjectTool*>(Scene->GetTool(OBJCLASS_SCENEOBJECT));
			for (CCustomObject* SceneObject : ObjectToolPtr->GetObjects())
			{
				CSceneObject* Object = (CSceneObject*)SceneObject;

				if (Object->m_ReferenceName == InputFile)
				{
					Object->m_ReferenceName = OutputFile;
					Object->ReferenceChange(nullptr);
				}
			}
		}
		ImGui::EndDisabled();
	}
	ImGui::End();

	if (Selected)
	{
		bool change = false;
		shared_str result;
		if (UIChooseForm::GetResult(change, result) && change)
		{
			xr_strcpy(OutputFile, *result);
			Selected = false;
		}

		UIChooseForm::Update();
	}
}

void UIReferenceReplacer::UpdateReferences()
{
	ESceneObjectTool* ObjectToolPtr = static_cast<ESceneObjectTool*>(Scene->GetTool(OBJCLASS_SCENEOBJECT));
	for (CCustomObject* SceneObject : ObjectToolPtr->GetObjects())
	{
		SceneReferences.emplace(SceneObject->RefName());
	}
}
