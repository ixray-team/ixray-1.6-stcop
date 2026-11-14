#include "stdafx.h"
#include "UIMoveToCamera.h"
#include "ui_main.h"

UIMoveToCamera::UIMoveToCamera()
{
	const Fvector& CameraCurrentPosition = EContext.UI->ActiveViewport().Camera.GetPosition();

	OldCameraPosition = CameraCurrentPosition;
	NewCameraPosition = CameraCurrentPosition;
}

void UIMoveToCamera::Draw()
{
	ImGui::SetNextWindowPos(ImGui::GetWindowViewport()->GetCenter(), ImGuiCond_Appearing, { 0.5f, 0.5f });

	if (ImGui::Begin("Move camera to", nullptr, ImGuiWindowFlags_::ImGuiWindowFlags_NoDocking
		| ImGuiWindowFlags_NoResize
		| ImGuiWindowFlags_NoSavedSettings
		| ImGuiWindowFlags_AlwaysAutoResize
		| ImGuiWindowFlags_NoCollapse
	))
	{
		ImGui::InputFloat3("XYZ", &NewCameraPosition.x, "%0.1f");

		if (ImGui::Button("Move"))
			Ok();

		ImGui::SameLine();
		if (ImGui::Button("Close"))
			Close();

		ImGui::SameLine();
		if (ImGui::Button("Reset"))
			Reset();
	}
	ImGui::End();
}

void UIMoveToCamera::Ok()
{
	EContext.UI->ActiveViewport().Camera.Set(EContext.UI->ActiveViewport().Camera.GetHPB(), NewCameraPosition);
	EContext.UI->RedrawScene();
}

void UIMoveToCamera::Close()
{
	NewCameraPosition = OldCameraPosition;
	bOpen = false;
}

void UIMoveToCamera::Reset()
{
	NewCameraPosition = OldCameraPosition;
}