#include "stdafx.h"
#include "UIMoveToCamera.h"
#include "ui_main.h"

UIMoveToCamera::UIMoveToCamera()
{
    const Fvector& CameraCurrentPosition = UI->CurrentView().m_Camera.GetPosition();

    OldCameraPosition = CameraCurrentPosition;
    NewCameraPosition = CameraCurrentPosition;
}

void UIMoveToCamera::Draw()
{
    ImGui::SetNextWindowPos(ImGui::GetWindowViewport()->GetCenter(), ImGuiCond_Appearing, { 0.5f, 0.5f });

    if (ImGui::Begin("Move camera to", 0, ImGuiWindowFlags_::ImGuiWindowFlags_NoDocking
        | ImGuiWindowFlags_NoResize
        | ImGuiWindowFlags_NoSavedSettings
        | ImGuiWindowFlags_AlwaysAutoResize
        | ImGuiWindowFlags_NoCollapse
        | ImGuiWindowFlags_NoSavedSettings
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
    UI->CurrentView().m_Camera.Set(UI->CurrentView().m_Camera.GetHPB(), NewCameraPosition);
    UI->RedrawScene();
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