#include "stdafx.h"
#include "UIFileLoad.h"
#include "../xrEUI/ImOpenFileDialog.h"

CUFileOpen::CUFileOpen()
{

}

void CUFileOpen::ShowDialog(const char* Path, const char* Filter)
{
    string_path Dir;
    FS.update_path(Dir, Path, "");

    ImGuiFileDialog::Instance()->OpenDialog("ChooseFileDlgKey", "Choose File", Filter, Dir);
}

void CUFileOpen::Draw()
{
    if (ImGuiFileDialog::Instance()->Display("ChooseFileDlgKey"))
    {
        // action if OK
        if (ImGuiFileDialog::Instance()->IsOk())
        {
            xr_string filePathName = ImGuiFileDialog::Instance()->GetFilePathName().c_str();
            AfterLoadCallback(filePathName);
        }

        // close
        ImGuiFileDialog::Instance()->Close();
    }
}
