include(FetchContent)

FetchContent_Declare(
    NRI
    GIT_REPOSITORY https://github.com/NVIDIA-RTX/NRI.git
    GIT_TAG main
)

set(NRI_ENABLE_IMGUI_EXTENSION TRUE)


FetchContent_MakeAvailable(NRI)


set_target_properties(NRI PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_Shared PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_NONE PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_D3D11 PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_D3D12 PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_VK PROPERTIES FOLDER "3rd Party")
set_target_properties(NRI_Validation PROPERTIES FOLDER "3rd Party")