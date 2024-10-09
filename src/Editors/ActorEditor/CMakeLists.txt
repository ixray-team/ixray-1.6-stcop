project(ActorEditor)

# Files import
file(GLOB EAE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.ico"
    "*.rc"
)

file(GLOB EAE_SOURCE_UI_FILES CONFIGURE_DEPENDS "UI*.*")

file(GLOB EPPE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "../../xrGame/PostprocessAnimator.*"
    "CustomTools/UIPostProcess.*"
)

set(EAE_SRC_FILES)

source_group("src/ae/main" FILES ${EAE_SOURCE_ALL_FILES})
source_group("src/ae/ui" FILES ${EAE_SOURCE_UI_FILES})
source_group("src/ppe" FILES ${EPPE_SOURCE_ALL_FILES})

# Apply list
list(APPEND EAE_SRC_FILES ${EAE_SOURCE_ALL_FILES})
list(APPEND EAE_SRC_FILES ${EPPE_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER EAE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# ActorEditor project
add_executable(ActorEditor WIN32 ${EAE_SRC_FILES})
target_include_directories(ActorEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(ActorEditor PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(ActorEditor PRIVATE _ACTOR_EDITOR)
target_compile_definitions(ActorEditor PUBLIC XR_EDITOR_NEW)
target_compile_definitions(ActorEditor PRIVATE _PP_EDITOR_)

target_compile_options(ActorEditor PRIVATE /W0)
target_compile_definitions(ActorEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(ActorEditor PUBLIC xrCore)
target_link_libraries(ActorEditor PUBLIC xrEUI)
target_link_libraries(ActorEditor PUBLIC xrEProps)
target_link_libraries(ActorEditor PUBLIC xrECore)

# Move precompiled header
set_target_properties(ActorEditor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE EAE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${EAE_SOURCE_PCH_FILES})

