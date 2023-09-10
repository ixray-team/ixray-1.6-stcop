project(xrGame)
set(XGM_SRC_FILES)

# Files import
file(GLOB_RECURSE XGM_SOURCE_ALL_FILES
    "*.cpp"
    "*.cxx"
    "*.c"
	"*.h"
    "../xrServerEntities/*.*"
    "../xrGameSpy/gamespy/md5c.c"
)

file(GLOB_RECURSE XGM_SOURCE_AI_FILES
    "ai/*.*"
    "ai_*.*"
)

file(GLOB_RECURSE XGM_SOURCE_SC_FILES
    "smart_cover*.*"
)

file(GLOB_RECURSE XGM_SOURCE_SR_FILES
    "space_restriction*.*"
)

file(GLOB_RECURSE XGM_SOURCE_UI_FILES
    "ui/*.*"
    "ui*.*"
    "UI*.*"
)

file(GLOB_RECURSE XGM_SOURCE_IKAN_FILES
    "ik/*.*"
    "ik_*.*"
    "IK*.*"
)

file(GLOB_RECURSE XGM_SOURCE_ACTOR_FILES
    "Actor*.*"
)

file(GLOB_RECURSE XGM_SOURCE_LEVEL_FILES
    "Level*.*"
    "level*.*"
)

file(GLOB_RECURSE XGM_SOURCE_INV_FILES
    "inventory*.*"
    "Inventory*.*"
)

file(GLOB XGM_SOURCE_ALIFE_FILES
    "../xrServerEntities/alife_*.*"
	"alife_*.*"
)

file(GLOB_RECURSE XGM_SOURCE_SCRIPT_FILES
    "../xrServerEntities/script_*.*"
    "script_*.*"
)

file(GLOB_RECURSE XGM_SOURCE_SERV_FILES
    "../xrServerEntities/xrServer*.*"
    "xrServer*.*"
    "game_*.*"
)

# Source groups
source_group("core" FILES ${XGM_SOURCE_ALL_FILES})
source_group("ui" FILES ${XGM_SOURCE_UI_FILES})
source_group("ai" FILES ${XGM_SOURCE_AI_FILES})
source_group("ai/smart_cover" FILES ${XGM_SOURCE_SC_FILES})
source_group("ai/space_restictor" FILES ${XGM_SOURCE_SR_FILES})
source_group("alife" FILES ${XGM_SOURCE_ALIFE_FILES})
source_group("script" FILES ${XGM_SOURCE_SCRIPT_FILES})
source_group("server" FILES ${XGM_SOURCE_SERV_FILES})
source_group("actor" FILES ${XGM_SOURCE_ACTOR_FILES})
source_group("level" FILES ${XGM_SOURCE_LEVEL_FILES})
source_group("ikan" FILES ${XGM_SOURCE_IKAN_FILES})
source_group("inventory" FILES ${XGM_SOURCE_INV_FILES})

# Apply list
list(APPEND XGM_SRC_FILES ${XGM_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "DynamicHeightMap.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "WeaponMounted.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "LevelFogOfWar.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ai/monsters/rats/ai_rat_fsm.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/IUWpnParams.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UIRankFaction.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UIRankFraction.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UITreeViewItem.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UIStalkersRankingWnd.*")

# xrGame project
add_library(xrGame SHARED ${XGM_SRC_FILES})
target_precompile_headers(xrGame PUBLIC "stdafx.h")

# Include directories
target_include_directories(xrGame PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrGame PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../xrServerEntities/")
target_include_directories(xrGame PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ui/")
target_include_directories(xrGame PUBLIC "${IXRAY_SDK_INC}")

# project defines 
target_compile_definitions(xrGame PRIVATE _WINDOWS)
target_compile_definitions(xrGame PRIVATE _USRDLL)
target_compile_definitions(xrGame PRIVATE XRGAME_EXPORTS)
target_compile_definitions(xrGame PRIVATE dSINGLE)

add_compile_options(/fp:fast)
target_compile_definitions(xrGame PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrGame PUBLIC xrEngine)
target_link_libraries(xrGame PUBLIC xrPhysics)
target_link_libraries(xrGame PUBLIC crypto)
target_link_libraries(xrGame PUBLIC CxImage)