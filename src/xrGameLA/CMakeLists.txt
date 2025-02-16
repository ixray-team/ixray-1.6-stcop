project(xrGameLA)
set(XGM_SRC_FILES)

# Files import
file(GLOB_RECURSE XGM_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.cxx"
	"*.c"
	"*.h"
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
file(GLOB_RECURSE XGM_SOURCE_CAR_FILES
	"Car*.*"
	"car*.*"
)
file(GLOB XGM_SOURCE_ALIFE_FILES
	"alife_*.*"
)
file(GLOB_RECURSE XGM_SOURCE_SCRIPT_FILES
	"script_*.*"
)
file(GLOB_RECURSE XGM_SOURCE_SERV_FILES
	"xrServer*.*"
	"game_*.*"
)

file(GLOB_RECURSE XGM_SOURCE_AI_BASE_FILES "ai/monsters/base_monster/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_GS_FILES "ai/monsters/group_states/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_STATE_FILES "ai/monsters/states/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_BS_FILES "ai/monsters/bloodsucker/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_DOG_FILES "ai/monsters/dog/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_BOAR_FILES "ai/monsters/boar/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_BURER_FILES "ai/monsters/burer/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_CAT_FILES "ai/monsters/cat/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_CHIM_FILES "ai/monsters/chimera/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_CONT_FILES "ai/monsters/controller/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_FLSH_FILES "ai/monsters/flesh/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_FACT_FILES "ai/monsters/fracture/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_POLT_FILES "ai/monsters/poltergeist/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_PDOG_FILES "ai/monsters/pseudodog/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_PGIG_FILES "ai/monsters/pseudogigant/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_RATS_FILES "ai/monsters/rats/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_SNORK_FILES "ai/monsters/snork/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_TUSH_FILES "ai/monsters/tushkano/*.*")
file(GLOB_RECURSE XGM_SOURCE_AI_ZOMB_FILES "ai/monsters/zombie/*.*")

file(GLOB_RECURSE XGM_SOURCE_LEGACY_FILES "Legacy/*.*")

# Source groups
source_group("core" FILES ${XGM_SOURCE_ALL_FILES})
source_group("ui" FILES ${XGM_SOURCE_UI_FILES})
#source_group("ai" FILES ${XGM_SOURCE_AI_FILES})
source_group("ai/car" FILES ${XGM_SOURCE_CAR_FILES})
source_group("ai/smart_cover" FILES ${XGM_SOURCE_SC_FILES})
source_group("ai/space_restictor" FILES ${XGM_SOURCE_SR_FILES})
source_group("ai/monsters/base" FILES ${XGM_SOURCE_AI_BASE_FILES})
source_group("ai/monsters/base/group_states" FILES ${XGM_SOURCE_AI_GS_FILES})
source_group("ai/monsters/base/states" FILES ${XGM_SOURCE_AI_STATE_FILES})
source_group("ai/monsters/boar" FILES ${XGM_SOURCE_AI_BOAR_FILES})
source_group("ai/monsters/bloodsucker" FILES ${XGM_SOURCE_AI_BS_FILES})
source_group("ai/monsters/dog" FILES ${XGM_SOURCE_AI_DOG_FILES})
source_group("ai/monsters/burer" FILES ${XGM_SOURCE_AI_BURER_FILES})
source_group("ai/monsters/cat" FILES ${XGM_SOURCE_AI_CAT_FILES})
source_group("ai/monsters/chimera" FILES ${XGM_SOURCE_AI_CHIM_FILES})
source_group("ai/monsters/controller" FILES ${XGM_SOURCE_AI_CONT_FILES})
source_group("ai/monsters/flesh" FILES ${XGM_SOURCE_AI_FLSH_FILES})
source_group("ai/monsters/fracture" FILES ${XGM_SOURCE_AI_FACT_FILES})
source_group("ai/monsters/poltergeist" FILES ${XGM_SOURCE_AI_POLT_FILES})
source_group("ai/monsters/pseudodog" FILES ${XGM_SOURCE_AI_PDOG_FILES})
source_group("ai/monsters/pseudogigant" FILES ${XGM_SOURCE_AI_PGIG_FILES})
source_group("ai/monsters/rats" FILES ${XGM_SOURCE_AI_RATS_FILES})
source_group("ai/monsters/snork" FILES ${XGM_SOURCE_AI_SNORK_FILES})
source_group("ai/monsters/tushkano" FILES ${XGM_SOURCE_AI_TUSH_FILES})
source_group("ai/monsters/zombie" FILES ${XGM_SOURCE_AI_ZOMB_FILES})
source_group("alife" FILES ${XGM_SOURCE_ALIFE_FILES})
source_group("script" FILES ${XGM_SOURCE_SCRIPT_FILES})
source_group("server" FILES ${XGM_SOURCE_SERV_FILES})
source_group("actor" FILES ${XGM_SOURCE_ACTOR_FILES})
source_group("level" FILES ${XGM_SOURCE_LEVEL_FILES})
source_group("ikan" FILES ${XGM_SOURCE_IKAN_FILES})
source_group("inventory" FILES ${XGM_SOURCE_INV_FILES})
source_group("Legacy" FILES ${XGM_SOURCE_LEGACY_FILES})

# Apply list
list(APPEND XGM_SRC_FILES ${XGM_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "DynamicHeightMap.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "LevelFogOfWar.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "CustomTimer*.cpp")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ai/monsters/rats/ai_rat_fsm.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/IUWpnParams.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UIRankFaction.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "ui/UIRankFraction.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "tri-colliderknoopc/dcTriListCollider.*")
list(FILTER XGM_SRC_FILES EXCLUDE REGEX "detectors_script.*")

# xrGameLA project
add_library(xrGameLA SHARED ${XGM_SRC_FILES})

if (IXRAY_UNITYBUILD)
	set_target_properties(xrGameLA PROPERTIES UNITY_BUILD ON UNITY_BUILD_BATCH_SIZE 32)
	target_compile_options(xrGameLA PRIVATE /bigobj)
	set_source_files_properties(script_engine_export.cpp PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
endif()

# Include directories
target_include_directories(xrGameLA PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrGameLA PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ui/")
target_include_directories(xrGameLA PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrGameLA PRIVATE _USRDLL)
target_compile_definitions(xrGameLA PRIVATE XRGAME_EXPORTS)
target_compile_definitions(xrGameLA PRIVATE dSINGLE)

if (IXRAY_MP)
	target_compile_definitions(xrGameLA PRIVATE XR_MP_BUILD)
endif()

add_compile_options(/fp:fast)
add_compile_options(/FS)
target_compile_definitions(xrGameLA PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrGameLA PUBLIC xrAbstractions)
target_link_libraries(xrGameLA PUBLIC xrParticles)
target_link_libraries(xrGameLA PUBLIC luabind)
target_link_libraries(xrGameLA PUBLIC crypto)

target_link_libraries(xrGameLA PRIVATE "shlwapi.lib")

if(IXRAY_MP)
    target_link_libraries(xrGameLA PUBLIC CxImage)
endif()

# Move precompiled header
set_target_properties(xrGameLA PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

if (NOT IXRAY_UNITYBUILD)
	set_target_properties(xrGameLA PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
	set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
	target_precompile_headers(xrGameLA PUBLIC "stdafx.h")

	file(GLOB_RECURSE GAME_SOURCE_ALL_C_FILES "*.c" "../xrGameSpy/gamespy/md5c.c")
	set_source_files_properties(${GAME_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
endif()

file(GLOB_RECURSE GAME_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${GAME_SOURCE_PCH_FILES})
