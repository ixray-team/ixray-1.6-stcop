project(xrUI)
set(XRPU_SRC_FILES)

# Files import
file(GLOB_RECURSE XRPU_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

file(GLOB_RECURSE XRPU_SOURCE_XML_FILES
    "UIXmlInit.*"
    "xrUIXmlParser.*"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_FILES
    "Widgets/*.cpp"
    "Widgets/*.h"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_LINES_FILES
    "Widgets/UILine.*"
    "Widgets/UILines.*"
    "Widgets/uilinestd.*"
    "Widgets/UISubLine.*"
)


file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_STATIC_FILES
    "Widgets/UIAnimatedStatic.*"
    "Widgets/UIStatic.*"
    "Widgets/UIStatic_script.*"
    "Widgets/UIStaticItem.*"
    "Widgets/UI_IB_Static.*"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_FRAME_FILES
    "Widgets/UIFrameWindow.*"
    "Widgets/UIFrameLineWnd.*"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_HINT_FILES
    "Widgets/UIHint.*"
    "Widgets/UIBtnHint.*"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_BTN_FILES
    "Widgets/UIButton.*"
    "Widgets/UI3tButton.*"
    "Widgets/UIRadioButton.*"
    "Widgets/UICheckButton.*"
    "Widgets/UIButton_script.*"
)

file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_LIST_FILES "Widgets/UIListBox*.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_SCROLL_FILES "Widgets/UIScroll*.*" "Widgets/UIFixedScrollBar.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_TAB_FILES "Widgets/UITab*.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_PROGRESS_FILES "Widgets/UIProgress*.*" "Widgets/UIDoubleProgressBar*.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_OPTIONS_FILES "Widgets/UIOptions*.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_TRACK_FILES "Widgets/UITrack*.*")
file(GLOB_RECURSE XRPU_SOURCE_ELEMENTS_EDIT_FILES "Widgets/UIEditBox*.*" "Widgets/UICustomEdit.*")

# Source groups
source_group("core" FILES ${XRPU_SOURCE_ALL_FILES})
source_group("xml" FILES ${XRPU_SOURCE_XML_FILES})
source_group("widgets/progress" FILES ${XRPU_SOURCE_ELEMENTS_PROGRESS_FILES})
source_group("widgets/editbox" FILES ${XRPU_SOURCE_ELEMENTS_EDIT_FILES})
source_group("widgets/listbox" FILES ${XRPU_SOURCE_ELEMENTS_LIST_FILES})
source_group("widgets/scroll" FILES ${XRPU_SOURCE_ELEMENTS_SCROLL_FILES})
source_group("widgets/track" FILES ${XRPU_SOURCE_ELEMENTS_TRACK_FILES})
source_group("widgets/hints" FILES ${XRPU_SOURCE_ELEMENTS_HINT_FILES})
source_group("widgets/buttons" FILES ${XRPU_SOURCE_ELEMENTS_BTN_FILES})
source_group("widgets/lines" FILES ${XRPU_SOURCE_ELEMENTS_LINES_FILES})
source_group("widgets/tab" FILES ${XRPU_SOURCE_ELEMENTS_TAB_FILES})
source_group("widgets/frames" FILES ${XRPU_SOURCE_ELEMENTS_FRAME_FILES})
source_group("widgets/static" FILES ${XRPU_SOURCE_ELEMENTS_STATIC_FILES})
source_group("widgets/options components" FILES ${XRPU_SOURCE_ELEMENTS_OPTIONS_FILES})
source_group("widgets/other" FILES ${XRPU_SOURCE_ELEMENTS_FILES})

# Apply list
list(APPEND XRPU_SRC_FILES ${XRPU_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER XRPU_SRC_FILES EXCLUDE REGEX "Widgets/UITreeViewItem.*")

# xrUI project
add_library(xrUI SHARED ${XRPU_SRC_FILES})
set_target_properties(xrUI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Include directories
target_include_directories(xrUI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrUI PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrUI PRIVATE XR_UI_EXPORTS)

# Linker list
target_link_libraries(xrUI PUBLIC xrAbstractions)
target_link_libraries(xrUI PUBLIC xrScripts)

# Move precompiled header
target_validate_pch(xrUI ${CMAKE_CURRENT_SOURCE_DIR})
