project(FFxExtendedEntityesLogic)

file(GLOB_RECURSE FFX_EXTENDED_ENTITYES_SOURCES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)
add_library(FFxExtendedEntityesLogicObjects OBJECT ${FFX_EXTENDED_ENTITYES_SOURCES})
add_library(FFxExtendedEntityesLogic SHARED $<TARGET_OBJECTS:FFxExtendedEntityesLogicObjects>)
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${FFX_EXTENDED_ENTITYES_SOURCES})
set_target_properties(FFxExtendedEntityesLogicObjects PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_include_directories(FFxExtendedEntityesLogicObjects PRIVATE
	"${CMAKE_CURRENT_SOURCE_DIR}"
	"${CMAKE_CURRENT_SOURCE_DIR}/.."
	"${CMAKE_CURRENT_SOURCE_DIR}/../xrGame"
	"${CMAKE_CURRENT_SOURCE_DIR}/../xrGame/ui"
	"${CMAKE_CURRENT_SOURCE_DIR}/../xrServerEntities"
)
target_compile_definitions(FFxExtendedEntityesLogicObjects PRIVATE
	FFX_EXTENDED_ENTITYES_LOGIC_BUILD
	XRGAME_EXPORTS
	_USRDLL
	dSINGLE
	"$<$<CONFIG:Debug>:DEBUG>"
)
target_link_libraries(FFxExtendedEntityesLogic PRIVATE xrGame)
if(MSVC)
	target_compile_options(FFxExtendedEntityesLogicObjects PRIVATE /bigobj)
	set_property(TARGET FFxExtendedEntityesLogicObjects PROPERTY IXRAY_DISABLE_GL ON)
endif()

target_link_libraries(FFxExtendedEntityesLogicObjects PRIVATE xrEngineCore xrPhysics xrScripts xrUI luabind IKAN)
# Object compilation is independent of the game. Only the final plugin links to its host.
add_dependencies(xrGame FFxExtendedEntityesLogicObjects)
set_property(TARGET xrGame APPEND PROPERTY LINK_DEPENDS "$<TARGET_OBJECTS:FFxExtendedEntityesLogicObjects>" "${CMAKE_SOURCE_DIR}/cmake/FFxGameHostExports.cmake")

if(MSVC)
	get_filename_component(FFX_COMPILER_DIRECTORY "${CMAKE_CXX_COMPILER}" DIRECTORY)
	find_program(FFX_DUMPBIN dumpbin HINTS "${FFX_COMPILER_DIRECTORY}" REQUIRED)
	set(FFX_MODULE_OBJECT_LIST "${CMAKE_CURRENT_BINARY_DIR}/module-objects-$<CONFIG>.txt")
	file(GENERATE OUTPUT "${FFX_MODULE_OBJECT_LIST}" CONTENT "$<JOIN:$<TARGET_OBJECTS:FFxExtendedEntityesLogicObjects>,\n>")
	set_property(TARGET xrGame PROPERTY FFX_DUMPBIN "${FFX_DUMPBIN}")
endif()
