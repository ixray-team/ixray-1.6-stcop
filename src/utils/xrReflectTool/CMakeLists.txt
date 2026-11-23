project(xrReflectTool)

# Build-time code generator: standard library only, so it can run before any engine module is built.
add_executable(xrReflectTool
	"main.cpp"
	"ReflectModel.h"
	"ReflectModel.cpp"
	"ReflectParser.h"
	"ReflectParser.cpp"
	"LuaGenerator.h"
	"LuaGenerator.cpp"
	"IniGenerator.h"
	"IniGenerator.cpp"
	"SoundGenerator.h"
	"SoundGenerator.cpp"
)
set_target_properties(xrReflectTool PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_compile_features(xrReflectTool PRIVATE cxx_std_23)

# Generates luabind registration for sources that use XREFLECT / XPROPERTY.
# ixray_reflect_lua(<target> <module> PCH <header> SOURCES <files...>)
# Defines `void XReflectRegister(lua_State*)` in every module. A reflected .cpp includes its
# generated "<stem>.reflect.inl" at the end.
function(ixray_reflect_lua TARGET MODULE)
	cmake_parse_arguments(ARG "" "PCH" "SOURCES" ${ARGN})
	set(GEN_DIR "${CMAKE_CURRENT_BINARY_DIR}/Generated")
	set(LIST_FILE "${GEN_DIR}/${MODULE}.reflect.sources")
	set(OUTPUT_FILE "${GEN_DIR}/${MODULE}.reflect.gen.cpp")
	set(INI_OUTPUT_FILE "${GEN_DIR}/${MODULE}.ini.gen.cpp")
	set(INI_OWNERS_FILE "${GEN_DIR}/IniOwners.gen.h")
	set(STAMP_FILE "${GEN_DIR}/${MODULE}.reflect.stamp")

	list(JOIN ARG_SOURCES "\n" SOURCE_LINES)
	file(GENERATE OUTPUT "${LIST_FILE}" CONTENT "${SOURCE_LINES}\n")

	set(PCH_ARGS)
	if(ARG_PCH)
		set(PCH_ARGS --pch "${ARG_PCH}")
	endif()

	# The tool rewrites its outputs only when their content changes; the stamp tracks the run.
	add_custom_command(
		OUTPUT "${STAMP_FILE}"
		BYPRODUCTS "${OUTPUT_FILE}" "${INI_OUTPUT_FILE}" "${INI_OWNERS_FILE}"
		COMMAND xrReflectTool --module "${MODULE}" --sources "${LIST_FILE}" --output-dir "${GEN_DIR}" ${PCH_ARGS} --stamp "${STAMP_FILE}"
		DEPENDS xrReflectTool ${ARG_SOURCES} "${LIST_FILE}"
		COMMENT "xrReflectTool: ${MODULE}"
		VERBATIM
	)

	add_custom_target(${TARGET}_reflect DEPENDS "${STAMP_FILE}")
	set_target_properties(${TARGET}_reflect PROPERTIES FOLDER "Engine/Pre Build")
	add_dependencies(${TARGET} ${TARGET}_reflect)

	set_source_files_properties("${OUTPUT_FILE}" "${INI_OUTPUT_FILE}" PROPERTIES GENERATED ON SKIP_UNITY_BUILD_INCLUSION ON)
	target_sources(${TARGET} PRIVATE "${OUTPUT_FILE}" "${INI_OUTPUT_FILE}")
	target_include_directories(${TARGET} PRIVATE "${GEN_DIR}")
	source_group("generated" FILES "${OUTPUT_FILE}" "${INI_OUTPUT_FILE}")
endfunction()

# Generates the SoundShaderType enum and the class infos of XREFLECT(SoundShader) classes.
# ixray_reflect_sound(<target> <module> PCH <header> SOURCES <files...>)
# SoundShaderType.gen.h is a public header of the target, the class infos are compiled into it.
function(ixray_reflect_sound TARGET MODULE)
	cmake_parse_arguments(ARG "" "PCH" "SOURCES" ${ARGN})
	set(GEN_DIR "${CMAKE_CURRENT_BINARY_DIR}/Generated")
	set(LIST_FILE "${GEN_DIR}/${MODULE}.sound.sources")
	set(HEADER_FILE "${GEN_DIR}/SoundShaderType.gen.h")
	set(OUTPUT_FILE "${GEN_DIR}/${MODULE}.sound.gen.cpp")
	set(STAMP_FILE "${GEN_DIR}/${MODULE}.sound.stamp")

	list(JOIN ARG_SOURCES "\n" SOURCE_LINES)
	file(GENERATE OUTPUT "${LIST_FILE}" CONTENT "${SOURCE_LINES}\n")

	set(PCH_ARGS)
	if(ARG_PCH)
		set(PCH_ARGS --pch "${ARG_PCH}")
	endif()

	add_custom_command(
		OUTPUT "${STAMP_FILE}"
		BYPRODUCTS "${HEADER_FILE}" "${OUTPUT_FILE}"
		COMMAND xrReflectTool --module "${MODULE}" --sources "${LIST_FILE}" --output-dir "${GEN_DIR}" ${PCH_ARGS} --stamp "${STAMP_FILE}" --targets sound
		DEPENDS xrReflectTool ${ARG_SOURCES} "${LIST_FILE}"
		COMMENT "xrReflectTool: ${MODULE} sound shaders"
		VERBATIM
	)

	add_custom_target(${TARGET}_reflect_sound DEPENDS "${STAMP_FILE}")
	set_target_properties(${TARGET}_reflect_sound PROPERTIES FOLDER "Engine/Pre Build")
	add_dependencies(${TARGET} ${TARGET}_reflect_sound)

	set_source_files_properties("${HEADER_FILE}" "${OUTPUT_FILE}" PROPERTIES GENERATED ON SKIP_UNITY_BUILD_INCLUSION ON)
	target_sources(${TARGET} PRIVATE "${OUTPUT_FILE}")
	target_include_directories(${TARGET} PUBLIC "${GEN_DIR}")
	source_group("generated" FILES "${OUTPUT_FILE}")
endfunction()
