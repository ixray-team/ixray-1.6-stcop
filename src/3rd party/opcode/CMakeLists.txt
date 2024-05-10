project(opcode-prosectors)

file(GLOB_RECURSE OPCP_SRC CONFIGURE_DEPENDS
	"*.*"
)

add_library(opcode-prosectors STATIC ${OPCP_SRC})
target_include_directories(opcode-prosectors PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(opcode-prosectors PUBLIC ICE_NO_DLL)
if (MSVC)
	target_compile_options(opcode-prosectors PRIVATE "/wd4002")
else()
	target_compile_options(opcode-prosectors PRIVATE "-fPIC")
endif()