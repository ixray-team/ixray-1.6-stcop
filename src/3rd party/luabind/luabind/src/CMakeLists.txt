set(LUABIND_SOURCES
	"class.cpp"
	"class_info.cpp"
	"class_registry.cpp"
	"class_rep.cpp"
	"create_class.cpp"
	"error.cpp"
	"exception_handler.cpp"
	"function.cpp"
	"function_introspection.cpp"
	"headertest.cpp"
	"inheritance.cpp"
	"link_compatibility.cpp"
	"memory.cpp"
	"object_rep.cpp"
	"open.cpp"
	"operator.cpp"
	"pcall.cpp"
	"scope.cpp"
	"set_package_preload.cpp"
	"stack_content_by_name.cpp"
	"weak_ref.cpp"
	"wrapper_base.cpp"
)

if (LUABIND_BUILD_SHARED)
	add_library(${PROJECT_NAME} SHARED ${LUABIND_SOURCES})
else()
	add_library(${PROJECT_NAME} STATIC ${LUABIND_SOURCES})
endif()

target_include_directories(${PROJECT_NAME}
	PUBLIC
	${LUA_INCLUDE_DIR}
)

target_link_libraries(${PROJECT_NAME}
	PUBLIC
	${LUA_LIBRARIES}
	xrLuabindApi
)

target_compile_definitions(${PROJECT_NAME}
	PRIVATE
	$<$<BOOL:${LUABIND_BUILD_SHARED}>:LUABIND_DYNAMIC_LINK>
)

target_compile_options(${PROJECT_NAME}
	PRIVATE
	$<$<CXX_COMPILER_ID:MSVC>:"/MP">
	$<$<CXX_COMPILER_ID:MSVC>:"/wd4251">
)

set_target_properties(${PROJECT_NAME} PROPERTIES
	PREFIX ""
)

install(TARGETS ${PROJECT_NAME} LIBRARY
	DESTINATION ${CMAKE_INSTALL_LIBDIR}
	PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
)
