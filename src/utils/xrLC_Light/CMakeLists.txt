project(xrLC_Light)

file(GLOB_RECURSE LCL_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.cpp" "*.h")

file(GLOB_RECURSE LCL_SOURCE_MUMODELS_FILES
	"xrMU_Model.*"
	"mu_model_*"
	"xrMU_Model_*"
	"fitter.*"
)

file(GLOB_RECURSE LCL_SOURCE_IMAGE_FILES
	"xrLoadSurface.*"
	"xrUVpoint.*"
	"../../xrEngine/xrLoadSurface.cpp"
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_FILES
	"xrLC_GlobalData.*"
	"xrLC_Light.*"
	"xrLightVertex.*"
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_LIGHTMAP_FILES
	"Lightmap*"
	"lm_layer*"
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_IMPLICIT_FILES
	"xrLight_*"
)

file(GLOB_RECURSE LCL_SOURCE_LIGHT_DEFLECTORS_FILES
	"xrDeflector*"
	"xrDeflectoL_Direct.cpp"
)

file(GLOB_RECURSE LCL_SOURCE_BASE_FILES
	"base_*"
	"b_build_texture.*"
	"ETextureParams.*"
	"R_light.*"
	"light_point.h"
)

file(GLOB_RECURSE LCL_SOURCE_DOLIGHT_FILES
	"detail_slot_calculate.*"
	"global_calculation_data.*"
	"global_slots_data.*"
	"recalculation.*"
	"LightThread.*"
	"DetailFormat.*"
	"compiler.*"
)

file(GLOB_RECURSE LCL_SOURCE_MESHDATA_FILES
	"face_smoth_flags.*"
	"tcf.*"
	"uv_tri.*"
	"xrFace*"
	"MeshStaic*"
	"MeshStructure*"
	"itterate_adjacents*"
	"calculate_normals.*"
	"vector_clear.*"
)

file(GLOB_RECURSE XLC_SOURCE_EMBREE_FILES "Embree*")
file(GLOB XLC_SOURCE_CUDABACKEND_FILES "${CMAKE_CURRENT_SOURCE_DIR}/CUDA/*.*")
file(GLOB XLC_SOURCE_CUDA_FILES "${CMAKE_CURRENT_SOURCE_DIR}/CUDA/*.cu")

set(LCL_SRC_FILES "")
list(APPEND LCL_SRC_FILES
	${LCL_SOURCE_ALL_FILES}
	${LCL_SOURCE_MUMODELS_FILES}
	${LCL_SOURCE_LIGHT_FILES}
	${LCL_SOURCE_BASE_FILES}
	${LCL_SOURCE_DOLIGHT_FILES}
	${LCL_SOURCE_MESHDATA_FILES}
	${LCL_SOURCE_IMAGE_FILES}
)

# Check CUDA + OptiX
find_package(CUDAToolkit)

if(CUDAToolkit_FOUND)
	message(STATUS "CUDA found: ${CUDAToolkit_VERSION}")
	find_package(OptiX REQUIRED)
	if(OptiX_FOUND)
		message(STATUS "OptiX found: ${OptiX_VERSION}")
		enable_language(CUDA)
		list(APPEND LCL_SRC_FILES ${XLC_SOURCE_CUDABACKEND_FILES})

		foreach(_cu_file IN LISTS XLC_SOURCE_CUDA_FILES)
			set_source_files_properties(${_cu_file} PROPERTIES HEADER_FILE_ONLY TRUE)
		endforeach()
	else()
		message(FATAL_ERROR "OptiX not found, but CUDA is present!")
		list(FILTER LCL_SRC_FILES EXCLUDE REGEX "xrDeflectorLight_Packed.*")
		list(FILTER LCL_SRC_FILES EXCLUDE REGEX "CUDA/.*")
	endif()
else()
	message(WARNING "CUDA toolkit not found, skipping CUDA/OptiX configuration.")
	list(FILTER LCL_SRC_FILES EXCLUDE REGEX "xrDeflectorLight_Packed.*")
	list(FILTER LCL_SRC_FILES EXCLUDE REGEX "CUDA/.*")
endif()

# Project
add_library(xrLC_Light ${LCL_SRC_FILES})
target_precompile_headers(xrLC_Light PRIVATE "stdafx.h")

target_include_directories(xrLC_Light PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLC_Light PUBLIC "${IXRAY_SDK_INC}")

target_link_directories(xrLC_Light PUBLIC "${IXRAY_SDK_LIB}")

target_compile_definitions(xrLC_Light PRIVATE _USRDLL LCLIGHT_BUILD)
if(OptiX_FOUND)
	target_compile_definitions(xrLC_Light PUBLIC LCCUDA_BUILD)
endif()

# Apply CUDA/OptiX
if(OptiX_FOUND)
	target_include_directories(xrLC_Light PUBLIC ${OptiX_INCLUDE_DIRS})
	target_link_libraries(xrLC_Light PRIVATE ${OptiX_LIBRARIES} CUDA::cudart_static CUDA::cuda_driver)
	set_target_properties(xrLC_Light PROPERTIES
		CUDA_SEPARABLE_COMPILATION ON
		CUDA_RESOLVE_DEVICE_SYMBOLS ON
	)

	add_custom_command(
		TARGET xrLC_Light PRE_BUILD
		COMMAND ${CUDAToolkit_NVCC_EXECUTABLE}
			-ptx
			"${CMAKE_CURRENT_SOURCE_DIR}/CUDA/CuTrace.cu"
			-o "${IXRAY_OUTPUT}/CuTrace.ptx"
			-Xcompiler "/MD"
			-I"${OptiX_INCLUDE_DIRS}"
			-I"${CUDAToolkit_INCLUDE_DIRS}"
			--gpu-architecture=compute_75
			--gpu-code=sm_75
		DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/CUDA/CuTrace.cu"
		COMMENT "Compiling CuTrace.cu to PTX"
		VERBATIM
	)
endif()

# Project filter
source_group("kernel" FILES ${LCL_SOURCE_ALL_FILES})
source_group("mu-models" FILES ${LCL_SOURCE_MUMODELS_FILES})
source_group("light_files" FILES ${LCL_SOURCE_LIGHT_FILES})
source_group("light_files/EmbreeRayTracing" FILES ${XLC_SOURCE_EMBREE_FILES})
source_group("light_files/CUDARayTracing" FILES ${XLC_SOURCE_CUDABACKEND_FILES})
source_group("light_files/deflectors" FILES ${LCL_SOURCE_LIGHT_DEFLECTORS_FILES})
source_group("light_files/implicit" FILES ${LCL_SOURCE_LIGHT_IMPLICIT_FILES})
source_group("light_files/lightmap" FILES ${LCL_SOURCE_LIGHT_LIGHTMAP_FILES})
source_group("base_files" FILES ${LCL_SOURCE_BASE_FILES})
source_group("dolight_files" FILES ${LCL_SOURCE_DOLIGHT_FILES})
source_group("mesh_files" FILES ${LCL_SOURCE_MESHDATA_FILES})
source_group("image_files" FILES ${LCL_SOURCE_IMAGE_FILES})

# Linker list
target_link_libraries(xrLC_Light PUBLIC xrCore)
target_link_libraries(xrLC_Light PUBLIC imgui)
target_link_libraries(xrLC_Light PUBLIC xrDXT)

# FreeImage
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.2023.8.19-open)
target_include_directories(xrLC_Light PUBLIC "${FREEIMAGE}/native/include/")
add_custom_command(TARGET xrLC_Light POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FREEIMAGE}/native/bin/${NUGET_PACKAGE_PLATFORM}/Release/FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}/meshoptimizer.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Embree.Runtimes.win-x64.4.3.3/runtimes/win-x64/native/Release/embree4.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
target_link_libraries(xrLC_Light PUBLIC "${FREEIMAGE}/native/lib/${NUGET_PACKAGE_PLATFORM}/Release/FreeImage.lib")

# DirectX SDK
target_include_directories(xrLC_Light PUBLIC
	"${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/"
	"${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/"
)

# Zlib
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/include/")
target_link_libraries(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.lib")

# Intel Embree
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Embree.Runtimes.win-x64.4.3.3/build/native/include/")
target_link_libraries(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Embree.Runtimes.win-x64.4.3.3/runtimes/win-x64/native/Release/embree4.lib")