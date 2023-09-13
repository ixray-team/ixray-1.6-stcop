# Engine libraries
add_subdirectory("Layers/xrAPI")
add_subdirectory("xrCore")
add_subdirectory("xrCDB")
add_subdirectory("xrSound")
add_subdirectory("xrNetServer")
add_subdirectory("xrEngine")
add_subdirectory("xrCPU_Pipe")
add_subdirectory("xrPhysics")
add_subdirectory("xrParticles")
add_subdirectory("Layers/xrRenderPC_R1")
add_subdirectory("Layers/xrRenderPC_R2")
add_subdirectory("Layers/xrRenderPC_R4")
add_subdirectory("Layers/New")
add_subdirectory("xrGame/")
add_subdirectory("xrGameSpy/")

add_subdirectory("3rd_party")

# Engine Utils
if (IXRAY_UTILS)
    add_subdirectory("utils")
endif()
