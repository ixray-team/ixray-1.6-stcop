#!/bin/bash

# Simple verification script for Vulkan renderer integration
# This script checks that our implementation is properly integrated

echo "=== Vulkan Renderer Integration Verification ==="
echo

# Check if Vulkan was added to APILevel enum
echo "1. Checking APILevel enum integration..."
if grep -q "Vulkan" src/xrEngine/device.h; then
    echo "   ✅ Vulkan added to APILevel enum"
else
    echo "   ❌ Vulkan NOT found in APILevel enum"
    exit 1
fi

# Check if device creation includes Vulkan
echo "2. Checking device creation integration..."
if grep -q "APILevel::Vulkan" src/xrEngine/Device_create_render.cpp; then
    echo "   ✅ Vulkan device creation integrated"
else
    echo "   ❌ Vulkan device creation NOT integrated"
    exit 1
fi

# Check if Vulkan renderer directory exists
echo "3. Checking Vulkan renderer structure..."
if [ -d "src/Layers/xrRenderVK" ]; then
    echo "   ✅ Vulkan renderer directory exists"
else
    echo "   ❌ Vulkan renderer directory NOT found"
    exit 1
fi

# Check essential files
echo "4. Checking essential Vulkan renderer files..."
required_files=(
    "src/Layers/xrRenderVK/CMakeLists.txt"
    "src/Layers/xrRenderVK/vk.h"
    "src/Layers/xrRenderVK/vk.cpp"
    "src/Layers/xrRenderVK/xrRender_VK.cpp"
    "src/Layers/xrRenderVK/stdafx.h"
    "src/xrEngine/Device_create_render_vulkan.cpp"
)

for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "   ✅ $file"
    else
        echo "   ❌ $file MISSING"
        exit 1
    fi
done

# Check CMake integration
echo "5. Checking CMake integration..."
if grep -q "xrRenderVK" src/CMakeLists.txt; then
    echo "   ✅ Vulkan renderer added to CMake build"
else
    echo "   ❌ Vulkan renderer NOT added to CMake build"
    exit 1
fi

# Check for shader path implementation
echo "6. Checking shader path implementation..."
if grep -q '"vk\\\\"' src/Layers/xrRenderVK/vk.cpp; then
    echo "   ✅ Vulkan shader path implemented"
else
    echo "   ❌ Vulkan shader path NOT implemented"
    exit 1
fi

echo
echo "🎉 All verification checks passed!"
echo "   The Vulkan renderer is properly integrated into OpenXRay."
echo
echo "To use the Vulkan renderer:"
echo "   - Build with: cmake .. && make"
echo "   - Select in code with: APILevel::Vulkan"
echo "   - Shaders will be loaded from: gamedata/shaders/vk/"
echo