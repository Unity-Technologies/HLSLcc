#pragma once

// In Unity, instancing array sizes should be able to be dynamically patched at runtime by defining the macro.

#include <string>
#define UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO "UNITY_RUNTIME_INSTANCING_ARRAY_SIZE"

const unsigned int kArraySizeConstantID = 0;

// TODO: share with Runtime/GfxDevice/InstancingUtilities.h
inline bool IsUnityInstancingConstantBufferName(const char* cbName)
{
    static const char kInstancedCbNamePrefix[] = "UnityInstancing";
    return strncmp(cbName, kInstancedCbNamePrefix, sizeof(kInstancedCbNamePrefix) - 1) == 0;
}
