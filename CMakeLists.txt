
cmake_minimum_required(VERSION 2.8)

project(HLSLcc)

option(HLSLCC_LIBRARY_SHARED "Build shared library instead of static." ON)

file(GLOB HLSLCC_HDRS "${CMAKE_CURRENT_SOURCE_DIR}/include/*")
set(HLSLCC_SRC
		src/ControlFlowGraph.cpp
		src/ControlFlowGraphUtils.cpp
		src/DataTypeAnalysis.cpp
		src/Declaration.cpp
		src/decode.cpp
		src/HLSLcc.cpp
		src/HLSLccToolkit.cpp
		src/HLSLCrossCompilerContext.cpp
		src/Instruction.cpp
		src/LoopTransform.cpp
		src/Operand.cpp
		src/reflect.cpp
		src/Shader.cpp
		src/ShaderInfo.cpp
		src/toGLSL.cpp
		src/toGLSLDeclaration.cpp
		src/toGLSLInstruction.cpp
		src/toGLSLOperand.cpp
		src/toMetal.cpp
		src/toMetalDeclaration.cpp
		src/toMetalInstruction.cpp
		src/toMetalOperand.cpp
		src/UseDefineChains.cpp
		src/cbstring/bsafe.c
		src/cbstring/bstraux.c
		src/cbstring/bstrlib.c)

if(HLSLCC_LIBRARY_SHARED)
	add_library(hlslcc SHARED ${HLSLCC_SRC})
else(HLSLCC_LIBRARY_SHARED)
	add_library(hlslcc STATIC ${HLSLCC_SRC})
endif(HLSLCC_LIBRARY_SHARED)

target_include_directories(hlslcc
    PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
    PRIVATE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/cbstring>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/internal_includes>)
set_target_properties(hlslcc PROPERTIES
    CXX_STANDARD 11
    CXX_STANDARD_REQUIRED ON
    PUBLIC_HEADERS ${HLSLCC_HDRS})
install(TARGETS hlslcc
    EXPORT hlslccConfig
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
    PUBLIC_HEADER DESTINATION include/hlslcc)
install(FILES ${HLSLCC_HDRS} DESTINATION include/hlslcc)
install(EXPORT hlslccConfig DESTINATION share/hlslcc/cmake)
export(TARGETS hlslcc FILE hlslccConfig.cmake)
