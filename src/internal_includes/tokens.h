#ifndef TOKENS_H
#define TOKENS_H

#include "hlslcc.h"

enum SHADER_PHASE_TYPE
{
    SHADER_PHASE_INVALID = -1,
    MAIN_PHASE = 0,
    HS_GLOBAL_DECL_PHASE = 1,
    HS_CTRL_POINT_PHASE = 2,
    HS_FORK_PHASE = 3,
    HS_JOIN_PHASE = 4
};

static SHADER_TYPE DecodeShaderType(uint32_t ui32Token)
{
    return (SHADER_TYPE)((ui32Token & 0xffff0000) >> 16);
}

static uint32_t DecodeProgramMajorVersion(uint32_t ui32Token)
{
    return (ui32Token & 0x000000f0) >> 4;
}

static uint32_t DecodeProgramMinorVersion(uint32_t ui32Token)
{
    return (ui32Token & 0x0000000f);
}

static uint32_t DecodeInstructionLength(uint32_t ui32Token)
{
    return (ui32Token & 0x7f000000) >> 24;
}

static uint32_t DecodeIsOpcodeExtended(uint32_t ui32Token)
{
    return (ui32Token & 0x80000000) >> 31;
}

typedef enum EXTENDED_OPCODE_TYPE
{
    EXTENDED_OPCODE_EMPTY           = 0,
    EXTENDED_OPCODE_SAMPLE_CONTROLS = 1,
    EXTENDED_OPCODE_RESOURCE_DIM = 2,
    EXTENDED_OPCODE_RESOURCE_RETURN_TYPE = 3,
} EXTENDED_OPCODE_TYPE;

static EXTENDED_OPCODE_TYPE DecodeExtendedOpcodeType(uint32_t ui32Token)
{
    return (EXTENDED_OPCODE_TYPE)(ui32Token & 0x0000003f);
}

static RESOURCE_RETURN_TYPE DecodeResourceReturnType(uint32_t ui32Coord, uint32_t ui32Token)
{
    return (RESOURCE_RETURN_TYPE)((ui32Token >> (ui32Coord * 4)) & 0xF);
}

static RESOURCE_RETURN_TYPE DecodeExtendedResourceReturnType(uint32_t ui32Coord, uint32_t ui32Token)
{
    return (RESOURCE_RETURN_TYPE)((ui32Token >> (ui32Coord * 4 + 6)) & 0xF);
}

enum OPCODE_TYPE
{
    //For DX9
    OPCODE_POW = -6,
    OPCODE_DP2ADD = -5,
    OPCODE_LRP = -4,
    OPCODE_ENDREP = -3,
    OPCODE_REP = -2,
    OPCODE_SPECIAL_DCL_IMMCONST = -1,

    OPCODE_ADD,
    OPCODE_AND,
    OPCODE_BREAK,
    OPCODE_BREAKC,
    OPCODE_CALL,
    OPCODE_CALLC,
    OPCODE_CASE,
    OPCODE_CONTINUE,
    OPCODE_CONTINUEC,
    OPCODE_CUT,
    OPCODE_DEFAULT,
    OPCODE_DERIV_RTX,
    OPCODE_DERIV_RTY,
    OPCODE_DISCARD,
    OPCODE_DIV,
    OPCODE_DP2,
    OPCODE_DP3,
    OPCODE_DP4,
    OPCODE_ELSE,
    OPCODE_EMIT,
    OPCODE_EMITTHENCUT,
    OPCODE_ENDIF,
    OPCODE_ENDLOOP,
    OPCODE_ENDSWITCH,
    OPCODE_EQ,
    OPCODE_EXP,
    OPCODE_FRC,
    OPCODE_FTOI,
    OPCODE_FTOU,
    OPCODE_GE,
    OPCODE_IADD,
    OPCODE_IF,
    OPCODE_IEQ,
    OPCODE_IGE,
    OPCODE_ILT,
    OPCODE_IMAD,
    OPCODE_IMAX,
    OPCODE_IMIN,
    OPCODE_IMUL,
    OPCODE_INE,
    OPCODE_INEG,
    OPCODE_ISHL,
    OPCODE_ISHR,
    OPCODE_ITOF,
    OPCODE_LABEL,
    OPCODE_LD,
    OPCODE_LD_MS,
    OPCODE_LOG,
    OPCODE_LOOP,
    OPCODE_LT,
    OPCODE_MAD,
    OPCODE_MIN,
    OPCODE_MAX,
    OPCODE_CUSTOMDATA,
    OPCODE_MOV,
    OPCODE_MOVC,
    OPCODE_MUL,
    OPCODE_NE,
    OPCODE_NOP,
    OPCODE_NOT,
    OPCODE_OR,
    OPCODE_RESINFO,
    OPCODE_RET,
    OPCODE_RETC,
    OPCODE_ROUND_NE,
    OPCODE_ROUND_NI,
    OPCODE_ROUND_PI,
    OPCODE_ROUND_Z,
    OPCODE_RSQ,
    OPCODE_SAMPLE,
    OPCODE_SAMPLE_C,
    OPCODE_SAMPLE_C_LZ,
    OPCODE_SAMPLE_L,
    OPCODE_SAMPLE_D,
    OPCODE_SAMPLE_B,
    OPCODE_SQRT,
    OPCODE_SWITCH,
    OPCODE_SINCOS,
    OPCODE_UDIV,
    OPCODE_ULT,
    OPCODE_UGE,
    OPCODE_UMUL,
    OPCODE_UMAD,
    OPCODE_UMAX,
    OPCODE_UMIN,
    OPCODE_USHR,
    OPCODE_UTOF,
    OPCODE_XOR,
    OPCODE_DCL_RESOURCE, // DCL* opcodes have
    OPCODE_DCL_CONSTANT_BUFFER, // custom operand formats.
    OPCODE_DCL_SAMPLER,
    OPCODE_DCL_INDEX_RANGE,
    OPCODE_DCL_GS_OUTPUT_PRIMITIVE_TOPOLOGY,
    OPCODE_DCL_GS_INPUT_PRIMITIVE,
    OPCODE_DCL_MAX_OUTPUT_VERTEX_COUNT,
    OPCODE_DCL_INPUT,
    OPCODE_DCL_INPUT_SGV,
    OPCODE_DCL_INPUT_SIV,
    OPCODE_DCL_INPUT_PS,
    OPCODE_DCL_INPUT_PS_SGV,
    OPCODE_DCL_INPUT_PS_SIV,
    OPCODE_DCL_OUTPUT,
    OPCODE_DCL_OUTPUT_SGV,
    OPCODE_DCL_OUTPUT_SIV,
    OPCODE_DCL_TEMPS,
    OPCODE_DCL_INDEXABLE_TEMP,
    OPCODE_DCL_GLOBAL_FLAGS,

// -----------------------------------------------

    OPCODE_RESERVED_10,

// ---------- DX 10.1 op codes---------------------

    OPCODE_LOD,
    OPCODE_GATHER4,
    OPCODE_SAMPLE_POS,
    OPCODE_SAMPLE_INFO,

// -----------------------------------------------

    // This should be 10.1's version of NUM_OPCODES
    OPCODE_RESERVED_10_1,

// ---------- DX 11 op codes---------------------
    OPCODE_HS_DECLS, // token marks beginning of HS sub-shader
    OPCODE_HS_CONTROL_POINT_PHASE, // token marks beginning of HS sub-shader
    OPCODE_HS_FORK_PHASE, // token marks beginning of HS sub-shader
    OPCODE_HS_JOIN_PHASE, // token marks beginning of HS sub-shader

    OPCODE_EMIT_STREAM,
    OPCODE_CUT_STREAM,
    OPCODE_EMITTHENCUT_STREAM,
    OPCODE_INTERFACE_CALL,

    OPCODE_BUFINFO,
    OPCODE_DERIV_RTX_COARSE,
    OPCODE_DERIV_RTX_FINE,
    OPCODE_DERIV_RTY_COARSE,
    OPCODE_DERIV_RTY_FINE,
    OPCODE_GATHER4_C,
    OPCODE_GATHER4_PO,
    OPCODE_GATHER4_PO_C,
    OPCODE_RCP,
    OPCODE_F32TOF16,
    OPCODE_F16TOF32,
    OPCODE_UADDC,
    OPCODE_USUBB,
    OPCODE_COUNTBITS,
    OPCODE_FIRSTBIT_HI,
    OPCODE_FIRSTBIT_LO,
    OPCODE_FIRSTBIT_SHI,
    OPCODE_UBFE,
    OPCODE_IBFE,
    OPCODE_BFI,
    OPCODE_BFREV,
    OPCODE_SWAPC,

    OPCODE_DCL_STREAM,
    OPCODE_DCL_FUNCTION_BODY,
    OPCODE_DCL_FUNCTION_TABLE,
    OPCODE_DCL_INTERFACE,

    OPCODE_DCL_INPUT_CONTROL_POINT_COUNT,
    OPCODE_DCL_OUTPUT_CONTROL_POINT_COUNT,
    OPCODE_DCL_TESS_DOMAIN,
    OPCODE_DCL_TESS_PARTITIONING,
    OPCODE_DCL_TESS_OUTPUT_PRIMITIVE,
    OPCODE_DCL_HS_MAX_TESSFACTOR,
    OPCODE_DCL_HS_FORK_PHASE_INSTANCE_COUNT,
    OPCODE_DCL_HS_JOIN_PHASE_INSTANCE_COUNT,

    OPCODE_DCL_THREAD_GROUP,
    OPCODE_DCL_UNORDERED_ACCESS_VIEW_TYPED,
    OPCODE_DCL_UNORDERED_ACCESS_VIEW_RAW,
    OPCODE_DCL_UNORDERED_ACCESS_VIEW_STRUCTURED,
    OPCODE_DCL_THREAD_GROUP_SHARED_MEMORY_RAW,
    OPCODE_DCL_THREAD_GROUP_SHARED_MEMORY_STRUCTURED,
    OPCODE_DCL_RESOURCE_RAW,
    OPCODE_DCL_RESOURCE_STRUCTURED,
    OPCODE_LD_UAV_TYPED,
    OPCODE_STORE_UAV_TYPED,
    OPCODE_LD_RAW,
    OPCODE_STORE_RAW,
    OPCODE_LD_STRUCTURED,
    OPCODE_STORE_STRUCTURED,
    OPCODE_ATOMIC_AND,
    OPCODE_ATOMIC_OR,
    OPCODE_ATOMIC_XOR,
    OPCODE_ATOMIC_CMP_STORE,
    OPCODE_ATOMIC_IADD,
    OPCODE_ATOMIC_IMAX,
    OPCODE_ATOMIC_IMIN,
    OPCODE_ATOMIC_UMAX,
    OPCODE_ATOMIC_UMIN,
    OPCODE_IMM_ATOMIC_ALLOC,
    OPCODE_IMM_ATOMIC_CONSUME,
    OPCODE_IMM_ATOMIC_IADD,
    OPCODE_IMM_ATOMIC_AND,
    OPCODE_IMM_ATOMIC_OR,
    OPCODE_IMM_ATOMIC_XOR,
    OPCODE_IMM_ATOMIC_EXCH,
    OPCODE_IMM_ATOMIC_CMP_EXCH,
    OPCODE_IMM_ATOMIC_IMAX,
    OPCODE_IMM_ATOMIC_IMIN,
    OPCODE_IMM_ATOMIC_UMAX,
    OPCODE_IMM_ATOMIC_UMIN,
    OPCODE_SYNC,

    OPCODE_DADD,
    OPCODE_DMAX,
    OPCODE_DMIN,
    OPCODE_DMUL,
    OPCODE_DEQ,
    OPCODE_DGE,
    OPCODE_DLT,
    OPCODE_DNE,
    OPCODE_DMOV,
    OPCODE_DMOVC,
    OPCODE_DTOF,
    OPCODE_FTOD,

    OPCODE_EVAL_SNAPPED,
    OPCODE_EVAL_SAMPLE_INDEX,
    OPCODE_EVAL_CENTROID,

    OPCODE_DCL_GS_INSTANCE_COUNT,

    OPCODE_ABORT,
    OPCODE_DEBUG_BREAK,

// -----------------------------------------------

    // This marks the end of D3D11.0 opcodes
    OPCODE_RESERVED_11,

    OPCODE_DDIV,
    OPCODE_DFMA,
    OPCODE_DRCP,

    OPCODE_MSAD,

    OPCODE_DTOI,
    OPCODE_DTOU,
    OPCODE_ITOD,
    OPCODE_UTOD,

// -----------------------------------------------

    // This marks the end of D3D11.1 opcodes
    OPCODE_RESERVED_11_1,

    NUM_OPCODES,
    OPCODE_INVALID = NUM_OPCODES,
};

static OPCODE_TYPE DecodeOpcodeType(uint32_t ui32Token)
{
    return (OPCODE_TYPE)(ui32Token & 0x00007ff);
}

typedef enum
{
    INDEX_0D,
    INDEX_1D,
    INDEX_2D,
    INDEX_3D,
} OPERAND_INDEX_DIMENSION;

static OPERAND_INDEX_DIMENSION DecodeOperandIndexDimension(uint32_t ui32Token)
{
    return (OPERAND_INDEX_DIMENSION)((ui32Token & 0x00300000) >> 20);
}

typedef enum OPERAND_TYPE
{
    OPERAND_TYPE_SPECIAL_LOOPCOUNTER = -10,
    OPERAND_TYPE_SPECIAL_IMMCONSTINT = -9,
    OPERAND_TYPE_SPECIAL_TEXCOORD = -8,
    OPERAND_TYPE_SPECIAL_POSITION = -7,
    OPERAND_TYPE_SPECIAL_FOG = -6,
    OPERAND_TYPE_SPECIAL_POINTSIZE = -5,
    OPERAND_TYPE_SPECIAL_OUTOFFSETCOLOUR = -4,
    OPERAND_TYPE_SPECIAL_OUTBASECOLOUR = -3,
    OPERAND_TYPE_SPECIAL_ADDRESS = -2,
    OPERAND_TYPE_SPECIAL_IMMCONST = -1,
    OPERAND_TYPE_TEMP           = 0,  // Temporary Register File
    OPERAND_TYPE_INPUT          = 1,  // General Input Register File
    OPERAND_TYPE_OUTPUT         = 2,  // General Output Register File
    OPERAND_TYPE_INDEXABLE_TEMP = 3,  // Temporary Register File (indexable)
    OPERAND_TYPE_IMMEDIATE32    = 4,  // 32bit/component immediate value(s)
    // If for example, operand token bits
    // [01:00]==OPERAND_4_COMPONENT,
    // this means that the operand type:
    // OPERAND_TYPE_IMMEDIATE32
    // results in 4 additional 32bit
    // DWORDS present for the operand.
    OPERAND_TYPE_IMMEDIATE64    = 5,  // 64bit/comp.imm.val(s)HI:LO
    OPERAND_TYPE_SAMPLER        = 6,  // Reference to sampler state
    OPERAND_TYPE_RESOURCE       = 7,  // Reference to memory resource (e.g. texture)
    OPERAND_TYPE_CONSTANT_BUFFER = 8,  // Reference to constant buffer
    OPERAND_TYPE_IMMEDIATE_CONSTANT_BUFFER = 9,  // Reference to immediate constant buffer
    OPERAND_TYPE_LABEL          = 10, // Label
    OPERAND_TYPE_INPUT_PRIMITIVEID = 11, // Input primitive ID
    OPERAND_TYPE_OUTPUT_DEPTH   = 12, // Output Depth
    OPERAND_TYPE_NULL           = 13, // Null register, used to discard results of operations
    // Below Are operands new in DX 10.1
    OPERAND_TYPE_RASTERIZER     = 14, // DX10.1 Rasterizer register, used to denote the depth/stencil and render target resources
    OPERAND_TYPE_OUTPUT_COVERAGE_MASK = 15, // DX10.1 PS output MSAA coverage mask (scalar)
                                            // Below Are operands new in DX 11
    OPERAND_TYPE_STREAM         = 16, // Reference to GS stream output resource
    OPERAND_TYPE_FUNCTION_BODY  = 17, // Reference to a function definition
    OPERAND_TYPE_FUNCTION_TABLE = 18, // Reference to a set of functions used by a class
    OPERAND_TYPE_INTERFACE      = 19, // Reference to an interface
    OPERAND_TYPE_FUNCTION_INPUT = 20, // Reference to an input parameter to a function
    OPERAND_TYPE_FUNCTION_OUTPUT = 21, // Reference to an output parameter to a function
    OPERAND_TYPE_OUTPUT_CONTROL_POINT_ID = 22, // HS Control Point phase input saying which output control point ID this is
    OPERAND_TYPE_INPUT_FORK_INSTANCE_ID = 23, // HS Fork Phase input instance ID
    OPERAND_TYPE_INPUT_JOIN_INSTANCE_ID = 24, // HS Join Phase input instance ID
    OPERAND_TYPE_INPUT_CONTROL_POINT = 25, // HS Fork+Join, DS phase input control points (array of them)
    OPERAND_TYPE_OUTPUT_CONTROL_POINT = 26, // HS Fork+Join phase output control points (array of them)
    OPERAND_TYPE_INPUT_PATCH_CONSTANT = 27, // DS+HSJoin Input Patch Constants (array of them)
    OPERAND_TYPE_INPUT_DOMAIN_POINT = 28, // DS Input Domain point
    OPERAND_TYPE_THIS_POINTER       = 29, // Reference to an interface this pointer
    OPERAND_TYPE_UNORDERED_ACCESS_VIEW = 30, // Reference to UAV u#
    OPERAND_TYPE_THREAD_GROUP_SHARED_MEMORY = 31, // Reference to Thread Group Shared Memory g#
    OPERAND_TYPE_INPUT_THREAD_ID = 32, // Compute Shader Thread ID
    OPERAND_TYPE_INPUT_THREAD_GROUP_ID = 33, // Compute Shader Thread Group ID
    OPERAND_TYPE_INPUT_THREAD_ID_IN_GROUP = 34, // Compute Shader Thread ID In Thread Group
    OPERAND_TYPE_INPUT_COVERAGE_MASK = 35, // Pixel shader coverage mask input
    OPERAND_TYPE_INPUT_THREAD_ID_IN_GROUP_FLATTENED = 36, // Compute Shader Thread ID In Group Flattened to a 1D value.
    OPERAND_TYPE_INPUT_GS_INSTANCE_ID = 37, // Input GS instance ID
    OPERAND_TYPE_OUTPUT_DEPTH_GREATER_EQUAL = 38, // Output Depth, forced to be greater than or equal than current depth
    OPERAND_TYPE_OUTPUT_DEPTH_LESS_EQUAL    = 39, // Output Depth, forced to be less than or equal to current depth
    OPERAND_TYPE_CYCLE_COUNTER = 40, // Cycle counter
} OPERAND_TYPE;

static OPERAND_TYPE DecodeOperandType(uint32_t ui32Token)
{
    return (OPERAND_TYPE)((ui32Token & 0x000ff000) >> 12);
}

static SPECIAL_NAME DecodeOperandSpecialName(uint32_t ui32Token)
{
    return (SPECIAL_NAME)(ui32Token & 0x0000ffff);
}

typedef enum OPERAND_INDEX_REPRESENTATION
{
    OPERAND_INDEX_IMMEDIATE32               = 0, // Extra DWORD
    OPERAND_INDEX_IMMEDIATE64               = 1, // 2 Extra DWORDs
    //   (HI32:LO32)
    OPERAND_INDEX_RELATIVE                  = 2, // Extra operand
    OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE = 3, // Extra DWORD followed by
    //   extra operand
    OPERAND_INDEX_IMMEDIATE64_PLUS_RELATIVE = 4, // 2 Extra DWORDS
    //   (HI32:LO32) followed
    //   by extra operand
} OPERAND_INDEX_REPRESENTATION;

static OPERAND_INDEX_REPRESENTATION DecodeOperandIndexRepresentation(uint32_t ui32Dimension, uint32_t ui32Token)
{
    return (OPERAND_INDEX_REPRESENTATION)((ui32Token & (0x7 << (22 + 3 * ((ui32Dimension) & 3)))) >> (22 + 3 * ((ui32Dimension) & 3)));
}

typedef enum OPERAND_NUM_COMPONENTS
{
    OPERAND_0_COMPONENT = 0,
    OPERAND_1_COMPONENT = 1,
    OPERAND_4_COMPONENT = 2,
    OPERAND_N_COMPONENT = 3 // unused for now
} OPERAND_NUM_COMPONENTS;

static OPERAND_NUM_COMPONENTS DecodeOperandNumComponents(uint32_t ui32Token)
{
    return (OPERAND_NUM_COMPONENTS)(ui32Token & 0x00000003);
}

typedef enum OPERAND_4_COMPONENT_SELECTION_MODE
{
    OPERAND_4_COMPONENT_MASK_MODE    = 0,  // mask 4 components
    OPERAND_4_COMPONENT_SWIZZLE_MODE = 1,  // swizzle 4 components
    OPERAND_4_COMPONENT_SELECT_1_MODE = 2, // select 1 of 4 components
} OPERAND_4_COMPONENT_SELECTION_MODE;

static OPERAND_4_COMPONENT_SELECTION_MODE DecodeOperand4CompSelMode(uint32_t ui32Token)
{
    return (OPERAND_4_COMPONENT_SELECTION_MODE)((ui32Token & 0x0000000c) >> 2);
}

#define OPERAND_4_COMPONENT_MASK_X      0x00000001
#define OPERAND_4_COMPONENT_MASK_Y      0x00000002
#define OPERAND_4_COMPONENT_MASK_Z      0x00000004
#define OPERAND_4_COMPONENT_MASK_W      0x00000008
#define OPERAND_4_COMPONENT_MASK_R      OPERAND_4_COMPONENT_MASK_X
#define OPERAND_4_COMPONENT_MASK_G      OPERAND_4_COMPONENT_MASK_Y
#define OPERAND_4_COMPONENT_MASK_B      OPERAND_4_COMPONENT_MASK_Z
#define OPERAND_4_COMPONENT_MASK_A      OPERAND_4_COMPONENT_MASK_W
#define OPERAND_4_COMPONENT_MASK_ALL    0x0000000f

static uint32_t DecodeOperand4CompMask(uint32_t ui32Token)
{
    return (uint32_t)((ui32Token & 0x000000f0) >> 4);
}

static uint32_t DecodeOperand4CompSwizzle(uint32_t ui32Token)
{
    return (uint32_t)((ui32Token & 0x00000ff0) >> 4);
}

static uint32_t DecodeOperand4CompSel1(uint32_t ui32Token)
{
    return (uint32_t)((ui32Token & 0x00000030) >> 4);
}

#define OPERAND_4_COMPONENT_X      0
#define OPERAND_4_COMPONENT_Y      1
#define OPERAND_4_COMPONENT_Z      2
#define OPERAND_4_COMPONENT_W      3

static const uint32_t NO_SWIZZLE = (((OPERAND_4_COMPONENT_X) | (OPERAND_4_COMPONENT_Y << 2) | (OPERAND_4_COMPONENT_Z << 4) | (OPERAND_4_COMPONENT_W << 6)) /*<<4*/);

static const uint32_t XXXX_SWIZZLE = (((OPERAND_4_COMPONENT_X) | (OPERAND_4_COMPONENT_X << 2) | (OPERAND_4_COMPONENT_X << 4) | (OPERAND_4_COMPONENT_X << 6)));
static const uint32_t YYYY_SWIZZLE = (((OPERAND_4_COMPONENT_Y) | (OPERAND_4_COMPONENT_Y << 2) | (OPERAND_4_COMPONENT_Y << 4) | (OPERAND_4_COMPONENT_Y << 6)));
static const uint32_t ZZZZ_SWIZZLE = (((OPERAND_4_COMPONENT_Z) | (OPERAND_4_COMPONENT_Z << 2) | (OPERAND_4_COMPONENT_Z << 4) | (OPERAND_4_COMPONENT_Z << 6)));
static const uint32_t WWWW_SWIZZLE = (((OPERAND_4_COMPONENT_W) | (OPERAND_4_COMPONENT_W << 2) | (OPERAND_4_COMPONENT_W << 4) | (OPERAND_4_COMPONENT_W << 6)));

static uint32_t DecodeOperand4CompSwizzleSource(uint32_t ui32Token, uint32_t comp)
{
    return (uint32_t)(((ui32Token) >> (4 + 2 * ((comp) & 3))) & 3);
}

typedef enum RESOURCE_DIMENSION
{
    RESOURCE_DIMENSION_UNKNOWN = 0,
    RESOURCE_DIMENSION_BUFFER = 1,
    RESOURCE_DIMENSION_TEXTURE1D = 2,
    RESOURCE_DIMENSION_TEXTURE2D = 3,
    RESOURCE_DIMENSION_TEXTURE2DMS = 4,
    RESOURCE_DIMENSION_TEXTURE3D = 5,
    RESOURCE_DIMENSION_TEXTURECUBE = 6,
    RESOURCE_DIMENSION_TEXTURE1DARRAY = 7,
    RESOURCE_DIMENSION_TEXTURE2DARRAY = 8,
    RESOURCE_DIMENSION_TEXTURE2DMSARRAY = 9,
    RESOURCE_DIMENSION_TEXTURECUBEARRAY = 10,
    RESOURCE_DIMENSION_RAW_BUFFER = 11,
    RESOURCE_DIMENSION_STRUCTURED_BUFFER = 12,
} RESOURCE_DIMENSION;

static RESOURCE_DIMENSION DecodeResourceDimension(uint32_t ui32Token)
{
    return (RESOURCE_DIMENSION)((ui32Token & 0x0000f800) >> 11);
}

static RESOURCE_DIMENSION DecodeExtendedResourceDimension(uint32_t ui32Token)
{
    return (RESOURCE_DIMENSION)((ui32Token & 0x000007C0) >> 6);
}

typedef enum INSTRUCTION_TEST_BOOLEAN
{
    INSTRUCTION_TEST_ZERO       = 0,
    INSTRUCTION_TEST_NONZERO    = 1
} INSTRUCTION_TEST_BOOLEAN;

static INSTRUCTION_TEST_BOOLEAN DecodeInstrTestBool(uint32_t ui32Token)
{
    return (INSTRUCTION_TEST_BOOLEAN)((ui32Token & 0x00040000) >> 18);
}

static uint32_t DecodeIsOperandExtended(uint32_t ui32Token)
{
    return (ui32Token & 0x80000000) >> 31;
}

typedef enum EXTENDED_OPERAND_TYPE
{
    EXTENDED_OPERAND_EMPTY            = 0,
    EXTENDED_OPERAND_MODIFIER         = 1,
} EXTENDED_OPERAND_TYPE;

static EXTENDED_OPERAND_TYPE DecodeExtendedOperandType(uint32_t ui32Token)
{
    return (EXTENDED_OPERAND_TYPE)(ui32Token & 0x0000003f);
}

typedef enum OPERAND_MODIFIER
{
    OPERAND_MODIFIER_NONE     = 0,
    OPERAND_MODIFIER_NEG      = 1,
    OPERAND_MODIFIER_ABS      = 2,
    OPERAND_MODIFIER_ABSNEG   = 3,
} OPERAND_MODIFIER;

static OPERAND_MODIFIER DecodeExtendedOperandModifier(uint32_t ui32Token)
{
    return (OPERAND_MODIFIER)((ui32Token & 0x00003fc0) >> 6);
}

static const uint32_t GLOBAL_FLAG_REFACTORING_ALLOWED = (1 << 11);
static const uint32_t GLOBAL_FLAG_ENABLE_DOUBLE_PRECISION_FLOAT_OPS = (1 << 12);
static const uint32_t GLOBAL_FLAG_FORCE_EARLY_DEPTH_STENCIL = (1 << 13);
static const uint32_t GLOBAL_FLAG_ENABLE_RAW_AND_STRUCTURED_BUFFERS = (1 << 14);
static const uint32_t GLOBAL_FLAG_SKIP_OPTIMIZATION = (1 << 15);
static const uint32_t GLOBAL_FLAG_ENABLE_MINIMUM_PRECISION = (1 << 16);
static const uint32_t GLOBAL_FLAG_ENABLE_DOUBLE_EXTENSIONS = (1 << 17);
static const uint32_t GLOBAL_FLAG_ENABLE_SHADER_EXTENSIONS = (1 << 18);

static uint32_t DecodeGlobalFlags(uint32_t ui32Token)
{
    return (uint32_t)(ui32Token & 0x00fff800);
}

static INTERPOLATION_MODE DecodeInterpolationMode(uint32_t ui32Token)
{
    return (INTERPOLATION_MODE)((ui32Token & 0x00007800) >> 11);
}

typedef enum PRIMITIVE_TOPOLOGY
{
    PRIMITIVE_TOPOLOGY_UNDEFINED = 0,
    PRIMITIVE_TOPOLOGY_POINTLIST = 1,
    PRIMITIVE_TOPOLOGY_LINELIST = 2,
    PRIMITIVE_TOPOLOGY_LINESTRIP = 3,
    PRIMITIVE_TOPOLOGY_TRIANGLELIST = 4,
    PRIMITIVE_TOPOLOGY_TRIANGLESTRIP = 5,
    // 6 is reserved for legacy triangle fans
    // Adjacency values should be equal to (0x8 & non-adjacency):
    PRIMITIVE_TOPOLOGY_LINELIST_ADJ = 10,
    PRIMITIVE_TOPOLOGY_LINESTRIP_ADJ = 11,
    PRIMITIVE_TOPOLOGY_TRIANGLELIST_ADJ = 12,
    PRIMITIVE_TOPOLOGY_TRIANGLESTRIP_ADJ = 13,
} PRIMITIVE_TOPOLOGY;

static PRIMITIVE_TOPOLOGY DecodeGSOutputPrimitiveTopology(uint32_t ui32Token)
{
    return (PRIMITIVE_TOPOLOGY)((ui32Token & 0x0001f800) >> 11);
}

typedef enum PRIMITIVE
{
    PRIMITIVE_UNDEFINED = 0,
    PRIMITIVE_POINT = 1,
    PRIMITIVE_LINE = 2,
    PRIMITIVE_TRIANGLE = 3,
    // Adjacency values should be equal to (0x4 & non-adjacency):
    PRIMITIVE_LINE_ADJ = 6,
    PRIMITIVE_TRIANGLE_ADJ = 7,
    PRIMITIVE_1_CONTROL_POINT_PATCH = 8,
    PRIMITIVE_2_CONTROL_POINT_PATCH = 9,
    PRIMITIVE_3_CONTROL_POINT_PATCH = 10,
    PRIMITIVE_4_CONTROL_POINT_PATCH = 11,
    PRIMITIVE_5_CONTROL_POINT_PATCH = 12,
    PRIMITIVE_6_CONTROL_POINT_PATCH = 13,
    PRIMITIVE_7_CONTROL_POINT_PATCH = 14,
    PRIMITIVE_8_CONTROL_POINT_PATCH = 15,
    PRIMITIVE_9_CONTROL_POINT_PATCH = 16,
    PRIMITIVE_10_CONTROL_POINT_PATCH = 17,
    PRIMITIVE_11_CONTROL_POINT_PATCH = 18,
    PRIMITIVE_12_CONTROL_POINT_PATCH = 19,
    PRIMITIVE_13_CONTROL_POINT_PATCH = 20,
    PRIMITIVE_14_CONTROL_POINT_PATCH = 21,
    PRIMITIVE_15_CONTROL_POINT_PATCH = 22,
    PRIMITIVE_16_CONTROL_POINT_PATCH = 23,
    PRIMITIVE_17_CONTROL_POINT_PATCH = 24,
    PRIMITIVE_18_CONTROL_POINT_PATCH = 25,
    PRIMITIVE_19_CONTROL_POINT_PATCH = 26,
    PRIMITIVE_20_CONTROL_POINT_PATCH = 27,
    PRIMITIVE_21_CONTROL_POINT_PATCH = 28,
    PRIMITIVE_22_CONTROL_POINT_PATCH = 29,
    PRIMITIVE_23_CONTROL_POINT_PATCH = 30,
    PRIMITIVE_24_CONTROL_POINT_PATCH = 31,
    PRIMITIVE_25_CONTROL_POINT_PATCH = 32,
    PRIMITIVE_26_CONTROL_POINT_PATCH = 33,
    PRIMITIVE_27_CONTROL_POINT_PATCH = 34,
    PRIMITIVE_28_CONTROL_POINT_PATCH = 35,
    PRIMITIVE_29_CONTROL_POINT_PATCH = 36,
    PRIMITIVE_30_CONTROL_POINT_PATCH = 37,
    PRIMITIVE_31_CONTROL_POINT_PATCH = 38,
    PRIMITIVE_32_CONTROL_POINT_PATCH = 39,
} PRIMITIVE;

static PRIMITIVE DecodeGSInputPrimitive(uint32_t ui32Token)
{
    return (PRIMITIVE)((ui32Token & 0x0001f800) >> 11);
}

static TESSELLATOR_PARTITIONING DecodeTessPartitioning(uint32_t ui32Token)
{
    return (TESSELLATOR_PARTITIONING)((ui32Token & 0x00003800) >> 11);
}

static TESSELLATOR_DOMAIN DecodeTessDomain(uint32_t ui32Token)
{
    return (TESSELLATOR_DOMAIN)((ui32Token & 0x00001800) >> 11);
}

static TESSELLATOR_OUTPUT_PRIMITIVE DecodeTessOutPrim(uint32_t ui32Token)
{
    return (TESSELLATOR_OUTPUT_PRIMITIVE)((ui32Token & 0x00003800) >> 11);
}

static const uint32_t SYNC_THREADS_IN_GROUP = 0x00000800;
static const uint32_t SYNC_THREAD_GROUP_SHARED_MEMORY = 0x00001000;
static const uint32_t SYNC_UNORDERED_ACCESS_VIEW_MEMORY_GROUP = 0x00002000;
static const uint32_t SYNC_UNORDERED_ACCESS_VIEW_MEMORY_GLOBAL = 0x00004000;

static uint32_t DecodeSyncFlags(uint32_t ui32Token)
{
    return ui32Token & 0x00007800;
}

// The number of types that implement this interface
static uint32_t DecodeInterfaceTableLength(uint32_t ui32Token)
{
    return (uint32_t)((ui32Token & 0x0000ffff) >> 0);
}

// The number of interfaces that are defined in this array.
static uint32_t DecodeInterfaceArrayLength(uint32_t ui32Token)
{
    return (uint32_t)((ui32Token & 0xffff0000) >> 16);
}

typedef enum CUSTOMDATA_CLASS
{
    CUSTOMDATA_COMMENT = 0,
    CUSTOMDATA_DEBUGINFO,
    CUSTOMDATA_OPAQUE,
    CUSTOMDATA_DCL_IMMEDIATE_CONSTANT_BUFFER,
    CUSTOMDATA_SHADER_MESSAGE,
} CUSTOMDATA_CLASS;

static CUSTOMDATA_CLASS DecodeCustomDataClass(uint32_t ui32Token)
{
    return (CUSTOMDATA_CLASS)((ui32Token & 0xfffff800) >> 11);
}

static uint32_t DecodeInstructionSaturate(uint32_t ui32Token)
{
    return (ui32Token & 0x00002000) ? 1 : 0;
}

static uint32_t DecodeInstructionPreciseMask(uint32_t ui32Token) // "precise" keyword
{
    return (uint32_t)((ui32Token & 0x00780000) >> 19);
}

typedef enum OPERAND_MIN_PRECISION
{
    OPERAND_MIN_PRECISION_DEFAULT    = 0, // Default precision
                                          // for the shader model
    OPERAND_MIN_PRECISION_FLOAT_16   = 1, // Min 16 bit/component float
    OPERAND_MIN_PRECISION_FLOAT_2_8  = 2, // Min 10(2.8)bit/comp. float
    OPERAND_MIN_PRECISION_SINT_16    = 4, // Min 16 bit/comp. signed integer
    OPERAND_MIN_PRECISION_UINT_16    = 5, // Min 16 bit/comp. unsigned integer
} OPERAND_MIN_PRECISION;

static uint32_t DecodeOperandMinPrecision(uint32_t ui32Token)
{
    return (ui32Token & 0x0001C000) >> 14;
}

static uint32_t DecodeOutputControlPointCount(uint32_t ui32Token)
{
    return ((ui32Token & 0x0001f800) >> 11);
}

typedef enum IMMEDIATE_ADDRESS_OFFSET_COORD
{
    IMMEDIATE_ADDRESS_OFFSET_U        = 0,
    IMMEDIATE_ADDRESS_OFFSET_V        = 1,
    IMMEDIATE_ADDRESS_OFFSET_W        = 2,
} IMMEDIATE_ADDRESS_OFFSET_COORD;


#define IMMEDIATE_ADDRESS_OFFSET_SHIFT(Coord) (9+4*((Coord)&3))
#define IMMEDIATE_ADDRESS_OFFSET_MASK(Coord) (0x0000000f<<IMMEDIATE_ADDRESS_OFFSET_SHIFT(Coord))

static uint32_t DecodeImmediateAddressOffset(IMMEDIATE_ADDRESS_OFFSET_COORD eCoord, uint32_t ui32Token)
{
    return ((((ui32Token) & IMMEDIATE_ADDRESS_OFFSET_MASK(eCoord)) >> (IMMEDIATE_ADDRESS_OFFSET_SHIFT(eCoord))));
}

// UAV access scope flags
static const uint32_t GLOBALLY_COHERENT_ACCESS = 0x00010000;
static uint32_t DecodeAccessCoherencyFlags(uint32_t ui32Token)
{
    return ui32Token & 0x00010000;
}

typedef enum RESINFO_RETURN_TYPE
{
    RESINFO_INSTRUCTION_RETURN_FLOAT      = 0,
    RESINFO_INSTRUCTION_RETURN_RCPFLOAT   = 1,
    RESINFO_INSTRUCTION_RETURN_UINT       = 2
} RESINFO_RETURN_TYPE;

static RESINFO_RETURN_TYPE DecodeResInfoReturnType(uint32_t ui32Token)
{
    return (RESINFO_RETURN_TYPE)((ui32Token & 0x00001800) >> 11);
}

typedef enum SB_SAMPLER_MODE
{
    D3D10_SB_SAMPLER_MODE_DEFAULT = 0,
    D3D10_SB_SAMPLER_MODE_COMPARISON = 1,
    D3D10_SB_SAMPLER_MODE_MONO = 2,
} SB_SAMPLER_MODE;

static SB_SAMPLER_MODE DecodeSamplerMode(uint32_t ui32Token)
{
    return (SB_SAMPLER_MODE)((ui32Token & 0x00001800) >> 11);
}

#endif
