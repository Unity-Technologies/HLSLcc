#include <memory>

#include "internal_includes/tokens.h"
#include "internal_includes/decode.h"
#include "stdlib.h"
#include "stdio.h"
#include "bstrlib.h"
#include "internal_includes/toGLSL.h"
#include "internal_includes/toGLSLOperand.h"
#include "internal_includes/Declaration.h"
#include "internal_includes/languages.h"
#include "internal_includes/debug.h"
#include "internal_includes/HLSLccToolkit.h"
#include "internal_includes/UseDefineChains.h"
#include "internal_includes/DataTypeAnalysis.h"
#include "internal_includes/Shader.h"
#include "internal_includes/HLSLCrossCompilerContext.h"
#include "internal_includes/Instruction.h"
#include "internal_includes/LoopTransform.h"
#include "UnityInstancingFlexibleArraySize.h"
#include <algorithm>
#include <sstream>

// In GLSL, the input and output names cannot clash.
// Also, the output name of previous stage must match the input name of the next stage.
// So, do gymnastics depending on which shader we're running on and which other shaders exist in this program.
//
void ToGLSL::SetIOPrefixes()
{
    switch (psContext->psShader->eShaderType)
    {
        case VERTEX_SHADER:
            psContext->inputPrefix = "in_";
            psContext->outputPrefix = "vs_";
            break;

        case HULL_SHADER:
            // Input always coming from vertex shader
            psContext->inputPrefix = "vs_";
            psContext->outputPrefix = "hs_";
            break;

        case DOMAIN_SHADER:
            // There's no domain shader without hull shader
            psContext->inputPrefix = "hs_";
            psContext->outputPrefix = "ds_";
            break;

        case GEOMETRY_SHADER:
            // The input depends on whether there's a tessellation shader before us
            if (psContext->psDependencies && (psContext->psDependencies->ui32ProgramStages & PS_FLAG_DOMAIN_SHADER))
                psContext->inputPrefix = "ds_";
            else
                psContext->inputPrefix = "vs_";

            psContext->outputPrefix = "gs_";
            break;

        case PIXEL_SHADER:
            // The inputs can come from geom shader, domain shader or directly from vertex shader
            if (psContext->psDependencies)
            {
                if (psContext->psDependencies->ui32ProgramStages & PS_FLAG_GEOMETRY_SHADER)
                {
                    psContext->inputPrefix = "gs_";
                }
                else if (psContext->psDependencies->ui32ProgramStages & PS_FLAG_DOMAIN_SHADER)
                {
                    psContext->inputPrefix = "ds_";
                }
                else
                {
                    psContext->inputPrefix = "vs_";
                }
            }
            else
            {
                psContext->inputPrefix = "vs_";
            }
            psContext->outputPrefix = "";
            break;


        case COMPUTE_SHADER:
        default:
            // No prefixes
            psContext->inputPrefix = "";
            psContext->outputPrefix = "";
            break;
    }
}

static void AddVersionDependentCode(HLSLCrossCompilerContext* psContext)
{
    bstring glsl = *psContext->currentGLSLString;
    bstring extensions = psContext->extensions;
    bool isES = (psContext->psShader->eTargetLanguage >= LANG_ES_100 && psContext->psShader->eTargetLanguage <= LANG_ES_310);
    bool GL_ARB_shader_storage_buffer_object = false;
    bool GL_ARB_shader_image_load_store = false;

    if (psContext->psShader->ui32MajorVersion > 3 && psContext->psShader->eTargetLanguage != LANG_ES_100 && psContext->psShader->eTargetLanguage != LANG_ES_300 && psContext->psShader->eTargetLanguage != LANG_ES_310 && !(psContext->psShader->eTargetLanguage >= LANG_330))
    {
        psContext->EnableExtension("GL_ARB_shader_bit_encoding");
    }

    if (!HaveCompute(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->eShaderType == COMPUTE_SHADER)
        {
            psContext->EnableExtension("GL_ARB_compute_shader");
        }

        if (psContext->psShader->aiOpcodeUsed[OPCODE_DCL_UNORDERED_ACCESS_VIEW_STRUCTURED] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DCL_UNORDERED_ACCESS_VIEW_RAW] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DCL_RESOURCE_STRUCTURED] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DCL_RESOURCE_RAW])
        {
            GL_ARB_shader_storage_buffer_object = true;
        }
    }

    if (!HaveAtomicMem(psContext->psShader->eTargetLanguage) ||
        !HaveAtomicCounter(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_ALLOC] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_CONSUME] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DCL_UNORDERED_ACCESS_VIEW_STRUCTURED])
        {
            psContext->EnableExtension("GL_ARB_shader_atomic_counters");
        }
    }

    if (psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_CMP_STORE] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_AND] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_AND] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_IADD] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_IADD] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_OR] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_XOR] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_IMIN] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_ATOMIC_UMIN] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_IMAX] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_IMIN] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_UMAX] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_UMIN] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_OR] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_XOR] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_EXCH] ||
        psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_CMP_EXCH])
    {
        if (!HaveAtomicMem(psContext->psShader->eTargetLanguage))
            GL_ARB_shader_storage_buffer_object = true;

        if (!HaveImageAtomics(psContext->psShader->eTargetLanguage))
        {
            if (isES)
                psContext->EnableExtension("GL_OES_shader_image_atomic");
            else
                GL_ARB_shader_image_load_store = true;
        }
    }

    if (!HaveGather(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO_C] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_C])
        {
            psContext->EnableExtension("GL_ARB_texture_gather");
        }
    }

    if (IsESLanguage(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTX_COARSE] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTX_FINE] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTX] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTY_COARSE] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTY_FINE] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_DERIV_RTY])
        {
            if (psContext->psShader->eTargetLanguage < LANG_ES_300)
            {
                psContext->EnableExtension("GL_OES_standard_derivatives");
            }
        }

        if (psContext->psShader->eShaderType == PIXEL_SHADER &&
            (psContext->psShader->aiOpcodeUsed[OPCODE_SAMPLE_L] ||
             psContext->psShader->aiOpcodeUsed[OPCODE_SAMPLE_C_LZ] ||
             psContext->psShader->aiOpcodeUsed[OPCODE_SAMPLE_D]))
        {
            psContext->EnableExtension("GL_EXT_shader_texture_lod");

            static const int tex_sampler_type_count = 4;
            static const char* tex_sampler_dim_name[tex_sampler_type_count] = {
                "1D", "2D", "3D", "Cube",
            };

            if (psContext->psShader->eTargetLanguage == LANG_ES_100)
            {
                bcatcstr(extensions, "#if !defined(GL_EXT_shader_texture_lod)\n");

                for (int dim = 0; dim < tex_sampler_type_count; dim++)
                {
                    bformata(extensions, "#define texture%sLodEXT texture%s\n", tex_sampler_dim_name[dim], tex_sampler_dim_name[dim]);

                    if (dim == 1) // 2D
                        bformata(extensions, "#define texture%sProjLodEXT texture%sProj\n", tex_sampler_dim_name[dim], tex_sampler_dim_name[dim]);
                }
                bcatcstr(extensions, "#endif\n");
            }
        }
    }

    if (!HaveGatherNonConstOffset(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO_C] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO])
        {
            psContext->EnableExtension("GL_ARB_gpu_shader5");
        }
    }

    if (!HaveQueryLod(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_LOD])
        {
            psContext->EnableExtension("GL_ARB_texture_query_lod");
        }
    }

    if (!HaveQueryLevels(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_RESINFO])
        {
            psContext->EnableExtension("GL_ARB_texture_query_levels");
            psContext->EnableExtension("GL_ARB_shader_image_size");
        }
    }

    if (psContext->psShader->aiOpcodeUsed[OPCODE_SAMPLE_INFO])
    {
        psContext->EnableExtension("GL_ARB_shader_texture_image_samples");
    }

    if (!HaveImageLoadStore(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->aiOpcodeUsed[OPCODE_STORE_UAV_TYPED] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_STORE_RAW] ||
            psContext->psShader->aiOpcodeUsed[OPCODE_STORE_STRUCTURED])
        {
            GL_ARB_shader_image_load_store = true;
            psContext->EnableExtension("GL_ARB_shader_bit_encoding");
        }
        else if (psContext->psShader->aiOpcodeUsed[OPCODE_LD_UAV_TYPED] ||
                 psContext->psShader->aiOpcodeUsed[OPCODE_LD_RAW] ||
                 psContext->psShader->aiOpcodeUsed[OPCODE_LD_STRUCTURED])
        {
            GL_ARB_shader_image_load_store = true;
        }
    }

    if (!HaveGeometryShaderARB(psContext->psShader->eTargetLanguage))
    {
        if (psContext->psShader->eShaderType == GEOMETRY_SHADER)
        {
            psContext->EnableExtension("GL_ARB_geometry_shader");
        }
    }

    if (psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310)
    {
        if (psContext->psShader->eShaderType == GEOMETRY_SHADER)
        {
            psContext->EnableExtension("GL_OES_geometry_shader");
            psContext->EnableExtension("GL_EXT_geometry_shader");
        }
    }

    if (psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310)
    {
        if (psContext->psShader->eShaderType == HULL_SHADER || psContext->psShader->eShaderType == DOMAIN_SHADER)
        {
            psContext->EnableExtension("GL_OES_tessellation_shader");
            psContext->EnableExtension("GL_EXT_tessellation_shader");
        }
    }

    if (GL_ARB_shader_storage_buffer_object)
        psContext->EnableExtension("GL_ARB_shader_storage_buffer_object");

    if (GL_ARB_shader_image_load_store)
        psContext->EnableExtension("GL_ARB_shader_image_load_store");

    if (psContext->psShader->eShaderType == PIXEL_SHADER && psContext->psShader->eTargetLanguage >= LANG_120 && !HaveFragmentCoordConventions(psContext->psShader->eTargetLanguage))
    {
        psContext->RequireExtension("GL_ARB_fragment_coord_conventions");
    }

    if (psContext->psShader->extensions->EXT_shader_framebuffer_fetch && psContext->psShader->eShaderType == PIXEL_SHADER && psContext->flags & HLSLCC_FLAG_SHADER_FRAMEBUFFER_FETCH)
    {
        psContext->EnableExtension("GL_EXT_shader_framebuffer_fetch");
    }

    //Handle fragment shader default precision
    if (psContext->psShader->eShaderType == PIXEL_SHADER &&
        (psContext->psShader->eTargetLanguage == LANG_ES_100 || psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310 || (psContext->flags & HLSLCC_FLAG_NVN_TARGET)))
    {
        if (psContext->psShader->eTargetLanguage == LANG_ES_100)
        {
            // gles 2.0 shaders can have mediump as default if the GPU doesn't have highp support
            bcatcstr(glsl,
                "#ifdef GL_FRAGMENT_PRECISION_HIGH\n"
                "    precision highp float;\n"
                "#else\n"
                "    precision mediump float;\n"
                "#endif\n");
        }
        else
        {
            bcatcstr(glsl, "precision highp float;\n");
        }

        // Define default int precision to highp to avoid issues on platforms that actually implement mediump
        bcatcstr(glsl, "precision highp int;\n");
    }

    if (psContext->psShader->eShaderType == PIXEL_SHADER && psContext->psShader->eTargetLanguage >= LANG_150)
    {
        if (psContext->flags & HLSLCC_FLAG_ORIGIN_UPPER_LEFT)
            bcatcstr(glsl, "layout(origin_upper_left) in vec4 gl_FragCoord;\n");

        if (psContext->flags & HLSLCC_FLAG_PIXEL_CENTER_INTEGER)
            bcatcstr(glsl, "layout(pixel_center_integer) in vec4 gl_FragCoord;\n");
    }


    /*
        OpenGL 4.1 API spec:
        To use any built-in input or output in the gl_PerVertex block in separable
        program objects, shader code must redeclare that block prior to use.
    */
    /* DISABLED FOR NOW */
/*  if(psContext->psShader->eShaderType == VERTEX_SHADER && psContext->psShader->eTargetLanguage >= LANG_410)
    {
        bcatcstr(glsl, "out gl_PerVertex {\n");
        bcatcstr(glsl, "vec4 gl_Position;\n");
        bcatcstr(glsl, "float gl_PointSize;\n");
        bcatcstr(glsl, "float gl_ClipDistance[];");
        bcatcstr(glsl, "};\n");
    }*/
}

GLLang ChooseLanguage(Shader* psShader)
{
    // Depends on the HLSL shader model extracted from bytecode.
    switch (psShader->ui32MajorVersion)
    {
        case 5:
        {
            return LANG_430;
        }
        case 4:
        {
            return LANG_330;
        }
        default:
        {
            return LANG_120;
        }
    }
}

const char* GetVersionString(GLLang language)
{
    switch (language)
    {
        case LANG_ES_100:
        {
            return "#version 100\n";
            break;
        }
        case LANG_ES_300:
        {
            return "#version 300 es\n";
            break;
        }
        case LANG_ES_310:
        {
            return "#version 310 es\n";
            break;
        }
        case LANG_120:
        {
            return "#version 120\n";
            break;
        }
        case LANG_130:
        {
            return "#version 130\n";
            break;
        }
        case LANG_140:
        {
            return "#version 140\n";
            break;
        }
        case LANG_150:
        {
            return "#version 150\n";
            break;
        }
        case LANG_330:
        {
            return "#version 330\n";
            break;
        }
        case LANG_400:
        {
            return "#version 400\n";
            break;
        }
        case LANG_410:
        {
            return "#version 410\n";
            break;
        }
        case LANG_420:
        {
            return "#version 420\n";
            break;
        }
        case LANG_430:
        {
            return "#version 430\n";
            break;
        }
        case LANG_440:
        {
            return "#version 440\n";
            break;
        }
        default:
        {
            return "";
            break;
        }
    }
}

static const char * GetPhaseFuncName(SHADER_PHASE_TYPE eType)
{
    switch (eType)
    {
        default:
        case MAIN_PHASE: return "";
        case HS_GLOBAL_DECL_PHASE: return "hs_global_decls";
        case HS_FORK_PHASE: return "fork_phase";
        case HS_CTRL_POINT_PHASE: return "control_point_phase";
        case HS_JOIN_PHASE: return "join_phase";
    }
}

static void DoHullShaderPassthrough(HLSLCrossCompilerContext *psContext)
{
    uint32_t i;
    bstring glsl = psContext->glsl;

    for (i = 0; i < psContext->psShader->sInfo.psInputSignatures.size(); i++)
    {
        ShaderInfo::InOutSignature *psSig = &psContext->psShader->sInfo.psInputSignatures[i];
        const char *Type;
        uint32_t ui32NumComponents = HLSLcc::GetNumberBitsSet(psSig->ui32Mask);
        switch (psSig->eComponentType)
        {
            default:
            case INOUT_COMPONENT_FLOAT32:
                Type = ui32NumComponents > 1 ? "vec" : "float";
                break;
            case INOUT_COMPONENT_SINT32:
                Type = ui32NumComponents > 1 ? "ivec" : "int";
                break;
            case INOUT_COMPONENT_UINT32:
                Type = ui32NumComponents > 1 ? "uvec" : "uint";
                break;
        }
        if ((psSig->eSystemValueType == NAME_POSITION || psSig->semanticName == "POS") && psSig->ui32SemanticIndex == 0)
            continue;

        std::string inputName;

        {
            std::ostringstream oss;
            oss << psContext->inputPrefix << psSig->semanticName << psSig->ui32SemanticIndex;
            inputName = oss.str();
        }

        std::string outputName;
        {
            std::ostringstream oss;
            oss << psContext->outputPrefix << psSig->semanticName << psSig->ui32SemanticIndex;
            outputName = oss.str();
        }

        const char * prec = "";
        if (HavePrecisionQualifiers(psContext))
        {
            if (psSig->eMinPrec != MIN_PRECISION_DEFAULT)
                prec = "mediump ";
            else
                prec = "highp ";
        }

        int inLoc = psContext->psDependencies->GetVaryingLocation(inputName, HULL_SHADER, true);
        int outLoc = psContext->psDependencies->GetVaryingLocation(outputName, HULL_SHADER, false);

        psContext->AddIndentation();
        if (ui32NumComponents > 1)
            bformata(glsl, "layout(location = %d) in %s%s%d %s%s%d[];\n", inLoc, prec, Type, ui32NumComponents, psContext->inputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
        else
            bformata(glsl, "layout(location = %d) in %s%s %s%s%d[];\n", inLoc, prec, Type, psContext->inputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);

        psContext->AddIndentation();
        if (ui32NumComponents > 1)
            bformata(glsl, "layout(location = %d) out %s%s%d %s%s%d[];\n", outLoc, prec, Type, ui32NumComponents, psContext->outputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
        else
            bformata(glsl, "layout(location = %d) out %s%s %s%s%d[];\n", outLoc, prec, Type, psContext->outputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
    }

    psContext->AddIndentation();
    bcatcstr(glsl, "void passthrough_ctrl_points()\n");
    psContext->AddIndentation();
    bcatcstr(glsl, "{\n");
    psContext->indent++;

    for (i = 0; i < psContext->psShader->sInfo.psInputSignatures.size(); i++)
    {
        const ShaderInfo::InOutSignature *psSig = &psContext->psShader->sInfo.psInputSignatures[i];

        psContext->AddIndentation();

        if ((psSig->eSystemValueType == NAME_POSITION || psSig->semanticName == "POS") && psSig->ui32SemanticIndex == 0)
            bformata(glsl, "gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;\n");
        else
            bformata(glsl, "%s%s%d[gl_InvocationID] = %s%s%d[gl_InvocationID];\n", psContext->outputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex, psContext->inputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
    }

    psContext->indent--;
    psContext->AddIndentation();
    bcatcstr(glsl, "}\n");
}

GLLang ToGLSL::SetLanguage(GLLang suggestedLanguage)
{
    language = suggestedLanguage;
    if (language == LANG_DEFAULT)
    {
        language = ChooseLanguage(psContext->psShader);
    }
    return language;
}

bool ToGLSL::Translate()
{
    bstring glsl;
    uint32_t i;
    Shader* psShader = psContext->psShader;
    uint32_t ui32Phase;

    psContext->psTranslator = this;

    if (language == LANG_DEFAULT)
        SetLanguage(LANG_DEFAULT);

    SetIOPrefixes();
    psShader->ExpandSWAPCs();
    psShader->ForcePositionToHighp();
    psShader->AnalyzeIOOverlap();
    psShader->FindUnusedGlobals(psContext->flags);

    psContext->indent = 0;

    glsl = bfromcstralloc(1024 * 10, "\n");
    bstring extensions = bfromcstralloc(1024 * 10, GetVersionString(language));
    psContext->extensions = extensions;

    psContext->glsl = glsl;
    for (i = 0; i < psShader->asPhases.size(); ++i)
    {
        psShader->asPhases[i].postShaderCode = bfromcstralloc(1024 * 5, "");
        psShader->asPhases[i].earlyMain = bfromcstralloc(1024 * 5, "");
    }
    psContext->currentGLSLString = &glsl;
    psShader->eTargetLanguage = language;
    psContext->currentPhase = MAIN_PHASE;

    if (psShader->extensions)
    {
        if (psContext->flags & HLSLCC_FLAG_NVN_TARGET)
        {
            psContext->EnableExtension("GL_ARB_separate_shader_objects");
            psContext->EnableExtension("GL_NV_desktop_lowp_mediump"); // This flag allow FP16 operations (mediump in GLSL)
        }
        if (psShader->extensions->ARB_explicit_attrib_location)
            psContext->RequireExtension("GL_ARB_explicit_attrib_location");
        if (psShader->extensions->ARB_explicit_uniform_location)
            psContext->RequireExtension("GL_ARB_explicit_uniform_location");
        if (psShader->extensions->ARB_shading_language_420pack)
            psContext->RequireExtension("GL_ARB_shading_language_420pack");
    }

    psContext->ClearDependencyData();

    AddVersionDependentCode(psContext);

    if (psShader->eShaderType == VERTEX_SHADER &&
        HaveLimitedInOutLocationQualifier(language, psShader->extensions) &&
        psContext->flags & HLSLCC_FLAG_NVN_TARGET)
    {
        bcatcstr(glsl, "out gl_PerVertex { vec4 gl_Position; };\n");
    }

    if (!psContext->psDependencies->m_ExtBlendModes.empty() && psShader->eShaderType == PIXEL_SHADER)
    {
        psContext->EnableExtension("GL_KHR_blend_equation_advanced");
        bcatcstr(glsl, "#if GL_KHR_blend_equation_advanced\n");
        for (i = 0; i < psContext->psDependencies->m_ExtBlendModes.size(); i++)
        {
            bformata(glsl, "layout(%s) out;\n", psContext->psDependencies->m_ExtBlendModes[i].c_str());
        }
        bcatcstr(glsl, "#endif\n");
    }


    psShader->PrepareStructuredBufferBindingSlots();

    for (ui32Phase = 0; ui32Phase < psShader->asPhases.size(); ui32Phase++)
    {
        ShaderPhase &phase = psShader->asPhases[ui32Phase];
        phase.UnvectorizeImmMoves();
        psContext->DoDataTypeAnalysis(&phase);
        phase.ResolveUAVProperties();
        psShader->ResolveStructuredBufferBindingSlots(&phase);
        if (!psContext->IsVulkan() && !psContext->IsSwitch())
            phase.PruneConstArrays();
    }

    psShader->PruneTempRegisters();

    for (ui32Phase = 0; ui32Phase < psShader->asPhases.size(); ui32Phase++)
    {
        // Loop transform can only be done after the temps have been pruned
        ShaderPhase &phase = psShader->asPhases[ui32Phase];
        HLSLcc::DoLoopTransform(psContext, phase);

        if ((psContext->flags & HLSLCC_FLAG_VULKAN_SPECIALIZATION_CONSTANTS) != 0)
        {
            IdentifyStaticBranches(&phase);
        }
    }

    //Special case. Can have multiple phases.
    if (psShader->eShaderType == HULL_SHADER)
    {
        const SHADER_PHASE_TYPE ePhaseFuncCallOrder[3] = { HS_CTRL_POINT_PHASE, HS_FORK_PHASE, HS_JOIN_PHASE };
        uint32_t ui32PhaseCallIndex;
        int perPatchSectionAdded = 0;
        int hasControlPointPhase = 0;

        psShader->ConsolidateHullTempVars();

        // Find out if we have a passthrough hull shader
        for (ui32Phase = 2; ui32Phase < psShader->asPhases.size(); ui32Phase++)
        {
            if (psShader->asPhases[ui32Phase].ePhase == HS_CTRL_POINT_PHASE)
                hasControlPointPhase = 1;
        }

        // Phase 1 is always the global decls phase, no instructions
        for (i = 0; i < psShader->asPhases[1].psDecl.size(); ++i)
        {
            TranslateDeclaration(&psShader->asPhases[1].psDecl[i]);
        }

        if (hasControlPointPhase == 0)
        {
            DoHullShaderPassthrough(psContext);
        }

        for (ui32Phase = 2; ui32Phase < psShader->asPhases.size(); ui32Phase++)
        {
            ShaderPhase *psPhase = &psShader->asPhases[ui32Phase];
            psContext->currentPhase = ui32Phase;

#ifdef _DEBUG
            bformata(glsl, "//%s declarations\n", GetPhaseFuncName(psPhase->ePhase));
#endif
            for (i = 0; i < psPhase->psDecl.size(); ++i)
            {
                TranslateDeclaration(&psPhase->psDecl[i]);
            }

            if ((psContext->flags & HLSLCC_FLAG_VULKAN_SPECIALIZATION_CONSTANTS) != 0)
            {
                DeclareSpecializationConstants(*psPhase);
            }


            bformata(glsl, "void %s%d(int phaseInstanceID)\n{\n", GetPhaseFuncName(psPhase->ePhase), ui32Phase);
            psContext->indent++;

            if (psPhase->psInst.size() > 0)
            {
                //The minus one here is remove the return statement at end of phases.
                //We don't want to translate that, we'll just end the function body.
                ASSERT(psPhase->psInst[psPhase->psInst.size() - 1].eOpcode == OPCODE_RET);
                for (i = 0; i < psPhase->psInst.size() - 1; ++i)
                {
                    TranslateInstruction(&psPhase->psInst[i]);
                }
            }


            psContext->indent--;
            bcatcstr(glsl, "}\n");
        }

        bcatcstr(glsl, "void main()\n{\n");

        psContext->indent++;

        // There are cases when there are no control point phases and we have to do passthrough
        if (hasControlPointPhase == 0)
        {
            // Passthrough control point phase, run the rest only once per patch
            psContext->AddIndentation();
            bcatcstr(glsl, "passthrough_ctrl_points();\n");
            psContext->AddIndentation();
            bcatcstr(glsl, "barrier();\n");
            psContext->AddIndentation();
            bcatcstr(glsl, "if (gl_InvocationID == 0)\n");
            psContext->AddIndentation();
            bcatcstr(glsl, "{\n");
            psContext->indent++;
            perPatchSectionAdded = 1;
        }

        for (ui32PhaseCallIndex = 0; ui32PhaseCallIndex < 3; ui32PhaseCallIndex++)
        {
            for (ui32Phase = 2; ui32Phase < psShader->asPhases.size(); ui32Phase++)
            {
                uint32_t i;
                ShaderPhase *psPhase = &psShader->asPhases[ui32Phase];
                if (psPhase->ePhase != ePhaseFuncCallOrder[ui32PhaseCallIndex])
                    continue;

                if (psPhase->earlyMain->slen > 1)
                {
#ifdef _DEBUG
                    psContext->AddIndentation();
                    bcatcstr(glsl, "//--- Start Early Main ---\n");
#endif
                    bconcat(glsl, psPhase->earlyMain);
#ifdef _DEBUG
                    psContext->AddIndentation();
                    bcatcstr(glsl, "//--- End Early Main ---\n");
#endif
                }

                for (i = 0; i < psPhase->ui32InstanceCount; i++)
                {
                    psContext->AddIndentation();
                    bformata(glsl, "%s%d(%d);\n", GetPhaseFuncName(psShader->asPhases[ui32Phase].ePhase), ui32Phase, i);
                }

                if (psPhase->hasPostShaderCode)
                {
#ifdef _DEBUG
                    psContext->AddIndentation();
                    bcatcstr(glsl, "//--- Post shader code ---\n");
#endif
                    bconcat(glsl, psPhase->postShaderCode);
#ifdef _DEBUG
                    psContext->AddIndentation();
                    bcatcstr(glsl, "//--- End post shader code ---\n");
#endif
                }


                if (psShader->asPhases[ui32Phase].ePhase == HS_CTRL_POINT_PHASE)
                {
                    // We're done printing control point phase, run the rest only once per patch
                    psContext->AddIndentation();
                    bcatcstr(glsl, "barrier();\n");
                    psContext->AddIndentation();
                    bcatcstr(glsl, "if (gl_InvocationID == 0)\n");
                    psContext->AddIndentation();
                    bcatcstr(glsl, "{\n");
                    psContext->indent++;
                    perPatchSectionAdded = 1;
                }
            }
        }

        if (perPatchSectionAdded != 0)
        {
            psContext->indent--;
            psContext->AddIndentation();
            bcatcstr(glsl, "}\n");
        }

        psContext->indent--;

        bcatcstr(glsl, "}\n");

        // Print out extra functions we generated, in reverse order for potential dependencies
        std::for_each(m_FunctionDefinitions.rbegin(), m_FunctionDefinitions.rend(), [&extensions](const FunctionDefinitions::value_type &p)
        {
            bcatcstr(extensions, p.second.c_str());
            bcatcstr(extensions, "\n");
        });

        // Concat extensions and glsl for the final shader code.
        if (m_NeedUnityInstancingArraySizeDecl)
        {
            if (psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS)
            {
                bformata(extensions, "layout(constant_id = %d) const int %s = 2;\n", kArraySizeConstantID, UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO);
            }
            else
            {
                bcatcstr(extensions, "#ifndef " UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO "\n\t#define " UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO " 2\n#endif\n");
            }
        }

        bconcat(extensions, glsl);
        bdestroy(glsl);
        psContext->glsl = extensions;
        glsl = NULL;

        if (psContext->psDependencies)
        {
            //Save partitioning and primitive type for use by domain shader.
            psContext->psDependencies->eTessOutPrim = psShader->sInfo.eTessOutPrim;

            psContext->psDependencies->eTessPartitioning = psShader->sInfo.eTessPartitioning;
        }

        return true;
    }

    if (psShader->eShaderType == DOMAIN_SHADER && psContext->psDependencies)
    {
        //Load partitioning and primitive type from hull shader.
        switch (psContext->psDependencies->eTessOutPrim)
        {
            case TESSELLATOR_OUTPUT_TRIANGLE_CCW:
            {
                bcatcstr(glsl, "layout(ccw) in;\n");
                break;
            }
            case TESSELLATOR_OUTPUT_TRIANGLE_CW:
            {
                bcatcstr(glsl, "layout(cw) in;\n");
                break;
            }
            case TESSELLATOR_OUTPUT_POINT:
            {
                bcatcstr(glsl, "layout(point_mode) in;\n");
                break;
            }
            default:
            {
                break;
            }
        }

        switch (psContext->psDependencies->eTessPartitioning)
        {
            case TESSELLATOR_PARTITIONING_FRACTIONAL_ODD:
            {
                bcatcstr(glsl, "layout(fractional_odd_spacing) in;\n");
                break;
            }
            case TESSELLATOR_PARTITIONING_FRACTIONAL_EVEN:
            {
                bcatcstr(glsl, "layout(fractional_even_spacing) in;\n");
                break;
            }
            default:
            {
                break;
            }
        }
    }

    bstring generatedFunctionsKeyword = bfromcstr("\n// Generated functions\n\n");
    bstring beforeMain = NULL;
    bstring beforeMainKeyword = NULL;

    if (!HaveDynamicIndexing(psContext))
    {
        beforeMain = bfromcstr("");
        beforeMainKeyword = bfromcstr("\n// Before Main\n\n");
        psContext->beforeMain = beforeMain;
    }

    for (i = 0; i < psShader->asPhases[0].psDecl.size(); ++i)
    {
        TranslateDeclaration(&psShader->asPhases[0].psDecl[i]);
    }

    if ((psContext->flags & HLSLCC_FLAG_VULKAN_SPECIALIZATION_CONSTANTS) != 0)
    {
        DeclareSpecializationConstants(psShader->asPhases[0]);
    }

    // Search and replace string, for injecting generated functions that need to be after default precision declarations
    bconcat(glsl, generatedFunctionsKeyword);

    // Search and replace string, for injecting stuff from translation that need to be after normal declarations and before main
    if (!HaveDynamicIndexing(psContext))
    {
        bconcat(glsl, beforeMainKeyword);
    }

    bcatcstr(glsl, "void main()\n{\n");

    psContext->indent++;

    if (psContext->psShader->asPhases[0].earlyMain->slen > 1)
    {
#ifdef _DEBUG
        psContext->AddIndentation();
        bcatcstr(glsl, "//--- Start Early Main ---\n");
#endif
        bconcat(glsl, psContext->psShader->asPhases[0].earlyMain);
#ifdef _DEBUG
        psContext->AddIndentation();
        bcatcstr(glsl, "//--- End Early Main ---\n");
#endif
    }

    for (i = 0; i < psShader->asPhases[0].psInst.size(); ++i)
    {
        TranslateInstruction(&psShader->asPhases[0].psInst[i]);
    }

    psContext->indent--;

    bcatcstr(glsl, "}\n");

    // Print out extra functions we generated in generation order to satisfy dependencies
    {
        bstring generatedFunctions = bfromcstr("");
        for (std::vector<std::string>::const_iterator funcNameIter = m_FunctionDefinitionsOrder.begin(); funcNameIter != m_FunctionDefinitionsOrder.end(); ++funcNameIter)
        {
            const FunctionDefinitions::const_iterator definition = m_FunctionDefinitions.find(*funcNameIter);
            ASSERT(definition != m_FunctionDefinitions.end());
            bcatcstr(generatedFunctions, definition->second.c_str());
            bcatcstr(generatedFunctions, "\n");
        }
        bfindreplace(glsl, generatedFunctionsKeyword, generatedFunctions, 0);
        bdestroy(generatedFunctions);
    }

    // Concat extensions and glsl for the final shader code.
    if (m_NeedUnityInstancingArraySizeDecl)
    {
        if (psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS)
        {
            bformata(extensions, "layout(constant_id = %d) const int %s = 2;\n", kArraySizeConstantID, UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO);
        }
        else
        {
            bcatcstr(extensions, "#ifndef " UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO "\n\t#define " UNITY_RUNTIME_INSTANCING_ARRAY_SIZE_MACRO " 2\n#endif\n");
        }
    }
    bconcat(extensions, glsl);
    bdestroy(glsl);

    if (!HaveDynamicIndexing(psContext))
    {
        bstring empty = bfromcstr("");

        if (beforeMain->slen > 1)
            bfindreplace(extensions, beforeMainKeyword, beforeMain, 0);
        else
            bfindreplace(extensions, beforeMainKeyword, empty, 0);

        psContext->beforeMain = NULL;

        bdestroy(empty);
        bdestroy(beforeMain);
        bdestroy(beforeMainKeyword);
    }

    psContext->glsl = extensions;
    glsl = NULL;

    return true;
}

bool ToGLSL::DeclareExtraFunction(const std::string &name, bstring body)
{
    if (m_FunctionDefinitions.find(name) != m_FunctionDefinitions.end())
        return true;
    m_FunctionDefinitions.insert(std::make_pair(name, (const char *)body->data));
    m_FunctionDefinitionsOrder.push_back(name);
    return false;
}

static void PrintComponentWrapper1(bstring code, const char *func, const char *type2, const char *type3, const char *type4)
{
    bformata(code, "%s %s(%s a) { a.x = %s(a.x); a.y = %s(a.y); return a; }\n", type2, func, type2, func, func);
    bformata(code, "%s %s(%s a) { a.x = %s(a.x); a.y = %s(a.y); a.z = %s(a.z); return a; }\n", type3, func, type3, func, func, func);
    bformata(code, "%s %s(%s a) { a.x = %s(a.x); a.y = %s(a.y); a.z = %s(a.z); a.w = %s(a.w); return a; }\n", type4, func, type4, func, func, func, func);
}

static void PrintComponentWrapper2(bstring code, const char *func, const char *type2, const char *type3, const char *type4)
{
    bformata(code, "%s %s(%s a, %s b) { a.x = %s(a.x, b.x); a.y = %s(a.y, b.y); return a; }\n", type2, func, type2, type2, func, func);
    bformata(code, "%s %s(%s a, %s b) { a.x = %s(a.x, b.x); a.y = %s(a.y, b.y); a.z = %s(a.z, b.z); return a; }\n", type3, func, type3, type3, func, func, func);
    bformata(code, "%s %s(%s a, %s b) { a.x = %s(a.x, b.x); a.y = %s(a.y, b.y); a.z = %s(a.z, b.z); a.w = %s(a.w, b.w); return a; }\n", type4, func, type4, type4, func, func, func, func);
}

static void PrintTrunc(bstring code, const char *type)
{
    bformata(code, "%s trunc(%s x) { return sign(x)*floor(abs(x)); }\n", type, type);
}

void ToGLSL::UseExtraFunctionDependency(const std::string &name)
{
    if (m_FunctionDefinitions.find(name) != m_FunctionDefinitions.end())
        return;

    bstring code = bfromcstr("");
    bool match = true;

    if (name == "trunc")
    {
        PrintTrunc(code, "float");
        PrintTrunc(code, "vec2");
        PrintTrunc(code, "vec3");
        PrintTrunc(code, "vec4");
    }
    else if (name == "roundEven")
    {
        bformata(code, "float roundEven(float x) { float y = floor(x + 0.5); return (y - x == 0.5) ? floor(0.5*y) * 2.0 : y; }\n");
        PrintComponentWrapper1(code, "roundEven", "vec2", "vec3", "vec4");
    }
    else if (name == "op_modi")
    {
        bformata(code, "const int BITWISE_BIT_COUNT = 32;\nint op_modi(int x, int y) { return x - y * (x / y); }\n");
        PrintComponentWrapper2(code, "op_modi", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_and")
    {
        UseExtraFunctionDependency("op_modi");

        bformata(code, "int op_and(int a, int b) { int result = 0; int n = 1; for (int i = 0; i < BITWISE_BIT_COUNT; i++) { if ((op_modi(a, 2) != 0) && (op_modi(b, 2) != 0)) { result += n; } a = a / 2; b = b / 2; n = n * 2; if (!(a > 0 && b > 0)) { break; } } return result; }\n");
        PrintComponentWrapper2(code, "op_and", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_or")
    {
        UseExtraFunctionDependency("op_modi");

        bformata(code, "int op_or(int a, int b) { int result = 0; int n = 1; for (int i = 0; i < BITWISE_BIT_COUNT; i++) { if ((op_modi(a, 2) != 0) || (op_modi(b, 2) != 0)) { result += n; } a = a / 2; b = b / 2; n = n * 2; if (!(a > 0 || b > 0)) { break; } } return result; }\n");
        PrintComponentWrapper2(code, "op_or", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_xor")
    {
        UseExtraFunctionDependency("op_and");

        bformata(code, "int op_xor(int a, int b) { return (a + b - 2 * op_and(a, b)); }\n");
        PrintComponentWrapper2(code, "op_xor", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_shr")
    {
        bformata(code, "int op_shr(int a, int b) { return int(floor(float(a) / pow(2.0, float(b)))); }\n");
        PrintComponentWrapper2(code, "op_shr", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_shl")
    {
        bformata(code, "int op_shl(int a, int b) { return int(floor(float(a) * pow(2.0, float(b)))); }\n");
        PrintComponentWrapper2(code, "op_shl", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "op_not")
    {
        bformata(code, "int op_not(int value) { return -value - 1; }\n");
        PrintComponentWrapper1(code, "op_not", "ivec2", "ivec3", "ivec4");
    }
    else if (name == "int_bitfieldInsert")
    {
        // Can't use the name 'bitfieldInsert' because Adreno fails with "can't redefine/overload built-in functions!"
        bcatcstr(code,
            "int int_bitfieldInsert(int base, int insert, int offset, int bits) {\n"
            "    uint mask = ~(uint(0xffffffff) << uint(bits)) << uint(offset);\n"
            "    return int((uint(base) & ~mask) | ((uint(insert) << uint(offset)) & mask));\n"
            "}\n");
    }
    else
    {
        match = false;
    }

    if (match)
        DeclareExtraFunction(name, code);

    bdestroy(code);
}

void ToGLSL::DeclareSpecializationConstants(ShaderPhase &phase)
{
    bstring glsl = psContext->glsl;
    // There may be several uses for the same branch condition, so we'll need to keep track of what we've already declared.
    std::set<uint32_t> alreadyDeclared;
    for (std::vector<Instruction *>::iterator itr = phase.m_StaticBranchInstructions.begin(); itr != phase.m_StaticBranchInstructions.end(); itr++)
    {
        Instruction &i = **itr;
        uint32_t slot = psContext->psDependencies->GetSpecializationConstantSlot(i.m_StaticBranchName);
        if (alreadyDeclared.insert(slot).second) // Only declare if the insertion actually succeeded
            bformata(glsl, "layout(constant_id = %d) const bool %s = false;\n", slot, i.m_StaticBranchName.c_str());
    }
}

std::string to64(uint32_t in)
{
    const char to64[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    char c_[2];
    c_[0] = to64[in];
    c_[1] = 0;
    char c = c_[0];
    if (c == 'X')
        return "XX";
    if (c == '+')
        return "XA";
    if (c == '/')
        return "XB";
    return std::string(c_);
}

// Slightly custom base64, espace non-identifier chars with 'X'
static void Base64Encode(const std::string &in, std::string& result)
{
    size_t len = in.length();
    size_t outputLen = (len + 2) / 3 * 4;
    unsigned char *bytes = (unsigned char *)&in[0];

    result.clear();
    result.reserve(outputLen);

    int i = 0;
    unsigned char b1, b2, b3;
    for (int chunk = 0; len > 0; len -= 3, chunk++)
    {
        b1 = bytes[i++];
        b2 = len > 1 ? bytes[i++] : '\0';
        result += to64(b1 >> 2);
        result += to64(((b1 & 3) << 4) | ((b2 & 0xf0) >> 4));
        if (len > 2)
        {
            b3 = bytes[i++];
            result += to64(((b2 & 0xF) << 2) | ((b3 & 0xC0) >> 6));
            result += to64(b3 & 0x3F);
        }
        else if (len == 2)
        {
            result += to64((b2 & 0xF) << 2);
            result += "XC";
            break;
        }
        else /* len == 1 */
        {
            result += "XC";
            break;
        }
    }
}

bool ToGLSL::BuildStaticBranchNameForInstruction(Instruction &inst)
{
    std::ostringstream oss;
    if (!inst.m_StaticBranchCondition)
    {
        // Simple case, just get the value, check if nonzero
        bstring varname = bfromcstr("");
        SHADER_VARIABLE_TYPE argType = inst.asOperands[0].GetDataType(psContext);
        uint32_t flag = TO_FLAG_NONE;
        switch (argType)
        {
            case SVT_BOOL:
                flag = TO_FLAG_BOOL;
                break;
            case SVT_INT:
            case SVT_INT12:
            case SVT_INT16:
                flag = TO_FLAG_INTEGER;
                break;
            case SVT_UINT:
            case SVT_UINT16:
            case SVT_UINT8:
                flag = TO_FLAG_UNSIGNED_INTEGER;
                break;
            default:
                break;
        }
        TranslateOperand(varname, &inst.asOperands[0], flag);
        char *str = bstr2cstr(varname, '\0');
        oss << str;
        bcstrfree(str);
        bdestroy(varname);
        oss << "!=0";
        std::string res = oss.str();
        // Sanity checks: no arrays, no matrices
        if (res.find('[') != std::string::npos)
            return false;
        if (res.find("hlslcc_mtx") != std::string::npos)
            return false;
        Base64Encode(res, inst.m_StaticBranchName);
    }
    else
    {
        // Indirect, just store the whole previous instruction and then the condition
        bstring res = bfromcstr("");

        bstring *oldglsl = psContext->currentGLSLString;
        psContext->currentGLSLString = &res;
        TranslateInstruction((Instruction *)inst.m_StaticBranchCondition, true);
        psContext->currentGLSLString = oldglsl;

        SHADER_VARIABLE_TYPE argType = inst.asOperands[0].GetDataType(psContext);
        uint32_t flag = TO_FLAG_NONE;
        switch (argType)
        {
            case SVT_BOOL:
                flag = TO_FLAG_BOOL;
                break;
            case SVT_INT:
            case SVT_INT12:
            case SVT_INT16:
                flag = TO_FLAG_INTEGER;
                break;
            case SVT_UINT:
            case SVT_UINT16:
            case SVT_UINT8:
                flag = TO_FLAG_UNSIGNED_INTEGER;
                break;
            default:
                break;
        }

        if (argType == SVT_BOOL)
        {
            if (inst.eBooleanTestType == INSTRUCTION_TEST_ZERO)
                bcatcstr(res, "!");
        }

        TranslateOperand(res, &inst.asOperands[0], flag);
        char *str = bstr2cstr(res, '\0');
        oss << str;
        bcstrfree(str);
        bdestroy(res);
        if (argType != SVT_BOOL)
            oss << "!=0";

        std::string ress = oss.str();
        // Sanity checks: no arrays, no matrices
        if (ress.find('[') != std::string::npos)
            return false;
        if (ress.find("hlslcc_mtx") != std::string::npos)
            return false;
        Base64Encode(ress, inst.m_StaticBranchName);
    }
    return true;
}

void ToGLSL::IdentifyStaticBranches(ShaderPhase *psPhase)
{
    for (std::vector<Instruction>::iterator itr = psPhase->psInst.begin(); itr != psPhase->psInst.end(); itr++)
    {
        Instruction &i = *itr;

        if (!i.IsConditionalBranchInstruction())
            continue;

        // Simple case, direct conditional branch
        if (i.asOperands[0].eType == OPERAND_TYPE_CONSTANT_BUFFER)
        {
            i.m_StaticBranchCondition = NULL;
            if (BuildStaticBranchNameForInstruction(i))
            {
                psPhase->m_StaticBranchInstructions.push_back(&i);
                i.m_IsStaticBranch = true;
            }
        }
        // Indirect, comparison via another instruction
        if (i.asOperands[0].eType == OPERAND_TYPE_TEMP)
        {
            // Check that the temp only has one visible definition
            if (i.asOperands[0].m_Defines.size() == 1)
            {
                // ...and that it only uses constant buffers and immediates

                Instruction &def = *i.asOperands[0].m_Defines[0].m_Inst;
                bool isStatic = true;
                for (uint32_t k = def.ui32FirstSrc; k < def.ui32NumOperands; k++)
                {
                    Operand &o = def.asOperands[k];
                    if (!(o.eType == OPERAND_TYPE_CONSTANT_BUFFER || o.eType == OPERAND_TYPE_IMMEDIATE32))
                    {
                        isStatic = false;
                        break;
                    }
                    // Also check that the constant buffer access is "simple"
                    if (o.eType == OPERAND_TYPE_CONSTANT_BUFFER)
                    {
                        if (o.m_SubOperands[0].get() || o.m_SubOperands[1].get())
                        {
                            isStatic = false;
                            break;
                        }
                    }
                }
                if (isStatic)
                {
                    i.m_StaticBranchCondition = &def;
                    if (BuildStaticBranchNameForInstruction(i))
                    {
                        psPhase->m_StaticBranchInstructions.push_back(&i);
                        i.m_IsStaticBranch = true;
                    }
                    else
                        i.m_StaticBranchCondition = NULL;
                }
            }
        }
    }
}
