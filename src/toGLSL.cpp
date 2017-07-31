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

	if(psContext->psShader->ui32MajorVersion > 3 && psContext->psShader->eTargetLanguage != LANG_ES_300 && psContext->psShader->eTargetLanguage != LANG_ES_310 && !(psContext->psShader->eTargetLanguage >= LANG_330))
	{
		bcatcstr(extensions,"#extension GL_ARB_shader_bit_encoding : enable\n");
	}

	if(!HaveCompute(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->eShaderType == COMPUTE_SHADER)
		{
			bcatcstr(extensions,"#extension GL_ARB_compute_shader : enable\n");
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
		if( psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_ALLOC] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_IMM_ATOMIC_CONSUME] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_DCL_UNORDERED_ACCESS_VIEW_STRUCTURED])
		{
			bcatcstr(extensions,"#extension GL_ARB_shader_atomic_counters : enable\n");
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
				bcatcstr(extensions, "#extension GL_OES_shader_image_atomic : enable\n");
			else
				GL_ARB_shader_image_load_store = true;
		}
	}

	if(!HaveGather(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO_C] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_C])
		{
			bcatcstr(extensions,"#extension GL_ARB_texture_gather : enable\n");
		}
	}

	if(!HaveGatherNonConstOffset(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO_C] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_GATHER4_PO])
		{
			bcatcstr(extensions,"#extension GL_ARB_gpu_shader5 : enable\n");
		}
	}

	if(!HaveQueryLod(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->aiOpcodeUsed[OPCODE_LOD])
		{
			bcatcstr(extensions,"#extension GL_ARB_texture_query_lod : enable\n");
		}
	}

	if(!HaveQueryLevels(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->aiOpcodeUsed[OPCODE_RESINFO])
		{
			bcatcstr(extensions,"#extension GL_ARB_texture_query_levels : enable\n");
			bcatcstr(extensions, "#extension GL_ARB_shader_image_size : enable\n");
		}
	}

	if (psContext->psShader->aiOpcodeUsed[OPCODE_SAMPLE_INFO ])
	{
		bcatcstr(extensions, "#extension GL_ARB_shader_texture_image_samples : enable\n");
	}

	if(!HaveImageLoadStore(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->aiOpcodeUsed[OPCODE_STORE_UAV_TYPED] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_STORE_RAW] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_STORE_STRUCTURED])
		{
			GL_ARB_shader_image_load_store = true;
			bcatcstr(extensions,"#extension GL_ARB_shader_bit_encoding : enable\n");
		}
		else
		if(psContext->psShader->aiOpcodeUsed[OPCODE_LD_UAV_TYPED] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_LD_RAW] ||
			psContext->psShader->aiOpcodeUsed[OPCODE_LD_STRUCTURED])
		{
			GL_ARB_shader_image_load_store = true;
		}
	}

	if(!HaveGeometryShaderARB(psContext->psShader->eTargetLanguage))
	{
		if(psContext->psShader->eShaderType == GEOMETRY_SHADER)
		{
			bcatcstr(extensions,"#extension GL_ARB_geometry_shader : enable\n");
		}
	}

	if(psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310)
	{
		if(psContext->psShader->eShaderType == GEOMETRY_SHADER)
		{
			bcatcstr(extensions,"#extension GL_OES_geometry_shader : enable\n");
			bcatcstr(extensions,"#extension GL_EXT_geometry_shader : enable\n");
		}
	}

	if(psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310)
	{
		if(psContext->psShader->eShaderType == HULL_SHADER || psContext->psShader->eShaderType == DOMAIN_SHADER)
		{
			bcatcstr(extensions,"#extension GL_OES_tessellation_shader : enable\n");
			bcatcstr(extensions,"#extension GL_EXT_tessellation_shader : enable\n");
		}
	}

	if (GL_ARB_shader_storage_buffer_object)
		bcatcstr(extensions, "#extension GL_ARB_shader_storage_buffer_object : enable\n");

	if (GL_ARB_shader_image_load_store)
		bcatcstr(extensions, "#extension GL_ARB_shader_image_load_store : enable\n");

	if(psContext->psShader->eShaderType == PIXEL_SHADER && psContext->psShader->eTargetLanguage >= LANG_120 && !HaveFragmentCoordConventions(psContext->psShader->eTargetLanguage))
	{
		bcatcstr(extensions,"#extension GL_ARB_fragment_coord_conventions : require\n");
	}

	if (psContext->psShader->extensions->EXT_shader_framebuffer_fetch && psContext->psShader->eShaderType == PIXEL_SHADER && psContext->flags & HLSLCC_FLAG_SHADER_FRAMEBUFFER_FETCH)
	{
		bcatcstr(extensions, "#ifdef GL_EXT_shader_framebuffer_fetch\n");
		bcatcstr(extensions, "#extension GL_EXT_shader_framebuffer_fetch : enable\n");
		bcatcstr(extensions, "#endif\n");
	}

	//Handle fragment shader default precision
	if ((psContext->psShader->eShaderType == PIXEL_SHADER) &&
		(psContext->psShader->eTargetLanguage == LANG_ES_100 || psContext->psShader->eTargetLanguage == LANG_ES_300 || psContext->psShader->eTargetLanguage == LANG_ES_310))
	{
		// Float default precision is patched during runtime in GlslGpuProgramGLES.cpp:PatchupFragmentShaderText()
		// Except on Vulkan
		if(psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS)
			bcatcstr(glsl, "precision highp float;\n");


		// Define default int precision to highp to avoid issues on platforms that actually implement mediump 
		bcatcstr(glsl, "precision highp int;\n");
	}

	if(psContext->psShader->eShaderType == PIXEL_SHADER && psContext->psShader->eTargetLanguage >= LANG_150)
	{
		if(psContext->flags & HLSLCC_FLAG_ORIGIN_UPPER_LEFT)
			bcatcstr(glsl,"layout(origin_upper_left) in vec4 gl_FragCoord;\n");

		if(psContext->flags & HLSLCC_FLAG_PIXEL_CENTER_INTEGER)
			bcatcstr(glsl,"layout(pixel_center_integer) in vec4 gl_FragCoord;\n");
	}


    /*
        OpenGL 4.1 API spec:
        To use any built-in input or output in the gl_PerVertex block in separable
        program objects, shader code must redeclare that block prior to use.
    */
	/* DISABLED FOR NOW */
/*	if(psContext->psShader->eShaderType == VERTEX_SHADER && psContext->psShader->eTargetLanguage >= LANG_410)
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
    switch(psShader->ui32MajorVersion)
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
    switch(language)
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

		const char * prec = HavePrecisionQualifiers(psContext) ? "highp ": "";

		psContext->AddIndentation();
		if (ui32NumComponents > 1) // TODO Precision
			bformata(glsl, "in %s%s%d %s%s%d[];\n", prec, Type, ui32NumComponents, psContext->inputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
		else
			bformata(glsl, "in %s%s %s%s%d[];\n", prec, Type, psContext->inputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);

		psContext->AddIndentation();
		if (ui32NumComponents > 1) // TODO Precision
			bformata(glsl, "out %s%s%d %s%s%d[];\n", prec, Type, ui32NumComponents, psContext->outputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
		else
			bformata(glsl, "out %s%s %s%s%d[];\n", prec, Type, psContext->outputPrefix, psSig->semanticName.c_str(), psSig->ui32SemanticIndex);
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

    glsl = bfromcstralloc (1024 * 10, "\n");
    bstring extensions = bfromcstralloc (1024 * 10, GetVersionString(language));
    psContext->extensions = extensions;

    psContext->glsl = glsl;
    for(i=0; i<psShader->asPhases.size();++i)
    {
        psShader->asPhases[i].postShaderCode = bfromcstralloc (1024 * 5, "");
		psShader->asPhases[i].earlyMain = bfromcstralloc(1024 * 5, "");
	}
    psContext->currentGLSLString = &glsl;
    psShader->eTargetLanguage = language;
    psContext->currentPhase = MAIN_PHASE;

	if (psShader->extensions)
	{
		if(psContext->flags & HLSLCC_FLAG_NVN_TARGET)
			bcatcstr(extensions, "#extension GL_ARB_separate_shader_objects : enable\n");
		if (psShader->extensions->ARB_explicit_attrib_location)
			bcatcstr(extensions, "#extension GL_ARB_explicit_attrib_location : require\n");
		if (psShader->extensions->ARB_explicit_uniform_location)
			bcatcstr(extensions, "#extension GL_ARB_explicit_uniform_location : require\n");
		if (psShader->extensions->ARB_shading_language_420pack)
			bcatcstr(extensions, "#extension GL_ARB_shading_language_420pack : require\n");
	}

    psContext->ClearDependencyData();

    AddVersionDependentCode(psContext);

    if(psShader->eShaderType == VERTEX_SHADER &&
        HaveLimitedInOutLocationQualifier(language, psShader->extensions) &&
        psContext->flags & HLSLCC_FLAG_NVN_TARGET)
    {
        bcatcstr(glsl, "out gl_PerVertex { vec4 gl_Position; };\n");
    }

	if (!psContext->psDependencies->m_ExtBlendModes.empty() && psShader->eShaderType == PIXEL_SHADER)
	{
		bcatcstr(extensions, "#extension GL_KHR_blend_equation_advanced : enable\n");
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
		phase.PruneConstArrays();

	}

	psShader->PruneTempRegisters();

	for (ui32Phase = 0; ui32Phase < psShader->asPhases.size(); ui32Phase++)
	{
		// Loop transform can only be done after the temps have been pruned
		ShaderPhase &phase = psShader->asPhases[ui32Phase];
		HLSLcc::DoLoopTransform(phase);

		if ((psContext->flags & HLSLCC_FLAG_VULKAN_SPECIALIZATION_CONSTANTS) != 0)
		{
			IdentifyStaticBranches(&phase);
		}

	}

	//Special case. Can have multiple phases.
    if(psShader->eShaderType == HULL_SHADER)
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
		for(i=0; i < psShader->asPhases[1].psDecl.size(); ++i)
        {
			TranslateDeclaration(&psShader->asPhases[1].psDecl[i]);
        }

		if (hasControlPointPhase == 0)
		{
			DoHullShaderPassthrough(psContext);
		}

		for(ui32Phase=2; ui32Phase<psShader->asPhases.size(); ui32Phase++)
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

		for(ui32PhaseCallIndex=0; ui32PhaseCallIndex<3; ui32PhaseCallIndex++)
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

        // Concat extensions and glsl for the final shader code.
        bconcat(extensions, glsl);
        bdestroy(glsl);
        psContext->glsl = extensions;
        glsl = NULL;

        if(psContext->psDependencies)
        {
            //Save partitioning and primitive type for use by domain shader.
            psContext->psDependencies->eTessOutPrim = psShader->sInfo.eTessOutPrim;

            psContext->psDependencies->eTessPartitioning = psShader->sInfo.eTessPartitioning;
        }

        return true;
	}

    if(psShader->eShaderType == DOMAIN_SHADER && psContext->psDependencies)
    {
        //Load partitioning and primitive type from hull shader.
        switch(psContext->psDependencies->eTessOutPrim)
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

        switch(psContext->psDependencies->eTessPartitioning)
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

	for (i = 0; i < psShader->asPhases[0].psDecl.size(); ++i)
	{
		TranslateDeclaration(&psShader->asPhases[0].psDecl[i]);
	}

	if ((psContext->flags & HLSLCC_FLAG_VULKAN_SPECIALIZATION_CONSTANTS) != 0)
	{
		DeclareSpecializationConstants(psShader->asPhases[0]);
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

    for(i=0; i < psShader->asPhases[0].psInst.size(); ++i)
    {
		TranslateInstruction(&psShader->asPhases[0].psInst[i]);
    }

    psContext->indent--;

    bcatcstr(glsl, "}\n");

    // Concat extensions and glsl for the final shader code.
    bconcat(extensions, glsl);
    bdestroy(glsl);
    psContext->glsl = extensions;
    glsl = NULL;

    return true;
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
		if(alreadyDeclared.insert(slot).second) // Only declare if the insertion actually succeeded
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
	for (int chunk = 0; len > 0; len -= 3, chunk++) {
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


void ToGLSL::BuildStaticBranchNameForInstruction(Instruction &inst)
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
		Base64Encode(oss.str(), inst.m_StaticBranchName);
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
		if(argType != SVT_BOOL)
			oss << "!=0";
		Base64Encode(oss.str(), inst.m_StaticBranchName);

	}

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
			psPhase->m_StaticBranchInstructions.push_back(&i);
			i.m_IsStaticBranch = true;
			i.m_StaticBranchCondition = NULL;
			BuildStaticBranchNameForInstruction(i);
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
					psPhase->m_StaticBranchInstructions.push_back(&i);
					i.m_IsStaticBranch = true;
					i.m_StaticBranchCondition = &def;
					BuildStaticBranchNameForInstruction(i);
				}
			}
		}
	}
}
