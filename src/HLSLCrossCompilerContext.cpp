
#include "internal_includes/HLSLCrossCompilerContext.h"
#include "internal_includes/HLSLccToolkit.h"
#include "internal_includes/Shader.h"
#include "internal_includes/DataTypeAnalysis.h"
#include "internal_includes/UseDefineChains.h"
#include "internal_includes/Declaration.h"
#include "internal_includes/debug.h"
#include "internal_includes/Translator.h"
#include "internal_includes/ControlFlowGraph.h"
#include <sstream>

void HLSLCrossCompilerContext::DoDataTypeAnalysis(ShaderPhase *psPhase)
{
	size_t ui32DeclCount = psPhase->psDecl.size();
	uint32_t i;

	psPhase->psTempDeclaration = NULL;
	psPhase->ui32OrigTemps = 0;
	psPhase->ui32TotalTemps = 0;

	// Retrieve the temp decl count
	for (i = 0; i < ui32DeclCount; ++i)
	{
		if (psPhase->psDecl[i].eOpcode == OPCODE_DCL_TEMPS)
		{
			psPhase->ui32TotalTemps = psPhase->psDecl[i].value.ui32NumTemps;
			psPhase->psTempDeclaration = &psPhase->psDecl[i];
			break;
		}
	}

	if (psPhase->ui32TotalTemps == 0)
		return;

	psPhase->ui32OrigTemps = psPhase->ui32TotalTemps;

	// The split table is a table containing the index of the original register this register was split out from, or 0xffffffff
	// Format: lowest 16 bits: original register. bits 16-23: rebase (eg value of 1 means .yzw was changed to .xyz): bits 24-31: component count
	psPhase->pui32SplitInfo.clear();
	psPhase->pui32SplitInfo.resize(psPhase->ui32TotalTemps * 2, 0xffffffff);

	// Build use-define chains and split temps based on those.
	{
		DefineUseChains duChains;
		UseDefineChains udChains;

		BuildUseDefineChains(psPhase->psInst, psPhase->ui32TotalTemps, duChains, udChains, psPhase->GetCFG());

		CalculateStandaloneDefinitions(duChains, psPhase->ui32TotalTemps);

		// Only do sampler precision downgrade on pixel shaders.
		if (psShader->eShaderType == PIXEL_SHADER)
			UpdateSamplerPrecisions(psShader->sInfo, duChains, psPhase->ui32TotalTemps);

		UDSplitTemps(&psPhase->ui32TotalTemps, duChains, udChains, psPhase->pui32SplitInfo);

		WriteBackUsesAndDefines(duChains);
	}
	
	HLSLcc::DataTypeAnalysis::SetDataTypes(this, psPhase->psInst, psPhase->ui32TotalTemps, psPhase->peTempTypes);

	if (psPhase->psTempDeclaration && (psPhase->ui32OrigTemps != psPhase->ui32TotalTemps))
		psPhase->psTempDeclaration->value.ui32NumTemps = psPhase->ui32TotalTemps;
}

void HLSLCrossCompilerContext::ClearDependencyData()
{

	switch (psShader->eShaderType)
	{
	case PIXEL_SHADER:
	{
		psDependencies->ClearCrossDependencyData();
		break;
	}
	case HULL_SHADER:
	{
		psDependencies->eTessPartitioning = TESSELLATOR_PARTITIONING_UNDEFINED;
		psDependencies->eTessOutPrim = TESSELLATOR_OUTPUT_UNDEFINED;
		break;
	}
	default:
		break;
	}
}

void HLSLCrossCompilerContext::AddIndentation()
{
	int i;
	bstring glsl = *currentGLSLString;
	for (i = 0; i < indent; ++i)
	{
		bcatcstr(glsl, "    ");
	}
}

void HLSLCrossCompilerContext::RequireExtension(const std::string &extName)
{
	if (m_EnabledExtensions.find(extName) != m_EnabledExtensions.end())
		return;

	m_EnabledExtensions.insert(extName);
    bformata(extensions, "#ifdef %s\n", extName.c_str());
	bformata(extensions, "#extension %s : require\n", extName.c_str());
    bcatcstr(extensions, "#endif\n");
}

std::string HLSLCrossCompilerContext::GetDeclaredInputName(const Operand* psOperand, int *piRebase, int iIgnoreRedirect, uint32_t *puiIgnoreSwizzle) const
{
	std::ostringstream oss;
	const ShaderInfo::InOutSignature* psIn = NULL;
	int regSpace = psOperand->GetRegisterSpace(this);

	if (iIgnoreRedirect == 0)
	{
		if ((regSpace == 0 && psShader->asPhases[currentPhase].acInputNeedsRedirect[psOperand->ui32RegisterNumber] == 0xfe)
			||
			(regSpace == 1 && psShader->asPhases[currentPhase].acPatchConstantsNeedsRedirect[psOperand->ui32RegisterNumber] == 0xfe))
		{
			oss << "phase" << currentPhase << "_Input" << regSpace << "_" << psOperand->ui32RegisterNumber;
			if (piRebase)
				*piRebase = 0;
			return oss.str();
		}
	}

	if (regSpace == 0)
		psShader->sInfo.GetInputSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->GetAccessMask(), &psIn, true);
	else
		psShader->sInfo.GetPatchConstantSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->GetAccessMask(), &psIn, true);

	if (psIn && piRebase)
		*piRebase = psIn->iRebase;

	std::string res = "";
	bool skipPrefix = false;
	if (psTranslator->TranslateSystemValue(psOperand, psIn, res, puiIgnoreSwizzle, psShader->aIndexedInput[regSpace][psOperand->ui32RegisterNumber] != 0, true, &skipPrefix))
	{
		if (psShader->eTargetLanguage == LANG_METAL && (iIgnoreRedirect == 0) && !skipPrefix)
			return inputPrefix + res;
		else
			return res;
	}

	ASSERT(psIn != NULL);
	oss << inputPrefix << (regSpace == 1 ? "patch" : "") << psIn->semanticName << psIn->ui32SemanticIndex;
	return oss.str();
}


std::string HLSLCrossCompilerContext::GetDeclaredOutputName(const Operand* psOperand,
	int* piStream,
	uint32_t *puiIgnoreSwizzle,
	int *piRebase,
	int iIgnoreRedirect) const
{
	std::ostringstream oss;
	const ShaderInfo::InOutSignature* psOut = NULL;
	int regSpace = psOperand->GetRegisterSpace(this);

	if (iIgnoreRedirect == 0)
	{
		if ((regSpace == 0 && psShader->asPhases[currentPhase].acOutputNeedsRedirect[psOperand->ui32RegisterNumber] == 0xfe)
			|| (regSpace == 1 && psShader->asPhases[currentPhase].acPatchConstantsNeedsRedirect[psOperand->ui32RegisterNumber] == 0xfe))
		{
			oss << "phase" << currentPhase << "_Output" << regSpace << "_" << psOperand->ui32RegisterNumber;
			if (piRebase)
				*piRebase = 0;
			return oss.str();
		}
	}

	if (regSpace == 0)
		psShader->sInfo.GetOutputSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->GetAccessMask(), psShader->ui32CurrentVertexOutputStream, &psOut, true);
	else
		psShader->sInfo.GetPatchConstantSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->GetAccessMask(), &psOut, true);


	if (psOut && piRebase)
		*piRebase = psOut->iRebase;

	if (psOut && (psOut->isIndexed.find(currentPhase) != psOut->isIndexed.end()))
	{
		// Need to route through temp output variable
		oss << "phase" << currentPhase << "_Output" << regSpace << "_" << psOut->indexStart.find(currentPhase)->second;
		if (!psOperand->m_SubOperands[0].get())
		{
			oss << "[" << psOperand->ui32RegisterNumber << "]";
		}
		if (piRebase)
			*piRebase = 0;
		return oss.str();
	}

	std::string res = "";
	if (psTranslator->TranslateSystemValue(psOperand, psOut, res, puiIgnoreSwizzle, psShader->aIndexedOutput[regSpace][psOperand->ui32RegisterNumber], false))
	{
		// HACK: i couldnt find better way to handle it
		// clip planes will always have interim variable, as HLSL operates on float4 but we need to size output accordingly with actual planes count
		// for some reason TranslateSystemValue return *outSkipPrefix = true for ALL system vars and then we simply ignore it here
		const bool isClipPlanes = psOut && psOut->eSystemValueType == NAME_CLIP_DISTANCE;

		if (psShader->eTargetLanguage == LANG_METAL && (iIgnoreRedirect == 0) && !isClipPlanes)

			return outputPrefix + res;
		else
			return res;
	}
	ASSERT(psOut != NULL);

	oss << outputPrefix << (regSpace == 1 ? "patch" : "") << psOut->semanticName << psOut->ui32SemanticIndex;
	return oss.str();
}

bool HLSLCrossCompilerContext::OutputNeedsDeclaring(const Operand* psOperand, const int count)
{
	char compMask = (char)psOperand->ui32CompMask;
	int regSpace = psOperand->GetRegisterSpace(this);
	uint32_t startIndex = psOperand->ui32RegisterNumber + (psShader->ui32CurrentVertexOutputStream * 1024); // Assume less than 1K input streams
	ASSERT(psShader->ui32CurrentVertexOutputStream < 4);

	// First check for various builtins, mostly depth-output ones.
	if (psShader->eShaderType == PIXEL_SHADER)
	{
		if (psOperand->eType == OPERAND_TYPE_OUTPUT_DEPTH_GREATER_EQUAL ||
			psOperand->eType == OPERAND_TYPE_OUTPUT_DEPTH_LESS_EQUAL)
		{
			return true;
		}

		if (psOperand->eType == OPERAND_TYPE_OUTPUT_DEPTH)
		{
			// GL doesn't need declaration, Metal does.
			return psShader->eTargetLanguage == LANG_METAL;
		}
	}

	// Needs declaring if any of the components hasn't been already declared
	if ((compMask & ~psShader->acOutputDeclared[regSpace][startIndex]) != 0)
	{
		int offset;
		const ShaderInfo::InOutSignature* psSignature = NULL;

		if (psOperand->eSpecialName == NAME_UNDEFINED)
		{
			// Need to fetch the actual comp mask
			if (regSpace == 0)
				psShader->sInfo.GetOutputSignatureFromRegister(
					psOperand->ui32RegisterNumber,
					psOperand->ui32CompMask,
					psShader->ui32CurrentVertexOutputStream,
					&psSignature);
			else
				psShader->sInfo.GetPatchConstantSignatureFromRegister(
					psOperand->ui32RegisterNumber,
					psOperand->ui32CompMask,
					&psSignature);

			compMask = (char)psSignature->ui32Mask;
		}
		for (offset = 0; offset < count; offset++)
		{
			psShader->acOutputDeclared[regSpace][startIndex + offset] |= compMask;
		}

		if (psSignature && (psSignature->semanticName == "PSIZE") && (psShader->eTargetLanguage != LANG_METAL))
		{
			// gl_PointSize, doesn't need declaring. TODO: Metal doesn't have pointsize at all?
			return false;
		}

		return true;
	}

	return false;
}
