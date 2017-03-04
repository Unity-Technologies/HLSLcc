#include "internal_includes/toGLSLOperand.h"
#include "internal_includes/HLSLccToolkit.h"
#include "internal_includes/HLSLCrossCompilerContext.h"
#include "bstrlib.h"
#include "hlslcc.h"
#include "internal_includes/debug.h"
#include "internal_includes/Shader.h"
#include "internal_includes/toGLSL.h"
#include <cmath>
#include <algorithm>

#include <sstream>

#include <float.h>
#include <stdlib.h>

using namespace HLSLcc;

#ifdef _MSC_VER
#define fpcheck(x) (_isnan(x) || !_finite(x))
#else
#define fpcheck(x) (std::isnan(x) || std::isinf(x))
#endif


// Returns nonzero if types are just different precisions of the same underlying type
static bool AreTypesCompatible(SHADER_VARIABLE_TYPE a, uint32_t ui32TOFlag)
{
	SHADER_VARIABLE_TYPE b = TypeFlagsToSVTType(ui32TOFlag);

	if (a == b)
		return true;

	// Special case for array indices: both uint and int are fine
	if ((ui32TOFlag & TO_FLAG_INTEGER) && (ui32TOFlag & TO_FLAG_UNSIGNED_INTEGER) &&
		(a == SVT_INT || a == SVT_INT16 || a == SVT_UINT || a == SVT_UINT16))
		return true;

	if ((a == SVT_FLOAT || a == SVT_FLOAT16 || a == SVT_FLOAT10) &&
		(b == SVT_FLOAT || b == SVT_FLOAT16 || b == SVT_FLOAT10))
		return true;

	if ((a == SVT_INT || a == SVT_INT16 || a == SVT_INT12) &&
		(b == SVT_INT || b == SVT_INT16 || a == SVT_INT12))
		return true;

	if ((a == SVT_UINT || a == SVT_UINT16) &&
		(b == SVT_UINT || b == SVT_UINT16))
		return true;

	return false;
}

void TranslateOperandSwizzle(HLSLCrossCompilerContext* psContext, const Operand* psOperand, int iRebase)
{
	TranslateOperandSwizzleWithMask(psContext, psOperand, OPERAND_4_COMPONENT_MASK_ALL, iRebase);
}

void TranslateOperandSwizzleWithMask(HLSLCrossCompilerContext* psContext, const Operand* psOperand, uint32_t ui32ComponentMask, int iRebase)
{
	bstring glsl = *psContext->currentGLSLString;
	uint32_t accessMask = ui32ComponentMask & psOperand->GetAccessMask();
	if(psOperand->eType == OPERAND_TYPE_INPUT)
	{
		int regSpace = psOperand->GetRegisterSpace(psContext);
		// Skip swizzle for scalar inputs, but only if we haven't redirected them
		if (regSpace == 0)
		{
			if ((psContext->psShader->asPhases[psContext->currentPhase].acInputNeedsRedirect[psOperand->ui32RegisterNumber] == 0) &&
				(psContext->psShader->abScalarInput[regSpace][psOperand->ui32RegisterNumber] & accessMask))
			{
				return;
			}
		}
		else
		{
			if ((psContext->psShader->asPhases[psContext->currentPhase].acPatchConstantsNeedsRedirect[psOperand->ui32RegisterNumber] == 0) &&
				(psContext->psShader->abScalarInput[regSpace][psOperand->ui32RegisterNumber] & accessMask))
			{
				return;
			}
		}
	}
	if (psOperand->eType == OPERAND_TYPE_OUTPUT)
	{
		int regSpace = psOperand->GetRegisterSpace(psContext);
		// Skip swizzle for scalar outputs, but only if we haven't redirected them
		if (regSpace == 0)
		{
			if ((psContext->psShader->asPhases[psContext->currentPhase].acOutputNeedsRedirect[psOperand->ui32RegisterNumber] == 0) &&
				(psContext->psShader->abScalarOutput[regSpace][psOperand->ui32RegisterNumber] & accessMask))
			{
				return;
			}
		}
		else
		{
			if ((psContext->psShader->asPhases[psContext->currentPhase].acPatchConstantsNeedsRedirect[psOperand->ui32RegisterNumber] == 0) &&
				(psContext->psShader->abScalarOutput[regSpace][psOperand->ui32RegisterNumber] & accessMask))
			{
				return;
			}
		}
	}

	if(psOperand->eType == OPERAND_TYPE_CONSTANT_BUFFER)
	{
        /*ConstantBuffer* psCBuf = NULL;
        ShaderVar* psVar = NULL;
        int32_t index = -1;
        GetConstantBufferFromBindingPoint(psOperand->aui32ArraySizes[0], &psContext->psShader->sInfo, &psCBuf);

        //Access the Nth vec4 (N=psOperand->aui32ArraySizes[1])
        //then apply the sizzle.

        GetShaderVarFromOffset(psOperand->aui32ArraySizes[1], psOperand->aui32Swizzle, psCBuf, &psVar, &index);

        bformata(glsl, ".%s", psVar->Name);
        if(index != -1)
        {
            bformata(glsl, "[%d]", index);
        }*/

		//return;
	}

	if(psOperand->iWriteMaskEnabled &&
	   psOperand->iNumComponents != 1)
	{
		//Component Mask
		if(psOperand->eSelMode == OPERAND_4_COMPONENT_MASK_MODE)
		{
			uint32_t mask;
			if (psOperand->ui32CompMask != 0)
				mask = psOperand->ui32CompMask & ui32ComponentMask;
			else
				mask = ui32ComponentMask;

			if(mask != 0 && mask != OPERAND_4_COMPONENT_MASK_ALL)
			{
				bcatcstr(glsl, ".");
				if(mask & OPERAND_4_COMPONENT_MASK_X)
				{
					ASSERT(iRebase == 0);
					bcatcstr(glsl, "x");
				}
				if(mask & OPERAND_4_COMPONENT_MASK_Y)
				{
					ASSERT(iRebase <= 1);
					bformata(glsl, "%c", "xy"[1 - iRebase]);
				}
				if(mask & OPERAND_4_COMPONENT_MASK_Z)
				{
					ASSERT(iRebase <= 2);
					bformata(glsl, "%c", "xyz"[2 - iRebase]);
				}
				if(mask & OPERAND_4_COMPONENT_MASK_W)
				{
					ASSERT(iRebase <= 3);
					bformata(glsl, "%c", "xyzw"[3 - iRebase]);
				}
			}
		}
		else
		//Component Swizzle
		if(psOperand->eSelMode == OPERAND_4_COMPONENT_SWIZZLE_MODE)
		{
			if (ui32ComponentMask != OPERAND_4_COMPONENT_MASK_ALL ||
				!(psOperand->aui32Swizzle[0] == OPERAND_4_COMPONENT_X &&
					psOperand->aui32Swizzle[1] == OPERAND_4_COMPONENT_Y &&
					psOperand->aui32Swizzle[2] == OPERAND_4_COMPONENT_Z &&
					psOperand->aui32Swizzle[3] == OPERAND_4_COMPONENT_W
				 )
				)
			{
				uint32_t i;

				bcatcstr(glsl, ".");

				for (i = 0; i < 4; ++i)
				{
					if (!(ui32ComponentMask & (OPERAND_4_COMPONENT_MASK_X << i)))
						continue;

					if (psOperand->aui32Swizzle[i] == OPERAND_4_COMPONENT_X)
					{
						ASSERT(iRebase == 0);
						bcatcstr(glsl, "x");
					}
					else if (psOperand->aui32Swizzle[i] == OPERAND_4_COMPONENT_Y)
					{
						ASSERT(iRebase <= 1);
						bformata(glsl, "%c", "xy"[1 - iRebase]);
					}
					else if (psOperand->aui32Swizzle[i] == OPERAND_4_COMPONENT_Z)
					{
						ASSERT(iRebase <= 2);
						bformata(glsl, "%c", "xyz"[2 - iRebase]);
					}
					else if (psOperand->aui32Swizzle[i] == OPERAND_4_COMPONENT_W)
					{
						ASSERT(iRebase <= 3);
						bformata(glsl, "%c", "xyzw"[3 - iRebase]);
					}
				}
			}
		}
		else
		if(psOperand->eSelMode == OPERAND_4_COMPONENT_SELECT_1_MODE) // ui32ComponentMask is ignored in this case
		{
			bcatcstr(glsl, ".");

			if(psOperand->aui32Swizzle[0] == OPERAND_4_COMPONENT_X)
			{
				ASSERT(iRebase == 0);
				bcatcstr(glsl, "x");
			}
			else
			if(psOperand->aui32Swizzle[0] == OPERAND_4_COMPONENT_Y)
			{
				ASSERT(iRebase <= 1);
				bformata(glsl, "%c", "xy"[1 - iRebase]);
			}
			else
			if(psOperand->aui32Swizzle[0] == OPERAND_4_COMPONENT_Z)
			{
				ASSERT(iRebase <= 2);
				bformata(glsl, "%c", "xyz"[2 - iRebase]);
			}
			else
			if(psOperand->aui32Swizzle[0] == OPERAND_4_COMPONENT_W)
			{
				ASSERT(iRebase <= 3);
				bformata(glsl, "%c", "xyzw"[3 - iRebase]);
			}
		}

		//Component Select 1
	}
}

void ToGLSL::TranslateOperandIndex(const Operand* psOperand, int index)
{
	int i = index;

	bstring glsl = *psContext->currentGLSLString;

	ASSERT(index < psOperand->iIndexDims);

	switch(psOperand->eIndexRep[i])
	{
		case OPERAND_INDEX_IMMEDIATE32:
		{
			bformata(glsl, "[%d]", psOperand->aui32ArraySizes[i]);
			break;
		}
		case OPERAND_INDEX_RELATIVE:
		{
			bcatcstr(glsl, "[");
			TranslateOperand(psOperand->m_SubOperands[i].get(), TO_FLAG_INTEGER);
			bcatcstr(glsl, "]");
			break;
		}
		case OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE:
		{
			bcatcstr(glsl, "["); //Indexes must be integral.
			TranslateOperand(psOperand->m_SubOperands[i].get(), TO_FLAG_INTEGER);
			bformata(glsl, " + %d]", psOperand->aui32ArraySizes[i]);
			break;
		}
		default:
		{
			break;
		}
	}
}

void ToGLSL::TranslateOperandIndexMAD(const Operand* psOperand, int index, uint32_t multiply, uint32_t add)
{
	int i = index;
	int isGeoShader = psContext->psShader->eShaderType == GEOMETRY_SHADER ? 1 : 0;

	bstring glsl = *psContext->currentGLSLString;

	ASSERT(index < psOperand->iIndexDims);

	switch(psOperand->eIndexRep[i])
	{
		case OPERAND_INDEX_IMMEDIATE32:
		{
			if(i > 0 || isGeoShader)
			{
				bformata(glsl, "[%d*%d+%d]", psOperand->aui32ArraySizes[i], multiply, add);
			}
			else
			{
				bformata(glsl, "%d*%d+%d", psOperand->aui32ArraySizes[i], multiply, add);
			}
			break;
		}
		case OPERAND_INDEX_RELATIVE:
		{
			bcatcstr(glsl, "[int("); //Indexes must be integral.
			TranslateOperand(psOperand->m_SubOperands[i].get(), TO_FLAG_NONE);
			bformata(glsl, ")*%d+%d]", multiply, add);
			break;
		}
		case OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE:
		{
			bcatcstr(glsl, "[(int("); //Indexes must be integral.
			TranslateOperand(psOperand->m_SubOperands[i].get(), TO_FLAG_NONE);
			bformata(glsl, ") + %d)*%d+%d]", psOperand->aui32ArraySizes[i], multiply, add);
			break;
		}
		default:
		{
			break;
		}
	}
}

static std::string GetBitcastOp(HLSLCrossCompilerContext *psContext, SHADER_VARIABLE_TYPE from, SHADER_VARIABLE_TYPE to, uint32_t numComponents)
{
	if (psContext->psShader->eTargetLanguage == LANG_METAL)
	{
		std::ostringstream oss;
		oss << "as_type<";
		oss << GetConstructorForTypeMetal(to, numComponents);
		oss << ">";
		return oss.str();
	}
	else
	{
	if ((to == SVT_FLOAT || to == SVT_FLOAT16 || to == SVT_FLOAT10) && from == SVT_INT)
		return "intBitsToFloat";
	else if ((to == SVT_FLOAT || to == SVT_FLOAT16 || to == SVT_FLOAT10) && from == SVT_UINT)
		return "uintBitsToFloat";
	else if (to == SVT_INT && (from == SVT_FLOAT || from == SVT_FLOAT16 || from == SVT_FLOAT10))
		return "floatBitsToInt";
	else if (to == SVT_UINT && (from == SVT_FLOAT || from == SVT_FLOAT16 || from == SVT_FLOAT10))
		return "floatBitsToUint";
	}

	ASSERT(0);
	return "ERROR missing components in GetBitcastOp()";
}

// Helper function to print out a single 32-bit immediate value in desired format
static void printImmediate32(HLSLCrossCompilerContext *psContext, uint32_t value, SHADER_VARIABLE_TYPE eType)
{
	bstring glsl = *psContext->currentGLSLString;
	int needsParenthesis = 0;

	// Print floats as bit patterns.
	if ((eType == SVT_FLOAT || eType == SVT_FLOAT16 || eType == SVT_FLOAT10) && psContext->psShader->ui32MajorVersion > 3 && fpcheck(*((float *)(&value))))
	{
		if (psContext->psShader->eTargetLanguage == LANG_METAL)
			bcatcstr(glsl, "as_type<float>(");
		else
		bcatcstr(glsl, "intBitsToFloat(");
		eType = SVT_INT;
		needsParenthesis = 1;
	}

	switch (eType)
	{
	default:
		ASSERT(0);
	case SVT_INT:
	case SVT_INT16:
	case SVT_INT12:
		// Need special handling for anything >= uint 0x3fffffff
		if (value > 0x3ffffffe)
			bformata(glsl, "int(0x%Xu)", value);
		else if(value <= 1024) // Print anything below 1024 as decimal, and hex after that
			bformata(glsl, "%d", value);
		else
			bformata(glsl, "0x%X", value);
		break;
	case SVT_UINT:
	case SVT_UINT16:
		// Adreno bug workaround (happens only on pre-lollipop Nexus 4's): '0u' is treated as int.
		if (value == 0 && psContext->psShader->eTargetLanguage == LANG_ES_300)
			bcatcstr(glsl, "uint(0u)");
		else
			bformata(glsl, "%uu", value);
		break;
	case SVT_FLOAT:
	case SVT_FLOAT10:
	case SVT_FLOAT16:
		HLSLcc::PrintFloat(glsl, *((float *)(&value)));
		break;
	case SVT_BOOL:
		if (value == 0)
			bcatcstr(glsl, "false");
		else
			bcatcstr(glsl, "true");
	}
	if (needsParenthesis)
		bcatcstr(glsl, ")");
}

void ToGLSL::TranslateVariableNameWithMask(const Operand* psOperand, uint32_t ui32TOFlag, uint32_t* pui32IgnoreSwizzle, uint32_t ui32CompMask, int *piRebase)
{
	int numParenthesis = 0;
	int hasCtor = 0;
	int needsBoolUpscale = 0; // If nonzero, bools need * 0xffffffff in them
	bstring glsl = *psContext->currentGLSLString;
	SHADER_VARIABLE_TYPE requestedType = TypeFlagsToSVTType(ui32TOFlag);
	SHADER_VARIABLE_TYPE eType = psOperand->GetDataType(psContext, requestedType);
	int numComponents = psOperand->GetNumSwizzleElements(ui32CompMask);
	int requestedComponents = 0;
	int scalarWithSwizzle = 0;

	*pui32IgnoreSwizzle = 0;

	if (psOperand->eType == OPERAND_TYPE_TEMP)
	{
		// Check for scalar
		if (psContext->psShader->GetTempComponentCount(eType, psOperand->ui32RegisterNumber) == 1 && psOperand->eSelMode == OPERAND_4_COMPONENT_SWIZZLE_MODE)
		{
			scalarWithSwizzle = 1; // Going to need a constructor
		}
	}

	if (psOperand->eType == OPERAND_TYPE_INPUT)
	{
		// Check for scalar
		if (psContext->psShader->abScalarInput[psOperand->GetRegisterSpace(psContext)][psOperand->ui32RegisterNumber] & psOperand->GetAccessMask()
			&& psOperand->eSelMode == OPERAND_4_COMPONENT_SWIZZLE_MODE)
		{
			scalarWithSwizzle = 1;
			*pui32IgnoreSwizzle = 1;
		}
	}

	if (psOperand->eType == OPERAND_TYPE_CONSTANT_BUFFER && psOperand->eSelMode == OPERAND_4_COMPONENT_SWIZZLE_MODE && psOperand->IsSwizzleReplicated())
	{
		// Needs scalar check as well
		const ConstantBuffer* psCBuf = NULL;
		const ShaderVarType* psVarType = NULL;
		int32_t rebase = 0;
		bool isArray;
		psContext->psShader->sInfo.GetConstantBufferFromBindingPoint(RGROUP_CBUFFER, psOperand->aui32ArraySizes[0], &psCBuf);
		ShaderInfo::GetShaderVarFromOffset(psOperand->aui32ArraySizes[1], psOperand->aui32Swizzle, psCBuf, &psVarType, &isArray, NULL, &rebase, psContext->flags);
		if (psVarType->Columns == 1)
		{
			scalarWithSwizzle = 1; // Needs a constructor
			*pui32IgnoreSwizzle = 1;
		}

	}

	if (piRebase)
		*piRebase = 0;

	if (ui32TOFlag & TO_AUTO_EXPAND_TO_VEC2)
		requestedComponents = 2;
	else if (ui32TOFlag & TO_AUTO_EXPAND_TO_VEC3)
		requestedComponents = 3;
	else if (ui32TOFlag & TO_AUTO_EXPAND_TO_VEC4)
		requestedComponents = 4;

	requestedComponents = std::max(requestedComponents, numComponents);

	if (!(ui32TOFlag & (TO_FLAG_DESTINATION | TO_FLAG_NAME_ONLY | TO_FLAG_DECLARATION_NAME)))
	{
		if (psOperand->eType == OPERAND_TYPE_IMMEDIATE32 || psOperand->eType == OPERAND_TYPE_IMMEDIATE64)
		{
			// Mark the operand type to match whatever we're asking for in the flags.
			((Operand *)psOperand)->aeDataType[0] = requestedType;
			((Operand *)psOperand)->aeDataType[1] = requestedType;
			((Operand *)psOperand)->aeDataType[2] = requestedType;
			((Operand *)psOperand)->aeDataType[3] = requestedType;
		}

		if (AreTypesCompatible(eType, ui32TOFlag) == 0)
		{
			if (CanDoDirectCast(eType, requestedType))
			{
				bformata(glsl, "%s(", GetConstructorForType(psContext, requestedType, requestedComponents, false));
				numParenthesis++;
				hasCtor = 1;
				if (eType == SVT_BOOL)
					needsBoolUpscale = 1;
			}
			else
			{
				// Direct cast not possible, need to do bitcast.
				bformata(glsl, "%s(", GetBitcastOp(psContext, eType, requestedType, requestedComponents).c_str());
				numParenthesis++;
			}
		}

		// Add ctor if needed (upscaling). Type conversion is already handled above, so here we must
		// use the original type to not make type conflicts in bitcasts
		if (((numComponents < requestedComponents)||(scalarWithSwizzle != 0)) && (hasCtor == 0))
		{
//			ASSERT(numComponents == 1);
			bformata(glsl, "%s(", GetConstructorForType(psContext, eType, requestedComponents, false));
			numParenthesis++;
			hasCtor = 1;
		}
	}


	switch(psOperand->eType)
	{
		case OPERAND_TYPE_IMMEDIATE32:
		{
			if(psOperand->iNumComponents == 1)
			{
				printImmediate32(psContext, *((unsigned int*)(&psOperand->afImmediates[0])), requestedType);
			}
			else
			{
				int i;
				int firstItemAdded = 0;
				if (hasCtor == 0)
				{
					bformata(glsl, "%s(", GetConstructorForType(psContext, requestedType, requestedComponents, false));
					numParenthesis++;
					hasCtor = 1;
				}
				for (i = 0; i < 4; i++)
				{
					uint32_t uval;
					if (!(ui32CompMask & (1 << i)))
						continue;

					if (firstItemAdded)
						bcatcstr(glsl, ", ");
					uval = *((uint32_t*)(&psOperand->afImmediates[i >= psOperand->iNumComponents ? psOperand->iNumComponents-1 : i]));
					printImmediate32(psContext, uval, requestedType);
					firstItemAdded = 1;
				}
				bcatcstr(glsl, ")");
				*pui32IgnoreSwizzle = 1;
				numParenthesis--;
			}
			break;
		}
		case OPERAND_TYPE_IMMEDIATE64:
		{
			if(psOperand->iNumComponents == 1)
			{
				bformata(glsl, "%.17g",
				    psOperand->adImmediates[0]);
			}
			else
			{
				bformata(glsl, "dvec4(%.17g, %.17g, %.17g, %.17g)",
				    psOperand->adImmediates[0],
				    psOperand->adImmediates[1],
				    psOperand->adImmediates[2],
				    psOperand->adImmediates[3]);
				if(psOperand->iNumComponents != 4)
				{
					AddSwizzleUsingElementCount(glsl, psOperand->iNumComponents);
				}
			}
			break;
		}
		case OPERAND_TYPE_INPUT:
		{
			int regSpace = psOperand->GetRegisterSpace(psContext);
			switch(psOperand->iIndexDims)
			{
				case INDEX_2D:
				{
					const ShaderInfo::InOutSignature *psSig = NULL;
					psContext->psShader->sInfo.GetInputSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->ui32CompMask, &psSig);

					if ((psSig->eSystemValueType == NAME_POSITION && psSig->ui32SemanticIndex == 0) ||
						(psSig->semanticName == "POS" && psSig->ui32SemanticIndex == 0) ||
						(psSig->semanticName == "SV_POSITION" && psSig->ui32SemanticIndex == 0))
					{
						bcatcstr(glsl, "gl_in");
						TranslateOperandIndex(psOperand, 0);//Vertex index
						bcatcstr(glsl, ".gl_Position");
					}
					else
					{
						std::string name = psContext->GetDeclaredInputName(psOperand, piRebase, 0, pui32IgnoreSwizzle);

						bformata(glsl, "%s", name.c_str());
						TranslateOperandIndex(psOperand, 0);//Vertex index
					}
					break;
				}
				default:
				{
					if(psOperand->eIndexRep[0] == OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE)
					{
						bformata(glsl, "phase%d_Input%d_%d[", psContext->currentPhase, regSpace, psOperand->ui32RegisterNumber);
						TranslateOperand(psOperand->m_SubOperands[0].get(), TO_FLAG_INTEGER);
						bcatcstr(glsl, "]");
					}
					else
					{
						if(psContext->psShader->aIndexedInput[regSpace][psOperand->ui32RegisterNumber] != 0)
						{
							const uint32_t parentIndex = psContext->psShader->aIndexedInputParents[regSpace][psOperand->ui32RegisterNumber];
							bformata(glsl, "phase%d_Input%d_%d[%d]", psContext->currentPhase, regSpace, parentIndex,
								psOperand->ui32RegisterNumber - parentIndex);
						}
						else
						{
							std::string name = psContext->GetDeclaredInputName(psOperand, piRebase, 0, pui32IgnoreSwizzle);
							bcatcstr(glsl, name.c_str());
						}
					}
					break;
				}
			}
			break;
		}
		case OPERAND_TYPE_OUTPUT:
		{
			/*if(psContext->psShader->eShaderType == HULL_SHADER && psOperand->eIndexRep[0] == OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE)
			{
				int stream = 0;
				const char* name = GetDeclaredOutputName(psContext, HULL_SHADER, psOperand, &stream);
				bcatcstr(glsl, name);
			}
			else*/
			{
				int stream = 0;
				std::string name = psContext->GetDeclaredOutputName(psOperand, &stream, pui32IgnoreSwizzle, piRebase, 0);
				bcatcstr(glsl, name.c_str());
				if (psOperand->m_SubOperands[0].get())
				{
					bcatcstr(glsl, "[");
					TranslateOperand(psOperand->m_SubOperands[0].get(), TO_AUTO_BITCAST_TO_INT);
					bcatcstr(glsl, "]");
				}
			}
			break;
		}
		case OPERAND_TYPE_OUTPUT_DEPTH:
		case OPERAND_TYPE_OUTPUT_DEPTH_GREATER_EQUAL:
		case OPERAND_TYPE_OUTPUT_DEPTH_LESS_EQUAL:
		{
			bcatcstr(glsl, "gl_FragDepth");
			break;
		}
		case OPERAND_TYPE_TEMP:
		{
			SHADER_VARIABLE_TYPE eTempType = psOperand->GetDataType(psContext);
			bcatcstr(glsl, HLSLCC_TEMP_PREFIX);
			ASSERT(psOperand->ui32RegisterNumber < 0x10000); // Sanity check after temp splitting.
			switch (eTempType)
			{
			case SVT_FLOAT:
				ASSERT(psContext->psShader->psFloatTempSizes[psOperand->ui32RegisterNumber] != 0);
				if (psContext->psShader->psFloatTempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_FLOAT16:
				ASSERT(psContext->psShader->psFloat16TempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "16_");
				if (psContext->psShader->psFloat16TempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_FLOAT10:
				ASSERT(psContext->psShader->psFloat10TempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "10_");
				if (psContext->psShader->psFloat10TempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_INT:
				ASSERT(psContext->psShader->psIntTempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "i");
				if (psContext->psShader->psIntTempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_INT16:
				ASSERT(psContext->psShader->psInt16TempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "i16_");
				if (psContext->psShader->psInt16TempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_INT12:
				ASSERT(psContext->psShader->psInt12TempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "i12_");
				if (psContext->psShader->psInt12TempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_UINT:
				ASSERT(psContext->psShader->psUIntTempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "u");
				if (psContext->psShader->psUIntTempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_UINT16:
				ASSERT(psContext->psShader->psUInt16TempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "u16_");
				if (psContext->psShader->psUInt16TempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_DOUBLE:
				ASSERT(psContext->psShader->psDoubleTempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "d");
				if (psContext->psShader->psDoubleTempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			case SVT_BOOL:
				ASSERT(psContext->psShader->psBoolTempSizes[psOperand->ui32RegisterNumber] != 0);
				bcatcstr(glsl, "b");
				if (psContext->psShader->psBoolTempSizes[psOperand->ui32RegisterNumber] == 1 && pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
				break;
			default:
				ASSERT(0 && "Should never get here!");
			}
			// m_ForLoopInductorName overrides the register number, if available
			if (psOperand->m_ForLoopInductorName != 0)
			{
				bformata(glsl, "_loop_%d", psOperand->m_ForLoopInductorName);
				if (pui32IgnoreSwizzle)
					*pui32IgnoreSwizzle = 1;
			}
			else
				bformata(glsl, "%d", psOperand->ui32RegisterNumber);
			break;
		}
		case OPERAND_TYPE_SPECIAL_IMMCONSTINT:
		{
			bformata(glsl, "IntImmConst%d", psOperand->ui32RegisterNumber);
			break;
		}
		case OPERAND_TYPE_SPECIAL_IMMCONST:
		{
			ASSERT(0 && "DX9 shaders no longer supported!");
			break;
		}
		case OPERAND_TYPE_SPECIAL_OUTBASECOLOUR:
		{
			bcatcstr(glsl, "BaseColour");
			break;
		}
		case OPERAND_TYPE_SPECIAL_OUTOFFSETCOLOUR:
		{
			bcatcstr(glsl, "OffsetColour");
			break;
		}
		case OPERAND_TYPE_SPECIAL_POSITION:
		{
			bcatcstr(glsl, "gl_Position");
			break;
		}
		case OPERAND_TYPE_SPECIAL_FOG:
		{
			bcatcstr(glsl, "Fog");
			break;
		}
		case OPERAND_TYPE_SPECIAL_POINTSIZE:
		{
			bcatcstr(glsl, "gl_PointSize");
			break;
		}
		case OPERAND_TYPE_SPECIAL_ADDRESS:
		{
			bcatcstr(glsl, "Address");
			break;
		}
		case OPERAND_TYPE_SPECIAL_LOOPCOUNTER:
		{
			bcatcstr(glsl, "LoopCounter");
			pui32IgnoreSwizzle[0] = 1;
			break;
		}
		case OPERAND_TYPE_SPECIAL_TEXCOORD:
		{
			bformata(glsl, "TexCoord%d", psOperand->ui32RegisterNumber);
			break;
		}
		case OPERAND_TYPE_CONSTANT_BUFFER:
		{
			const char* StageName = "VS";
			const ConstantBuffer* psCBuf = NULL;
			const ShaderVarType* psVarType = NULL;
			int32_t index = -1;
			std::vector<uint32_t> arrayIndices;
			bool isArray;
			psContext->psShader->sInfo.GetConstantBufferFromBindingPoint(RGROUP_CBUFFER, psOperand->aui32ArraySizes[0], &psCBuf);

			switch(psContext->psShader->eShaderType)
			{
				case PIXEL_SHADER:
				{
					StageName = "PS";
					break;
				}
				case HULL_SHADER:
				{
					StageName = "HS";
					break;
				}
				case DOMAIN_SHADER:
				{
					StageName = "DS";
					break;
				}
				case GEOMETRY_SHADER:
				{
					StageName = "GS";
					break;
				}
				case COMPUTE_SHADER:
				{
					StageName = "CS";
					break;
				}
				default:
				{
					break;
				}
			}

			if(ui32TOFlag & TO_FLAG_DECLARATION_NAME)
			{
				pui32IgnoreSwizzle[0] = 1;
			}

			// FIXME: With ES 3.0 the buffer name is often not prepended to variable names
			if(((psContext->flags & HLSLCC_FLAG_UNIFORM_BUFFER_OBJECT)!=HLSLCC_FLAG_UNIFORM_BUFFER_OBJECT) &&
				((psContext->flags & HLSLCC_FLAG_DISABLE_GLOBALS_STRUCT)!=HLSLCC_FLAG_DISABLE_GLOBALS_STRUCT))
			{
				if(psCBuf)
				{
					//$Globals.
					if(psCBuf->name[0] == '$')
					{
						bformata(glsl, "Globals%s", StageName);
					}
					else
					{
						bformata(glsl, "%s%s", psCBuf->name.c_str(), StageName);
					}
					if((ui32TOFlag & TO_FLAG_DECLARATION_NAME) != TO_FLAG_DECLARATION_NAME)
					{
						bcatcstr(glsl, ".");
					}
				}
				else
				{
					//bformata(glsl, "cb%d", psOperand->aui32ArraySizes[0]);
				}
			}

			if((ui32TOFlag & TO_FLAG_DECLARATION_NAME) != TO_FLAG_DECLARATION_NAME)
			{
				//Work out the variable name. Don't apply swizzle to that variable yet.
				int32_t rebase = 0;

				if(psCBuf)
				{
					uint32_t componentsNeeded = 1;
					uint32_t minSwiz = 3;
					uint32_t maxSwiz = 0;
					if (psOperand->eSelMode != OPERAND_4_COMPONENT_SELECT_1_MODE)
					{
						int i;
						for (i = 0; i < 4; i++)
						{
							if ((ui32CompMask & (1 << i)) == 0)
								continue;
							minSwiz = std::min(minSwiz, psOperand->aui32Swizzle[i]);
							maxSwiz = std::max(maxSwiz, psOperand->aui32Swizzle[i]);
						}
						componentsNeeded = maxSwiz - minSwiz + 1;
					}
					else
					{
						minSwiz = maxSwiz = 1;
					}

					// When we have a component mask that doesn't have .x set (this basically only happens when we manually open operands into components)
					// We have to pull down the swizzle array to match the first bit that's actually set
					uint32_t tmpSwizzle[4] = { 0 };
					int firstBitSet = 0;
					if (ui32CompMask == 0)
						ui32CompMask = 0xf;
					while ((ui32CompMask & (1 << firstBitSet)) == 0)
						firstBitSet++;
					std::copy(&psOperand->aui32Swizzle[firstBitSet], &psOperand->aui32Swizzle[4], &tmpSwizzle[0]);

					ShaderInfo::GetShaderVarFromOffset(psOperand->aui32ArraySizes[1], tmpSwizzle, psCBuf, &psVarType, &isArray, &arrayIndices, &rebase, psContext->flags);
					if (psOperand->eSelMode == OPERAND_4_COMPONENT_SELECT_1_MODE || ((componentsNeeded+minSwiz) <= psVarType->Columns))
					{
						// Simple case: just access one component
						std::string fullName = ShaderInfo::GetShaderVarIndexedFullName(psVarType, arrayIndices);

						if (((psContext->flags & HLSLCC_FLAG_TRANSLATE_MATRICES) != 0) && ((psVarType->Class == SVC_MATRIX_ROWS) || (psVarType->Class == SVC_MATRIX_COLUMNS)))
						{
							// We'll need to add the prefix only to the last section of the name
							size_t commaPos = fullName.find_last_of('.');
							char prefix[256];
							sprintf(prefix, HLSLCC_TRANSLATE_MATRIX_FORMAT_STRING, psVarType->Rows, psVarType->Columns);
							if (commaPos == std::string::npos)
								fullName.insert(0, prefix);
							else
								fullName.insert(commaPos + 1, prefix);

							bformata(glsl, "%s", fullName.c_str());
						}
						else
							bformata(glsl, "%s", fullName.c_str());
					}
					else
					{
						// Non-simple case: build vec4 and apply mask
						uint32_t i;
						std::vector<uint32_t> tmpArrayIndices;
						bool tmpIsArray;
						int32_t tmpRebase;
						int firstItemAdded = 0;

						bformata(glsl, "%s(", GetConstructorForType(psContext, psVarType->Type, GetNumberBitsSet(ui32CompMask), false));
						for (i = 0; i < 4; i++)
						{
							const ShaderVarType *tmpVarType = NULL;
							if ((ui32CompMask & (1 << i)) == 0)
								continue;
							tmpRebase = 0;
							if (firstItemAdded != 0)
								bcatcstr(glsl, ", ");
							else
								firstItemAdded = 1;

							memset(tmpSwizzle, 0, sizeof(uint32_t) * 4);
							std::copy(&psOperand->aui32Swizzle[i], &psOperand->aui32Swizzle[4], &tmpSwizzle[0]);

							ShaderInfo::GetShaderVarFromOffset(psOperand->aui32ArraySizes[1], tmpSwizzle, psCBuf, &tmpVarType, &tmpIsArray, &tmpArrayIndices, &tmpRebase, psContext->flags);
							std::string fullName = ShaderInfo::GetShaderVarIndexedFullName(tmpVarType, tmpArrayIndices);

							if (tmpVarType->Class == SVC_SCALAR)
							{
								bformata(glsl, "%s", fullName.c_str());
							}
							else
							{
								uint32_t swizzle;
								tmpRebase /= 4; // 0 => 0, 4 => 1, 8 => 2, 12 /= 3
								swizzle = psOperand->aui32Swizzle[i] - tmpRebase;

								bformata(glsl, "%s", fullName.c_str());
								bformata(glsl, ".%c", "xyzw"[swizzle]);
							}
						}
						bcatcstr(glsl, ")");
						// Clear rebase, we've already done it.
						rebase = 0;
						// Also swizzle.
						*pui32IgnoreSwizzle = 1;
					}
				}
				else // We don't have a semantic for this variable, so try the raw dump appoach.
				{
					ASSERT(0);
					//bformata(glsl, "cb%d.data", psOperand->aui32ArraySizes[0]);//
					//index = psOperand->aui32ArraySizes[1];
				}

				if (isArray)
					index = arrayIndices.back();

				//Dx9 only?
				if(psOperand->m_SubOperands[0].get() != NULL)
				{
					// Array of matrices is treated as array of vec4s in HLSL,
					// but that would mess up uniform types in GLSL. Do gymnastics.
					uint32_t opFlags = TO_FLAG_INTEGER;

					if (((psVarType->Class == SVC_MATRIX_COLUMNS) || (psVarType->Class == SVC_MATRIX_ROWS)) && (psVarType->Elements > 1) && ((psContext->flags & HLSLCC_FLAG_TRANSLATE_MATRICES) == 0))
					{
						// Special handling for matrix arrays
						bcatcstr(glsl, "[(");
						TranslateOperand(psOperand->m_SubOperands[0].get(), opFlags);
						bformata(glsl, ") / 4]");
						{
							bcatcstr(glsl, "[((");
							TranslateOperand(psOperand->m_SubOperands[0].get(), opFlags, OPERAND_4_COMPONENT_MASK_X);
							bformata(glsl, ") %% 4)]");
						}
					}
					else
					{
						bcatcstr(glsl, "[");
						TranslateOperand(psOperand->m_SubOperands[0].get(), opFlags);
						bformata(glsl, "]");
					}
				}
				else
				if(index != -1 && psOperand->m_SubOperands[1].get() != NULL)
				{
					// Array of matrices is treated as array of vec4s in HLSL,
					// but that would mess up uniform types in GLSL. Do gymnastics.
					SHADER_VARIABLE_TYPE eType = psOperand->m_SubOperands[1].get()->GetDataType(psContext);
					uint32_t opFlags = TO_FLAG_INTEGER;
					if (eType != SVT_INT && eType != SVT_UINT)
						opFlags = TO_AUTO_BITCAST_TO_INT;

					if (((psVarType->Class == SVC_MATRIX_COLUMNS) ||( psVarType->Class == SVC_MATRIX_ROWS)) && (psVarType->Elements > 1) && ((psContext->flags & HLSLCC_FLAG_TRANSLATE_MATRICES) == 0))
					{
						// Special handling for matrix arrays
						bcatcstr(glsl, "[(");
						TranslateOperand(psOperand->m_SubOperands[1].get(), opFlags);
						bformata(glsl, " + %d) / 4]", index);
						{
							bcatcstr(glsl, "[((");
							TranslateOperand(psOperand->m_SubOperands[1].get(), opFlags);
							bformata(glsl, " + %d) %% 4)]", index);
						}
					}
					else
					{
						bcatcstr(glsl, "[");
						TranslateOperand(psOperand->m_SubOperands[1].get(), opFlags);
						if (index != 0)
							bformata(glsl, " + %d]", index);
						else
							bcatcstr(glsl, "]");
					}
				}
				else if(index != -1)
				{
					if (((psVarType->Class == SVC_MATRIX_COLUMNS) || (psVarType->Class == SVC_MATRIX_ROWS)) && (psVarType->Elements > 1) && ((psContext->flags & HLSLCC_FLAG_TRANSLATE_MATRICES) == 0))
					{
						// Special handling for matrix arrays, open them up into vec4's
						size_t matidx = index / 4;
						size_t rowidx = index - (matidx*4);
						bformata(glsl, "[%d][%d]", matidx, rowidx);
					}
					else
					{
						bformata(glsl, "[%d]", index);
					}
				}
				else if(psOperand->m_SubOperands[1].get() != NULL)
				{
					bcatcstr(glsl, "[");
					TranslateOperand(psOperand->m_SubOperands[1].get(), TO_FLAG_INTEGER);
					bcatcstr(glsl, "]");
				}

				if(psVarType && psVarType->Class == SVC_VECTOR && !*pui32IgnoreSwizzle)
				{
					switch(rebase)
					{
						case 4:
						{
							if(psVarType->Columns == 2)
							{
								//.x(GLSL) is .y(HLSL). .y(GLSL) is .z(HLSL)
								bcatcstr(glsl, ".xxyx");
							}
							else if(psVarType->Columns == 3)
							{
								//.x(GLSL) is .y(HLSL). .y(GLSL) is .z(HLSL) .z(GLSL) is .w(HLSL)
								bcatcstr(glsl, ".xxyz");
							}
							break;
						}
						case 8:
						{
							if(psVarType->Columns == 2)
							{
								//.x(GLSL) is .z(HLSL). .y(GLSL) is .w(HLSL)
								bcatcstr(glsl, ".xxxy");
							}
							break;
						}
						case 0:
						default:
						{
							//No rebase, but extend to vec4 if needed
							uint32_t maxComp = psOperand->GetMaxComponent();
							if(psVarType->Columns == 2 && maxComp > 2)
							{
								bcatcstr(glsl, ".xyxx");
							}
							else if(psVarType->Columns == 3 && maxComp > 3)
							{
								bcatcstr(glsl, ".xyzx");
							}
							break;
						}

					}
				}

				if(psVarType && psVarType->Class == SVC_SCALAR)
				{
					*pui32IgnoreSwizzle = 1;
				}
			}
			break;
		}
		case OPERAND_TYPE_RESOURCE:
		{
			ResourceName(glsl, psContext, RGROUP_TEXTURE, psOperand->ui32RegisterNumber, 0);
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_SAMPLER:
		{
			bformata(glsl, "Sampler%d", psOperand->ui32RegisterNumber);
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_FUNCTION_BODY:
		{
			const uint32_t ui32FuncBody = psOperand->ui32RegisterNumber;
			const uint32_t ui32FuncTable = psContext->psShader->aui32FuncBodyToFuncTable[ui32FuncBody];
			//const uint32_t ui32FuncPointer = psContext->psShader->aui32FuncTableToFuncPointer[ui32FuncTable];
			const uint32_t ui32ClassType = psContext->psShader->sInfo.aui32TableIDToTypeID[ui32FuncTable];
			const char* ClassTypeName = &psContext->psShader->sInfo.psClassTypes[ui32ClassType].name[0];
			const uint32_t ui32UniqueClassFuncIndex = psContext->psShader->ui32NextClassFuncName[ui32ClassType]++;

			bformata(glsl, "%s_Func%d", ClassTypeName, ui32UniqueClassFuncIndex);
			break;
		}
		case OPERAND_TYPE_INPUT_FORK_INSTANCE_ID:
		case OPERAND_TYPE_INPUT_JOIN_INSTANCE_ID:
		{
			bcatcstr(glsl, "phaseInstanceID"); // Not a real builtin, but passed as a function parameter.
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_IMMEDIATE_CONSTANT_BUFFER:
		{
			bformata(glsl, "ImmCB_%d_%d_%d", psContext->currentPhase, psOperand->ui32RegisterNumber, psOperand->m_Rebase);
			if(psOperand->m_SubOperands[0].get())
			{
				bcatcstr(glsl, "["); //Indexes must be integral. Offset is already taken care of above.
				TranslateOperand(psOperand->m_SubOperands[0].get(), TO_FLAG_INTEGER);
				bcatcstr(glsl, "]");
			}
			if (psOperand->m_Size == 1)
				*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_INPUT_DOMAIN_POINT:
		{
			bcatcstr(glsl, "gl_TessCoord");
			break;
		}
		case OPERAND_TYPE_INPUT_CONTROL_POINT:
		{
			const ShaderInfo::InOutSignature *psSig = NULL;
			psContext->psShader->sInfo.GetInputSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->ui32CompMask, &psSig);

			if ((psSig->eSystemValueType == NAME_POSITION && psSig->ui32SemanticIndex == 0) ||
				(psSig->semanticName == "POS" && psSig->ui32SemanticIndex == 0) ||
				(psSig->semanticName == "SV_POSITION" && psSig->ui32SemanticIndex == 0))
			{
				bcatcstr(glsl, "gl_in");
				TranslateOperandIndex(psOperand, 0);//Vertex index
				bcatcstr(glsl, ".gl_Position");
			}
			else
			{
				std::string name = psContext->GetDeclaredInputName(psOperand, piRebase, 0, pui32IgnoreSwizzle);

				bformata(glsl, "%s", name.c_str());
				TranslateOperandIndex(psOperand, 0);//Vertex index

				// Check for scalar
				if ((psContext->psShader->abScalarInput[psOperand->GetRegisterSpace(psContext)][psOperand->ui32RegisterNumber] & psOperand->GetAccessMask()) != 0)
					*pui32IgnoreSwizzle = 1;
			}
			break;
		}
		case OPERAND_TYPE_NULL:
		{
			// Null register, used to discard results of operations
			bcatcstr(glsl, "//null");
			break;
		}
		case OPERAND_TYPE_OUTPUT_CONTROL_POINT_ID:
		{
			bcatcstr(glsl, "gl_InvocationID");
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_OUTPUT_COVERAGE_MASK:
		{
			bcatcstr(glsl, "gl_SampleMask[0]");
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_INPUT_COVERAGE_MASK:
		{
			bcatcstr(glsl, "gl_SampleMaskIn[0]");
			//Skip swizzle on scalar types.
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_INPUT_THREAD_ID://SV_DispatchThreadID
		{
			bcatcstr(glsl, "gl_GlobalInvocationID");
			break;
		}
		case OPERAND_TYPE_INPUT_THREAD_ID_IN_GROUP://SV_GroupThreadID
		{
			bcatcstr(glsl, "gl_LocalInvocationID");
			break;
		}
		case OPERAND_TYPE_INPUT_THREAD_GROUP_ID://SV_GroupID
		{
			bcatcstr(glsl, "gl_WorkGroupID");
			break;
		}
		case OPERAND_TYPE_INPUT_THREAD_ID_IN_GROUP_FLATTENED://SV_GroupIndex
		{
			bcatcstr(glsl, "gl_LocalInvocationIndex");
			*pui32IgnoreSwizzle = 1; // No swizzle meaningful for scalar.
			break;
		}
		case OPERAND_TYPE_UNORDERED_ACCESS_VIEW:
		{
			ResourceName(glsl, psContext, RGROUP_UAV, psOperand->ui32RegisterNumber, 0);
			break;
		}
		case OPERAND_TYPE_THREAD_GROUP_SHARED_MEMORY:
		{
			bformata(glsl, "TGSM%d", psOperand->ui32RegisterNumber);
			*pui32IgnoreSwizzle = 1;
			break;
		}
		case OPERAND_TYPE_INPUT_PRIMITIVEID:
		{
			if(psContext->psShader->eShaderType == GEOMETRY_SHADER)
				bcatcstr(glsl, "gl_PrimitiveIDIn"); // LOL OpenGL
			else
				bcatcstr(glsl, "gl_PrimitiveID");

			break;
		}
		case OPERAND_TYPE_INDEXABLE_TEMP:
		{
			bformata(glsl, "TempArray%d", psOperand->aui32ArraySizes[0]);
			bcatcstr(glsl, "[");
			if (psOperand->aui32ArraySizes[1] != 0 || !psOperand->m_SubOperands[1].get())
				bformata(glsl, "%d", psOperand->aui32ArraySizes[1]);

			if(psOperand->m_SubOperands[1].get())
			{
				if (psOperand->aui32ArraySizes[1] != 0)
					bcatcstr(glsl, "+");
				TranslateOperand(psOperand->m_SubOperands[1].get(), TO_FLAG_INTEGER);

			}
			bcatcstr(glsl, "]");
			break;
		}
		case OPERAND_TYPE_STREAM:
		{
			bformata(glsl, "%d", psOperand->ui32RegisterNumber);
			break;
		}
		case OPERAND_TYPE_INPUT_GS_INSTANCE_ID:
		{
			// In HLSL the instance id is uint, so cast here.
			bcatcstr(glsl, "uint(gl_InvocationID)");
			break;
		}
		case OPERAND_TYPE_THIS_POINTER:
		{
			/*
				The "this" register is a register that provides up to 4 pieces of information:
				X: Which CB holds the instance data
				Y: Base element offset of the instance data within the instance CB
				Z: Base sampler index
				W: Base Texture index

				Can be different for each function call
			*/
			break;
		}
		case OPERAND_TYPE_INPUT_PATCH_CONSTANT:
		{
			const ShaderInfo::InOutSignature* psIn;
			psContext->psShader->sInfo.GetPatchConstantSignatureFromRegister(psOperand->ui32RegisterNumber, psOperand->GetAccessMask(), &psIn);
			*piRebase = psIn->iRebase;
			switch (psIn->eSystemValueType)
			{
			case NAME_POSITION:
				bcatcstr(glsl, "gl_Position");
				break;
			case NAME_RENDER_TARGET_ARRAY_INDEX:
				bcatcstr(glsl, "gl_Layer");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_CLIP_DISTANCE:
				bcatcstr(glsl, "gl_ClipDistance");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_VIEWPORT_ARRAY_INDEX:
				bcatcstr(glsl, "gl_ViewportIndex");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_VERTEX_ID:
				if ((psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS) != 0)
					bcatcstr(glsl, "gl_VertexIndex");
				else
					bcatcstr(glsl, "gl_VertexID");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_INSTANCE_ID:
				if ((psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS) != 0)
					bcatcstr(glsl, "gl_InstanceIndex");
				else
					bcatcstr(glsl, "gl_InstanceID");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_IS_FRONT_FACE:
				bcatcstr(glsl, "(gl_FrontFacing ? 0xffffffffu : uint(0))");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_PRIMITIVE_ID:
				bcatcstr(glsl, "gl_PrimitiveID");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_FINAL_QUAD_U_EQ_0_EDGE_TESSFACTOR:
			case NAME_FINAL_TRI_U_EQ_0_EDGE_TESSFACTOR:
			case NAME_FINAL_LINE_DENSITY_TESSFACTOR:
				if (psContext->psShader->aIndexedOutput[1][psOperand->ui32RegisterNumber])
					bcatcstr(glsl, "gl_TessLevelOuter");
				else
					bcatcstr(glsl, "gl_TessLevelOuter[0]");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_FINAL_QUAD_V_EQ_0_EDGE_TESSFACTOR:
			case NAME_FINAL_TRI_V_EQ_0_EDGE_TESSFACTOR:
			case NAME_FINAL_LINE_DETAIL_TESSFACTOR:
				bcatcstr(glsl, "gl_TessLevelOuter[1]");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_FINAL_QUAD_U_EQ_1_EDGE_TESSFACTOR:
			case NAME_FINAL_TRI_W_EQ_0_EDGE_TESSFACTOR:
				bcatcstr(glsl, "gl_TessLevelOuter[2]");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_FINAL_QUAD_V_EQ_1_EDGE_TESSFACTOR:
				bcatcstr(glsl, "gl_TessLevelOuter[3]");
				*pui32IgnoreSwizzle = 1;
				break;

			case NAME_FINAL_TRI_INSIDE_TESSFACTOR:
			case NAME_FINAL_QUAD_U_INSIDE_TESSFACTOR:
				if (psContext->psShader->aIndexedOutput[1][psOperand->ui32RegisterNumber])
					bcatcstr(glsl, "gl_TessLevelInner");
				else
					bcatcstr(glsl, "gl_TessLevelInner[0]");
				*pui32IgnoreSwizzle = 1;
				break;
			case NAME_FINAL_QUAD_V_INSIDE_TESSFACTOR:
				bcatcstr(glsl, "gl_TessLevelInner[1]");
				*pui32IgnoreSwizzle = 1;
				break;
			default:
				bformata(glsl, "%spatch%s%d", psContext->psShader->eShaderType == HULL_SHADER ? psContext->outputPrefix : psContext->inputPrefix, psIn->semanticName.c_str(), psIn->ui32SemanticIndex);
				// Disable swizzles if this is a scalar
				if (psContext->psShader->eShaderType == HULL_SHADER)
				{
					if ((psContext->psShader->abScalarOutput[1][psOperand->ui32RegisterNumber] & psOperand->GetAccessMask()) != 0)
						*pui32IgnoreSwizzle = 1;
				}
				else
				{
					if ((psContext->psShader->abScalarInput[1][psOperand->ui32RegisterNumber] & psOperand->GetAccessMask()) != 0)
						*pui32IgnoreSwizzle = 1;
				}

				break;
			}


			break;
		}
		default:
		{
			ASSERT(0);
			break;
		}
	}

	if (hasCtor && (*pui32IgnoreSwizzle == 0))
	{
		TranslateOperandSwizzleWithMask(psContext, psOperand, ui32CompMask, piRebase ? *piRebase : 0);
		*pui32IgnoreSwizzle = 1;
	}

	if (needsBoolUpscale)
	{
		if (requestedType == SVT_UINT || requestedType == SVT_UINT16 || requestedType == SVT_UINT8)
			bcatcstr(glsl, ") * 0xffffffffu");
		else
			bcatcstr(glsl, ") * int(0xffffffffu)");
		numParenthesis--;
	}

	while (numParenthesis != 0)
	{
		bcatcstr(glsl, ")");
		numParenthesis--;
	}
}

void ToGLSL::TranslateOperand(const Operand* psOperand, uint32_t ui32TOFlag, uint32_t ui32ComponentMask)
{
	bstring glsl = *psContext->currentGLSLString;
	uint32_t ui32IgnoreSwizzle = 0;
	int iRebase = 0;

	// in single-component mode there is no need to use mask
	if (psOperand->eSelMode == OPERAND_4_COMPONENT_SELECT_1_MODE)
		ui32ComponentMask = OPERAND_4_COMPONENT_MASK_ALL;

	if(psContext->psShader->ui32MajorVersion <=3)
	{
		ui32TOFlag &= ~(TO_AUTO_BITCAST_TO_FLOAT|TO_AUTO_BITCAST_TO_INT|TO_AUTO_BITCAST_TO_UINT);
	}

	if(ui32TOFlag & TO_FLAG_NAME_ONLY)
	{
		TranslateVariableNameWithMask(psOperand, ui32TOFlag, &ui32IgnoreSwizzle, OPERAND_4_COMPONENT_MASK_ALL, &iRebase);
		return;
	}

	switch(psOperand->eModifier)
	{
		case OPERAND_MODIFIER_NONE:
		{
			break;
		}
		case OPERAND_MODIFIER_NEG:
		{
			bcatcstr(glsl, "(-");
			break;
		}
		case OPERAND_MODIFIER_ABS:
		{
			bcatcstr(glsl, "abs(");
			break;
		}
		case OPERAND_MODIFIER_ABSNEG:
		{
			bcatcstr(glsl, "-abs(");
			break;
		}
	}

	TranslateVariableNameWithMask(psOperand, ui32TOFlag, &ui32IgnoreSwizzle, ui32ComponentMask, &iRebase);

	if(psContext->psShader->eShaderType == HULL_SHADER && psOperand->eType == OPERAND_TYPE_OUTPUT &&
		psOperand->ui32RegisterNumber != 0 && psOperand->iArrayElements != 0 && psOperand->eIndexRep[0] != OPERAND_INDEX_IMMEDIATE32_PLUS_RELATIVE
		&& psContext->psShader->asPhases[psContext->currentPhase].ePhase == HS_CTRL_POINT_PHASE)
	{
		bcatcstr(glsl, "[gl_InvocationID]");
	}

	if(!ui32IgnoreSwizzle)
	{
		TranslateOperandSwizzleWithMask(psContext, psOperand, ui32ComponentMask, iRebase);
	}

	switch(psOperand->eModifier)
	{
		case OPERAND_MODIFIER_NONE:
		{
			break;
		}
		case OPERAND_MODIFIER_NEG:
		{
			bcatcstr(glsl, ")");
			break;
		}
		case OPERAND_MODIFIER_ABS:
		{
			bcatcstr(glsl, ")");
			break;
		}
		case OPERAND_MODIFIER_ABSNEG:
		{
			bcatcstr(glsl, ")");
			break;
		}
	}

}

std::string ResourceName(HLSLCrossCompilerContext* psContext, ResourceGroup group, const uint32_t ui32RegisterNumber, const int bZCompare)
{
	std::ostringstream oss;
	const ResourceBinding* psBinding = 0;
	int found;

	found = psContext->psShader->sInfo.GetResourceFromBindingPoint(group, ui32RegisterNumber, &psBinding);

	if(bZCompare)
	{
		oss << "hlslcc_zcmp";
	}

	if(found)
	{
		int i = 0;
		std::string name = psBinding->name;
		uint32_t ui32ArrayOffset = ui32RegisterNumber - psBinding->ui32BindPoint;

		while(i < name.length())
		{
			//array syntax [X] becomes _0_
			//Otherwise declarations could end up as:
			//uniform sampler2D SomeTextures[0];
			//uniform sampler2D SomeTextures[1];
			if(name[i] == '[' || name[i] == ']')
				name[i] = '_';

			++i;
		}

		if(ui32ArrayOffset)
		{
			oss << name << ui32ArrayOffset;
		}
		else
		{
			oss << name;
		}
		if (((psContext->flags & HLSLCC_FLAG_VULKAN_BINDINGS) != 0) && group == RGROUP_UAV)
			oss << "_origX" << ui32RegisterNumber << "X";
	}
	else
	{
		oss << "UnknownResource" << ui32RegisterNumber;
	}
	return oss.str();
}
void ResourceName(bstring targetStr, HLSLCrossCompilerContext* psContext, ResourceGroup group, const uint32_t ui32RegisterNumber, const int bZCompare)
{
	bstring glsl = (targetStr == NULL) ? *psContext->currentGLSLString : targetStr;
	std::string res = ResourceName(psContext, group, ui32RegisterNumber, bZCompare);
	bcatcstr(glsl, res.c_str());
}

std::string TextureSamplerName(ShaderInfo* psShaderInfo, const uint32_t ui32TextureRegisterNumber, const uint32_t ui32SamplerRegisterNumber, const int bZCompare)
{
	std::ostringstream oss;
	const ResourceBinding* psTextureBinding = 0;
	const ResourceBinding* psSamplerBinding = 0;
	int foundTexture, foundSampler;
	uint32_t i = 0;
	uint32_t ui32ArrayOffset;

	foundTexture = psShaderInfo->GetResourceFromBindingPoint(RGROUP_TEXTURE, ui32TextureRegisterNumber, &psTextureBinding);
	foundSampler = psShaderInfo->GetResourceFromBindingPoint(RGROUP_SAMPLER, ui32SamplerRegisterNumber, &psSamplerBinding);

	if (!foundTexture || !foundSampler)
	{
		oss << "UnknownResource" << ui32TextureRegisterNumber << "_" << ui32SamplerRegisterNumber;
		return oss.str();
	}

	ui32ArrayOffset = ui32TextureRegisterNumber - psTextureBinding->ui32BindPoint;

	std::string texName = psTextureBinding->name;

	while (i < texName.length())
	{
		//array syntax [X] becomes _0_
		//Otherwise declarations could end up as:
		//uniform sampler2D SomeTextures[0];
		//uniform sampler2D SomeTextures[1];
		if(texName[i] == '[' || texName[i] == ']')
		{
			texName[i] = '_';
		}

		++i;
	}


	if(bZCompare)
	{
		oss << "hlslcc_zcmp";
	}


	if(ui32ArrayOffset)
	{
		oss << texName << ui32ArrayOffset << "_X_" << psSamplerBinding->name;
	}
	else
	{
		if((i>0) && (texName[i-1] == '_'))//Prevent double underscore which is reserved
		{
			oss << texName << "X_" << psSamplerBinding->name;
		}
		else
		{
			oss << texName << "_X_" << psSamplerBinding->name;
		}
	}

	return oss.str();
}

void ConcatTextureSamplerName(bstring str, ShaderInfo* psShaderInfo, const uint32_t ui32TextureRegisterNumber, const uint32_t ui32SamplerRegisterNumber, const int bZCompare)
{
	std::string texturesamplername = TextureSamplerName(psShaderInfo, ui32TextureRegisterNumber, ui32SamplerRegisterNumber, bZCompare);
	bcatcstr(str, texturesamplername.c_str());
}
