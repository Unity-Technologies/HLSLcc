#pragma once

#include "hlslcc.h"
#include "internal_includes/Translator.h"

class HLSLCrossCompilerContext;

class ToGLSL : public Translator
{
protected:
    GLLang language;
    bool m_NeedUnityInstancingArraySizeDecl;

public:
    explicit ToGLSL(HLSLCrossCompilerContext *ctx) : Translator(ctx), language(LANG_DEFAULT), m_NeedUnityInstancingArraySizeDecl(false), m_NumDeclaredWhileTrueLoops(0) {}
    // Sets the target language according to given input. if LANG_DEFAULT, does autodetect and returns the selected language
    GLLang SetLanguage(GLLang suggestedLanguage);

    virtual bool Translate();
    virtual void TranslateDeclaration(const Declaration* psDecl);
    virtual bool TranslateSystemValue(const Operand *psOperand, const ShaderInfo::InOutSignature *sig, std::string &result, uint32_t *pui32IgnoreSwizzle, bool isIndexed, bool isInput, bool *outSkipPrefix = NULL, int *iIgnoreRedirect = NULL);
    virtual void SetIOPrefixes();

private:
    // Vulkan-only: detect which branches only depend on uniforms and immediate values and can be turned into specialization constants.
    void IdentifyStaticBranches(ShaderPhase *psPhase);
    // May return false when we detect too complex stuff (matrices, arrays etc)
    bool BuildStaticBranchNameForInstruction(Instruction &inst);

    void DeclareSpecializationConstants(ShaderPhase &phase);

    void TranslateOperand(bstring glsl, const Operand *psOp, uint32_t flags, uint32_t ui32ComponentMask = OPERAND_4_COMPONENT_MASK_ALL);
    void TranslateOperand(const Operand *psOp, uint32_t flags, uint32_t ui32ComponentMask = OPERAND_4_COMPONENT_MASK_ALL);
    void TranslateInstruction(Instruction* psInst, bool isEmbedded = false);

    void TranslateVariableNameWithMask(bstring glsl, const Operand* psOperand, uint32_t ui32TOFlag, uint32_t* pui32IgnoreSwizzle, uint32_t ui32CompMask, int *piRebase);
    void TranslateVariableNameWithMask(const Operand* psOperand, uint32_t ui32TOFlag, uint32_t* pui32IgnoreSwizzle, uint32_t ui32CompMask, int *piRebase);

    void TranslateOperandIndex(const Operand* psOperand, int index);
    void TranslateOperandIndexMAD(const Operand* psOperand, int index, uint32_t multiply, uint32_t add);

    void AddOpAssignToDestWithMask(const Operand* psDest,
        SHADER_VARIABLE_TYPE eSrcType, uint32_t ui32SrcElementCount, const char *szAssignmentOp, int *pNeedsParenthesis, uint32_t ui32CompMask);
    void AddAssignToDest(const Operand* psDest,
        SHADER_VARIABLE_TYPE eSrcType, uint32_t ui32SrcElementCount, int* pNeedsParenthesis);
    void AddAssignPrologue(int numParenthesis, bool isEmbedded = false);


    void AddBuiltinOutput(const Declaration* psDecl, int arrayElements, const char* builtinName);
    void AddBuiltinInput(const Declaration* psDecl, const char* builtinName);
    void HandleOutputRedirect(const Declaration *psDecl, const char *Precision);
    void HandleInputRedirect(const Declaration *psDecl, const char *Precision);

    void AddUserOutput(const Declaration* psDecl);
    void DeclareStructConstants(const uint32_t ui32BindingPoint, const ConstantBuffer* psCBuf, const Operand* psOperand, bstring glsl);
    void DeclareConstBufferShaderVariable(const char* varName, const struct ShaderVarType* psType, const struct ConstantBuffer* psCBuf, int unsizedArray, bool addUniformPrefix = false);
    void PreDeclareStructType(const std::string &name, const struct ShaderVarType* psType);
    void DeclareUBOConstants(const uint32_t ui32BindingPoint, const ConstantBuffer* psCBuf, bstring glsl);

    typedef enum
    {
        CMP_EQ,
        CMP_LT,
        CMP_GE,
        CMP_NE,
    } ComparisonType;

    void AddComparison(Instruction* psInst, ComparisonType eType,
        uint32_t typeFlag);

    void AddMOVBinaryOp(const Operand *pDest, Operand *pSrc, bool isEmbedded = false);
    void AddMOVCBinaryOp(const Operand *pDest, const Operand *src0, Operand *src1, Operand *src2);
    void CallBinaryOp(const char* name, Instruction* psInst,
        int dest, int src0, int src1, SHADER_VARIABLE_TYPE eDataType, bool isEmbedded = false);
    void CallTernaryOp(const char* op1, const char* op2, Instruction* psInst,
        int dest, int src0, int src1, int src2, uint32_t dataType);
    void CallHelper3(const char* name, Instruction* psInst,
        int dest, int src0, int src1, int src2, int paramsShouldFollowWriteMask);
    void CallHelper2(const char* name, Instruction* psInst,
        int dest, int src0, int src1, int paramsShouldFollowWriteMask);
    void CallHelper2Int(const char* name, Instruction* psInst,
        int dest, int src0, int src1, int paramsShouldFollowWriteMask);
    void CallHelper2UInt(const char* name, Instruction* psInst,
        int dest, int src0, int src1, int paramsShouldFollowWriteMask);
    void CallHelper1(const char* name, Instruction* psInst,
        int dest, int src0, int paramsShouldFollowWriteMask);
    void CallHelper1Int(
        const char* name,
        Instruction* psInst,
        const int dest,
        const int src0,
        int paramsShouldFollowWriteMask);
    void TranslateTexelFetch(
        Instruction* psInst,
        const ResourceBinding* psBinding,
        bstring glsl);
    void TranslateTexCoord(
        const RESOURCE_DIMENSION eResDim,
        Operand* psTexCoordOperand);
    void GetResInfoData(Instruction* psInst, int index, int destElem);
    void TranslateTextureSample(Instruction* psInst,
        uint32_t ui32Flags);
    void TranslateDynamicComponentSelection(const ShaderVarType* psVarType,
        const Operand* psByteAddr, uint32_t offset, uint32_t mask);
    void TranslateShaderStorageStore(Instruction* psInst);
    void TranslateShaderStorageLoad(Instruction* psInst);
    void TranslateAtomicMemOp(Instruction* psInst);
    void TranslateConditional(
        Instruction* psInst,
        bstring glsl);

    // Add an extra function to the m_FunctionDefinitions list, unless it's already there.
    bool DeclareExtraFunction(const std::string &name, bstring body);
    void UseExtraFunctionDependency(const std::string &name);

    void DeclareDynamicIndexWrapper(const struct ShaderVarType* psType);
    void DeclareDynamicIndexWrapper(const char* psName, SHADER_VARIABLE_CLASS eClass, SHADER_VARIABLE_TYPE eType, uint32_t ui32Rows, uint32_t ui32Columns, uint32_t ui32Elements);

    bool RenderTargetDeclared(uint32_t input);

    std::string GetVulkanDummySamplerName();

    // A <function name, body text> map of extra helper functions we'll need.
    FunctionDefinitions m_FunctionDefinitions;
    std::vector<std::string> m_FunctionDefinitionsOrder;

    std::set<uint32_t> m_DeclaredRenderTarget;
    int m_NumDeclaredWhileTrueLoops;
};
