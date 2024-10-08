//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     ANTLR Version: 4.13.1
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

// Generated from FmlMapping.g4 by ANTLR 4.13.1

// Unreachable code detected
#pragma warning disable 0162
// The variable '...' is assigned but its value is never used
#pragma warning disable 0219
// Missing XML comment for publicly visible type or member '...'
#pragma warning disable 1591
// Ambiguous reference in cref attribute
#pragma warning disable 419

// Disable the warning for CLSCompliant
#pragma warning disable 3021

using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using IToken = Antlr4.Runtime.IToken;

/// <summary>
/// This interface defines a complete generic visitor for a parse tree produced
/// by <see cref="FmlMappingParser"/>.
/// </summary>
/// <typeparam name="Result">The return type of the visit operation.</typeparam>
[System.CodeDom.Compiler.GeneratedCode("ANTLR", "4.13.1")]
[System.CLSCompliant(false)]
public interface IFmlMappingVisitor<Result> : IParseTreeVisitor<Result> {
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.structureMap"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitStructureMap([NotNull] FmlMappingParser.StructureMapContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.conceptMapDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConceptMapDeclaration([NotNull] FmlMappingParser.ConceptMapDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.conceptMapPrefix"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConceptMapPrefix([NotNull] FmlMappingParser.ConceptMapPrefixContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.conceptMapCodeMap"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConceptMapCodeMap([NotNull] FmlMappingParser.ConceptMapCodeMapContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.conceptMapSource"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConceptMapSource([NotNull] FmlMappingParser.ConceptMapSourceContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.conceptMapTarget"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConceptMapTarget([NotNull] FmlMappingParser.ConceptMapTargetContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.code"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitCode([NotNull] FmlMappingParser.CodeContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapDeclaration([NotNull] FmlMappingParser.MapDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.metadataDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMetadataDeclaration([NotNull] FmlMappingParser.MetadataDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.markdownLiteral"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMarkdownLiteral([NotNull] FmlMappingParser.MarkdownLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.url"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitUrl([NotNull] FmlMappingParser.UrlContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.identifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitIdentifier([NotNull] FmlMappingParser.IdentifierContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.structureDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitStructureDeclaration([NotNull] FmlMappingParser.StructureDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.constantDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConstantDeclaration([NotNull] FmlMappingParser.ConstantDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.groupDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitGroupDeclaration([NotNull] FmlMappingParser.GroupDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.parameters"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitParameters([NotNull] FmlMappingParser.ParametersContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.parameter"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitParameter([NotNull] FmlMappingParser.ParameterContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.groupExpressions"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitGroupExpressions([NotNull] FmlMappingParser.GroupExpressionsContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.typeMode"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTypeMode([NotNull] FmlMappingParser.TypeModeContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.extends"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitExtends([NotNull] FmlMappingParser.ExtendsContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.typeIdentifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTypeIdentifier([NotNull] FmlMappingParser.TypeIdentifierContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>mapSimpleCopy</c>
	/// labeled alternative in <see cref="FmlMappingParser.expression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapSimpleCopy([NotNull] FmlMappingParser.MapSimpleCopyContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>mapFhirMarkup</c>
	/// labeled alternative in <see cref="FmlMappingParser.expression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapFhirMarkup([NotNull] FmlMappingParser.MapFhirMarkupContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapExpression([NotNull] FmlMappingParser.MapExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapExpressionName"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapExpressionName([NotNull] FmlMappingParser.MapExpressionNameContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapExpressionSource"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapExpressionSource([NotNull] FmlMappingParser.MapExpressionSourceContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapExpressionTarget"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapExpressionTarget([NotNull] FmlMappingParser.MapExpressionTargetContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.sourceCardinality"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitSourceCardinality([NotNull] FmlMappingParser.SourceCardinalityContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.upperBound"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitUpperBound([NotNull] FmlMappingParser.UpperBoundContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.qualifiedIdentifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQualifiedIdentifier([NotNull] FmlMappingParser.QualifiedIdentifierContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.sourceDefault"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitSourceDefault([NotNull] FmlMappingParser.SourceDefaultContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.alias"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitAlias([NotNull] FmlMappingParser.AliasContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.whereClause"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitWhereClause([NotNull] FmlMappingParser.WhereClauseContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.checkClause"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitCheckClause([NotNull] FmlMappingParser.CheckClauseContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.log"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitLog([NotNull] FmlMappingParser.LogContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.dependentExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitDependentExpression([NotNull] FmlMappingParser.DependentExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.importDeclaration"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitImportDeclaration([NotNull] FmlMappingParser.ImportDeclarationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.mapLineTarget"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMapLineTarget([NotNull] FmlMappingParser.MapLineTargetContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.transform"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTransform([NotNull] FmlMappingParser.TransformContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.invocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitInvocation([NotNull] FmlMappingParser.InvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.paramList"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitParamList([NotNull] FmlMappingParser.ParamListContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.param"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitParam([NotNull] FmlMappingParser.ParamContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>indexerExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitIndexerExpression([NotNull] FmlMappingParser.IndexerExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>polarityExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitPolarityExpression([NotNull] FmlMappingParser.PolarityExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>additiveExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitAdditiveExpression([NotNull] FmlMappingParser.AdditiveExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>multiplicativeExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMultiplicativeExpression([NotNull] FmlMappingParser.MultiplicativeExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>unionExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitUnionExpression([NotNull] FmlMappingParser.UnionExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>orExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitOrExpression([NotNull] FmlMappingParser.OrExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>andExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitAndExpression([NotNull] FmlMappingParser.AndExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>membershipExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMembershipExpression([NotNull] FmlMappingParser.MembershipExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>inequalityExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitInequalityExpression([NotNull] FmlMappingParser.InequalityExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>invocationExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitInvocationExpression([NotNull] FmlMappingParser.InvocationExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>equalityExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitEqualityExpression([NotNull] FmlMappingParser.EqualityExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>impliesExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitImpliesExpression([NotNull] FmlMappingParser.ImpliesExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>termExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTermExpression([NotNull] FmlMappingParser.TermExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>typeExpression</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpExpression"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTypeExpression([NotNull] FmlMappingParser.TypeExpressionContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>invocationTerm</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpTerm"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitInvocationTerm([NotNull] FmlMappingParser.InvocationTermContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>literalTerm</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpTerm"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitLiteralTerm([NotNull] FmlMappingParser.LiteralTermContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>externalConstantTerm</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpTerm"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitExternalConstantTerm([NotNull] FmlMappingParser.ExternalConstantTermContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>parenthesizedTerm</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpTerm"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitParenthesizedTerm([NotNull] FmlMappingParser.ParenthesizedTermContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>functionInvocation</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpInvocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitFunctionInvocation([NotNull] FmlMappingParser.FunctionInvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>memberInvocation</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpInvocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitMemberInvocation([NotNull] FmlMappingParser.MemberInvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>thisInvocation</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpInvocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitThisInvocation([NotNull] FmlMappingParser.ThisInvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>indexInvocation</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpInvocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitIndexInvocation([NotNull] FmlMappingParser.IndexInvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>totalInvocation</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpInvocation"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTotalInvocation([NotNull] FmlMappingParser.TotalInvocationContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.fpExternalConstant"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitFpExternalConstant([NotNull] FmlMappingParser.FpExternalConstantContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.fpFunction"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitFpFunction([NotNull] FmlMappingParser.FpFunctionContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.fpParamList"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitFpParamList([NotNull] FmlMappingParser.FpParamListContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.fpTypeSpecifier"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitFpTypeSpecifier([NotNull] FmlMappingParser.FpTypeSpecifierContext context);
	/// <summary>
	/// Visit a parse tree produced by <see cref="FmlMappingParser.constant"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitConstant([NotNull] FmlMappingParser.ConstantContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>nullLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitNullLiteral([NotNull] FmlMappingParser.NullLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>booleanLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitBooleanLiteral([NotNull] FmlMappingParser.BooleanLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>quantityLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQuantityLiteral([NotNull] FmlMappingParser.QuantityLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>longNumberLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitLongNumberLiteral([NotNull] FmlMappingParser.LongNumberLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>numberLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitNumberLiteral([NotNull] FmlMappingParser.NumberLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>dateLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitDateLiteral([NotNull] FmlMappingParser.DateLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>dateTimeLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitDateTimeLiteral([NotNull] FmlMappingParser.DateTimeLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>timeLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitTimeLiteral([NotNull] FmlMappingParser.TimeLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>stringLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitStringLiteral([NotNull] FmlMappingParser.StringLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>quotedStringLiteral</c>
	/// labeled alternative in <see cref="FmlMappingParser.literal"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQuotedStringLiteral([NotNull] FmlMappingParser.QuotedStringLiteralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>quantityWithDate</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpQuantity"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQuantityWithDate([NotNull] FmlMappingParser.QuantityWithDateContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>quantityWithDatePlural</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpQuantity"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQuantityWithDatePlural([NotNull] FmlMappingParser.QuantityWithDatePluralContext context);
	/// <summary>
	/// Visit a parse tree produced by the <c>quantityWithUcum</c>
	/// labeled alternative in <see cref="FmlMappingParser.fpQuantity"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	/// <return>The visitor result.</return>
	Result VisitQuantityWithUcum([NotNull] FmlMappingParser.QuantityWithUcumContext context);
}
