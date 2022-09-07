// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// EnteralFormulaAdditiveType: Codes for the type of modular component such as protein, carbohydrate or fiber to be provided in addition to or mixed with the base formula. This value set is provided as a suggestive example.
  /// </summary>
  public static class EntformulaAdditiveCodes
  {
    /// <summary>
    /// Carbohydrate
    /// </summary>
    public static readonly Coding Carbohydrate = new Coding
    {
      Code = "carbohydrate",
      Display = "Carbohydrate",
      System = "http://terminology.hl7.org/CodeSystem/entformula-additive"
    };
    /// <summary>
    /// Fiber
    /// </summary>
    public static readonly Coding Fiber = new Coding
    {
      Code = "fiber",
      Display = "Fiber",
      System = "http://terminology.hl7.org/CodeSystem/entformula-additive"
    };
    /// <summary>
    /// Lipid
    /// </summary>
    public static readonly Coding Lipid = new Coding
    {
      Code = "lipid",
      Display = "Lipid",
      System = "http://terminology.hl7.org/CodeSystem/entformula-additive"
    };
    /// <summary>
    /// Protein
    /// </summary>
    public static readonly Coding Protein = new Coding
    {
      Code = "protein",
      Display = "Protein",
      System = "http://terminology.hl7.org/CodeSystem/entformula-additive"
    };
    /// <summary>
    /// Water
    /// </summary>
    public static readonly Coding Water = new Coding
    {
      Code = "water",
      Display = "Water",
      System = "http://terminology.hl7.org/CodeSystem/entformula-additive"
    };

    /// <summary>
    /// Literal for code: Carbohydrate
    /// </summary>
    public const string LiteralCarbohydrate = "carbohydrate";

    /// <summary>
    /// Literal for code: EntformulaAdditiveCarbohydrate
    /// </summary>
    public const string LiteralEntformulaAdditiveCarbohydrate = "http://terminology.hl7.org/CodeSystem/entformula-additive#carbohydrate";

    /// <summary>
    /// Literal for code: Fiber
    /// </summary>
    public const string LiteralFiber = "fiber";

    /// <summary>
    /// Literal for code: EntformulaAdditiveFiber
    /// </summary>
    public const string LiteralEntformulaAdditiveFiber = "http://terminology.hl7.org/CodeSystem/entformula-additive#fiber";

    /// <summary>
    /// Literal for code: Lipid
    /// </summary>
    public const string LiteralLipid = "lipid";

    /// <summary>
    /// Literal for code: EntformulaAdditiveLipid
    /// </summary>
    public const string LiteralEntformulaAdditiveLipid = "http://terminology.hl7.org/CodeSystem/entformula-additive#lipid";

    /// <summary>
    /// Literal for code: Protein
    /// </summary>
    public const string LiteralProtein = "protein";

    /// <summary>
    /// Literal for code: EntformulaAdditiveProtein
    /// </summary>
    public const string LiteralEntformulaAdditiveProtein = "http://terminology.hl7.org/CodeSystem/entformula-additive#protein";

    /// <summary>
    /// Literal for code: Water
    /// </summary>
    public const string LiteralWater = "water";

    /// <summary>
    /// Literal for code: EntformulaAdditiveWater
    /// </summary>
    public const string LiteralEntformulaAdditiveWater = "http://terminology.hl7.org/CodeSystem/entformula-additive#water";

    /// <summary>
    /// Dictionary for looking up EntformulaAdditive Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "carbohydrate", Carbohydrate }, 
      { "http://terminology.hl7.org/CodeSystem/entformula-additive#carbohydrate", Carbohydrate }, 
      { "fiber", Fiber }, 
      { "http://terminology.hl7.org/CodeSystem/entformula-additive#fiber", Fiber }, 
      { "lipid", Lipid }, 
      { "http://terminology.hl7.org/CodeSystem/entformula-additive#lipid", Lipid }, 
      { "protein", Protein }, 
      { "http://terminology.hl7.org/CodeSystem/entformula-additive#protein", Protein }, 
      { "water", Water }, 
      { "http://terminology.hl7.org/CodeSystem/entformula-additive#water", Water }, 
    };
  };
}
