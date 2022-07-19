// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// FluidConsistencyType :  Codes used to represent the consistency of fluids and liquids provided to the patient. This value set includes all the [SNOMED CT](http://snomed.info/sct)(US Extension) where concept is a 435681000124103  (Dietary liquid consistency diet (regime/therapy)). It is provided as a suggestive example.
  /// </summary>
  public static class ConsistencyTypeCodes
  {
    /// <summary>
    /// nectar thick liquid
    /// </summary>
    public static readonly Coding NectarThickLiquid = new Coding
    {
      Code = "439021000124105",
      Display = "nectar thick liquid",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// honey thick liquid
    /// </summary>
    public static readonly Coding HoneyThickLiquid = new Coding
    {
      Code = "439031000124108",
      Display = "honey thick liquid",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// spoon thick liquid
    /// </summary>
    public static readonly Coding SpoonThickLiquid = new Coding
    {
      Code = "439041000124103",
      Display = "spoon thick liquid",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// thin liquid
    /// </summary>
    public static readonly Coding ThinLiquid = new Coding
    {
      Code = "439081000124109",
      Display = "thin liquid",
      System = "http://snomed.info/sct"
    };

    /// <summary>
    /// Literal for code: NectarThickLiquid
    /// </summary>
    public const string LiteralNectarThickLiquid = "439021000124105";

    /// <summary>
    /// Literal for code: NONENectarThickLiquid
    /// </summary>
    public const string LiteralNONENectarThickLiquid = "http://snomed.info/sct#439021000124105";

    /// <summary>
    /// Literal for code: HoneyThickLiquid
    /// </summary>
    public const string LiteralHoneyThickLiquid = "439031000124108";

    /// <summary>
    /// Literal for code: NONEHoneyThickLiquid
    /// </summary>
    public const string LiteralNONEHoneyThickLiquid = "http://snomed.info/sct#439031000124108";

    /// <summary>
    /// Literal for code: SpoonThickLiquid
    /// </summary>
    public const string LiteralSpoonThickLiquid = "439041000124103";

    /// <summary>
    /// Literal for code: NONESpoonThickLiquid
    /// </summary>
    public const string LiteralNONESpoonThickLiquid = "http://snomed.info/sct#439041000124103";

    /// <summary>
    /// Literal for code: ThinLiquid
    /// </summary>
    public const string LiteralThinLiquid = "439081000124109";

    /// <summary>
    /// Literal for code: NONEThinLiquid
    /// </summary>
    public const string LiteralNONEThinLiquid = "http://snomed.info/sct#439081000124109";

    /// <summary>
    /// Dictionary for looking up ConsistencyType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "439021000124105", NectarThickLiquid }, 
      { "http://snomed.info/sct#439021000124105", NectarThickLiquid }, 
      { "439031000124108", HoneyThickLiquid }, 
      { "http://snomed.info/sct#439031000124108", HoneyThickLiquid }, 
      { "439041000124103", SpoonThickLiquid }, 
      { "http://snomed.info/sct#439041000124103", SpoonThickLiquid }, 
      { "439081000124109", ThinLiquid }, 
      { "http://snomed.info/sct#439081000124109", ThinLiquid }, 
    };
  };
}
