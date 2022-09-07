// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// An anatomical origin of the source material within an organism.
  /// </summary>
  public static class SubstanceSourceMaterialPartCodes
  {
    /// <summary>
    /// animal
    /// </summary>
    public static readonly Coding Animal = new Coding
    {
      Code = "Animal",
      Display = "animal",
      System = "http://hl7.org/fhir/substance-source-material-part"
    };
    /// <summary>
    /// mineral
    /// </summary>
    public static readonly Coding Mineral = new Coding
    {
      Code = "Mineral",
      Display = "mineral",
      System = "http://hl7.org/fhir/substance-source-material-part"
    };
    /// <summary>
    /// plant
    /// </summary>
    public static readonly Coding Plant = new Coding
    {
      Code = "Plant",
      Display = "plant",
      System = "http://hl7.org/fhir/substance-source-material-part"
    };

    /// <summary>
    /// Literal for code: Animal
    /// </summary>
    public const string LiteralAnimal = "Animal";

    /// <summary>
    /// Literal for code: SubstanceSourceMaterialPartAnimal
    /// </summary>
    public const string LiteralSubstanceSourceMaterialPartAnimal = "http://hl7.org/fhir/substance-source-material-part#Animal";

    /// <summary>
    /// Literal for code: Mineral
    /// </summary>
    public const string LiteralMineral = "Mineral";

    /// <summary>
    /// Literal for code: SubstanceSourceMaterialPartMineral
    /// </summary>
    public const string LiteralSubstanceSourceMaterialPartMineral = "http://hl7.org/fhir/substance-source-material-part#Mineral";

    /// <summary>
    /// Literal for code: Plant
    /// </summary>
    public const string LiteralPlant = "Plant";

    /// <summary>
    /// Literal for code: SubstanceSourceMaterialPartPlant
    /// </summary>
    public const string LiteralSubstanceSourceMaterialPartPlant = "http://hl7.org/fhir/substance-source-material-part#Plant";

    /// <summary>
    /// Dictionary for looking up SubstanceSourceMaterialPart Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "Animal", Animal }, 
      { "http://hl7.org/fhir/substance-source-material-part#Animal", Animal }, 
      { "Mineral", Mineral }, 
      { "http://hl7.org/fhir/substance-source-material-part#Mineral", Mineral }, 
      { "Plant", Plant }, 
      { "http://hl7.org/fhir/substance-source-material-part#Plant", Plant }, 
    };
  };
}
