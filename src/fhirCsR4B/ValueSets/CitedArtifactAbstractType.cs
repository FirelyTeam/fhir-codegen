// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// Used to express the reason and specific aspect for the variant abstract, such as language and specific language.
  /// </summary>
  public static class CitedArtifactAbstractTypeCodes
  {
    /// <summary>
    /// Machine translated form of abstract in a different language, language element codes the language into which it was translated by machine
    /// </summary>
    public static readonly Coding DifferentLanguageDerivedFromAutotranslation = new Coding
    {
      Code = "autotranslated",
      Display = "Different language derived from autotranslation",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Abstract produced by a different publisher than the cited artifact
    /// </summary>
    public static readonly Coding DifferentPublisherForAbstract = new Coding
    {
      Code = "different-publisher",
      Display = "Different publisher for abstract",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Alternative form of abstract in two or more Medline entries
    /// </summary>
    public static readonly Coding DifferentTextInAdditionalMedlineEntry = new Coding
    {
      Code = "duplicate-pmid",
      Display = "Different text in additional Medline entry",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Alternative form of abstract in an earlier version such as epub ahead of print
    /// </summary>
    public static readonly Coding DifferentTextInAnEarlierVersion = new Coding
    {
      Code = "earlier-abstract",
      Display = "Different text in an earlier version",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Additional form of abstract in a different language
    /// </summary>
    public static readonly Coding DifferentLanguage = new Coding
    {
      Code = "language",
      Display = "Different language",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Long version of the abstract, for use when abstracts are provided in different sizes or lengths
    /// </summary>
    public static readonly Coding LongAbstract = new Coding
    {
      Code = "long-abstract",
      Display = "Long abstract",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Additional form of abstract written for the general public
    /// </summary>
    public static readonly Coding PlainLanguage = new Coding
    {
      Code = "plain-language",
      Display = "Plain language",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Human-friendly main or official abstract
    /// </summary>
    public static readonly Coding PrimaryHumanUse = new Coding
    {
      Code = "primary-human-use",
      Display = "Primary human use",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Machine-friendly main or official abstract
    /// </summary>
    public static readonly Coding PrimaryMachineUse = new Coding
    {
      Code = "primary-machine-use",
      Display = "Primary machine use",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Brief abstract, for use when abstracts are provided in different sizes or lengths
    /// </summary>
    public static readonly Coding ShortAbstract = new Coding
    {
      Code = "short-abstract",
      Display = "Short abstract",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };
    /// <summary>
    /// Truncated abstract
    /// </summary>
    public static readonly Coding Truncated = new Coding
    {
      Code = "truncated",
      Display = "Truncated",
      System = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type"
    };

    /// <summary>
    /// Literal for code: DifferentLanguageDerivedFromAutotranslation
    /// </summary>
    public const string LiteralDifferentLanguageDerivedFromAutotranslation = "autotranslated";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeDifferentLanguageDerivedFromAutotranslation
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeDifferentLanguageDerivedFromAutotranslation = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#autotranslated";

    /// <summary>
    /// Literal for code: DifferentPublisherForAbstract
    /// </summary>
    public const string LiteralDifferentPublisherForAbstract = "different-publisher";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeDifferentPublisherForAbstract
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeDifferentPublisherForAbstract = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#different-publisher";

    /// <summary>
    /// Literal for code: DifferentTextInAdditionalMedlineEntry
    /// </summary>
    public const string LiteralDifferentTextInAdditionalMedlineEntry = "duplicate-pmid";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeDifferentTextInAdditionalMedlineEntry
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeDifferentTextInAdditionalMedlineEntry = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#duplicate-pmid";

    /// <summary>
    /// Literal for code: DifferentTextInAnEarlierVersion
    /// </summary>
    public const string LiteralDifferentTextInAnEarlierVersion = "earlier-abstract";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeDifferentTextInAnEarlierVersion
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeDifferentTextInAnEarlierVersion = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#earlier-abstract";

    /// <summary>
    /// Literal for code: DifferentLanguage
    /// </summary>
    public const string LiteralDifferentLanguage = "language";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeDifferentLanguage
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeDifferentLanguage = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#language";

    /// <summary>
    /// Literal for code: LongAbstract
    /// </summary>
    public const string LiteralLongAbstract = "long-abstract";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeLongAbstract
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeLongAbstract = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#long-abstract";

    /// <summary>
    /// Literal for code: PlainLanguage
    /// </summary>
    public const string LiteralPlainLanguage = "plain-language";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypePlainLanguage
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypePlainLanguage = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#plain-language";

    /// <summary>
    /// Literal for code: PrimaryHumanUse
    /// </summary>
    public const string LiteralPrimaryHumanUse = "primary-human-use";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypePrimaryHumanUse
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypePrimaryHumanUse = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#primary-human-use";

    /// <summary>
    /// Literal for code: PrimaryMachineUse
    /// </summary>
    public const string LiteralPrimaryMachineUse = "primary-machine-use";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypePrimaryMachineUse
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypePrimaryMachineUse = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#primary-machine-use";

    /// <summary>
    /// Literal for code: ShortAbstract
    /// </summary>
    public const string LiteralShortAbstract = "short-abstract";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeShortAbstract
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeShortAbstract = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#short-abstract";

    /// <summary>
    /// Literal for code: Truncated
    /// </summary>
    public const string LiteralTruncated = "truncated";

    /// <summary>
    /// Literal for code: CitedArtifactAbstractTypeTruncated
    /// </summary>
    public const string LiteralCitedArtifactAbstractTypeTruncated = "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#truncated";

    /// <summary>
    /// Dictionary for looking up CitedArtifactAbstractType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "autotranslated", DifferentLanguageDerivedFromAutotranslation }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#autotranslated", DifferentLanguageDerivedFromAutotranslation }, 
      { "different-publisher", DifferentPublisherForAbstract }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#different-publisher", DifferentPublisherForAbstract }, 
      { "duplicate-pmid", DifferentTextInAdditionalMedlineEntry }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#duplicate-pmid", DifferentTextInAdditionalMedlineEntry }, 
      { "earlier-abstract", DifferentTextInAnEarlierVersion }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#earlier-abstract", DifferentTextInAnEarlierVersion }, 
      { "language", DifferentLanguage }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#language", DifferentLanguage }, 
      { "long-abstract", LongAbstract }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#long-abstract", LongAbstract }, 
      { "plain-language", PlainLanguage }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#plain-language", PlainLanguage }, 
      { "primary-human-use", PrimaryHumanUse }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#primary-human-use", PrimaryHumanUse }, 
      { "primary-machine-use", PrimaryMachineUse }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#primary-machine-use", PrimaryMachineUse }, 
      { "short-abstract", ShortAbstract }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#short-abstract", ShortAbstract }, 
      { "truncated", Truncated }, 
      { "http://terminology.hl7.org/CodeSystem/cited-artifact-abstract-type#truncated", Truncated }, 
    };
  };
}