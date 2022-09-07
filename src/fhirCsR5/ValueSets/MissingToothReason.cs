// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes sample Missing Tooth Reason codes.
  /// </summary>
  public static class MissingToothReasonCodes
  {
    /// <summary>
    /// C
    /// </summary>
    public static readonly Coding C = new Coding
    {
      Code = "c",
      Display = "C",
      System = "http://terminology.hl7.org/CodeSystem/missingtoothreason"
    };
    /// <summary>
    /// E
    /// </summary>
    public static readonly Coding E = new Coding
    {
      Code = "e",
      Display = "E",
      System = "http://terminology.hl7.org/CodeSystem/missingtoothreason"
    };
    /// <summary>
    /// O
    /// </summary>
    public static readonly Coding O = new Coding
    {
      Code = "o",
      Display = "O",
      System = "http://terminology.hl7.org/CodeSystem/missingtoothreason"
    };
    /// <summary>
    /// U
    /// </summary>
    public static readonly Coding U = new Coding
    {
      Code = "u",
      Display = "U",
      System = "http://terminology.hl7.org/CodeSystem/missingtoothreason"
    };

    /// <summary>
    /// Literal for code: C
    /// </summary>
    public const string LiteralC = "c";

    /// <summary>
    /// Literal for code: MissingtoothreasonC
    /// </summary>
    public const string LiteralMissingtoothreasonC = "http://terminology.hl7.org/CodeSystem/missingtoothreason#c";

    /// <summary>
    /// Literal for code: E
    /// </summary>
    public const string LiteralE = "e";

    /// <summary>
    /// Literal for code: MissingtoothreasonE
    /// </summary>
    public const string LiteralMissingtoothreasonE = "http://terminology.hl7.org/CodeSystem/missingtoothreason#e";

    /// <summary>
    /// Literal for code: O
    /// </summary>
    public const string LiteralO = "o";

    /// <summary>
    /// Literal for code: MissingtoothreasonO
    /// </summary>
    public const string LiteralMissingtoothreasonO = "http://terminology.hl7.org/CodeSystem/missingtoothreason#o";

    /// <summary>
    /// Literal for code: U
    /// </summary>
    public const string LiteralU = "u";

    /// <summary>
    /// Literal for code: MissingtoothreasonU
    /// </summary>
    public const string LiteralMissingtoothreasonU = "http://terminology.hl7.org/CodeSystem/missingtoothreason#u";

    /// <summary>
    /// Dictionary for looking up MissingToothReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "c", C }, 
      { "http://terminology.hl7.org/CodeSystem/missingtoothreason#c", C }, 
      { "e", E }, 
      { "http://terminology.hl7.org/CodeSystem/missingtoothreason#e", E }, 
      { "o", O }, 
      { "http://terminology.hl7.org/CodeSystem/missingtoothreason#o", O }, 
      { "u", U }, 
      { "http://terminology.hl7.org/CodeSystem/missingtoothreason#u", U }, 
    };
  };
}
