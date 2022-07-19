// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The presentation types of notes.
  /// </summary>
  public static class NoteTypeCodes
  {
    /// <summary>
    /// Display the note.
    /// </summary>
    public static readonly Coding Display = new Coding
    {
      Code = "display",
      Display = "Display",
      System = "http://hl7.org/fhir/note-type"
    };
    /// <summary>
    /// Print the note on the form.
    /// </summary>
    public static readonly Coding PrintForm = new Coding
    {
      Code = "print",
      Display = "Print (Form)",
      System = "http://hl7.org/fhir/note-type"
    };
    /// <summary>
    /// Print the note for the operator.
    /// </summary>
    public static readonly Coding PrintOperator = new Coding
    {
      Code = "printoper",
      Display = "Print (Operator)",
      System = "http://hl7.org/fhir/note-type"
    };

    /// <summary>
    /// Literal for code: Display
    /// </summary>
    public const string LiteralDisplay = "display";

    /// <summary>
    /// Literal for code: NoteTypeDisplay
    /// </summary>
    public const string LiteralNoteTypeDisplay = "http://hl7.org/fhir/note-type#display";

    /// <summary>
    /// Literal for code: PrintForm
    /// </summary>
    public const string LiteralPrintForm = "print";

    /// <summary>
    /// Literal for code: NoteTypePrintForm
    /// </summary>
    public const string LiteralNoteTypePrintForm = "http://hl7.org/fhir/note-type#print";

    /// <summary>
    /// Literal for code: PrintOperator
    /// </summary>
    public const string LiteralPrintOperator = "printoper";

    /// <summary>
    /// Literal for code: NoteTypePrintOperator
    /// </summary>
    public const string LiteralNoteTypePrintOperator = "http://hl7.org/fhir/note-type#printoper";

    /// <summary>
    /// Dictionary for looking up NoteType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "display", Display }, 
      { "http://hl7.org/fhir/note-type#display", Display }, 
      { "print", PrintForm }, 
      { "http://hl7.org/fhir/note-type#print", PrintForm }, 
      { "printoper", PrintOperator }, 
      { "http://hl7.org/fhir/note-type#printoper", PrintOperator }, 
    };
  };
}
