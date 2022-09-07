// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes describing the reason why a family member's history is not available.
  /// </summary>
  public static class HistoryAbsentReasonCodes
  {
    /// <summary>
    /// Patient does not have the information now, but can provide the information at a later date.
    /// </summary>
    public static readonly Coding Deferred = new Coding
    {
      Code = "deferred",
      Display = "Deferred",
      System = "http://terminology.hl7.org/CodeSystem/history-absent-reason"
    };
    /// <summary>
    /// Patient does not know the subject, e.g. the biological parent of an adopted patient.
    /// </summary>
    public static readonly Coding SubjectUnknown = new Coding
    {
      Code = "subject-unknown",
      Display = "Subject Unknown",
      System = "http://terminology.hl7.org/CodeSystem/history-absent-reason"
    };
    /// <summary>
    /// Information cannot be obtained; e.g. unconscious patient.
    /// </summary>
    public static readonly Coding UnableToObtain = new Coding
    {
      Code = "unable-to-obtain",
      Display = "Unable To Obtain",
      System = "http://terminology.hl7.org/CodeSystem/history-absent-reason"
    };
    /// <summary>
    /// The patient withheld or refused to share the information.
    /// </summary>
    public static readonly Coding InformationWithheld = new Coding
    {
      Code = "withheld",
      Display = "Information Withheld",
      System = "http://terminology.hl7.org/CodeSystem/history-absent-reason"
    };

    /// <summary>
    /// Literal for code: Deferred
    /// </summary>
    public const string LiteralDeferred = "deferred";

    /// <summary>
    /// Literal for code: HistoryAbsentReasonDeferred
    /// </summary>
    public const string LiteralHistoryAbsentReasonDeferred = "http://terminology.hl7.org/CodeSystem/history-absent-reason#deferred";

    /// <summary>
    /// Literal for code: SubjectUnknown
    /// </summary>
    public const string LiteralSubjectUnknown = "subject-unknown";

    /// <summary>
    /// Literal for code: HistoryAbsentReasonSubjectUnknown
    /// </summary>
    public const string LiteralHistoryAbsentReasonSubjectUnknown = "http://terminology.hl7.org/CodeSystem/history-absent-reason#subject-unknown";

    /// <summary>
    /// Literal for code: UnableToObtain
    /// </summary>
    public const string LiteralUnableToObtain = "unable-to-obtain";

    /// <summary>
    /// Literal for code: HistoryAbsentReasonUnableToObtain
    /// </summary>
    public const string LiteralHistoryAbsentReasonUnableToObtain = "http://terminology.hl7.org/CodeSystem/history-absent-reason#unable-to-obtain";

    /// <summary>
    /// Literal for code: InformationWithheld
    /// </summary>
    public const string LiteralInformationWithheld = "withheld";

    /// <summary>
    /// Literal for code: HistoryAbsentReasonInformationWithheld
    /// </summary>
    public const string LiteralHistoryAbsentReasonInformationWithheld = "http://terminology.hl7.org/CodeSystem/history-absent-reason#withheld";

    /// <summary>
    /// Dictionary for looking up HistoryAbsentReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "deferred", Deferred }, 
      { "http://terminology.hl7.org/CodeSystem/history-absent-reason#deferred", Deferred }, 
      { "subject-unknown", SubjectUnknown }, 
      { "http://terminology.hl7.org/CodeSystem/history-absent-reason#subject-unknown", SubjectUnknown }, 
      { "unable-to-obtain", UnableToObtain }, 
      { "http://terminology.hl7.org/CodeSystem/history-absent-reason#unable-to-obtain", UnableToObtain }, 
      { "withheld", InformationWithheld }, 
      { "http://terminology.hl7.org/CodeSystem/history-absent-reason#withheld", InformationWithheld }, 
    };
  };
}