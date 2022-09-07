// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Identifies the level of importance to be assigned to actioning the request.
  /// </summary>
  public static class RequestPriorityCodes
  {
    /// <summary>
    /// The request should be actioned as soon as possible - higher priority than urgent.
    /// </summary>
    public static readonly Coding ASAP = new Coding
    {
      Code = "asap",
      Display = "ASAP",
      System = "http://hl7.org/fhir/request-priority"
    };
    /// <summary>
    /// The request has normal priority.
    /// </summary>
    public static readonly Coding Routine = new Coding
    {
      Code = "routine",
      Display = "Routine",
      System = "http://hl7.org/fhir/request-priority"
    };
    /// <summary>
    /// The request should be actioned immediately - highest possible priority.  E.g. an emergency.
    /// </summary>
    public static readonly Coding STAT = new Coding
    {
      Code = "stat",
      Display = "STAT",
      System = "http://hl7.org/fhir/request-priority"
    };
    /// <summary>
    /// The request should be actioned promptly - higher priority than routine.
    /// </summary>
    public static readonly Coding Urgent = new Coding
    {
      Code = "urgent",
      Display = "Urgent",
      System = "http://hl7.org/fhir/request-priority"
    };

    /// <summary>
    /// Literal for code: ASAP
    /// </summary>
    public const string LiteralASAP = "asap";

    /// <summary>
    /// Literal for code: RequestPriorityASAP
    /// </summary>
    public const string LiteralRequestPriorityASAP = "http://hl7.org/fhir/request-priority#asap";

    /// <summary>
    /// Literal for code: Routine
    /// </summary>
    public const string LiteralRoutine = "routine";

    /// <summary>
    /// Literal for code: RequestPriorityRoutine
    /// </summary>
    public const string LiteralRequestPriorityRoutine = "http://hl7.org/fhir/request-priority#routine";

    /// <summary>
    /// Literal for code: STAT
    /// </summary>
    public const string LiteralSTAT = "stat";

    /// <summary>
    /// Literal for code: RequestPrioritySTAT
    /// </summary>
    public const string LiteralRequestPrioritySTAT = "http://hl7.org/fhir/request-priority#stat";

    /// <summary>
    /// Literal for code: Urgent
    /// </summary>
    public const string LiteralUrgent = "urgent";

    /// <summary>
    /// Literal for code: RequestPriorityUrgent
    /// </summary>
    public const string LiteralRequestPriorityUrgent = "http://hl7.org/fhir/request-priority#urgent";

    /// <summary>
    /// Dictionary for looking up RequestPriority Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "asap", ASAP }, 
      { "http://hl7.org/fhir/request-priority#asap", ASAP }, 
      { "routine", Routine }, 
      { "http://hl7.org/fhir/request-priority#routine", Routine }, 
      { "stat", STAT }, 
      { "http://hl7.org/fhir/request-priority#stat", STAT }, 
      { "urgent", Urgent }, 
      { "http://hl7.org/fhir/request-priority#urgent", Urgent }, 
    };
  };
}
