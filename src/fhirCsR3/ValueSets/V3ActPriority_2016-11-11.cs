// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  ///  A set of codes (e.g., for routine, emergency), specifying the urgency under which the Act happened, can happen, is happening, is intended to happen, or is requested/demanded to happen.
  /// </summary>
  public static class V3ActPriorityCodes
  {
    /// <summary>
    /// As soon as possible, next highest priority after stat.
    /// </summary>
    public static readonly Coding ASAP = new Coding
    {
      Code = "A",
      Display = "ASAP",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Filler should contact the placer as soon as results are available, even for preliminary results.  (Was "C" in HL7 version 2.3's reporting priority.)
    /// </summary>
    public static readonly Coding CallbackResults = new Coding
    {
      Code = "CR",
      Display = "callback results",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Filler should contact the placer (or target) to schedule the service.  (Was "C" in HL7 version 2.3's TQ-priority component.)
    /// </summary>
    public static readonly Coding CallbackForScheduling = new Coding
    {
      Code = "CS",
      Display = "callback for scheduling",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Filler should contact the placer to schedule the service.  (Was "C" in HL7 version 2.3's TQ-priority component.)
    /// </summary>
    public static readonly Coding CallbackPlacerForScheduling = new Coding
    {
      Code = "CSP",
      Display = "callback placer for scheduling",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Filler should contact the service recipient (target) to schedule the service.  (Was "C" in HL7 version 2.3's TQ-priority component.)
    /// </summary>
    public static readonly Coding ContactRecipientForScheduling = new Coding
    {
      Code = "CSR",
      Display = "contact recipient for scheduling",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Beneficial to the patient but not essential for survival.
    /// </summary>
    public static readonly Coding Elective = new Coding
    {
      Code = "EL",
      Display = "elective",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// An unforeseen combination of circumstances or the resulting state that calls for immediate action.
    /// </summary>
    public static readonly Coding Emergency = new Coding
    {
      Code = "EM",
      Display = "emergency",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Used to indicate that a service is to be performed prior to a scheduled surgery.  When ordering a service and using the pre-op priority, a check is done to see the amount of time that must be allowed for performance of the service.  When the order is placed, a message can be generated indicating the time needed for the service so that it is not ordered in conflict with a scheduled operation.
    /// </summary>
    public static readonly Coding Preop = new Coding
    {
      Code = "P",
      Display = "preop",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// An "as needed" order should be accompanied by a description of what constitutes a need. This description is represented by an observation service predicate as a precondition.
    /// </summary>
    public static readonly Coding AsNeeded = new Coding
    {
      Code = "PRN",
      Display = "as needed",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Routine service, do at usual work hours.
    /// </summary>
    public static readonly Coding Routine = new Coding
    {
      Code = "R",
      Display = "routine",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// A report should be prepared and sent as quickly as possible.
    /// </summary>
    public static readonly Coding RushReporting = new Coding
    {
      Code = "RR",
      Display = "rush reporting",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// With highest priority (e.g., emergency).
    /// </summary>
    public static readonly Coding Stat = new Coding
    {
      Code = "S",
      Display = "stat",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// It is critical to come as close as possible to the requested time (e.g., for a through antimicrobial level).
    /// </summary>
    public static readonly Coding TimingCritical = new Coding
    {
      Code = "T",
      Display = "timing critical",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Drug is to be used as directed by the prescriber.
    /// </summary>
    public static readonly Coding UseAsDirected = new Coding
    {
      Code = "UD",
      Display = "use as directed",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };
    /// <summary>
    /// Calls for prompt action.
    /// </summary>
    public static readonly Coding Urgent = new Coding
    {
      Code = "UR",
      Display = "urgent",
      System = "http://hl7.org/fhir/v3/ActPriority"
    };

    /// <summary>
    /// Literal for code: ASAP
    /// </summary>
    public const string LiteralASAP = "A";

    /// <summary>
    /// Literal for code: V3ActPriorityASAP
    /// </summary>
    public const string LiteralV3ActPriorityASAP = "http://hl7.org/fhir/v3/ActPriority#A";

    /// <summary>
    /// Literal for code: CallbackResults
    /// </summary>
    public const string LiteralCallbackResults = "CR";

    /// <summary>
    /// Literal for code: V3ActPriorityCallbackResults
    /// </summary>
    public const string LiteralV3ActPriorityCallbackResults = "http://hl7.org/fhir/v3/ActPriority#CR";

    /// <summary>
    /// Literal for code: CallbackForScheduling
    /// </summary>
    public const string LiteralCallbackForScheduling = "CS";

    /// <summary>
    /// Literal for code: V3ActPriorityCallbackForScheduling
    /// </summary>
    public const string LiteralV3ActPriorityCallbackForScheduling = "http://hl7.org/fhir/v3/ActPriority#CS";

    /// <summary>
    /// Literal for code: CallbackPlacerForScheduling
    /// </summary>
    public const string LiteralCallbackPlacerForScheduling = "CSP";

    /// <summary>
    /// Literal for code: V3ActPriorityCallbackPlacerForScheduling
    /// </summary>
    public const string LiteralV3ActPriorityCallbackPlacerForScheduling = "http://hl7.org/fhir/v3/ActPriority#CSP";

    /// <summary>
    /// Literal for code: ContactRecipientForScheduling
    /// </summary>
    public const string LiteralContactRecipientForScheduling = "CSR";

    /// <summary>
    /// Literal for code: V3ActPriorityContactRecipientForScheduling
    /// </summary>
    public const string LiteralV3ActPriorityContactRecipientForScheduling = "http://hl7.org/fhir/v3/ActPriority#CSR";

    /// <summary>
    /// Literal for code: Elective
    /// </summary>
    public const string LiteralElective = "EL";

    /// <summary>
    /// Literal for code: V3ActPriorityElective
    /// </summary>
    public const string LiteralV3ActPriorityElective = "http://hl7.org/fhir/v3/ActPriority#EL";

    /// <summary>
    /// Literal for code: Emergency
    /// </summary>
    public const string LiteralEmergency = "EM";

    /// <summary>
    /// Literal for code: V3ActPriorityEmergency
    /// </summary>
    public const string LiteralV3ActPriorityEmergency = "http://hl7.org/fhir/v3/ActPriority#EM";

    /// <summary>
    /// Literal for code: Preop
    /// </summary>
    public const string LiteralPreop = "P";

    /// <summary>
    /// Literal for code: V3ActPriorityPreop
    /// </summary>
    public const string LiteralV3ActPriorityPreop = "http://hl7.org/fhir/v3/ActPriority#P";

    /// <summary>
    /// Literal for code: AsNeeded
    /// </summary>
    public const string LiteralAsNeeded = "PRN";

    /// <summary>
    /// Literal for code: V3ActPriorityAsNeeded
    /// </summary>
    public const string LiteralV3ActPriorityAsNeeded = "http://hl7.org/fhir/v3/ActPriority#PRN";

    /// <summary>
    /// Literal for code: Routine
    /// </summary>
    public const string LiteralRoutine = "R";

    /// <summary>
    /// Literal for code: V3ActPriorityRoutine
    /// </summary>
    public const string LiteralV3ActPriorityRoutine = "http://hl7.org/fhir/v3/ActPriority#R";

    /// <summary>
    /// Literal for code: RushReporting
    /// </summary>
    public const string LiteralRushReporting = "RR";

    /// <summary>
    /// Literal for code: V3ActPriorityRushReporting
    /// </summary>
    public const string LiteralV3ActPriorityRushReporting = "http://hl7.org/fhir/v3/ActPriority#RR";

    /// <summary>
    /// Literal for code: Stat
    /// </summary>
    public const string LiteralStat = "S";

    /// <summary>
    /// Literal for code: V3ActPriorityStat
    /// </summary>
    public const string LiteralV3ActPriorityStat = "http://hl7.org/fhir/v3/ActPriority#S";

    /// <summary>
    /// Literal for code: TimingCritical
    /// </summary>
    public const string LiteralTimingCritical = "T";

    /// <summary>
    /// Literal for code: V3ActPriorityTimingCritical
    /// </summary>
    public const string LiteralV3ActPriorityTimingCritical = "http://hl7.org/fhir/v3/ActPriority#T";

    /// <summary>
    /// Literal for code: UseAsDirected
    /// </summary>
    public const string LiteralUseAsDirected = "UD";

    /// <summary>
    /// Literal for code: V3ActPriorityUseAsDirected
    /// </summary>
    public const string LiteralV3ActPriorityUseAsDirected = "http://hl7.org/fhir/v3/ActPriority#UD";

    /// <summary>
    /// Literal for code: Urgent
    /// </summary>
    public const string LiteralUrgent = "UR";

    /// <summary>
    /// Literal for code: V3ActPriorityUrgent
    /// </summary>
    public const string LiteralV3ActPriorityUrgent = "http://hl7.org/fhir/v3/ActPriority#UR";

    /// <summary>
    /// Dictionary for looking up V3ActPriority Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "A", ASAP }, 
      { "http://hl7.org/fhir/v3/ActPriority#A", ASAP }, 
      { "CR", CallbackResults }, 
      { "http://hl7.org/fhir/v3/ActPriority#CR", CallbackResults }, 
      { "CS", CallbackForScheduling }, 
      { "http://hl7.org/fhir/v3/ActPriority#CS", CallbackForScheduling }, 
      { "CSP", CallbackPlacerForScheduling }, 
      { "http://hl7.org/fhir/v3/ActPriority#CSP", CallbackPlacerForScheduling }, 
      { "CSR", ContactRecipientForScheduling }, 
      { "http://hl7.org/fhir/v3/ActPriority#CSR", ContactRecipientForScheduling }, 
      { "EL", Elective }, 
      { "http://hl7.org/fhir/v3/ActPriority#EL", Elective }, 
      { "EM", Emergency }, 
      { "http://hl7.org/fhir/v3/ActPriority#EM", Emergency }, 
      { "P", Preop }, 
      { "http://hl7.org/fhir/v3/ActPriority#P", Preop }, 
      { "PRN", AsNeeded }, 
      { "http://hl7.org/fhir/v3/ActPriority#PRN", AsNeeded }, 
      { "R", Routine }, 
      { "http://hl7.org/fhir/v3/ActPriority#R", Routine }, 
      { "RR", RushReporting }, 
      { "http://hl7.org/fhir/v3/ActPriority#RR", RushReporting }, 
      { "S", Stat }, 
      { "http://hl7.org/fhir/v3/ActPriority#S", Stat }, 
      { "T", TimingCritical }, 
      { "http://hl7.org/fhir/v3/ActPriority#T", TimingCritical }, 
      { "UD", UseAsDirected }, 
      { "http://hl7.org/fhir/v3/ActPriority#UD", UseAsDirected }, 
      { "UR", Urgent }, 
      { "http://hl7.org/fhir/v3/ActPriority#UR", Urgent }, 
    };
  };
}
