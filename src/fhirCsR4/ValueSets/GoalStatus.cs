// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes that reflect the current state of a goal and whether the goal is still being targeted.
  /// </summary>
  public static class GoalStatusCodes
  {
    /// <summary>
    /// A proposed goal was accepted or acknowledged.
    /// </summary>
    public static readonly Coding Accepted = new Coding
    {
      Code = "accepted",
      Display = "Accepted",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// The goal is being sought actively.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// The goal has been abandoned.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// The goal is no longer being sought.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// The goal was entered in error and voided.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// The goal remains a long term objective but is no longer being actively pursued for a temporary period of time.
    /// </summary>
    public static readonly Coding OnHold = new Coding
    {
      Code = "on-hold",
      Display = "On Hold",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// A goal is planned for this patient.
    /// </summary>
    public static readonly Coding Planned = new Coding
    {
      Code = "planned",
      Display = "Planned",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// A goal is proposed for this patient.
    /// </summary>
    public static readonly Coding Proposed = new Coding
    {
      Code = "proposed",
      Display = "Proposed",
      System = "http://hl7.org/fhir/goal-status"
    };
    /// <summary>
    /// A proposed goal was rejected.
    /// </summary>
    public static readonly Coding Rejected = new Coding
    {
      Code = "rejected",
      Display = "Rejected",
      System = "http://hl7.org/fhir/goal-status"
    };

    /// <summary>
    /// Literal for code: Accepted
    /// </summary>
    public const string LiteralAccepted = "accepted";

    /// <summary>
    /// Literal for code: GoalStatusAccepted
    /// </summary>
    public const string LiteralGoalStatusAccepted = "http://hl7.org/fhir/goal-status#accepted";

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: GoalStatusActive
    /// </summary>
    public const string LiteralGoalStatusActive = "http://hl7.org/fhir/goal-status#active";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: GoalStatusCancelled
    /// </summary>
    public const string LiteralGoalStatusCancelled = "http://hl7.org/fhir/goal-status#cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: GoalStatusCompleted
    /// </summary>
    public const string LiteralGoalStatusCompleted = "http://hl7.org/fhir/goal-status#completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: GoalStatusEnteredInError
    /// </summary>
    public const string LiteralGoalStatusEnteredInError = "http://hl7.org/fhir/goal-status#entered-in-error";

    /// <summary>
    /// Literal for code: OnHold
    /// </summary>
    public const string LiteralOnHold = "on-hold";

    /// <summary>
    /// Literal for code: GoalStatusOnHold
    /// </summary>
    public const string LiteralGoalStatusOnHold = "http://hl7.org/fhir/goal-status#on-hold";

    /// <summary>
    /// Literal for code: Planned
    /// </summary>
    public const string LiteralPlanned = "planned";

    /// <summary>
    /// Literal for code: GoalStatusPlanned
    /// </summary>
    public const string LiteralGoalStatusPlanned = "http://hl7.org/fhir/goal-status#planned";

    /// <summary>
    /// Literal for code: Proposed
    /// </summary>
    public const string LiteralProposed = "proposed";

    /// <summary>
    /// Literal for code: GoalStatusProposed
    /// </summary>
    public const string LiteralGoalStatusProposed = "http://hl7.org/fhir/goal-status#proposed";

    /// <summary>
    /// Literal for code: Rejected
    /// </summary>
    public const string LiteralRejected = "rejected";

    /// <summary>
    /// Literal for code: GoalStatusRejected
    /// </summary>
    public const string LiteralGoalStatusRejected = "http://hl7.org/fhir/goal-status#rejected";

    /// <summary>
    /// Dictionary for looking up GoalStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "accepted", Accepted }, 
      { "http://hl7.org/fhir/goal-status#accepted", Accepted }, 
      { "active", Active }, 
      { "http://hl7.org/fhir/goal-status#active", Active }, 
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/goal-status#cancelled", Cancelled }, 
      { "completed", Completed }, 
      { "http://hl7.org/fhir/goal-status#completed", Completed }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/goal-status#entered-in-error", EnteredInError }, 
      { "on-hold", OnHold }, 
      { "http://hl7.org/fhir/goal-status#on-hold", OnHold }, 
      { "planned", Planned }, 
      { "http://hl7.org/fhir/goal-status#planned", Planned }, 
      { "proposed", Proposed }, 
      { "http://hl7.org/fhir/goal-status#proposed", Proposed }, 
      { "rejected", Rejected }, 
      { "http://hl7.org/fhir/goal-status#rejected", Rejected }, 
    };
  };
}
