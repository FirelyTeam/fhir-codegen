// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The status of the episode of care.
  /// </summary>
  public static class EpisodeOfCareStatusCodes
  {
    /// <summary>
    /// This episode of care is current.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// The episode of care was cancelled, or withdrawn from service, often selected during the planned stage as the patient may have gone elsewhere, or the circumstances have changed and the organization is unable to provide the care. It indicates that services terminated outside the planned/expected workflow.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// This instance should not have been part of this patient's medical record.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// This episode of care is finished and the organization is not expecting to be providing further care to the patient. Can also be known as "closed", "completed" or other similar terms.
    /// </summary>
    public static readonly Coding Finished = new Coding
    {
      Code = "finished",
      Display = "Finished",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// This episode of care is on hold; the organization has limited responsibility for the patient (such as while on respite).
    /// </summary>
    public static readonly Coding OnHold = new Coding
    {
      Code = "onhold",
      Display = "On Hold",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// This episode of care is planned to start at the date specified in the period.start. During this status, an organization may perform assessments to determine if the patient is eligible to receive services, or be organizing to make resources available to provide care services.
    /// </summary>
    public static readonly Coding Planned = new Coding
    {
      Code = "planned",
      Display = "Planned",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };
    /// <summary>
    /// This episode has been placed on a waitlist, pending the episode being made active (or cancelled).
    /// </summary>
    public static readonly Coding Waitlist = new Coding
    {
      Code = "waitlist",
      Display = "Waitlist",
      System = "http://hl7.org/fhir/episode-of-care-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusActive
    /// </summary>
    public const string LiteralEpisodeOfCareStatusActive = "http://hl7.org/fhir/episode-of-care-status#active";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusCancelled
    /// </summary>
    public const string LiteralEpisodeOfCareStatusCancelled = "http://hl7.org/fhir/episode-of-care-status#cancelled";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusEnteredInError
    /// </summary>
    public const string LiteralEpisodeOfCareStatusEnteredInError = "http://hl7.org/fhir/episode-of-care-status#entered-in-error";

    /// <summary>
    /// Literal for code: Finished
    /// </summary>
    public const string LiteralFinished = "finished";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusFinished
    /// </summary>
    public const string LiteralEpisodeOfCareStatusFinished = "http://hl7.org/fhir/episode-of-care-status#finished";

    /// <summary>
    /// Literal for code: OnHold
    /// </summary>
    public const string LiteralOnHold = "onhold";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusOnHold
    /// </summary>
    public const string LiteralEpisodeOfCareStatusOnHold = "http://hl7.org/fhir/episode-of-care-status#onhold";

    /// <summary>
    /// Literal for code: Planned
    /// </summary>
    public const string LiteralPlanned = "planned";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusPlanned
    /// </summary>
    public const string LiteralEpisodeOfCareStatusPlanned = "http://hl7.org/fhir/episode-of-care-status#planned";

    /// <summary>
    /// Literal for code: Waitlist
    /// </summary>
    public const string LiteralWaitlist = "waitlist";

    /// <summary>
    /// Literal for code: EpisodeOfCareStatusWaitlist
    /// </summary>
    public const string LiteralEpisodeOfCareStatusWaitlist = "http://hl7.org/fhir/episode-of-care-status#waitlist";

    /// <summary>
    /// Dictionary for looking up EpisodeOfCareStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/episode-of-care-status#active", Active }, 
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/episode-of-care-status#cancelled", Cancelled }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/episode-of-care-status#entered-in-error", EnteredInError }, 
      { "finished", Finished }, 
      { "http://hl7.org/fhir/episode-of-care-status#finished", Finished }, 
      { "onhold", OnHold }, 
      { "http://hl7.org/fhir/episode-of-care-status#onhold", OnHold }, 
      { "planned", Planned }, 
      { "http://hl7.org/fhir/episode-of-care-status#planned", Planned }, 
      { "waitlist", Waitlist }, 
      { "http://hl7.org/fhir/episode-of-care-status#waitlist", Waitlist }, 
    };
  };
}
