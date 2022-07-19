// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The processing mode that applies to this list.
  /// </summary>
  public static class ListModeCodes
  {
    /// <summary>
    /// A point-in-time list that shows what changes have been made or recommended.  E.g. a discharge medication list showing what was added and removed during an encounter.
    /// </summary>
    public static readonly Coding ChangeList = new Coding
    {
      Code = "changes",
      Display = "Change List",
      System = "http://hl7.org/fhir/list-mode"
    };
    /// <summary>
    /// This list was prepared as a snapshot. It should not be assumed to be current.
    /// </summary>
    public static readonly Coding SnapshotList = new Coding
    {
      Code = "snapshot",
      Display = "Snapshot List",
      System = "http://hl7.org/fhir/list-mode"
    };
    /// <summary>
    /// This list is the master list, maintained in an ongoing fashion with regular updates as the real world list it is tracking changes.
    /// </summary>
    public static readonly Coding WorkingList = new Coding
    {
      Code = "working",
      Display = "Working List",
      System = "http://hl7.org/fhir/list-mode"
    };

    /// <summary>
    /// Literal for code: ChangeList
    /// </summary>
    public const string LiteralChangeList = "changes";

    /// <summary>
    /// Literal for code: ListModeChangeList
    /// </summary>
    public const string LiteralListModeChangeList = "http://hl7.org/fhir/list-mode#changes";

    /// <summary>
    /// Literal for code: SnapshotList
    /// </summary>
    public const string LiteralSnapshotList = "snapshot";

    /// <summary>
    /// Literal for code: ListModeSnapshotList
    /// </summary>
    public const string LiteralListModeSnapshotList = "http://hl7.org/fhir/list-mode#snapshot";

    /// <summary>
    /// Literal for code: WorkingList
    /// </summary>
    public const string LiteralWorkingList = "working";

    /// <summary>
    /// Literal for code: ListModeWorkingList
    /// </summary>
    public const string LiteralListModeWorkingList = "http://hl7.org/fhir/list-mode#working";

    /// <summary>
    /// Dictionary for looking up ListMode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "changes", ChangeList }, 
      { "http://hl7.org/fhir/list-mode#changes", ChangeList }, 
      { "snapshot", SnapshotList }, 
      { "http://hl7.org/fhir/list-mode#snapshot", SnapshotList }, 
      { "working", WorkingList }, 
      { "http://hl7.org/fhir/list-mode#working", WorkingList }, 
    };
  };
}
