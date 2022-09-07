// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Indicates who is expected to pay a part of the account balance.
  /// </summary>
  public static class AccountAggregateCodes
  {
    /// <summary>
    /// This (aggregated) balance is expected to be paid by Insurance coverage(s)
    /// </summary>
    public static readonly Coding Insurance = new Coding
    {
      Code = "insurance",
      Display = "Insurance",
      System = "http://hl7.org/fhir/account-aggregate"
    };
    /// <summary>
    /// This (aggregated) balance is expected to be paid by the Patient
    /// </summary>
    public static readonly Coding Patient = new Coding
    {
      Code = "patient",
      Display = "Patient",
      System = "http://hl7.org/fhir/account-aggregate"
    };
    /// <summary>
    /// There is no aggregation on this balance
    /// </summary>
    public static readonly Coding Total = new Coding
    {
      Code = "total",
      Display = "Total",
      System = "http://hl7.org/fhir/account-aggregate"
    };

    /// <summary>
    /// Literal for code: Insurance
    /// </summary>
    public const string LiteralInsurance = "insurance";

    /// <summary>
    /// Literal for code: AccountAggregateInsurance
    /// </summary>
    public const string LiteralAccountAggregateInsurance = "http://hl7.org/fhir/account-aggregate#insurance";

    /// <summary>
    /// Literal for code: Patient
    /// </summary>
    public const string LiteralPatient = "patient";

    /// <summary>
    /// Literal for code: AccountAggregatePatient
    /// </summary>
    public const string LiteralAccountAggregatePatient = "http://hl7.org/fhir/account-aggregate#patient";

    /// <summary>
    /// Literal for code: Total
    /// </summary>
    public const string LiteralTotal = "total";

    /// <summary>
    /// Literal for code: AccountAggregateTotal
    /// </summary>
    public const string LiteralAccountAggregateTotal = "http://hl7.org/fhir/account-aggregate#total";

    /// <summary>
    /// Dictionary for looking up AccountAggregate Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "insurance", Insurance }, 
      { "http://hl7.org/fhir/account-aggregate#insurance", Insurance }, 
      { "patient", Patient }, 
      { "http://hl7.org/fhir/account-aggregate#patient", Patient }, 
      { "total", Total }, 
      { "http://hl7.org/fhir/account-aggregate#total", Total }, 
    };
  };
}
