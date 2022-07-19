// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Base values for the order of the items in a list resource.
  /// </summary>
  public static class ListOrderCodes
  {
    /// <summary>
    /// The list is sorted alphabetically by an unspecified property of the items in the list.
    /// </summary>
    public static readonly Coding SortedAlphabetically = new Coding
    {
      Code = "alphabetic",
      Display = "Sorted Alphabetically",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list is sorted categorically by an unspecified property of the items in the list.
    /// </summary>
    public static readonly Coding SortedByCategory = new Coding
    {
      Code = "category",
      Display = "Sorted by Category",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list is sorted by the date the item was added to the list. Note that the date added to the list is not explicit in the list itself.
    /// </summary>
    public static readonly Coding SortedByItemDate = new Coding
    {
      Code = "entry-date",
      Display = "Sorted by Item Date",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list is sorted by the data of the event. This can be used when the list has items which are dates with past or future events.
    /// </summary>
    public static readonly Coding SortedByEventDate = new Coding
    {
      Code = "event-date",
      Display = "Sorted by Event Date",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list is sorted by patient, with items for each patient grouped together.
    /// </summary>
    public static readonly Coding SortedByPatient = new Coding
    {
      Code = "patient",
      Display = "Sorted by Patient",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list is sorted by priority. The exact method in which priority has been determined is not specified.
    /// </summary>
    public static readonly Coding SortedByPriority = new Coding
    {
      Code = "priority",
      Display = "Sorted by Priority",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list was sorted by the system. The criteria the user used are not specified; define additional codes to specify a particular order (or use other defined codes).
    /// </summary>
    public static readonly Coding SortedBySystem = new Coding
    {
      Code = "system",
      Display = "Sorted by System",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };
    /// <summary>
    /// The list was sorted by a user. The criteria the user used are not specified.
    /// </summary>
    public static readonly Coding SortedByUser = new Coding
    {
      Code = "user",
      Display = "Sorted by User",
      System = "http://terminology.hl7.org/CodeSystem/list-order"
    };

    /// <summary>
    /// Literal for code: SortedAlphabetically
    /// </summary>
    public const string LiteralSortedAlphabetically = "alphabetic";

    /// <summary>
    /// Literal for code: ListOrderSortedAlphabetically
    /// </summary>
    public const string LiteralListOrderSortedAlphabetically = "http://terminology.hl7.org/CodeSystem/list-order#alphabetic";

    /// <summary>
    /// Literal for code: SortedByCategory
    /// </summary>
    public const string LiteralSortedByCategory = "category";

    /// <summary>
    /// Literal for code: ListOrderSortedByCategory
    /// </summary>
    public const string LiteralListOrderSortedByCategory = "http://terminology.hl7.org/CodeSystem/list-order#category";

    /// <summary>
    /// Literal for code: SortedByItemDate
    /// </summary>
    public const string LiteralSortedByItemDate = "entry-date";

    /// <summary>
    /// Literal for code: ListOrderSortedByItemDate
    /// </summary>
    public const string LiteralListOrderSortedByItemDate = "http://terminology.hl7.org/CodeSystem/list-order#entry-date";

    /// <summary>
    /// Literal for code: SortedByEventDate
    /// </summary>
    public const string LiteralSortedByEventDate = "event-date";

    /// <summary>
    /// Literal for code: ListOrderSortedByEventDate
    /// </summary>
    public const string LiteralListOrderSortedByEventDate = "http://terminology.hl7.org/CodeSystem/list-order#event-date";

    /// <summary>
    /// Literal for code: SortedByPatient
    /// </summary>
    public const string LiteralSortedByPatient = "patient";

    /// <summary>
    /// Literal for code: ListOrderSortedByPatient
    /// </summary>
    public const string LiteralListOrderSortedByPatient = "http://terminology.hl7.org/CodeSystem/list-order#patient";

    /// <summary>
    /// Literal for code: SortedByPriority
    /// </summary>
    public const string LiteralSortedByPriority = "priority";

    /// <summary>
    /// Literal for code: ListOrderSortedByPriority
    /// </summary>
    public const string LiteralListOrderSortedByPriority = "http://terminology.hl7.org/CodeSystem/list-order#priority";

    /// <summary>
    /// Literal for code: SortedBySystem
    /// </summary>
    public const string LiteralSortedBySystem = "system";

    /// <summary>
    /// Literal for code: ListOrderSortedBySystem
    /// </summary>
    public const string LiteralListOrderSortedBySystem = "http://terminology.hl7.org/CodeSystem/list-order#system";

    /// <summary>
    /// Literal for code: SortedByUser
    /// </summary>
    public const string LiteralSortedByUser = "user";

    /// <summary>
    /// Literal for code: ListOrderSortedByUser
    /// </summary>
    public const string LiteralListOrderSortedByUser = "http://terminology.hl7.org/CodeSystem/list-order#user";

    /// <summary>
    /// Dictionary for looking up ListOrder Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "alphabetic", SortedAlphabetically }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#alphabetic", SortedAlphabetically }, 
      { "category", SortedByCategory }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#category", SortedByCategory }, 
      { "entry-date", SortedByItemDate }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#entry-date", SortedByItemDate }, 
      { "event-date", SortedByEventDate }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#event-date", SortedByEventDate }, 
      { "patient", SortedByPatient }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#patient", SortedByPatient }, 
      { "priority", SortedByPriority }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#priority", SortedByPriority }, 
      { "system", SortedBySystem }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#system", SortedBySystem }, 
      { "user", SortedByUser }, 
      { "http://terminology.hl7.org/CodeSystem/list-order#user", SortedByUser }, 
    };
  };
}
