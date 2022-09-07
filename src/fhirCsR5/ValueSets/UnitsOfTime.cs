// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// A unit of time (units from UCUM).
  /// </summary>
  public static class UnitsOfTimeCodes
  {
    /// <summary>
    /// year
    /// </summary>
    public static readonly Coding Year = new Coding
    {
      Code = "a",
      Display = "year",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// day
    /// </summary>
    public static readonly Coding Day = new Coding
    {
      Code = "d",
      Display = "day",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// hour
    /// </summary>
    public static readonly Coding Hour = new Coding
    {
      Code = "h",
      Display = "hour",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// minute
    /// </summary>
    public static readonly Coding Minute = new Coding
    {
      Code = "min",
      Display = "minute",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// month
    /// </summary>
    public static readonly Coding Month = new Coding
    {
      Code = "mo",
      Display = "month",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// second
    /// </summary>
    public static readonly Coding Second = new Coding
    {
      Code = "s",
      Display = "second",
      System = "http://unitsofmeasure.org"
    };
    /// <summary>
    /// week
    /// </summary>
    public static readonly Coding Week = new Coding
    {
      Code = "wk",
      Display = "week",
      System = "http://unitsofmeasure.org"
    };

    /// <summary>
    /// Literal for code: Year
    /// </summary>
    public const string LiteralYear = "a";

    /// <summary>
    /// Literal for code: NoneYear
    /// </summary>
    public const string LiteralNoneYear = "http://unitsofmeasure.org#a";

    /// <summary>
    /// Literal for code: Day
    /// </summary>
    public const string LiteralDay = "d";

    /// <summary>
    /// Literal for code: NoneDay
    /// </summary>
    public const string LiteralNoneDay = "http://unitsofmeasure.org#d";

    /// <summary>
    /// Literal for code: Hour
    /// </summary>
    public const string LiteralHour = "h";

    /// <summary>
    /// Literal for code: NoneHour
    /// </summary>
    public const string LiteralNoneHour = "http://unitsofmeasure.org#h";

    /// <summary>
    /// Literal for code: Minute
    /// </summary>
    public const string LiteralMinute = "min";

    /// <summary>
    /// Literal for code: NoneMinute
    /// </summary>
    public const string LiteralNoneMinute = "http://unitsofmeasure.org#min";

    /// <summary>
    /// Literal for code: Month
    /// </summary>
    public const string LiteralMonth = "mo";

    /// <summary>
    /// Literal for code: NoneMonth
    /// </summary>
    public const string LiteralNoneMonth = "http://unitsofmeasure.org#mo";

    /// <summary>
    /// Literal for code: Second
    /// </summary>
    public const string LiteralSecond = "s";

    /// <summary>
    /// Literal for code: NoneSecond
    /// </summary>
    public const string LiteralNoneSecond = "http://unitsofmeasure.org#s";

    /// <summary>
    /// Literal for code: Week
    /// </summary>
    public const string LiteralWeek = "wk";

    /// <summary>
    /// Literal for code: NoneWeek
    /// </summary>
    public const string LiteralNoneWeek = "http://unitsofmeasure.org#wk";

    /// <summary>
    /// Dictionary for looking up UnitsOfTime Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "a", Year }, 
      { "http://unitsofmeasure.org#a", Year }, 
      { "d", Day }, 
      { "http://unitsofmeasure.org#d", Day }, 
      { "h", Hour }, 
      { "http://unitsofmeasure.org#h", Hour }, 
      { "min", Minute }, 
      { "http://unitsofmeasure.org#min", Minute }, 
      { "mo", Month }, 
      { "http://unitsofmeasure.org#mo", Month }, 
      { "s", Second }, 
      { "http://unitsofmeasure.org#s", Second }, 
      { "wk", Week }, 
      { "http://unitsofmeasure.org#wk", Week }, 
    };
  };
}
