// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// The type of an element - one of the FHIR data types.
  /// </summary>
  public static class DataTypesCodes
  {
    /// <summary>
    /// There is a variety of postal address formats defined around the world. This format defines a superset that is the basis for all addresses around the world.
    /// </summary>
    public static readonly Coding Address = new Coding
    {
      Code = "Address",
      Display = "Address",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Age
    /// </summary>
    public static readonly Coding Age = new Coding
    {
      Code = "Age",
      Display = "Age",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A  text note which also  contains information about who made the statement and when.
    /// </summary>
    public static readonly Coding Annotation = new Coding
    {
      Code = "Annotation",
      Display = "Annotation",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// For referring to data content defined in other formats.
    /// </summary>
    public static readonly Coding Attachment = new Coding
    {
      Code = "Attachment",
      Display = "Attachment",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Base definition for all elements that are defined inside a resource - but not those in a data type.
    /// </summary>
    public static readonly Coding BackboneElement = new Coding
    {
      Code = "BackboneElement",
      Display = "BackboneElement",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A stream of bytes
    /// </summary>
    public static readonly Coding Base64Binary = new Coding
    {
      Code = "base64Binary",
      Display = "base64Binary",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Value of "true" or "false"
    /// </summary>
    public static readonly Coding Boolean = new Coding
    {
      Code = "boolean",
      Display = "boolean",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A string which has at least one character and no leading or trailing whitespace and where there is no whitespace other than single spaces in the contents
    /// </summary>
    public static readonly Coding Code = new Coding
    {
      Code = "code",
      Display = "code",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A concept that may be defined by a formal reference to a terminology or ontology or may be provided by text.
    /// </summary>
    public static readonly Coding CodeableConcept = new Coding
    {
      Code = "CodeableConcept",
      Display = "CodeableConcept",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A reference to a code defined by a terminology system.
    /// </summary>
    public static readonly Coding Coding = new Coding
    {
      Code = "Coding",
      Display = "Coding",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Details for all kinds of technology mediated contact points for a person or organization, including telephone, email, etc.
    /// </summary>
    public static readonly Coding ContactPoint = new Coding
    {
      Code = "ContactPoint",
      Display = "ContactPoint",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Count
    /// </summary>
    public static readonly Coding Count = new Coding
    {
      Code = "Count",
      Display = "Count",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A date or partial date (e.g. just year or year + month). There is no time zone. The format is a union of the schema types gYear, gYearMonth and date.  Dates SHALL be valid dates.
    /// </summary>
    public static readonly Coding Date = new Coding
    {
      Code = "date",
      Display = "date",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A date, date-time or partial date (e.g. just year or year + month).  If hours and minutes are specified, a time zone SHALL be populated. The format is a union of the schema types gYear, gYearMonth, date and dateTime. Seconds must be provided due to schema type constraints but may be zero-filled and may be ignored.                 Dates SHALL be valid dates.
    /// </summary>
    public static readonly Coding DateTime = new Coding
    {
      Code = "dateTime",
      Display = "dateTime",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A rational number with implicit precision
    /// </summary>
    public static readonly Coding VALDecimal = new Coding
    {
      Code = "decimal",
      Display = "decimal",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Distance
    /// </summary>
    public static readonly Coding Distance = new Coding
    {
      Code = "Distance",
      Display = "Distance",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Duration
    /// </summary>
    public static readonly Coding Duration = new Coding
    {
      Code = "Duration",
      Display = "Duration",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Base definition for all elements in a resource.
    /// </summary>
    public static readonly Coding Element = new Coding
    {
      Code = "Element",
      Display = "Element",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Captures constraints on each element within the resource, profile, or extension.
    /// </summary>
    public static readonly Coding ElementDefinition = new Coding
    {
      Code = "ElementDefinition",
      Display = "ElementDefinition",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Optional Extensions Element - found in all resources.
    /// </summary>
    public static readonly Coding Extension = new Coding
    {
      Code = "Extension",
      Display = "Extension",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A human's name with the ability to identify parts and usage.
    /// </summary>
    public static readonly Coding HumanName = new Coding
    {
      Code = "HumanName",
      Display = "HumanName",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Any combination of letters, numerals, "-" and ".", with a length limit of 64 characters.  (This might be an integer, an unprefixed OID, UUID or any other identifier pattern that meets these constraints.)  Ids are case-insensitive.
    /// </summary>
    public static readonly Coding Id = new Coding
    {
      Code = "id",
      Display = "id",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A technical identifier - identifies some entity uniquely and unambiguously.
    /// </summary>
    public static readonly Coding Identifier = new Coding
    {
      Code = "Identifier",
      Display = "Identifier",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// An instant in time - known at least to the second
    /// </summary>
    public static readonly Coding Instant = new Coding
    {
      Code = "instant",
      Display = "instant",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A whole number
    /// </summary>
    public static readonly Coding Integer = new Coding
    {
      Code = "integer",
      Display = "integer",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A string that may contain markdown syntax for optional processing by a mark down presentation engine
    /// </summary>
    public static readonly Coding Markdown = new Coding
    {
      Code = "markdown",
      Display = "markdown",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// The metadata about a resource. This is content in the resource that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource.
    /// </summary>
    public static readonly Coding Meta = new Coding
    {
      Code = "Meta",
      Display = "Meta",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Money
    /// </summary>
    public static readonly Coding Money = new Coding
    {
      Code = "Money",
      Display = "Money",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A human-readable formatted text, including images.
    /// </summary>
    public static readonly Coding Narrative = new Coding
    {
      Code = "Narrative",
      Display = "Narrative",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// An oid represented as a URI
    /// </summary>
    public static readonly Coding Oid = new Coding
    {
      Code = "oid",
      Display = "oid",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A time period defined by a start and end date and optionally time.
    /// </summary>
    public static readonly Coding Period = new Coding
    {
      Code = "Period",
      Display = "Period",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// An integer with a value that is positive (e.g. &gt;0)
    /// </summary>
    public static readonly Coding PositiveInt = new Coding
    {
      Code = "positiveInt",
      Display = "positiveInt",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A measured amount (or an amount that can potentially be measured). Note that measured amounts include amounts that are not precisely quantified, including amounts involving arbitrary units and floating currencies.
    /// </summary>
    public static readonly Coding Quantity = new Coding
    {
      Code = "Quantity",
      Display = "Quantity",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A set of ordered Quantities defined by a low and high limit.
    /// </summary>
    public static readonly Coding Range = new Coding
    {
      Code = "Range",
      Display = "Range",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A relationship of two Quantity values - expressed as a numerator and a denominator.
    /// </summary>
    public static readonly Coding Ratio = new Coding
    {
      Code = "Ratio",
      Display = "Ratio",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A reference from one resource to another.
    /// </summary>
    public static readonly Coding Reference = new Coding
    {
      Code = "Reference",
      Display = "Reference",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A series of measurements taken by a device, with upper and lower limits. There may be more than one dimension in the data.
    /// </summary>
    public static readonly Coding SampledData = new Coding
    {
      Code = "SampledData",
      Display = "SampledData",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A digital signature along with supporting context. The signature may be electronic/cryptographic in nature, or a graphical image representing a hand-written signature, or a signature process. Different Signature approaches have different utilities.
    /// </summary>
    public static readonly Coding Signature = new Coding
    {
      Code = "Signature",
      Display = "Signature",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// SimpleQuantity
    /// </summary>
    public static readonly Coding SimpleQuantity = new Coding
    {
      Code = "SimpleQuantity",
      Display = "SimpleQuantity",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A sequence of Unicode characters
    /// </summary>
    public static readonly Coding VALString = new Coding
    {
      Code = "string",
      Display = "string",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A time during the day, with no date specified
    /// </summary>
    public static readonly Coding Time = new Coding
    {
      Code = "time",
      Display = "time",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// Specifies an event that may occur multiple times. Timing schedules are used to record when things are expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds.
    /// </summary>
    public static readonly Coding Timing = new Coding
    {
      Code = "Timing",
      Display = "Timing",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// An integer with a value that is not negative (e.g. &gt;= 0)
    /// </summary>
    public static readonly Coding UnsignedInt = new Coding
    {
      Code = "unsignedInt",
      Display = "unsignedInt",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// String of characters used to identify a name or a resource
    /// </summary>
    public static readonly Coding Uri = new Coding
    {
      Code = "uri",
      Display = "uri",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// A UUID, represented as a URI
    /// </summary>
    public static readonly Coding Uuid = new Coding
    {
      Code = "uuid",
      Display = "uuid",
      System = "http://hl7.org/fhir/data-types"
    };
    /// <summary>
    /// XHTML format, as defined by W3C, but restricted usage (mainly, no active content)
    /// </summary>
    public static readonly Coding XHTML = new Coding
    {
      Code = "xhtml",
      Display = "XHTML",
      System = "http://hl7.org/fhir/data-types"
    };

    /// <summary>
    /// Literal for code: Address
    /// </summary>
    public const string LiteralAddress = "Address";

    /// <summary>
    /// Literal for code: DataTypesAddress
    /// </summary>
    public const string LiteralDataTypesAddress = "http://hl7.org/fhir/data-types#Address";

    /// <summary>
    /// Literal for code: Age
    /// </summary>
    public const string LiteralAge = "Age";

    /// <summary>
    /// Literal for code: DataTypesAge
    /// </summary>
    public const string LiteralDataTypesAge = "http://hl7.org/fhir/data-types#Age";

    /// <summary>
    /// Literal for code: Annotation
    /// </summary>
    public const string LiteralAnnotation = "Annotation";

    /// <summary>
    /// Literal for code: DataTypesAnnotation
    /// </summary>
    public const string LiteralDataTypesAnnotation = "http://hl7.org/fhir/data-types#Annotation";

    /// <summary>
    /// Literal for code: Attachment
    /// </summary>
    public const string LiteralAttachment = "Attachment";

    /// <summary>
    /// Literal for code: DataTypesAttachment
    /// </summary>
    public const string LiteralDataTypesAttachment = "http://hl7.org/fhir/data-types#Attachment";

    /// <summary>
    /// Literal for code: BackboneElement
    /// </summary>
    public const string LiteralBackboneElement = "BackboneElement";

    /// <summary>
    /// Literal for code: DataTypesBackboneElement
    /// </summary>
    public const string LiteralDataTypesBackboneElement = "http://hl7.org/fhir/data-types#BackboneElement";

    /// <summary>
    /// Literal for code: Base64Binary
    /// </summary>
    public const string LiteralBase64Binary = "base64Binary";

    /// <summary>
    /// Literal for code: DataTypesBase64Binary
    /// </summary>
    public const string LiteralDataTypesBase64Binary = "http://hl7.org/fhir/data-types#base64Binary";

    /// <summary>
    /// Literal for code: Boolean
    /// </summary>
    public const string LiteralBoolean = "boolean";

    /// <summary>
    /// Literal for code: DataTypesBoolean
    /// </summary>
    public const string LiteralDataTypesBoolean = "http://hl7.org/fhir/data-types#boolean";

    /// <summary>
    /// Literal for code: Code
    /// </summary>
    public const string LiteralCode = "code";

    /// <summary>
    /// Literal for code: DataTypesCode
    /// </summary>
    public const string LiteralDataTypesCode = "http://hl7.org/fhir/data-types#code";

    /// <summary>
    /// Literal for code: CodeableConcept
    /// </summary>
    public const string LiteralCodeableConcept = "CodeableConcept";

    /// <summary>
    /// Literal for code: DataTypesCodeableConcept
    /// </summary>
    public const string LiteralDataTypesCodeableConcept = "http://hl7.org/fhir/data-types#CodeableConcept";

    /// <summary>
    /// Literal for code: Coding
    /// </summary>
    public const string LiteralCoding = "Coding";

    /// <summary>
    /// Literal for code: DataTypesCoding
    /// </summary>
    public const string LiteralDataTypesCoding = "http://hl7.org/fhir/data-types#Coding";

    /// <summary>
    /// Literal for code: ContactPoint
    /// </summary>
    public const string LiteralContactPoint = "ContactPoint";

    /// <summary>
    /// Literal for code: DataTypesContactPoint
    /// </summary>
    public const string LiteralDataTypesContactPoint = "http://hl7.org/fhir/data-types#ContactPoint";

    /// <summary>
    /// Literal for code: Count
    /// </summary>
    public const string LiteralCount = "Count";

    /// <summary>
    /// Literal for code: DataTypesCount
    /// </summary>
    public const string LiteralDataTypesCount = "http://hl7.org/fhir/data-types#Count";

    /// <summary>
    /// Literal for code: Date
    /// </summary>
    public const string LiteralDate = "date";

    /// <summary>
    /// Literal for code: DataTypesDate
    /// </summary>
    public const string LiteralDataTypesDate = "http://hl7.org/fhir/data-types#date";

    /// <summary>
    /// Literal for code: DateTime
    /// </summary>
    public const string LiteralDateTime = "dateTime";

    /// <summary>
    /// Literal for code: DataTypesDateTime
    /// </summary>
    public const string LiteralDataTypesDateTime = "http://hl7.org/fhir/data-types#dateTime";

    /// <summary>
    /// Literal for code: VALDecimal
    /// </summary>
    public const string LiteralVALDecimal = "decimal";

    /// <summary>
    /// Literal for code: DataTypesVALDecimal
    /// </summary>
    public const string LiteralDataTypesVALDecimal = "http://hl7.org/fhir/data-types#decimal";

    /// <summary>
    /// Literal for code: Distance
    /// </summary>
    public const string LiteralDistance = "Distance";

    /// <summary>
    /// Literal for code: DataTypesDistance
    /// </summary>
    public const string LiteralDataTypesDistance = "http://hl7.org/fhir/data-types#Distance";

    /// <summary>
    /// Literal for code: Duration
    /// </summary>
    public const string LiteralDuration = "Duration";

    /// <summary>
    /// Literal for code: DataTypesDuration
    /// </summary>
    public const string LiteralDataTypesDuration = "http://hl7.org/fhir/data-types#Duration";

    /// <summary>
    /// Literal for code: Element
    /// </summary>
    public const string LiteralElement = "Element";

    /// <summary>
    /// Literal for code: DataTypesElement
    /// </summary>
    public const string LiteralDataTypesElement = "http://hl7.org/fhir/data-types#Element";

    /// <summary>
    /// Literal for code: ElementDefinition
    /// </summary>
    public const string LiteralElementDefinition = "ElementDefinition";

    /// <summary>
    /// Literal for code: DataTypesElementDefinition
    /// </summary>
    public const string LiteralDataTypesElementDefinition = "http://hl7.org/fhir/data-types#ElementDefinition";

    /// <summary>
    /// Literal for code: Extension
    /// </summary>
    public const string LiteralExtension = "Extension";

    /// <summary>
    /// Literal for code: DataTypesExtension
    /// </summary>
    public const string LiteralDataTypesExtension = "http://hl7.org/fhir/data-types#Extension";

    /// <summary>
    /// Literal for code: HumanName
    /// </summary>
    public const string LiteralHumanName = "HumanName";

    /// <summary>
    /// Literal for code: DataTypesHumanName
    /// </summary>
    public const string LiteralDataTypesHumanName = "http://hl7.org/fhir/data-types#HumanName";

    /// <summary>
    /// Literal for code: Id
    /// </summary>
    public const string LiteralId = "id";

    /// <summary>
    /// Literal for code: DataTypesId
    /// </summary>
    public const string LiteralDataTypesId = "http://hl7.org/fhir/data-types#id";

    /// <summary>
    /// Literal for code: Identifier
    /// </summary>
    public const string LiteralIdentifier = "Identifier";

    /// <summary>
    /// Literal for code: DataTypesIdentifier
    /// </summary>
    public const string LiteralDataTypesIdentifier = "http://hl7.org/fhir/data-types#Identifier";

    /// <summary>
    /// Literal for code: Instant
    /// </summary>
    public const string LiteralInstant = "instant";

    /// <summary>
    /// Literal for code: DataTypesInstant
    /// </summary>
    public const string LiteralDataTypesInstant = "http://hl7.org/fhir/data-types#instant";

    /// <summary>
    /// Literal for code: Integer
    /// </summary>
    public const string LiteralInteger = "integer";

    /// <summary>
    /// Literal for code: DataTypesInteger
    /// </summary>
    public const string LiteralDataTypesInteger = "http://hl7.org/fhir/data-types#integer";

    /// <summary>
    /// Literal for code: Markdown
    /// </summary>
    public const string LiteralMarkdown = "markdown";

    /// <summary>
    /// Literal for code: DataTypesMarkdown
    /// </summary>
    public const string LiteralDataTypesMarkdown = "http://hl7.org/fhir/data-types#markdown";

    /// <summary>
    /// Literal for code: Meta
    /// </summary>
    public const string LiteralMeta = "Meta";

    /// <summary>
    /// Literal for code: DataTypesMeta
    /// </summary>
    public const string LiteralDataTypesMeta = "http://hl7.org/fhir/data-types#Meta";

    /// <summary>
    /// Literal for code: Money
    /// </summary>
    public const string LiteralMoney = "Money";

    /// <summary>
    /// Literal for code: DataTypesMoney
    /// </summary>
    public const string LiteralDataTypesMoney = "http://hl7.org/fhir/data-types#Money";

    /// <summary>
    /// Literal for code: Narrative
    /// </summary>
    public const string LiteralNarrative = "Narrative";

    /// <summary>
    /// Literal for code: DataTypesNarrative
    /// </summary>
    public const string LiteralDataTypesNarrative = "http://hl7.org/fhir/data-types#Narrative";

    /// <summary>
    /// Literal for code: Oid
    /// </summary>
    public const string LiteralOid = "oid";

    /// <summary>
    /// Literal for code: DataTypesOid
    /// </summary>
    public const string LiteralDataTypesOid = "http://hl7.org/fhir/data-types#oid";

    /// <summary>
    /// Literal for code: Period
    /// </summary>
    public const string LiteralPeriod = "Period";

    /// <summary>
    /// Literal for code: DataTypesPeriod
    /// </summary>
    public const string LiteralDataTypesPeriod = "http://hl7.org/fhir/data-types#Period";

    /// <summary>
    /// Literal for code: PositiveInt
    /// </summary>
    public const string LiteralPositiveInt = "positiveInt";

    /// <summary>
    /// Literal for code: DataTypesPositiveInt
    /// </summary>
    public const string LiteralDataTypesPositiveInt = "http://hl7.org/fhir/data-types#positiveInt";

    /// <summary>
    /// Literal for code: Quantity
    /// </summary>
    public const string LiteralQuantity = "Quantity";

    /// <summary>
    /// Literal for code: DataTypesQuantity
    /// </summary>
    public const string LiteralDataTypesQuantity = "http://hl7.org/fhir/data-types#Quantity";

    /// <summary>
    /// Literal for code: Range
    /// </summary>
    public const string LiteralRange = "Range";

    /// <summary>
    /// Literal for code: DataTypesRange
    /// </summary>
    public const string LiteralDataTypesRange = "http://hl7.org/fhir/data-types#Range";

    /// <summary>
    /// Literal for code: Ratio
    /// </summary>
    public const string LiteralRatio = "Ratio";

    /// <summary>
    /// Literal for code: DataTypesRatio
    /// </summary>
    public const string LiteralDataTypesRatio = "http://hl7.org/fhir/data-types#Ratio";

    /// <summary>
    /// Literal for code: Reference
    /// </summary>
    public const string LiteralReference = "Reference";

    /// <summary>
    /// Literal for code: DataTypesReference
    /// </summary>
    public const string LiteralDataTypesReference = "http://hl7.org/fhir/data-types#Reference";

    /// <summary>
    /// Literal for code: SampledData
    /// </summary>
    public const string LiteralSampledData = "SampledData";

    /// <summary>
    /// Literal for code: DataTypesSampledData
    /// </summary>
    public const string LiteralDataTypesSampledData = "http://hl7.org/fhir/data-types#SampledData";

    /// <summary>
    /// Literal for code: Signature
    /// </summary>
    public const string LiteralSignature = "Signature";

    /// <summary>
    /// Literal for code: DataTypesSignature
    /// </summary>
    public const string LiteralDataTypesSignature = "http://hl7.org/fhir/data-types#Signature";

    /// <summary>
    /// Literal for code: SimpleQuantity
    /// </summary>
    public const string LiteralSimpleQuantity = "SimpleQuantity";

    /// <summary>
    /// Literal for code: DataTypesSimpleQuantity
    /// </summary>
    public const string LiteralDataTypesSimpleQuantity = "http://hl7.org/fhir/data-types#SimpleQuantity";

    /// <summary>
    /// Literal for code: VALString
    /// </summary>
    public const string LiteralVALString = "string";

    /// <summary>
    /// Literal for code: DataTypesVALString
    /// </summary>
    public const string LiteralDataTypesVALString = "http://hl7.org/fhir/data-types#string";

    /// <summary>
    /// Literal for code: Time
    /// </summary>
    public const string LiteralTime = "time";

    /// <summary>
    /// Literal for code: DataTypesTime
    /// </summary>
    public const string LiteralDataTypesTime = "http://hl7.org/fhir/data-types#time";

    /// <summary>
    /// Literal for code: Timing
    /// </summary>
    public const string LiteralTiming = "Timing";

    /// <summary>
    /// Literal for code: DataTypesTiming
    /// </summary>
    public const string LiteralDataTypesTiming = "http://hl7.org/fhir/data-types#Timing";

    /// <summary>
    /// Literal for code: UnsignedInt
    /// </summary>
    public const string LiteralUnsignedInt = "unsignedInt";

    /// <summary>
    /// Literal for code: DataTypesUnsignedInt
    /// </summary>
    public const string LiteralDataTypesUnsignedInt = "http://hl7.org/fhir/data-types#unsignedInt";

    /// <summary>
    /// Literal for code: Uri
    /// </summary>
    public const string LiteralUri = "uri";

    /// <summary>
    /// Literal for code: DataTypesUri
    /// </summary>
    public const string LiteralDataTypesUri = "http://hl7.org/fhir/data-types#uri";

    /// <summary>
    /// Literal for code: Uuid
    /// </summary>
    public const string LiteralUuid = "uuid";

    /// <summary>
    /// Literal for code: DataTypesUuid
    /// </summary>
    public const string LiteralDataTypesUuid = "http://hl7.org/fhir/data-types#uuid";

    /// <summary>
    /// Literal for code: XHTML
    /// </summary>
    public const string LiteralXHTML = "xhtml";

    /// <summary>
    /// Literal for code: DataTypesXHTML
    /// </summary>
    public const string LiteralDataTypesXHTML = "http://hl7.org/fhir/data-types#xhtml";

    /// <summary>
    /// Dictionary for looking up DataTypes Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "Address", Address }, 
      { "http://hl7.org/fhir/data-types#Address", Address }, 
      { "Age", Age }, 
      { "http://hl7.org/fhir/data-types#Age", Age }, 
      { "Annotation", Annotation }, 
      { "http://hl7.org/fhir/data-types#Annotation", Annotation }, 
      { "Attachment", Attachment }, 
      { "http://hl7.org/fhir/data-types#Attachment", Attachment }, 
      { "BackboneElement", BackboneElement }, 
      { "http://hl7.org/fhir/data-types#BackboneElement", BackboneElement }, 
      { "base64Binary", Base64Binary }, 
      { "http://hl7.org/fhir/data-types#base64Binary", Base64Binary }, 
      { "boolean", Boolean }, 
      { "http://hl7.org/fhir/data-types#boolean", Boolean }, 
      { "code", Code }, 
      { "http://hl7.org/fhir/data-types#code", Code }, 
      { "CodeableConcept", CodeableConcept }, 
      { "http://hl7.org/fhir/data-types#CodeableConcept", CodeableConcept }, 
      { "Coding", Coding }, 
      { "http://hl7.org/fhir/data-types#Coding", Coding }, 
      { "ContactPoint", ContactPoint }, 
      { "http://hl7.org/fhir/data-types#ContactPoint", ContactPoint }, 
      { "Count", Count }, 
      { "http://hl7.org/fhir/data-types#Count", Count }, 
      { "date", Date }, 
      { "http://hl7.org/fhir/data-types#date", Date }, 
      { "dateTime", DateTime }, 
      { "http://hl7.org/fhir/data-types#dateTime", DateTime }, 
      { "decimal", VALDecimal }, 
      { "http://hl7.org/fhir/data-types#decimal", VALDecimal }, 
      { "Distance", Distance }, 
      { "http://hl7.org/fhir/data-types#Distance", Distance }, 
      { "Duration", Duration }, 
      { "http://hl7.org/fhir/data-types#Duration", Duration }, 
      { "Element", Element }, 
      { "http://hl7.org/fhir/data-types#Element", Element }, 
      { "ElementDefinition", ElementDefinition }, 
      { "http://hl7.org/fhir/data-types#ElementDefinition", ElementDefinition }, 
      { "Extension", Extension }, 
      { "http://hl7.org/fhir/data-types#Extension", Extension }, 
      { "HumanName", HumanName }, 
      { "http://hl7.org/fhir/data-types#HumanName", HumanName }, 
      { "id", Id }, 
      { "http://hl7.org/fhir/data-types#id", Id }, 
      { "Identifier", Identifier }, 
      { "http://hl7.org/fhir/data-types#Identifier", Identifier }, 
      { "instant", Instant }, 
      { "http://hl7.org/fhir/data-types#instant", Instant }, 
      { "integer", Integer }, 
      { "http://hl7.org/fhir/data-types#integer", Integer }, 
      { "markdown", Markdown }, 
      { "http://hl7.org/fhir/data-types#markdown", Markdown }, 
      { "Meta", Meta }, 
      { "http://hl7.org/fhir/data-types#Meta", Meta }, 
      { "Money", Money }, 
      { "http://hl7.org/fhir/data-types#Money", Money }, 
      { "Narrative", Narrative }, 
      { "http://hl7.org/fhir/data-types#Narrative", Narrative }, 
      { "oid", Oid }, 
      { "http://hl7.org/fhir/data-types#oid", Oid }, 
      { "Period", Period }, 
      { "http://hl7.org/fhir/data-types#Period", Period }, 
      { "positiveInt", PositiveInt }, 
      { "http://hl7.org/fhir/data-types#positiveInt", PositiveInt }, 
      { "Quantity", Quantity }, 
      { "http://hl7.org/fhir/data-types#Quantity", Quantity }, 
      { "Range", Range }, 
      { "http://hl7.org/fhir/data-types#Range", Range }, 
      { "Ratio", Ratio }, 
      { "http://hl7.org/fhir/data-types#Ratio", Ratio }, 
      { "Reference", Reference }, 
      { "http://hl7.org/fhir/data-types#Reference", Reference }, 
      { "SampledData", SampledData }, 
      { "http://hl7.org/fhir/data-types#SampledData", SampledData }, 
      { "Signature", Signature }, 
      { "http://hl7.org/fhir/data-types#Signature", Signature }, 
      { "SimpleQuantity", SimpleQuantity }, 
      { "http://hl7.org/fhir/data-types#SimpleQuantity", SimpleQuantity }, 
      { "string", VALString }, 
      { "http://hl7.org/fhir/data-types#string", VALString }, 
      { "time", Time }, 
      { "http://hl7.org/fhir/data-types#time", Time }, 
      { "Timing", Timing }, 
      { "http://hl7.org/fhir/data-types#Timing", Timing }, 
      { "unsignedInt", UnsignedInt }, 
      { "http://hl7.org/fhir/data-types#unsignedInt", UnsignedInt }, 
      { "uri", Uri }, 
      { "http://hl7.org/fhir/data-types#uri", Uri }, 
      { "uuid", Uuid }, 
      { "http://hl7.org/fhir/data-types#uuid", Uuid }, 
      { "xhtml", XHTML }, 
      { "http://hl7.org/fhir/data-types#xhtml", XHTML }, 
    };
  };
}
