// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Xml;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// Optional Extension Element - found in all resources.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<Extension>))]
  public class Extension : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// The definition may point directly to a computable or human-readable definition of the extensibility codes, or it may be a logical URI as declared in some other specification. The definition SHALL be a URI for the Structure Definition defining the extension.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public byte[] ValueBase64Binary { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public bool? ValueBoolean { get; set; }
    /// <summary>
    /// Extension container element for ValueBoolean
    /// </summary>
    public Element _ValueBoolean { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueCanonical { get; set; }
    /// <summary>
    /// Extension container element for ValueCanonical
    /// </summary>
    public Element _ValueCanonical { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueCode { get; set; }
    /// <summary>
    /// Extension container element for ValueCode
    /// </summary>
    public Element _ValueCode { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueDate { get; set; }
    /// <summary>
    /// Extension container element for ValueDate
    /// </summary>
    public Element _ValueDate { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueDateTime { get; set; }
    /// <summary>
    /// Extension container element for ValueDateTime
    /// </summary>
    public Element _ValueDateTime { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public decimal? ValueDecimal { get; set; }
    /// <summary>
    /// Extension container element for ValueDecimal
    /// </summary>
    public Element _ValueDecimal { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueId { get; set; }
    /// <summary>
    /// Extension container element for ValueId
    /// </summary>
    public Element _ValueId { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueInstant { get; set; }
    /// <summary>
    /// Extension container element for ValueInstant
    /// </summary>
    public Element _ValueInstant { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public int? ValueInteger { get; set; }
    /// <summary>
    /// Extension container element for ValueInteger
    /// </summary>
    public Element _ValueInteger { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public long? ValueInteger64 { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueMarkdown { get; set; }
    /// <summary>
    /// Extension container element for ValueMarkdown
    /// </summary>
    public Element _ValueMarkdown { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueOid { get; set; }
    /// <summary>
    /// Extension container element for ValueOid
    /// </summary>
    public Element _ValueOid { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public uint? ValuePositiveInt { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueString { get; set; }
    /// <summary>
    /// Extension container element for ValueString
    /// </summary>
    public Element _ValueString { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueTime { get; set; }
    /// <summary>
    /// Extension container element for ValueTime
    /// </summary>
    public Element _ValueTime { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public uint? ValueUnsignedInt { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueUri { get; set; }
    /// <summary>
    /// Extension container element for ValueUri
    /// </summary>
    public Element _ValueUri { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public string ValueUrl { get; set; }
    /// <summary>
    /// Extension container element for ValueUrl
    /// </summary>
    public Element _ValueUrl { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Guid? ValueUuid { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Address ValueAddress { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Age ValueAge { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Annotation ValueAnnotation { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Attachment ValueAttachment { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public CodeableConcept ValueCodeableConcept { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public CodeableReference ValueCodeableReference { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Coding ValueCoding { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public ContactPoint ValueContactPoint { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Count ValueCount { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Distance ValueDistance { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Duration ValueDuration { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public HumanName ValueHumanName { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Identifier ValueIdentifier { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Money ValueMoney { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Period ValuePeriod { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Quantity ValueQuantity { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Range ValueRange { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Ratio ValueRatio { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public RatioRange ValueRatioRange { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Reference ValueReference { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public SampledData ValueSampledData { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Signature ValueSignature { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Timing ValueTiming { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public ContactDetail ValueContactDetail { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public DataRequirement ValueDataRequirement { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Expression ValueExpression { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public ParameterDefinition ValueParameterDefinition { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public RelatedArtifact ValueRelatedArtifact { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public TriggerDefinition ValueTriggerDefinition { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public UsageContext ValueUsageContext { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Availability ValueAvailability { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public ExtendedContactDetail ValueExtendedContactDetail { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Dosage ValueDosage { get; set; }
    /// <summary>
    /// Value of extension - must be one of a constrained set of the data types (see [Extensibility](extensibility.html) for a list).
    /// </summary>
    public Meta ValueMeta { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.DataType)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Url))
      {
        writer.WriteString("url", (string)Url!);
      }

      if (_Url != null)
      {
        writer.WritePropertyName("_url");
        _Url.SerializeJson(writer, options);
      }

      if (ValueBase64Binary != null)
      {
        writer.WriteString("valueBase64Binary", System.Convert.ToBase64String(ValueBase64Binary));
      }

      if (ValueBoolean != null)
      {
        writer.WriteBoolean("valueBoolean", (bool)ValueBoolean!);
      }

      if (_ValueBoolean != null)
      {
        writer.WritePropertyName("_valueBoolean");
        _ValueBoolean.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueCanonical))
      {
        writer.WriteString("valueCanonical", (string)ValueCanonical!);
      }

      if (_ValueCanonical != null)
      {
        writer.WritePropertyName("_valueCanonical");
        _ValueCanonical.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueCode))
      {
        writer.WriteString("valueCode", (string)ValueCode!);
      }

      if (_ValueCode != null)
      {
        writer.WritePropertyName("_valueCode");
        _ValueCode.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueDate))
      {
        writer.WriteString("valueDate", (string)ValueDate!);
      }

      if (_ValueDate != null)
      {
        writer.WritePropertyName("_valueDate");
        _ValueDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueDateTime))
      {
        writer.WriteString("valueDateTime", (string)ValueDateTime!);
      }

      if (_ValueDateTime != null)
      {
        writer.WritePropertyName("_valueDateTime");
        _ValueDateTime.SerializeJson(writer, options);
      }

      if (ValueDecimal != null)
      {
        writer.WriteNumber("valueDecimal", (decimal)ValueDecimal!);
      }

      if (_ValueDecimal != null)
      {
        writer.WritePropertyName("_valueDecimal");
        _ValueDecimal.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueId))
      {
        writer.WriteString("valueId", (string)ValueId!);
      }

      if (_ValueId != null)
      {
        writer.WritePropertyName("_valueId");
        _ValueId.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueInstant))
      {
        writer.WriteString("valueInstant", (string)ValueInstant!);
      }

      if (_ValueInstant != null)
      {
        writer.WritePropertyName("_valueInstant");
        _ValueInstant.SerializeJson(writer, options);
      }

      if (ValueInteger != null)
      {
        writer.WriteNumber("valueInteger", (int)ValueInteger!);
      }

      if (_ValueInteger != null)
      {
        writer.WritePropertyName("_valueInteger");
        _ValueInteger.SerializeJson(writer, options);
      }

      if (ValueInteger64 != null)
      {
        writer.WriteString("valueInteger64", ValueInteger64.ToString());
      }

      if (!string.IsNullOrEmpty(ValueMarkdown))
      {
        writer.WriteString("valueMarkdown", (string)ValueMarkdown!);
      }

      if (_ValueMarkdown != null)
      {
        writer.WritePropertyName("_valueMarkdown");
        _ValueMarkdown.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueOid))
      {
        writer.WriteString("valueOid", (string)ValueOid!);
      }

      if (_ValueOid != null)
      {
        writer.WritePropertyName("_valueOid");
        _ValueOid.SerializeJson(writer, options);
      }

      if (ValuePositiveInt != null)
      {
        writer.WriteNumber("valuePositiveInt", (uint)ValuePositiveInt!);
      }

      if (!string.IsNullOrEmpty(ValueString))
      {
        writer.WriteString("valueString", (string)ValueString!);
      }

      if (_ValueString != null)
      {
        writer.WritePropertyName("_valueString");
        _ValueString.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueTime))
      {
        writer.WriteString("valueTime", (string)ValueTime!);
      }

      if (_ValueTime != null)
      {
        writer.WritePropertyName("_valueTime");
        _ValueTime.SerializeJson(writer, options);
      }

      if (ValueUnsignedInt != null)
      {
        writer.WriteNumber("valueUnsignedInt", (uint)ValueUnsignedInt!);
      }

      if (!string.IsNullOrEmpty(ValueUri))
      {
        writer.WriteString("valueUri", (string)ValueUri!);
      }

      if (_ValueUri != null)
      {
        writer.WritePropertyName("_valueUri");
        _ValueUri.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValueUrl))
      {
        writer.WriteString("valueUrl", (string)ValueUrl!);
      }

      if (_ValueUrl != null)
      {
        writer.WritePropertyName("_valueUrl");
        _ValueUrl.SerializeJson(writer, options);
      }

      if (ValueUuid != null)
      {
        writer.WriteString("valueUuid", (Guid)ValueUuid!);
      }

      if (ValueAddress != null)
      {
        writer.WritePropertyName("valueAddress");
        ValueAddress.SerializeJson(writer, options);
      }

      if (ValueAge != null)
      {
        writer.WritePropertyName("valueAge");
        ValueAge.SerializeJson(writer, options);
      }

      if (ValueAnnotation != null)
      {
        writer.WritePropertyName("valueAnnotation");
        ValueAnnotation.SerializeJson(writer, options);
      }

      if (ValueAttachment != null)
      {
        writer.WritePropertyName("valueAttachment");
        ValueAttachment.SerializeJson(writer, options);
      }

      if (ValueCodeableConcept != null)
      {
        writer.WritePropertyName("valueCodeableConcept");
        ValueCodeableConcept.SerializeJson(writer, options);
      }

      if (ValueCodeableReference != null)
      {
        writer.WritePropertyName("valueCodeableReference");
        ValueCodeableReference.SerializeJson(writer, options);
      }

      if (ValueCoding != null)
      {
        writer.WritePropertyName("valueCoding");
        ValueCoding.SerializeJson(writer, options);
      }

      if (ValueContactPoint != null)
      {
        writer.WritePropertyName("valueContactPoint");
        ValueContactPoint.SerializeJson(writer, options);
      }

      if (ValueCount != null)
      {
        writer.WritePropertyName("valueCount");
        ValueCount.SerializeJson(writer, options);
      }

      if (ValueDistance != null)
      {
        writer.WritePropertyName("valueDistance");
        ValueDistance.SerializeJson(writer, options);
      }

      if (ValueDuration != null)
      {
        writer.WritePropertyName("valueDuration");
        ValueDuration.SerializeJson(writer, options);
      }

      if (ValueHumanName != null)
      {
        writer.WritePropertyName("valueHumanName");
        ValueHumanName.SerializeJson(writer, options);
      }

      if (ValueIdentifier != null)
      {
        writer.WritePropertyName("valueIdentifier");
        ValueIdentifier.SerializeJson(writer, options);
      }

      if (ValueMoney != null)
      {
        writer.WritePropertyName("valueMoney");
        ValueMoney.SerializeJson(writer, options);
      }

      if (ValuePeriod != null)
      {
        writer.WritePropertyName("valuePeriod");
        ValuePeriod.SerializeJson(writer, options);
      }

      if (ValueQuantity != null)
      {
        writer.WritePropertyName("valueQuantity");
        ValueQuantity.SerializeJson(writer, options);
      }

      if (ValueRange != null)
      {
        writer.WritePropertyName("valueRange");
        ValueRange.SerializeJson(writer, options);
      }

      if (ValueRatio != null)
      {
        writer.WritePropertyName("valueRatio");
        ValueRatio.SerializeJson(writer, options);
      }

      if (ValueRatioRange != null)
      {
        writer.WritePropertyName("valueRatioRange");
        ValueRatioRange.SerializeJson(writer, options);
      }

      if (ValueReference != null)
      {
        writer.WritePropertyName("valueReference");
        ValueReference.SerializeJson(writer, options);
      }

      if (ValueSampledData != null)
      {
        writer.WritePropertyName("valueSampledData");
        ValueSampledData.SerializeJson(writer, options);
      }

      if (ValueSignature != null)
      {
        writer.WritePropertyName("valueSignature");
        ValueSignature.SerializeJson(writer, options);
      }

      if (ValueTiming != null)
      {
        writer.WritePropertyName("valueTiming");
        ValueTiming.SerializeJson(writer, options);
      }

      if (ValueContactDetail != null)
      {
        writer.WritePropertyName("valueContactDetail");
        ValueContactDetail.SerializeJson(writer, options);
      }

      if (ValueDataRequirement != null)
      {
        writer.WritePropertyName("valueDataRequirement");
        ValueDataRequirement.SerializeJson(writer, options);
      }

      if (ValueExpression != null)
      {
        writer.WritePropertyName("valueExpression");
        ValueExpression.SerializeJson(writer, options);
      }

      if (ValueParameterDefinition != null)
      {
        writer.WritePropertyName("valueParameterDefinition");
        ValueParameterDefinition.SerializeJson(writer, options);
      }

      if (ValueRelatedArtifact != null)
      {
        writer.WritePropertyName("valueRelatedArtifact");
        ValueRelatedArtifact.SerializeJson(writer, options);
      }

      if (ValueTriggerDefinition != null)
      {
        writer.WritePropertyName("valueTriggerDefinition");
        ValueTriggerDefinition.SerializeJson(writer, options);
      }

      if (ValueUsageContext != null)
      {
        writer.WritePropertyName("valueUsageContext");
        ValueUsageContext.SerializeJson(writer, options);
      }

      if (ValueAvailability != null)
      {
        writer.WritePropertyName("valueAvailability");
        ValueAvailability.SerializeJson(writer, options);
      }

      if (ValueExtendedContactDetail != null)
      {
        writer.WritePropertyName("valueExtendedContactDetail");
        ValueExtendedContactDetail.SerializeJson(writer, options);
      }

      if (ValueDosage != null)
      {
        writer.WritePropertyName("valueDosage");
        ValueDosage.SerializeJson(writer, options);
      }

      if (ValueMeta != null)
      {
        writer.WritePropertyName("valueMeta");
        ValueMeta.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR5.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "valueBase64Binary":
          ValueBase64Binary = System.Convert.FromBase64String(reader.GetString());
          break;

        case "valueBoolean":
          ValueBoolean = reader.GetBoolean();
          break;

        case "_valueBoolean":
          _ValueBoolean = new fhirCsR5.Models.Element();
          _ValueBoolean.DeserializeJson(ref reader, options);
          break;

        case "valueCanonical":
          ValueCanonical = reader.GetString();
          break;

        case "_valueCanonical":
          _ValueCanonical = new fhirCsR5.Models.Element();
          _ValueCanonical.DeserializeJson(ref reader, options);
          break;

        case "valueCode":
          ValueCode = reader.GetString();
          break;

        case "_valueCode":
          _ValueCode = new fhirCsR5.Models.Element();
          _ValueCode.DeserializeJson(ref reader, options);
          break;

        case "valueDate":
          ValueDate = reader.GetString();
          break;

        case "_valueDate":
          _ValueDate = new fhirCsR5.Models.Element();
          _ValueDate.DeserializeJson(ref reader, options);
          break;

        case "valueDateTime":
          ValueDateTime = reader.GetString();
          break;

        case "_valueDateTime":
          _ValueDateTime = new fhirCsR5.Models.Element();
          _ValueDateTime.DeserializeJson(ref reader, options);
          break;

        case "valueDecimal":
          ValueDecimal = reader.GetDecimal();
          break;

        case "_valueDecimal":
          _ValueDecimal = new fhirCsR5.Models.Element();
          _ValueDecimal.DeserializeJson(ref reader, options);
          break;

        case "valueId":
          ValueId = reader.GetString();
          break;

        case "_valueId":
          _ValueId = new fhirCsR5.Models.Element();
          _ValueId.DeserializeJson(ref reader, options);
          break;

        case "valueInstant":
          ValueInstant = reader.GetString();
          break;

        case "_valueInstant":
          _ValueInstant = new fhirCsR5.Models.Element();
          _ValueInstant.DeserializeJson(ref reader, options);
          break;

        case "valueInteger":
          ValueInteger = reader.GetInt32();
          break;

        case "_valueInteger":
          _ValueInteger = new fhirCsR5.Models.Element();
          _ValueInteger.DeserializeJson(ref reader, options);
          break;

        case "valueInteger64":
          string strValValueInteger64 = reader.GetString();
          if (long.TryParse(strValValueInteger64, out long longValValueInteger64))
          {
            ValueInteger64 = longValValueInteger64;
          }

          break;

        case "valueMarkdown":
          ValueMarkdown = reader.GetString();
          break;

        case "_valueMarkdown":
          _ValueMarkdown = new fhirCsR5.Models.Element();
          _ValueMarkdown.DeserializeJson(ref reader, options);
          break;

        case "valueOid":
          ValueOid = reader.GetString();
          break;

        case "_valueOid":
          _ValueOid = new fhirCsR5.Models.Element();
          _ValueOid.DeserializeJson(ref reader, options);
          break;

        case "valuePositiveInt":
          ValuePositiveInt = reader.GetUInt32();
          break;

        case "valueString":
          ValueString = reader.GetString();
          break;

        case "_valueString":
          _ValueString = new fhirCsR5.Models.Element();
          _ValueString.DeserializeJson(ref reader, options);
          break;

        case "valueTime":
          ValueTime = reader.GetString();
          break;

        case "_valueTime":
          _ValueTime = new fhirCsR5.Models.Element();
          _ValueTime.DeserializeJson(ref reader, options);
          break;

        case "valueUnsignedInt":
          ValueUnsignedInt = reader.GetUInt32();
          break;

        case "valueUri":
          ValueUri = reader.GetString();
          break;

        case "_valueUri":
          _ValueUri = new fhirCsR5.Models.Element();
          _ValueUri.DeserializeJson(ref reader, options);
          break;

        case "valueUrl":
          ValueUrl = reader.GetString();
          break;

        case "_valueUrl":
          _ValueUrl = new fhirCsR5.Models.Element();
          _ValueUrl.DeserializeJson(ref reader, options);
          break;

        case "valueUuid":
          ValueUuid = reader.GetGuid();
          break;

        case "valueAddress":
          ValueAddress = new fhirCsR5.Models.Address();
          ValueAddress.DeserializeJson(ref reader, options);
          break;

        case "valueAge":
          ValueAge = new fhirCsR5.Models.Age();
          ValueAge.DeserializeJson(ref reader, options);
          break;

        case "valueAnnotation":
          ValueAnnotation = new fhirCsR5.Models.Annotation();
          ValueAnnotation.DeserializeJson(ref reader, options);
          break;

        case "valueAttachment":
          ValueAttachment = new fhirCsR5.Models.Attachment();
          ValueAttachment.DeserializeJson(ref reader, options);
          break;

        case "valueCodeableConcept":
          ValueCodeableConcept = new fhirCsR5.Models.CodeableConcept();
          ValueCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "valueCodeableReference":
          ValueCodeableReference = new fhirCsR5.Models.CodeableReference();
          ValueCodeableReference.DeserializeJson(ref reader, options);
          break;

        case "valueCoding":
          ValueCoding = new fhirCsR5.Models.Coding();
          ValueCoding.DeserializeJson(ref reader, options);
          break;

        case "valueContactPoint":
          ValueContactPoint = new fhirCsR5.Models.ContactPoint();
          ValueContactPoint.DeserializeJson(ref reader, options);
          break;

        case "valueCount":
          ValueCount = new fhirCsR5.Models.Count();
          ValueCount.DeserializeJson(ref reader, options);
          break;

        case "valueDistance":
          ValueDistance = new fhirCsR5.Models.Distance();
          ValueDistance.DeserializeJson(ref reader, options);
          break;

        case "valueDuration":
          ValueDuration = new fhirCsR5.Models.Duration();
          ValueDuration.DeserializeJson(ref reader, options);
          break;

        case "valueHumanName":
          ValueHumanName = new fhirCsR5.Models.HumanName();
          ValueHumanName.DeserializeJson(ref reader, options);
          break;

        case "valueIdentifier":
          ValueIdentifier = new fhirCsR5.Models.Identifier();
          ValueIdentifier.DeserializeJson(ref reader, options);
          break;

        case "valueMoney":
          ValueMoney = new fhirCsR5.Models.Money();
          ValueMoney.DeserializeJson(ref reader, options);
          break;

        case "valuePeriod":
          ValuePeriod = new fhirCsR5.Models.Period();
          ValuePeriod.DeserializeJson(ref reader, options);
          break;

        case "valueQuantity":
          ValueQuantity = new fhirCsR5.Models.Quantity();
          ValueQuantity.DeserializeJson(ref reader, options);
          break;

        case "valueRange":
          ValueRange = new fhirCsR5.Models.Range();
          ValueRange.DeserializeJson(ref reader, options);
          break;

        case "valueRatio":
          ValueRatio = new fhirCsR5.Models.Ratio();
          ValueRatio.DeserializeJson(ref reader, options);
          break;

        case "valueRatioRange":
          ValueRatioRange = new fhirCsR5.Models.RatioRange();
          ValueRatioRange.DeserializeJson(ref reader, options);
          break;

        case "valueReference":
          ValueReference = new fhirCsR5.Models.Reference();
          ValueReference.DeserializeJson(ref reader, options);
          break;

        case "valueSampledData":
          ValueSampledData = new fhirCsR5.Models.SampledData();
          ValueSampledData.DeserializeJson(ref reader, options);
          break;

        case "valueSignature":
          ValueSignature = new fhirCsR5.Models.Signature();
          ValueSignature.DeserializeJson(ref reader, options);
          break;

        case "valueTiming":
          ValueTiming = new fhirCsR5.Models.Timing();
          ValueTiming.DeserializeJson(ref reader, options);
          break;

        case "valueContactDetail":
          ValueContactDetail = new fhirCsR5.Models.ContactDetail();
          ValueContactDetail.DeserializeJson(ref reader, options);
          break;

        case "valueDataRequirement":
          ValueDataRequirement = new fhirCsR5.Models.DataRequirement();
          ValueDataRequirement.DeserializeJson(ref reader, options);
          break;

        case "valueExpression":
          ValueExpression = new fhirCsR5.Models.Expression();
          ValueExpression.DeserializeJson(ref reader, options);
          break;

        case "valueParameterDefinition":
          ValueParameterDefinition = new fhirCsR5.Models.ParameterDefinition();
          ValueParameterDefinition.DeserializeJson(ref reader, options);
          break;

        case "valueRelatedArtifact":
          ValueRelatedArtifact = new fhirCsR5.Models.RelatedArtifact();
          ValueRelatedArtifact.DeserializeJson(ref reader, options);
          break;

        case "valueTriggerDefinition":
          ValueTriggerDefinition = new fhirCsR5.Models.TriggerDefinition();
          ValueTriggerDefinition.DeserializeJson(ref reader, options);
          break;

        case "valueUsageContext":
          ValueUsageContext = new fhirCsR5.Models.UsageContext();
          ValueUsageContext.DeserializeJson(ref reader, options);
          break;

        case "valueAvailability":
          ValueAvailability = new fhirCsR5.Models.Availability();
          ValueAvailability.DeserializeJson(ref reader, options);
          break;

        case "valueExtendedContactDetail":
          ValueExtendedContactDetail = new fhirCsR5.Models.ExtendedContactDetail();
          ValueExtendedContactDetail.DeserializeJson(ref reader, options);
          break;

        case "valueDosage":
          ValueDosage = new fhirCsR5.Models.Dosage();
          ValueDosage.DeserializeJson(ref reader, options);
          break;

        case "valueMeta":
          ValueMeta = new fhirCsR5.Models.Meta();
          ValueMeta.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DataType)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
}
