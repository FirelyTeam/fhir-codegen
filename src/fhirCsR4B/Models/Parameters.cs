// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4B.Serialization;

namespace fhirCsR4B.Models
{
  /// <summary>
  /// A parameter passed to or received from the operation.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<ParametersParameter>))]
  public class ParametersParameter : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The name of the parameter (reference to the operation definition).
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Only one level of nested parameters is allowed.
    /// </summary>
    public List<ParametersParameter> Part { get; set; }
    /// <summary>
    /// When resolving references in resources, the operation definition may specify how references may be resolved between parameters. If a reference cannot be resolved between the parameters, the application should fall back to it's general resource resolution methods.
    /// </summary>
    public Resource Resource { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public byte[] ValueBase64Binary { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public bool? ValueBoolean { get; set; }
    /// <summary>
    /// Extension container element for ValueBoolean
    /// </summary>
    public Element _ValueBoolean { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueCanonical { get; set; }
    /// <summary>
    /// Extension container element for ValueCanonical
    /// </summary>
    public Element _ValueCanonical { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueCode { get; set; }
    /// <summary>
    /// Extension container element for ValueCode
    /// </summary>
    public Element _ValueCode { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueDate { get; set; }
    /// <summary>
    /// Extension container element for ValueDate
    /// </summary>
    public Element _ValueDate { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueDateTime { get; set; }
    /// <summary>
    /// Extension container element for ValueDateTime
    /// </summary>
    public Element _ValueDateTime { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public decimal? ValueDecimal { get; set; }
    /// <summary>
    /// Extension container element for ValueDecimal
    /// </summary>
    public Element _ValueDecimal { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueId { get; set; }
    /// <summary>
    /// Extension container element for ValueId
    /// </summary>
    public Element _ValueId { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueInstant { get; set; }
    /// <summary>
    /// Extension container element for ValueInstant
    /// </summary>
    public Element _ValueInstant { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public int? ValueInteger { get; set; }
    /// <summary>
    /// Extension container element for ValueInteger
    /// </summary>
    public Element _ValueInteger { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueMarkdown { get; set; }
    /// <summary>
    /// Extension container element for ValueMarkdown
    /// </summary>
    public Element _ValueMarkdown { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueOid { get; set; }
    /// <summary>
    /// Extension container element for ValueOid
    /// </summary>
    public Element _ValueOid { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public uint? ValuePositiveInt { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueString { get; set; }
    /// <summary>
    /// Extension container element for ValueString
    /// </summary>
    public Element _ValueString { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueTime { get; set; }
    /// <summary>
    /// Extension container element for ValueTime
    /// </summary>
    public Element _ValueTime { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public uint? ValueUnsignedInt { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueUri { get; set; }
    /// <summary>
    /// Extension container element for ValueUri
    /// </summary>
    public Element _ValueUri { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public string ValueUrl { get; set; }
    /// <summary>
    /// Extension container element for ValueUrl
    /// </summary>
    public Element _ValueUrl { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Guid? ValueUuid { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Address ValueAddress { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Age ValueAge { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Annotation ValueAnnotation { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Attachment ValueAttachment { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public CodeableConcept ValueCodeableConcept { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Coding ValueCoding { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public ContactPoint ValueContactPoint { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Count ValueCount { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Distance ValueDistance { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Duration ValueDuration { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public HumanName ValueHumanName { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Identifier ValueIdentifier { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Money ValueMoney { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Period ValuePeriod { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Quantity ValueQuantity { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Range ValueRange { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Ratio ValueRatio { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Reference ValueReference { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public SampledData ValueSampledData { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Signature ValueSignature { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Timing ValueTiming { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public ContactDetail ValueContactDetail { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Contributor ValueContributor { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public DataRequirement ValueDataRequirement { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Expression ValueExpression { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public ParameterDefinition ValueParameterDefinition { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public RelatedArtifact ValueRelatedArtifact { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public TriggerDefinition ValueTriggerDefinition { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public UsageContext ValueUsageContext { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
    /// </summary>
    public Dosage ValueDosage { get; set; }
    /// <summary>
    /// Conveys the content if the parameter is a data type.
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
      ((fhirCsR4B.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
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

      if (ValueContributor != null)
      {
        writer.WritePropertyName("valueContributor");
        ValueContributor.SerializeJson(writer, options);
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

      if (Resource != null)
      {
        writer.WritePropertyName("resource");
        JsonSerializer.Serialize<fhirCsR4B.Models.Resource>(writer, (fhirCsR4B.Models.Resource)Resource, options);
      }

      if ((Part != null) && (Part.Count != 0))
      {
        writer.WritePropertyName("part");
        writer.WriteStartArray();

        foreach (ParametersParameter valPart in Part)
        {
          valPart.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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
        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR4B.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "part":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Part = new List<ParametersParameter>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.ParametersParameter objPart = new fhirCsR4B.Models.ParametersParameter();
            objPart.DeserializeJson(ref reader, options);
            Part.Add(objPart);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Part.Count == 0)
          {
            Part = null;
          }

          break;

        case "resource":
          Resource = JsonSerializer.Deserialize<fhirCsR4B.Models.Resource>(ref reader, options);
          break;

        case "valueBase64Binary":
          ValueBase64Binary = System.Convert.FromBase64String(reader.GetString());
          break;

        case "valueBoolean":
          ValueBoolean = reader.GetBoolean();
          break;

        case "_valueBoolean":
          _ValueBoolean = new fhirCsR4B.Models.Element();
          _ValueBoolean.DeserializeJson(ref reader, options);
          break;

        case "valueCanonical":
          ValueCanonical = reader.GetString();
          break;

        case "_valueCanonical":
          _ValueCanonical = new fhirCsR4B.Models.Element();
          _ValueCanonical.DeserializeJson(ref reader, options);
          break;

        case "valueCode":
          ValueCode = reader.GetString();
          break;

        case "_valueCode":
          _ValueCode = new fhirCsR4B.Models.Element();
          _ValueCode.DeserializeJson(ref reader, options);
          break;

        case "valueDate":
          ValueDate = reader.GetString();
          break;

        case "_valueDate":
          _ValueDate = new fhirCsR4B.Models.Element();
          _ValueDate.DeserializeJson(ref reader, options);
          break;

        case "valueDateTime":
          ValueDateTime = reader.GetString();
          break;

        case "_valueDateTime":
          _ValueDateTime = new fhirCsR4B.Models.Element();
          _ValueDateTime.DeserializeJson(ref reader, options);
          break;

        case "valueDecimal":
          ValueDecimal = reader.GetDecimal();
          break;

        case "_valueDecimal":
          _ValueDecimal = new fhirCsR4B.Models.Element();
          _ValueDecimal.DeserializeJson(ref reader, options);
          break;

        case "valueId":
          ValueId = reader.GetString();
          break;

        case "_valueId":
          _ValueId = new fhirCsR4B.Models.Element();
          _ValueId.DeserializeJson(ref reader, options);
          break;

        case "valueInstant":
          ValueInstant = reader.GetString();
          break;

        case "_valueInstant":
          _ValueInstant = new fhirCsR4B.Models.Element();
          _ValueInstant.DeserializeJson(ref reader, options);
          break;

        case "valueInteger":
          ValueInteger = reader.GetInt32();
          break;

        case "_valueInteger":
          _ValueInteger = new fhirCsR4B.Models.Element();
          _ValueInteger.DeserializeJson(ref reader, options);
          break;

        case "valueMarkdown":
          ValueMarkdown = reader.GetString();
          break;

        case "_valueMarkdown":
          _ValueMarkdown = new fhirCsR4B.Models.Element();
          _ValueMarkdown.DeserializeJson(ref reader, options);
          break;

        case "valueOid":
          ValueOid = reader.GetString();
          break;

        case "_valueOid":
          _ValueOid = new fhirCsR4B.Models.Element();
          _ValueOid.DeserializeJson(ref reader, options);
          break;

        case "valuePositiveInt":
          ValuePositiveInt = reader.GetUInt32();
          break;

        case "valueString":
          ValueString = reader.GetString();
          break;

        case "_valueString":
          _ValueString = new fhirCsR4B.Models.Element();
          _ValueString.DeserializeJson(ref reader, options);
          break;

        case "valueTime":
          ValueTime = reader.GetString();
          break;

        case "_valueTime":
          _ValueTime = new fhirCsR4B.Models.Element();
          _ValueTime.DeserializeJson(ref reader, options);
          break;

        case "valueUnsignedInt":
          ValueUnsignedInt = reader.GetUInt32();
          break;

        case "valueUri":
          ValueUri = reader.GetString();
          break;

        case "_valueUri":
          _ValueUri = new fhirCsR4B.Models.Element();
          _ValueUri.DeserializeJson(ref reader, options);
          break;

        case "valueUrl":
          ValueUrl = reader.GetString();
          break;

        case "_valueUrl":
          _ValueUrl = new fhirCsR4B.Models.Element();
          _ValueUrl.DeserializeJson(ref reader, options);
          break;

        case "valueUuid":
          ValueUuid = reader.GetGuid();
          break;

        case "valueAddress":
          ValueAddress = new fhirCsR4B.Models.Address();
          ValueAddress.DeserializeJson(ref reader, options);
          break;

        case "valueAge":
          ValueAge = new fhirCsR4B.Models.Age();
          ValueAge.DeserializeJson(ref reader, options);
          break;

        case "valueAnnotation":
          ValueAnnotation = new fhirCsR4B.Models.Annotation();
          ValueAnnotation.DeserializeJson(ref reader, options);
          break;

        case "valueAttachment":
          ValueAttachment = new fhirCsR4B.Models.Attachment();
          ValueAttachment.DeserializeJson(ref reader, options);
          break;

        case "valueCodeableConcept":
          ValueCodeableConcept = new fhirCsR4B.Models.CodeableConcept();
          ValueCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "valueCoding":
          ValueCoding = new fhirCsR4B.Models.Coding();
          ValueCoding.DeserializeJson(ref reader, options);
          break;

        case "valueContactPoint":
          ValueContactPoint = new fhirCsR4B.Models.ContactPoint();
          ValueContactPoint.DeserializeJson(ref reader, options);
          break;

        case "valueCount":
          ValueCount = new fhirCsR4B.Models.Count();
          ValueCount.DeserializeJson(ref reader, options);
          break;

        case "valueDistance":
          ValueDistance = new fhirCsR4B.Models.Distance();
          ValueDistance.DeserializeJson(ref reader, options);
          break;

        case "valueDuration":
          ValueDuration = new fhirCsR4B.Models.Duration();
          ValueDuration.DeserializeJson(ref reader, options);
          break;

        case "valueHumanName":
          ValueHumanName = new fhirCsR4B.Models.HumanName();
          ValueHumanName.DeserializeJson(ref reader, options);
          break;

        case "valueIdentifier":
          ValueIdentifier = new fhirCsR4B.Models.Identifier();
          ValueIdentifier.DeserializeJson(ref reader, options);
          break;

        case "valueMoney":
          ValueMoney = new fhirCsR4B.Models.Money();
          ValueMoney.DeserializeJson(ref reader, options);
          break;

        case "valuePeriod":
          ValuePeriod = new fhirCsR4B.Models.Period();
          ValuePeriod.DeserializeJson(ref reader, options);
          break;

        case "valueQuantity":
          ValueQuantity = new fhirCsR4B.Models.Quantity();
          ValueQuantity.DeserializeJson(ref reader, options);
          break;

        case "valueRange":
          ValueRange = new fhirCsR4B.Models.Range();
          ValueRange.DeserializeJson(ref reader, options);
          break;

        case "valueRatio":
          ValueRatio = new fhirCsR4B.Models.Ratio();
          ValueRatio.DeserializeJson(ref reader, options);
          break;

        case "valueReference":
          ValueReference = new fhirCsR4B.Models.Reference();
          ValueReference.DeserializeJson(ref reader, options);
          break;

        case "valueSampledData":
          ValueSampledData = new fhirCsR4B.Models.SampledData();
          ValueSampledData.DeserializeJson(ref reader, options);
          break;

        case "valueSignature":
          ValueSignature = new fhirCsR4B.Models.Signature();
          ValueSignature.DeserializeJson(ref reader, options);
          break;

        case "valueTiming":
          ValueTiming = new fhirCsR4B.Models.Timing();
          ValueTiming.DeserializeJson(ref reader, options);
          break;

        case "valueContactDetail":
          ValueContactDetail = new fhirCsR4B.Models.ContactDetail();
          ValueContactDetail.DeserializeJson(ref reader, options);
          break;

        case "valueContributor":
          ValueContributor = new fhirCsR4B.Models.Contributor();
          ValueContributor.DeserializeJson(ref reader, options);
          break;

        case "valueDataRequirement":
          ValueDataRequirement = new fhirCsR4B.Models.DataRequirement();
          ValueDataRequirement.DeserializeJson(ref reader, options);
          break;

        case "valueExpression":
          ValueExpression = new fhirCsR4B.Models.Expression();
          ValueExpression.DeserializeJson(ref reader, options);
          break;

        case "valueParameterDefinition":
          ValueParameterDefinition = new fhirCsR4B.Models.ParameterDefinition();
          ValueParameterDefinition.DeserializeJson(ref reader, options);
          break;

        case "valueRelatedArtifact":
          ValueRelatedArtifact = new fhirCsR4B.Models.RelatedArtifact();
          ValueRelatedArtifact.DeserializeJson(ref reader, options);
          break;

        case "valueTriggerDefinition":
          ValueTriggerDefinition = new fhirCsR4B.Models.TriggerDefinition();
          ValueTriggerDefinition.DeserializeJson(ref reader, options);
          break;

        case "valueUsageContext":
          ValueUsageContext = new fhirCsR4B.Models.UsageContext();
          ValueUsageContext.DeserializeJson(ref reader, options);
          break;

        case "valueDosage":
          ValueDosage = new fhirCsR4B.Models.Dosage();
          ValueDosage.DeserializeJson(ref reader, options);
          break;

        case "valueMeta":
          ValueMeta = new fhirCsR4B.Models.Meta();
          ValueMeta.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4B.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// <summary>
  /// This resource is a non-persisted resource used to pass information into and back from an [operation](operations.html). It has no other use, and there is no RESTful endpoint associated with it.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<Parameters>))]
  public class Parameters : Resource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Parameters";
    /// <summary>
    /// A parameter passed to or received from the operation.
    /// </summary>
    public List<ParametersParameter> Parameter { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR4B.Models.Resource)this).SerializeJson(writer, options, false);

      if ((Parameter != null) && (Parameter.Count != 0))
      {
        writer.WritePropertyName("parameter");
        writer.WriteStartArray();

        foreach (ParametersParameter valParameter in Parameter)
        {
          valParameter.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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
        case "parameter":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Parameter = new List<ParametersParameter>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.ParametersParameter objParameter = new fhirCsR4B.Models.ParametersParameter();
            objParameter.DeserializeJson(ref reader, options);
            Parameter.Add(objParameter);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Parameter.Count == 0)
          {
            Parameter = null;
          }

          break;

        default:
          ((fhirCsR4B.Models.Resource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
