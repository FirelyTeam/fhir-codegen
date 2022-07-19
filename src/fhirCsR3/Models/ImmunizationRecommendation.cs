// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// Vaccine date recommendations.  For example, earliest date to administer, latest date to administer, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImmunizationRecommendationRecommendationDateCriterion>))]
  public class ImmunizationRecommendationRecommendationDateCriterion : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Date classification of recommendation.  For example, earliest date to give, latest date to give, etc.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// The date whose meaning is specified by dateCriterion.code.
    /// </summary>
    public string Value { get; set; }
    /// <summary>
    /// Extension container element for Value
    /// </summary>
    public Element _Value { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Value))
      {
        writer.WriteString("value", (string)Value!);
      }

      if (_Value != null)
      {
        writer.WritePropertyName("_value");
        _Value.SerializeJson(writer, options);
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
        case "code":
          Code = new fhirCsR3.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "value":
          Value = reader.GetString();
          break;

        case "_value":
          _Value = new fhirCsR3.Models.Element();
          _Value.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Contains information about the protocol under which the vaccine was administered.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImmunizationRecommendationRecommendationProtocol>))]
  public class ImmunizationRecommendationRecommendationProtocol : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Indicates the authority who published the protocol.  For example, ACIP.
    /// </summary>
    public Reference Authority { get; set; }
    /// <summary>
    /// Contains the description about the protocol under which the vaccine was administered.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Indicates the nominal position in a series of the next dose.  This is the recommended dose number as per a specified protocol.
    /// </summary>
    public uint? DoseSequence { get; set; }
    /// <summary>
    /// One possible path to achieve presumed immunity against a disease - within the context of an authority.
    /// </summary>
    public string Series { get; set; }
    /// <summary>
    /// Extension container element for Series
    /// </summary>
    public Element _Series { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (DoseSequence != null)
      {
        writer.WriteNumber("doseSequence", (uint)DoseSequence!);
      }

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if (Authority != null)
      {
        writer.WritePropertyName("authority");
        Authority.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Series))
      {
        writer.WriteString("series", (string)Series!);
      }

      if (_Series != null)
      {
        writer.WritePropertyName("_series");
        _Series.SerializeJson(writer, options);
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
        case "authority":
          Authority = new fhirCsR3.Models.Reference();
          Authority.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "doseSequence":
          DoseSequence = reader.GetUInt32();
          break;

        case "series":
          Series = reader.GetString();
          break;

        case "_series":
          _Series = new fhirCsR3.Models.Element();
          _Series.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Vaccine administration recommendations.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImmunizationRecommendationRecommendation>))]
  public class ImmunizationRecommendationRecommendation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The date the immunization recommendation was created.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// Vaccine date recommendations.  For example, earliest date to administer, latest date to administer, etc.
    /// </summary>
    public List<ImmunizationRecommendationRecommendationDateCriterion> DateCriterion { get; set; }
    /// <summary>
    /// May need other dose concepts such as administered vs. valid.
    /// </summary>
    public uint? DoseNumber { get; set; }
    /// <summary>
    /// Vaccine administration status.
    /// </summary>
    public CodeableConcept ForecastStatus { get; set; }
    /// <summary>
    /// Contains information about the protocol under which the vaccine was administered.
    /// </summary>
    public ImmunizationRecommendationRecommendationProtocol Protocol { get; set; }
    /// <summary>
    /// Immunization event history that supports the status and recommendation.
    /// </summary>
    public List<Reference> SupportingImmunization { get; set; }
    /// <summary>
    /// Patient Information that supports the status and recommendation.  This includes patient observations, adverse reactions and allergy/intolerance information.
    /// </summary>
    public List<Reference> SupportingPatientInformation { get; set; }
    /// <summary>
    /// The targeted disease for the recommendation.
    /// </summary>
    public CodeableConcept TargetDisease { get; set; }
    /// <summary>
    /// Vaccine that pertains to the recommendation.
    /// </summary>
    public CodeableConcept VaccineCode { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (VaccineCode != null)
      {
        writer.WritePropertyName("vaccineCode");
        VaccineCode.SerializeJson(writer, options);
      }

      if (TargetDisease != null)
      {
        writer.WritePropertyName("targetDisease");
        TargetDisease.SerializeJson(writer, options);
      }

      if (DoseNumber != null)
      {
        writer.WriteNumber("doseNumber", (uint)DoseNumber!);
      }

      if (ForecastStatus != null)
      {
        writer.WritePropertyName("forecastStatus");
        ForecastStatus.SerializeJson(writer, options);
      }

      if ((DateCriterion != null) && (DateCriterion.Count != 0))
      {
        writer.WritePropertyName("dateCriterion");
        writer.WriteStartArray();

        foreach (ImmunizationRecommendationRecommendationDateCriterion valDateCriterion in DateCriterion)
        {
          valDateCriterion.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Protocol != null)
      {
        writer.WritePropertyName("protocol");
        Protocol.SerializeJson(writer, options);
      }

      if ((SupportingImmunization != null) && (SupportingImmunization.Count != 0))
      {
        writer.WritePropertyName("supportingImmunization");
        writer.WriteStartArray();

        foreach (Reference valSupportingImmunization in SupportingImmunization)
        {
          valSupportingImmunization.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((SupportingPatientInformation != null) && (SupportingPatientInformation.Count != 0))
      {
        writer.WritePropertyName("supportingPatientInformation");
        writer.WriteStartArray();

        foreach (Reference valSupportingPatientInformation in SupportingPatientInformation)
        {
          valSupportingPatientInformation.SerializeJson(writer, options, true);
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
        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR3.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "dateCriterion":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DateCriterion = new List<ImmunizationRecommendationRecommendationDateCriterion>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ImmunizationRecommendationRecommendationDateCriterion objDateCriterion = new fhirCsR3.Models.ImmunizationRecommendationRecommendationDateCriterion();
            objDateCriterion.DeserializeJson(ref reader, options);
            DateCriterion.Add(objDateCriterion);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DateCriterion.Count == 0)
          {
            DateCriterion = null;
          }

          break;

        case "doseNumber":
          DoseNumber = reader.GetUInt32();
          break;

        case "forecastStatus":
          ForecastStatus = new fhirCsR3.Models.CodeableConcept();
          ForecastStatus.DeserializeJson(ref reader, options);
          break;

        case "protocol":
          Protocol = new fhirCsR3.Models.ImmunizationRecommendationRecommendationProtocol();
          Protocol.DeserializeJson(ref reader, options);
          break;

        case "supportingImmunization":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          SupportingImmunization = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objSupportingImmunization = new fhirCsR3.Models.Reference();
            objSupportingImmunization.DeserializeJson(ref reader, options);
            SupportingImmunization.Add(objSupportingImmunization);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (SupportingImmunization.Count == 0)
          {
            SupportingImmunization = null;
          }

          break;

        case "supportingPatientInformation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          SupportingPatientInformation = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objSupportingPatientInformation = new fhirCsR3.Models.Reference();
            objSupportingPatientInformation.DeserializeJson(ref reader, options);
            SupportingPatientInformation.Add(objSupportingPatientInformation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (SupportingPatientInformation.Count == 0)
          {
            SupportingPatientInformation = null;
          }

          break;

        case "targetDisease":
          TargetDisease = new fhirCsR3.Models.CodeableConcept();
          TargetDisease.DeserializeJson(ref reader, options);
          break;

        case "vaccineCode":
          VaccineCode = new fhirCsR3.Models.CodeableConcept();
          VaccineCode.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// A patient's point-in-time immunization and recommendation (i.e. forecasting a patient's immunization eligibility according to a published schedule) with optional supporting justification.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImmunizationRecommendation>))]
  public class ImmunizationRecommendation : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ImmunizationRecommendation";
    /// <summary>
    /// A unique identifier assigned to this particular recommendation record.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The patient the recommendations are for.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// Vaccine administration recommendations.
    /// </summary>
    public List<ImmunizationRecommendationRecommendation> Recommendation { get; set; }
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


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if ((Recommendation != null) && (Recommendation.Count != 0))
      {
        writer.WritePropertyName("recommendation");
        writer.WriteStartArray();

        foreach (ImmunizationRecommendationRecommendation valRecommendation in Recommendation)
        {
          valRecommendation.SerializeJson(writer, options, true);
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
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Identifier objIdentifier = new fhirCsR3.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "patient":
          Patient = new fhirCsR3.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "recommendation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Recommendation = new List<ImmunizationRecommendationRecommendation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ImmunizationRecommendationRecommendation objRecommendation = new fhirCsR3.Models.ImmunizationRecommendationRecommendation();
            objRecommendation.DeserializeJson(ref reader, options);
            Recommendation.Add(objRecommendation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Recommendation.Count == 0)
          {
            Recommendation = null;
          }

          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
