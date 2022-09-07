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
  /// Specifies contact information for a specific purpose over a period of time, might be handled/monitored by a specific named person or organization.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<ExtendedContactDetail>))]
  public class ExtendedContactDetail : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// More than 1 address would be for different purposes, and thus should be entered as a different entry,.
    /// </summary>
    public Address Address { get; set; }
    /// <summary>
    /// If there is no named individual, the telecom/address information is not generally monitored by a specific individual.
    /// </summary>
    public List<HumanName> Name { get; set; }
    /// <summary>
    /// This contact detail is handled/monitored by a specific organization. If the name is provided in the contact, then it is referring to the named individual within this organization.
    /// </summary>
    public Reference Organization { get; set; }
    /// <summary>
    /// If the details have multiple periods, then enter in a new ExtendedContact with the new period.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// If no purpose is defined, then these contact details may be used for any purpose.
    /// </summary>
    public CodeableConcept Purpose { get; set; }
    /// <summary>
    /// The contact details application for the purpose defined.
    /// </summary>
    public List<ContactPoint> Telecom { get; set; }
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

      if (Purpose != null)
      {
        writer.WritePropertyName("purpose");
        Purpose.SerializeJson(writer, options);
      }

      if ((Name != null) && (Name.Count != 0))
      {
        writer.WritePropertyName("name");
        writer.WriteStartArray();

        foreach (HumanName valName in Name)
        {
          valName.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Telecom != null) && (Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();

        foreach (ContactPoint valTelecom in Telecom)
        {
          valTelecom.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Address != null)
      {
        writer.WritePropertyName("address");
        Address.SerializeJson(writer, options);
      }

      if (Organization != null)
      {
        writer.WritePropertyName("organization");
        Organization.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
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
        case "address":
          Address = new fhirCsR5.Models.Address();
          Address.DeserializeJson(ref reader, options);
          break;

        case "name":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Name = new List<HumanName>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.HumanName objName = new fhirCsR5.Models.HumanName();
            objName.DeserializeJson(ref reader, options);
            Name.Add(objName);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Name.Count == 0)
          {
            Name = null;
          }

          break;

        case "organization":
          Organization = new fhirCsR5.Models.Reference();
          Organization.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR5.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "purpose":
          Purpose = new fhirCsR5.Models.CodeableConcept();
          Purpose.DeserializeJson(ref reader, options);
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.ContactPoint objTelecom = new fhirCsR5.Models.ContactPoint();
            objTelecom.DeserializeJson(ref reader, options);
            Telecom.Add(objTelecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Telecom.Count == 0)
          {
            Telecom = null;
          }

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
