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
  /// Times the {item} is available.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<AvailabilityAvailableTime>))]
  public class AvailabilityAvailableTime : Element,  IFhirJsonSerializable {
    /// <summary>
    /// Always available? i.e. 24 hour service.
    /// </summary>
    public bool? AllDay { get; set; }
    /// <summary>
    /// Extension container element for AllDay
    /// </summary>
    public Element _AllDay { get; set; }
    /// <summary>
    /// The time zone is expected to be specified or implied by the context this datatype is used.
    /// </summary>
    public string AvailableEndTime { get; set; }
    /// <summary>
    /// Extension container element for AvailableEndTime
    /// </summary>
    public Element _AvailableEndTime { get; set; }
    /// <summary>
    /// The time zone is expected to be specified or implied by the context this datatype is used.
    /// </summary>
    public string AvailableStartTime { get; set; }
    /// <summary>
    /// Extension container element for AvailableStartTime
    /// </summary>
    public Element _AvailableStartTime { get; set; }
    /// <summary>
    /// mon | tue | wed | thu | fri | sat | sun.
    /// </summary>
    public List<string> DaysOfWeek { get; set; }
    /// <summary>
    /// Extension container element for DaysOfWeek
    /// </summary>
    public List<Element> _DaysOfWeek { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.Element)this).SerializeJson(writer, options, false);

      if ((DaysOfWeek != null) && (DaysOfWeek.Count != 0))
      {
        writer.WritePropertyName("daysOfWeek");
        writer.WriteStartArray();

        foreach (string valDaysOfWeek in DaysOfWeek)
        {
          writer.WriteStringValue(valDaysOfWeek);
        }

        writer.WriteEndArray();
      }

      if ((_DaysOfWeek != null) && (_DaysOfWeek.Count != 0))
      {
        writer.WritePropertyName("_daysOfWeek");
        writer.WriteStartArray();

        foreach (Element val_DaysOfWeek in _DaysOfWeek)
        {
          val_DaysOfWeek.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (AllDay != null)
      {
        writer.WriteBoolean("allDay", (bool)AllDay!);
      }

      if (_AllDay != null)
      {
        writer.WritePropertyName("_allDay");
        _AllDay.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AvailableStartTime))
      {
        writer.WriteString("availableStartTime", (string)AvailableStartTime!);
      }

      if (_AvailableStartTime != null)
      {
        writer.WritePropertyName("_availableStartTime");
        _AvailableStartTime.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AvailableEndTime))
      {
        writer.WriteString("availableEndTime", (string)AvailableEndTime!);
      }

      if (_AvailableEndTime != null)
      {
        writer.WritePropertyName("_availableEndTime");
        _AvailableEndTime.SerializeJson(writer, options);
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
        case "allDay":
          AllDay = reader.GetBoolean();
          break;

        case "_allDay":
          _AllDay = new fhirCsR5.Models.Element();
          _AllDay.DeserializeJson(ref reader, options);
          break;

        case "availableEndTime":
          AvailableEndTime = reader.GetString();
          break;

        case "_availableEndTime":
          _AvailableEndTime = new fhirCsR5.Models.Element();
          _AvailableEndTime.DeserializeJson(ref reader, options);
          break;

        case "availableStartTime":
          AvailableStartTime = reader.GetString();
          break;

        case "_availableStartTime":
          _AvailableStartTime = new fhirCsR5.Models.Element();
          _AvailableStartTime.DeserializeJson(ref reader, options);
          break;

        case "daysOfWeek":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DaysOfWeek = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            DaysOfWeek.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DaysOfWeek.Count == 0)
          {
            DaysOfWeek = null;
          }

          break;

        case "_daysOfWeek":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _DaysOfWeek = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Element obj_DaysOfWeek = new fhirCsR5.Models.Element();
            obj_DaysOfWeek.DeserializeJson(ref reader, options);
            _DaysOfWeek.Add(obj_DaysOfWeek);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_DaysOfWeek.Count == 0)
          {
            _DaysOfWeek = null;
          }

          break;

        default:
          ((fhirCsR5.Models.Element)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the Availability.availableTime.daysOfWeek field
  /// </summary>
  public static class AvailabilityAvailableTimeDaysOfWeekCodes {
    public const string MON = "mon";
    public const string TUE = "tue";
    public const string WED = "wed";
    public const string THU = "thu";
    public const string FRI = "fri";
    public const string SAT = "sat";
    public const string SUN = "sun";
    public static HashSet<string> Values = new HashSet<string>() {
      "mon",
      "tue",
      "wed",
      "thu",
      "fri",
      "sat",
      "sun",
    };
  }
  /// <summary>
  /// Not available during this time due to provided reason.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<AvailabilityNotAvailableTime>))]
  public class AvailabilityNotAvailableTime : Element,  IFhirJsonSerializable {
    /// <summary>
    /// The reason will generally be provided to give the textual reason for displaying when the {item} is not available, e.g. 'Closed public holidays' or 'Independence Day'. In cases such as this, the `during` might not be included and local knowledge would be required in such cases (as don't desire to keep updating when the holiday occurs each year).
    /// e.g.2: 'Closed for maintenance over the summer' for this example you would want to include the `during` period, unless this was a university hospital and the "summer" period was well known, but would recommend its inclusion anyway.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Service not available during this period.
    /// </summary>
    public Period During { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.Element)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if (During != null)
      {
        writer.WritePropertyName("during");
        During.SerializeJson(writer, options);
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
        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR5.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "during":
          During = new fhirCsR5.Models.Period();
          During.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.Element)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Availability data for an {item}.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<Availability>))]
  public class Availability : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// Times the {item} is available.
    /// </summary>
    public List<AvailabilityAvailableTime> AvailableTime { get; set; }
    /// <summary>
    /// Not available during this time due to provided reason.
    /// </summary>
    public List<AvailabilityNotAvailableTime> NotAvailableTime { get; set; }
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

      if ((AvailableTime != null) && (AvailableTime.Count != 0))
      {
        writer.WritePropertyName("availableTime");
        writer.WriteStartArray();

        foreach (AvailabilityAvailableTime valAvailableTime in AvailableTime)
        {
          valAvailableTime.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((NotAvailableTime != null) && (NotAvailableTime.Count != 0))
      {
        writer.WritePropertyName("notAvailableTime");
        writer.WriteStartArray();

        foreach (AvailabilityNotAvailableTime valNotAvailableTime in NotAvailableTime)
        {
          valNotAvailableTime.SerializeJson(writer, options, true);
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
        case "availableTime":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AvailableTime = new List<AvailabilityAvailableTime>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.AvailabilityAvailableTime objAvailableTime = new fhirCsR5.Models.AvailabilityAvailableTime();
            objAvailableTime.DeserializeJson(ref reader, options);
            AvailableTime.Add(objAvailableTime);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AvailableTime.Count == 0)
          {
            AvailableTime = null;
          }

          break;

        case "notAvailableTime":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          NotAvailableTime = new List<AvailabilityNotAvailableTime>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.AvailabilityNotAvailableTime objNotAvailableTime = new fhirCsR5.Models.AvailabilityNotAvailableTime();
            objNotAvailableTime.DeserializeJson(ref reader, options);
            NotAvailableTime.Add(objNotAvailableTime);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (NotAvailableTime.Count == 0)
          {
            NotAvailableTime = null;
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
