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
  /// A set of rules that describe when the event is scheduled.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<TimingRepeat>))]
  public class TimingRepeat : Element,  IFhirJsonSerializable {
    /// <summary>
    /// Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule.
    /// </summary>
    public Duration BoundsDuration { get; set; }
    /// <summary>
    /// Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule.
    /// </summary>
    public Range BoundsRange { get; set; }
    /// <summary>
    /// Either a duration for the length of the timing schedule, a range of possible length, or outer bounds for start and/or end limits of the timing schedule.
    /// </summary>
    public Period BoundsPeriod { get; set; }
    /// <summary>
    /// If you have both bounds and count, then this should be understood as within the bounds period, until count times happens.
    /// </summary>
    public uint? Count { get; set; }
    /// <summary>
    /// If present, indicates that the count is a range - so to perform the action between [count] and [countMax] times.
    /// </summary>
    public uint? CountMax { get; set; }
    /// <summary>
    /// If no days are specified, the action is assumed to happen every day as otherwise specified. The elements frequency and period cannot be used as well as dayOfWeek.
    /// </summary>
    public List<string> DayOfWeek { get; set; }
    /// <summary>
    /// Extension container element for DayOfWeek
    /// </summary>
    public List<Element> _DayOfWeek { get; set; }
    /// <summary>
    /// For some events the duration is part of the definition of the event (e.g. IV infusions, where the duration is implicit in the specified quantity and rate). For others, it's part of the timing specification (e.g. exercise).
    /// </summary>
    public decimal? Duration { get; set; }
    /// <summary>
    /// Extension container element for Duration
    /// </summary>
    public Element _Duration { get; set; }
    /// <summary>
    /// For some events the duration is part of the definition of the event (e.g. IV infusions, where the duration is implicit in the specified quantity and rate). For others, it's part of the timing specification (e.g. exercise).
    /// </summary>
    public decimal? DurationMax { get; set; }
    /// <summary>
    /// Extension container element for DurationMax
    /// </summary>
    public Element _DurationMax { get; set; }
    /// <summary>
    /// The units of time for the duration, in UCUM units.
    /// </summary>
    public string DurationUnit { get; set; }
    /// <summary>
    /// Extension container element for DurationUnit
    /// </summary>
    public Element _DurationUnit { get; set; }
    /// <summary>
    /// The number of times to repeat the action within the specified period. If frequencyMax is present, this element indicates the lower bound of the allowed range of the frequency.
    /// </summary>
    public uint? Frequency { get; set; }
    /// <summary>
    /// If present, indicates that the frequency is a range - so to repeat between [frequency] and [frequencyMax] times within the period or period range.
    /// </summary>
    public uint? FrequencyMax { get; set; }
    /// <summary>
    /// The number of minutes from the event. If the event code does not indicate whether the minutes is before or after the event, then the offset is assumed to be after the event.
    /// </summary>
    public uint? Offset { get; set; }
    /// <summary>
    /// Indicates the duration of time over which repetitions are to occur; e.g. to express "3 times per day", 3 would be the frequency and "1 day" would be the period. If periodMax is present, this element indicates the lower bound of the allowed range of the period length.
    /// </summary>
    public decimal? Period { get; set; }
    /// <summary>
    /// Extension container element for Period
    /// </summary>
    public Element _Period { get; set; }
    /// <summary>
    /// If present, indicates that the period is a range from [period] to [periodMax], allowing expressing concepts such as "do this once every 3-5 days.
    /// </summary>
    public decimal? PeriodMax { get; set; }
    /// <summary>
    /// Extension container element for PeriodMax
    /// </summary>
    public Element _PeriodMax { get; set; }
    /// <summary>
    /// The units of time for the period in UCUM units.
    /// </summary>
    public string PeriodUnit { get; set; }
    /// <summary>
    /// Extension container element for PeriodUnit
    /// </summary>
    public Element _PeriodUnit { get; set; }
    /// <summary>
    /// When time of day is specified, it is inferred that the action happens every day (as filtered by dayofWeek) on the specified times. The elements when, frequency and period cannot be used as well as timeOfDay.
    /// </summary>
    public List<string> TimeOfDay { get; set; }
    /// <summary>
    /// Extension container element for TimeOfDay
    /// </summary>
    public List<Element> _TimeOfDay { get; set; }
    /// <summary>
    /// When more than one event is listed, the event is tied to the union of the specified events.
    /// </summary>
    public List<string> When { get; set; }
    /// <summary>
    /// Extension container element for When
    /// </summary>
    public List<Element> _When { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4B.Models.Element)this).SerializeJson(writer, options, false);

      if (BoundsDuration != null)
      {
        writer.WritePropertyName("boundsDuration");
        BoundsDuration.SerializeJson(writer, options);
      }

      if (BoundsRange != null)
      {
        writer.WritePropertyName("boundsRange");
        BoundsRange.SerializeJson(writer, options);
      }

      if (BoundsPeriod != null)
      {
        writer.WritePropertyName("boundsPeriod");
        BoundsPeriod.SerializeJson(writer, options);
      }

      if (Count != null)
      {
        writer.WriteNumber("count", (uint)Count!);
      }

      if (CountMax != null)
      {
        writer.WriteNumber("countMax", (uint)CountMax!);
      }

      if (Duration != null)
      {
        writer.WriteNumber("duration", (decimal)Duration!);
      }

      if (_Duration != null)
      {
        writer.WritePropertyName("_duration");
        _Duration.SerializeJson(writer, options);
      }

      if (DurationMax != null)
      {
        writer.WriteNumber("durationMax", (decimal)DurationMax!);
      }

      if (_DurationMax != null)
      {
        writer.WritePropertyName("_durationMax");
        _DurationMax.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(DurationUnit))
      {
        writer.WriteString("durationUnit", (string)DurationUnit!);
      }

      if (_DurationUnit != null)
      {
        writer.WritePropertyName("_durationUnit");
        _DurationUnit.SerializeJson(writer, options);
      }

      if (Frequency != null)
      {
        writer.WriteNumber("frequency", (uint)Frequency!);
      }

      if (FrequencyMax != null)
      {
        writer.WriteNumber("frequencyMax", (uint)FrequencyMax!);
      }

      if (Period != null)
      {
        writer.WriteNumber("period", (decimal)Period!);
      }

      if (_Period != null)
      {
        writer.WritePropertyName("_period");
        _Period.SerializeJson(writer, options);
      }

      if (PeriodMax != null)
      {
        writer.WriteNumber("periodMax", (decimal)PeriodMax!);
      }

      if (_PeriodMax != null)
      {
        writer.WritePropertyName("_periodMax");
        _PeriodMax.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(PeriodUnit))
      {
        writer.WriteString("periodUnit", (string)PeriodUnit!);
      }

      if (_PeriodUnit != null)
      {
        writer.WritePropertyName("_periodUnit");
        _PeriodUnit.SerializeJson(writer, options);
      }

      if ((DayOfWeek != null) && (DayOfWeek.Count != 0))
      {
        writer.WritePropertyName("dayOfWeek");
        writer.WriteStartArray();

        foreach (string valDayOfWeek in DayOfWeek)
        {
          writer.WriteStringValue(valDayOfWeek);
        }

        writer.WriteEndArray();
      }

      if ((_DayOfWeek != null) && (_DayOfWeek.Count != 0))
      {
        writer.WritePropertyName("_dayOfWeek");
        writer.WriteStartArray();

        foreach (Element val_DayOfWeek in _DayOfWeek)
        {
          val_DayOfWeek.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((TimeOfDay != null) && (TimeOfDay.Count != 0))
      {
        writer.WritePropertyName("timeOfDay");
        writer.WriteStartArray();

        foreach (string valTimeOfDay in TimeOfDay)
        {
          writer.WriteStringValue(valTimeOfDay);
        }

        writer.WriteEndArray();
      }

      if ((_TimeOfDay != null) && (_TimeOfDay.Count != 0))
      {
        writer.WritePropertyName("_timeOfDay");
        writer.WriteStartArray();

        foreach (Element val_TimeOfDay in _TimeOfDay)
        {
          val_TimeOfDay.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((When != null) && (When.Count != 0))
      {
        writer.WritePropertyName("when");
        writer.WriteStartArray();

        foreach (string valWhen in When)
        {
          writer.WriteStringValue(valWhen);
        }

        writer.WriteEndArray();
      }

      if ((_When != null) && (_When.Count != 0))
      {
        writer.WritePropertyName("_when");
        writer.WriteStartArray();

        foreach (Element val_When in _When)
        {
          val_When.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Offset != null)
      {
        writer.WriteNumber("offset", (uint)Offset!);
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
        case "boundsDuration":
          BoundsDuration = new fhirCsR4B.Models.Duration();
          BoundsDuration.DeserializeJson(ref reader, options);
          break;

        case "boundsRange":
          BoundsRange = new fhirCsR4B.Models.Range();
          BoundsRange.DeserializeJson(ref reader, options);
          break;

        case "boundsPeriod":
          BoundsPeriod = new fhirCsR4B.Models.Period();
          BoundsPeriod.DeserializeJson(ref reader, options);
          break;

        case "count":
          Count = reader.GetUInt32();
          break;

        case "countMax":
          CountMax = reader.GetUInt32();
          break;

        case "dayOfWeek":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DayOfWeek = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            DayOfWeek.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DayOfWeek.Count == 0)
          {
            DayOfWeek = null;
          }

          break;

        case "_dayOfWeek":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _DayOfWeek = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Element obj_DayOfWeek = new fhirCsR4B.Models.Element();
            obj_DayOfWeek.DeserializeJson(ref reader, options);
            _DayOfWeek.Add(obj_DayOfWeek);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_DayOfWeek.Count == 0)
          {
            _DayOfWeek = null;
          }

          break;

        case "duration":
          Duration = reader.GetDecimal();
          break;

        case "_duration":
          _Duration = new fhirCsR4B.Models.Element();
          _Duration.DeserializeJson(ref reader, options);
          break;

        case "durationMax":
          DurationMax = reader.GetDecimal();
          break;

        case "_durationMax":
          _DurationMax = new fhirCsR4B.Models.Element();
          _DurationMax.DeserializeJson(ref reader, options);
          break;

        case "durationUnit":
          DurationUnit = reader.GetString();
          break;

        case "_durationUnit":
          _DurationUnit = new fhirCsR4B.Models.Element();
          _DurationUnit.DeserializeJson(ref reader, options);
          break;

        case "frequency":
          Frequency = reader.GetUInt32();
          break;

        case "frequencyMax":
          FrequencyMax = reader.GetUInt32();
          break;

        case "offset":
          Offset = reader.GetUInt32();
          break;

        case "period":
          Period = reader.GetDecimal();
          break;

        case "_period":
          _Period = new fhirCsR4B.Models.Element();
          _Period.DeserializeJson(ref reader, options);
          break;

        case "periodMax":
          PeriodMax = reader.GetDecimal();
          break;

        case "_periodMax":
          _PeriodMax = new fhirCsR4B.Models.Element();
          _PeriodMax.DeserializeJson(ref reader, options);
          break;

        case "periodUnit":
          PeriodUnit = reader.GetString();
          break;

        case "_periodUnit":
          _PeriodUnit = new fhirCsR4B.Models.Element();
          _PeriodUnit.DeserializeJson(ref reader, options);
          break;

        case "timeOfDay":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          TimeOfDay = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            TimeOfDay.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (TimeOfDay.Count == 0)
          {
            TimeOfDay = null;
          }

          break;

        case "_timeOfDay":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _TimeOfDay = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Element obj_TimeOfDay = new fhirCsR4B.Models.Element();
            obj_TimeOfDay.DeserializeJson(ref reader, options);
            _TimeOfDay.Add(obj_TimeOfDay);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_TimeOfDay.Count == 0)
          {
            _TimeOfDay = null;
          }

          break;

        case "when":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          When = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            When.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (When.Count == 0)
          {
            When = null;
          }

          break;

        case "_when":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _When = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Element obj_When = new fhirCsR4B.Models.Element();
            obj_When.DeserializeJson(ref reader, options);
            _When.Add(obj_When);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_When.Count == 0)
          {
            _When = null;
          }

          break;

        default:
          ((fhirCsR4B.Models.Element)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the Timing.repeat.dayOfWeek field
  /// </summary>
  public static class TimingRepeatDayOfWeekCodes {
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
  /// Code Values for the Timing.repeat.durationUnit field
  /// </summary>
  public static class TimingRepeatDurationUnitCodes {
    public const string S = "s";
    public const string MIN = "min";
    public const string H = "h";
    public const string D = "d";
    public const string WK = "wk";
    public const string MO = "mo";
    public const string A = "a";
    public static HashSet<string> Values = new HashSet<string>() {
      "s",
      "min",
      "h",
      "d",
      "wk",
      "mo",
      "a",
    };
  }
  /// <summary>
  /// Code Values for the Timing.repeat.periodUnit field
  /// </summary>
  public static class TimingRepeatPeriodUnitCodes {
    public const string S = "s";
    public const string MIN = "min";
    public const string H = "h";
    public const string D = "d";
    public const string WK = "wk";
    public const string MO = "mo";
    public const string A = "a";
    public static HashSet<string> Values = new HashSet<string>() {
      "s",
      "min",
      "h",
      "d",
      "wk",
      "mo",
      "a",
    };
  }
  /// <summary>
  /// Specifies an event that may occur multiple times. Timing schedules are used to record when things are planned, expected or requested to occur. The most common usage is in dosage instructions for medications. They are also used when planning care of various kinds, and may be used for reporting the schedule to which past regular activities were carried out.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<Timing>))]
  public class Timing : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// BID etc. are defined as 'at institutionally specified times'. For example, an institution may choose that BID is "always at 7am and 6pm".  If it is inappropriate for this choice to be made, the code BID should not be used. Instead, a distinct organization-specific code should be used in place of the HL7-defined BID code and/or a structured representation should be used (in this case, specifying the two event times).
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// Identifies specific times when the event occurs.
    /// </summary>
    public List<string> Event { get; set; }
    /// <summary>
    /// Extension container element for Event
    /// </summary>
    public List<Element> _Event { get; set; }
    /// <summary>
    /// A set of rules that describe when the event is scheduled.
    /// </summary>
    public TimingRepeat Repeat { get; set; }
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

      if ((Event != null) && (Event.Count != 0))
      {
        writer.WritePropertyName("event");
        writer.WriteStartArray();

        foreach (string valEvent in Event)
        {
          writer.WriteStringValue(valEvent);
        }

        writer.WriteEndArray();
      }

      if ((_Event != null) && (_Event.Count != 0))
      {
        writer.WritePropertyName("_event");
        writer.WriteStartArray();

        foreach (Element val_Event in _Event)
        {
          val_Event.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Repeat != null)
      {
        writer.WritePropertyName("repeat");
        Repeat.SerializeJson(writer, options);
      }

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
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
          Code = new fhirCsR4B.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "event":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Event = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Event.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Event.Count == 0)
          {
            Event = null;
          }

          break;

        case "_event":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Event = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Element obj_Event = new fhirCsR4B.Models.Element();
            obj_Event.DeserializeJson(ref reader, options);
            _Event.Add(obj_Event);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Event.Count == 0)
          {
            _Event = null;
          }

          break;

        case "repeat":
          Repeat = new fhirCsR4B.Models.TimingRepeat();
          Repeat.DeserializeJson(ref reader, options);
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
}
