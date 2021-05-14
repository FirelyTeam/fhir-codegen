// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for AllergyIntolerance
  /// </summary>
  public static class AllergyIntoleranceJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR AllergyIntolerance into JSON
    /// </summary>
    public static void SerializeJson(this AllergyIntolerance current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","AllergyIntolerance");
      // Complex: AllergyIntolerance, Export: AllergyIntolerance, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.ClinicalStatus != null)
      {
        writer.WritePropertyName("clinicalStatus");
        current.ClinicalStatus.SerializeJson(writer, options);
      }

      if (current.VerificationStatus != null)
      {
        writer.WritePropertyName("verificationStatus");
        current.VerificationStatus.SerializeJson(writer, options);
      }

      if (current.TypeElement != null)
      {
        writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));
      }

      if ((current.CategoryElement != null) && (current.CategoryElement.Count != 0))
      {
        writer.WritePropertyName("category");
        writer.WriteStartArray();
        foreach (Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCategory> val in current.CategoryElement)
        {
          writer.WriteStringValue(Hl7.Fhir.Utility.EnumUtility.GetLiteral(val.Value));
        }
        writer.WriteEndArray();
      }

      if (current.CriticalityElement != null)
      {
        writer.WriteString("criticality",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.CriticalityElement.Value));
      }

      if (current.Code != null)
      {
        writer.WritePropertyName("code");
        current.Code.SerializeJson(writer, options);
      }

      writer.WritePropertyName("patient");
      current.Patient.SerializeJson(writer, options);

      if (current.Encounter != null)
      {
        writer.WritePropertyName("encounter");
        current.Encounter.SerializeJson(writer, options);
      }

      if (current.Onset != null)
      {
        switch (current.Onset)
        {
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("onsetDateTime",v_FhirDateTime.Value);
            break;
          case Age v_Age:
            writer.WritePropertyName("onsetAge");
            v_Age.SerializeJson(writer, options);
            break;
          case Period v_Period:
            writer.WritePropertyName("onsetPeriod");
            v_Period.SerializeJson(writer, options);
            break;
          case Range v_Range:
            writer.WritePropertyName("onsetRange");
            v_Range.SerializeJson(writer, options);
            break;
          case FhirString v_FhirString:
            writer.WriteString("onsetString",v_FhirString.Value);
            break;
        }
      }
      if ((current.RecordedDateElement != null) && (current.RecordedDateElement.Value != null))
      {
        writer.WriteString("recordedDate",current.RecordedDateElement.Value);
      }

      if (current.Recorder != null)
      {
        writer.WritePropertyName("recorder");
        current.Recorder.SerializeJson(writer, options);
      }

      if (current.Asserter != null)
      {
        writer.WritePropertyName("asserter");
        current.Asserter.SerializeJson(writer, options);
      }

      if ((current.LastOccurrenceElement != null) && (current.LastOccurrenceElement.Value != null))
      {
        writer.WriteString("lastOccurrence",current.LastOccurrenceElement.Value);
      }

      if ((current.Note != null) && (current.Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();
        foreach (Annotation val in current.Note)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Reaction != null) && (current.Reaction.Count != 0))
      {
        writer.WritePropertyName("reaction");
        writer.WriteStartArray();
        foreach (AllergyIntolerance.ReactionComponent val in current.Reaction)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR AllergyIntolerance
    /// </summary>
    public static void DeserializeJson(this AllergyIntolerance current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR AllergyIntolerance
    /// </summary>
    public static void DeserializeJsonProperty(this AllergyIntolerance current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "clinicalStatus":
          current.ClinicalStatus = new Hl7.Fhir.Model.CodeableConcept();
          current.ClinicalStatus.DeserializeJson(ref reader, options);
          break;

        case "verificationStatus":
          current.VerificationStatus = new Hl7.Fhir.Model.CodeableConcept();
          current.VerificationStatus.DeserializeJson(ref reader, options);
          break;

        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceType>(reader.GetString()));
          break;

        case "category":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.CategoryElement = new List<Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCategory>>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.CategoryElement.Add(new Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCategory>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCategory>(reader.GetString())));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.CategoryElement.Count == 0)
          {
            current.CategoryElement = null;
          }
          break;

        case "criticality":
          current.CriticalityElement =new Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCriticality>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceCriticality>(reader.GetString()));
          break;

        case "code":
          current.Code = new Hl7.Fhir.Model.CodeableConcept();
          current.Code.DeserializeJson(ref reader, options);
          break;

        case "patient":
          current.Patient = new Hl7.Fhir.Model.ResourceReference();
          current.Patient.DeserializeJson(ref reader, options);
          break;

        case "encounter":
          current.Encounter = new Hl7.Fhir.Model.ResourceReference();
          current.Encounter.DeserializeJson(ref reader, options);
          break;

        case "onsetDateTime":
          current.Onset = new FhirDateTime(reader.GetString());
          break;

        case "onsetAge":
          current.Onset = new Hl7.Fhir.Model.Age();
          current.Onset.DeserializeJson(ref reader, options);
          break;

        case "onsetPeriod":
          current.Onset = new Hl7.Fhir.Model.Period();
          current.Onset.DeserializeJson(ref reader, options);
          break;

        case "onsetRange":
          current.Onset = new Hl7.Fhir.Model.Range();
          current.Onset.DeserializeJson(ref reader, options);
          break;

        case "onsetString":
          current.Onset = new FhirString(reader.GetString());
          break;

        case "recordedDate":
          current.RecordedDateElement = new FhirDateTime(reader.GetString());
          break;

        case "recorder":
          current.Recorder = new Hl7.Fhir.Model.ResourceReference();
          current.Recorder.DeserializeJson(ref reader, options);
          break;

        case "asserter":
          current.Asserter = new Hl7.Fhir.Model.ResourceReference();
          current.Asserter.DeserializeJson(ref reader, options);
          break;

        case "lastOccurrence":
          current.LastOccurrenceElement = new FhirDateTime(reader.GetString());
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Annotation v_Note = new Hl7.Fhir.Model.Annotation();
            v_Note.DeserializeJson(ref reader, options);
            current.Note.Add(v_Note);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Note.Count == 0)
          {
            current.Note = null;
          }
          break;

        case "reaction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Reaction = new List<AllergyIntolerance.ReactionComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.AllergyIntolerance.ReactionComponent v_Reaction = new Hl7.Fhir.Model.AllergyIntolerance.ReactionComponent();
            v_Reaction.DeserializeJson(ref reader, options);
            current.Reaction.Add(v_Reaction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Reaction.Count == 0)
          {
            current.Reaction = null;
          }
          break;

        // Complex: AllergyIntolerance, Export: AllergyIntolerance, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR AllergyIntolerance#Reaction into JSON
    /// </summary>
    public static void SerializeJson(this AllergyIntolerance.ReactionComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: AllergyIntolerance#Reaction, Export: ReactionComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Substance != null)
      {
        writer.WritePropertyName("substance");
        current.Substance.SerializeJson(writer, options);
      }

      if ((current.Manifestation != null) && (current.Manifestation.Count != 0))
      {
        writer.WritePropertyName("manifestation");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Manifestation)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.DescriptionElement != null) && (current.DescriptionElement.Value != null))
      {
        writer.WriteString("description",current.DescriptionElement.Value);
      }

      if ((current.OnsetElement != null) && (current.OnsetElement.Value != null))
      {
        writer.WriteString("onset",current.OnsetElement.Value);
      }

      if (current.SeverityElement != null)
      {
        writer.WriteString("severity",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.SeverityElement.Value));
      }

      if (current.ExposureRoute != null)
      {
        writer.WritePropertyName("exposureRoute");
        current.ExposureRoute.SerializeJson(writer, options);
      }

      if ((current.Note != null) && (current.Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();
        foreach (Annotation val in current.Note)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR AllergyIntolerance#Reaction
    /// </summary>
    public static void DeserializeJson(this AllergyIntolerance.ReactionComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR AllergyIntolerance#Reaction
    /// </summary>
    public static void DeserializeJsonProperty(this AllergyIntolerance.ReactionComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "substance":
          current.Substance = new Hl7.Fhir.Model.CodeableConcept();
          current.Substance.DeserializeJson(ref reader, options);
          break;

        case "manifestation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Manifestation = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Manifestation = new Hl7.Fhir.Model.CodeableConcept();
            v_Manifestation.DeserializeJson(ref reader, options);
            current.Manifestation.Add(v_Manifestation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Manifestation.Count == 0)
          {
            current.Manifestation = null;
          }
          break;

        case "description":
          current.DescriptionElement = new FhirString(reader.GetString());
          break;

        case "onset":
          current.OnsetElement = new FhirDateTime(reader.GetString());
          break;

        case "severity":
          current.SeverityElement =new Code<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceSeverity>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AllergyIntolerance.AllergyIntoleranceSeverity>(reader.GetString()));
          break;

        case "exposureRoute":
          current.ExposureRoute = new Hl7.Fhir.Model.CodeableConcept();
          current.ExposureRoute.DeserializeJson(ref reader, options);
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Annotation v_Note = new Hl7.Fhir.Model.Annotation();
            v_Note.DeserializeJson(ref reader, options);
            current.Note.Add(v_Note);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Note.Count == 0)
          {
            current.Note = null;
          }
          break;

        // Complex: reaction, Export: ReactionComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class AllergyIntoleranceJsonConverter : JsonConverter<AllergyIntolerance>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(AllergyIntolerance).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, AllergyIntolerance value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override AllergyIntolerance Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        AllergyIntolerance target = new AllergyIntolerance();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
