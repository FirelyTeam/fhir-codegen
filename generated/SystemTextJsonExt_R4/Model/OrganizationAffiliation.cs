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
  /// JSON Serialization Extensions for OrganizationAffiliation
  /// </summary>
  public static class OrganizationAffiliationJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR OrganizationAffiliation into JSON
    /// </summary>
    public static void SerializeJson(this OrganizationAffiliation current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","OrganizationAffiliation");
      // Complex: OrganizationAffiliation, Export: OrganizationAffiliation, Base: DomainResource (DomainResource)
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

      if (current.ActiveElement != null)
      {
        if (current.ActiveElement.Value != null)
        {
          writer.WriteBoolean("active",(bool)current.ActiveElement.Value);
        }
        if (current.ActiveElement.HasExtensions() || (!string.IsNullOrEmpty(current.ActiveElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_active",false,current.ActiveElement.Extension,current.ActiveElement.ElementId);
        }
      }

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if (current.Organization != null)
      {
        writer.WritePropertyName("organization");
        current.Organization.SerializeJson(writer, options);
      }

      if (current.ParticipatingOrganization != null)
      {
        writer.WritePropertyName("participatingOrganization");
        current.ParticipatingOrganization.SerializeJson(writer, options);
      }

      if ((current.Network != null) && (current.Network.Count != 0))
      {
        writer.WritePropertyName("network");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Network)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Code != null) && (current.Code.Count != 0))
      {
        writer.WritePropertyName("code");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Code)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Specialty != null) && (current.Specialty.Count != 0))
      {
        writer.WritePropertyName("specialty");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Specialty)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Location != null) && (current.Location.Count != 0))
      {
        writer.WritePropertyName("location");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Location)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.HealthcareService != null) && (current.HealthcareService.Count != 0))
      {
        writer.WritePropertyName("healthcareService");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.HealthcareService)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Telecom != null) && (current.Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();
        foreach (ContactPoint val in current.Telecom)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Endpoint != null) && (current.Endpoint.Count != 0))
      {
        writer.WritePropertyName("endpoint");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Endpoint)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR OrganizationAffiliation
    /// </summary>
    public static void DeserializeJson(this OrganizationAffiliation current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          if (Hl7.Fhir.Serialization.FhirSerializerOptions.Debug) { Console.WriteLine($"OrganizationAffiliation >>> OrganizationAffiliation.{propertyName}, depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}"); }
          reader.Read();
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException($"OrganizationAffiliation: invalid state! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
    }

    /// <summary>
    /// Deserialize JSON into a FHIR OrganizationAffiliation
    /// </summary>
    public static void DeserializeJsonProperty(this OrganizationAffiliation current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'identifier' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'identifier' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "active":
          if (reader.TokenType == JsonTokenType.Null)
          {
            current.ActiveElement = new FhirBoolean();
            reader.Skip();
          }
          else
          {
            current.ActiveElement = new FhirBoolean(reader.GetBoolean());
          }
          break;

        case "_active":
          if (current.ActiveElement == null) { current.ActiveElement = new FhirBoolean(); }
          ((Hl7.Fhir.Model.Element)current.ActiveElement).DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Period).DeserializeJson(ref reader, options);
          break;

        case "organization":
          current.Organization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Organization).DeserializeJson(ref reader, options);
          break;

        case "participatingOrganization":
          current.ParticipatingOrganization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.ParticipatingOrganization).DeserializeJson(ref reader, options);
          break;

        case "network":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'network' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Network = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Network = new Hl7.Fhir.Model.ResourceReference();
            v_Network.DeserializeJson(ref reader, options);
            current.Network.Add(v_Network);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'network' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Network.Count == 0)
          {
            current.Network = null;
          }
          break;

        case "code":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'code' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Code = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Code = new Hl7.Fhir.Model.CodeableConcept();
            v_Code.DeserializeJson(ref reader, options);
            current.Code.Add(v_Code);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'code' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Code.Count == 0)
          {
            current.Code = null;
          }
          break;

        case "specialty":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'specialty' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Specialty = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Specialty = new Hl7.Fhir.Model.CodeableConcept();
            v_Specialty.DeserializeJson(ref reader, options);
            current.Specialty.Add(v_Specialty);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'specialty' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Specialty.Count == 0)
          {
            current.Specialty = null;
          }
          break;

        case "location":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'location' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Location = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Location = new Hl7.Fhir.Model.ResourceReference();
            v_Location.DeserializeJson(ref reader, options);
            current.Location.Add(v_Location);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'location' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Location.Count == 0)
          {
            current.Location = null;
          }
          break;

        case "healthcareService":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'healthcareService' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.HealthcareService = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_HealthcareService = new Hl7.Fhir.Model.ResourceReference();
            v_HealthcareService.DeserializeJson(ref reader, options);
            current.HealthcareService.Add(v_HealthcareService);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'healthcareService' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.HealthcareService.Count == 0)
          {
            current.HealthcareService = null;
          }
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'telecom' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ContactPoint v_Telecom = new Hl7.Fhir.Model.ContactPoint();
            v_Telecom.DeserializeJson(ref reader, options);
            current.Telecom.Add(v_Telecom);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'telecom' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Telecom.Count == 0)
          {
            current.Telecom = null;
          }
          break;

        case "endpoint":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException($"OrganizationAffiliation error reading 'endpoint' expected StartArray, found {reader.TokenType}! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
          }

          current.Endpoint = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Endpoint = new Hl7.Fhir.Model.ResourceReference();
            v_Endpoint.DeserializeJson(ref reader, options);
            current.Endpoint.Add(v_Endpoint);

            if (!reader.Read())
            {
              throw new JsonException($"OrganizationAffiliation error reading 'endpoint' array, read failed! depth: {reader.CurrentDepth}, pos: {reader.BytesConsumed}");
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Endpoint.Count == 0)
          {
            current.Endpoint = null;
          }
          break;

        // Complex: OrganizationAffiliation, Export: OrganizationAffiliation, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class OrganizationAffiliationJsonConverter : JsonConverter<OrganizationAffiliation>
    {
      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, OrganizationAffiliation value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override OrganizationAffiliation Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        OrganizationAffiliation target = new OrganizationAffiliation();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
