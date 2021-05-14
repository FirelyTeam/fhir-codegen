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
  /// JSON Serialization Extensions for Practitioner
  /// </summary>
  public static class PractitionerJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR Practitioner into JSON
    /// </summary>
    public static void SerializeJson(this Practitioner current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","Practitioner");
      // Complex: Practitioner, Export: Practitioner, Base: DomainResource (DomainResource)
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

      if ((current.ActiveElement != null) && (current.ActiveElement.Value != null))
      {
        writer.WriteBoolean("active",(bool)current.ActiveElement.Value);
      }

      if ((current.Name != null) && (current.Name.Count != 0))
      {
        writer.WritePropertyName("name");
        writer.WriteStartArray();
        foreach (HumanName val in current.Name)
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

      if ((current.Address != null) && (current.Address.Count != 0))
      {
        writer.WritePropertyName("address");
        writer.WriteStartArray();
        foreach (Address val in current.Address)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.GenderElement != null)
      {
        writer.WriteString("gender",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.GenderElement.Value));
      }

      if ((current.BirthDateElement != null) && (current.BirthDateElement.Value != null))
      {
        writer.WriteString("birthDate",current.BirthDateElement.Value);
      }

      if ((current.Photo != null) && (current.Photo.Count != 0))
      {
        writer.WritePropertyName("photo");
        writer.WriteStartArray();
        foreach (Attachment val in current.Photo)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Qualification != null) && (current.Qualification.Count != 0))
      {
        writer.WritePropertyName("qualification");
        writer.WriteStartArray();
        foreach (Practitioner.QualificationComponent val in current.Qualification)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Communication != null) && (current.Communication.Count != 0))
      {
        writer.WritePropertyName("communication");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Communication)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Practitioner
    /// </summary>
    public static void DeserializeJson(this Practitioner current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Practitioner
    /// </summary>
    public static void DeserializeJsonProperty(this Practitioner current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "active":
          current.ActiveElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "name":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Name = new List<HumanName>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.HumanName v_Name = new Hl7.Fhir.Model.HumanName();
            v_Name.DeserializeJson(ref reader, options);
            current.Name.Add(v_Name);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Name.Count == 0)
          {
            current.Name = null;
          }
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ContactPoint v_Telecom = new Hl7.Fhir.Model.ContactPoint();
            v_Telecom.DeserializeJson(ref reader, options);
            current.Telecom.Add(v_Telecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Telecom.Count == 0)
          {
            current.Telecom = null;
          }
          break;

        case "address":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Address = new List<Address>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Address v_Address = new Hl7.Fhir.Model.Address();
            v_Address.DeserializeJson(ref reader, options);
            current.Address.Add(v_Address);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Address.Count == 0)
          {
            current.Address = null;
          }
          break;

        case "gender":
          current.GenderElement =new Code<Hl7.Fhir.Model.AdministrativeGender>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AdministrativeGender>(reader.GetString()));
          break;

        case "birthDate":
          current.BirthDateElement = new Date(reader.GetString());
          break;

        case "_birthDate":
          ((Hl7.Fhir.Model.Element)current.BirthDateElement).DeserializeJson(ref reader, options);
          break;

        case "photo":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Photo = new List<Attachment>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Attachment v_Photo = new Hl7.Fhir.Model.Attachment();
            v_Photo.DeserializeJson(ref reader, options);
            current.Photo.Add(v_Photo);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Photo.Count == 0)
          {
            current.Photo = null;
          }
          break;

        case "qualification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Qualification = new List<Practitioner.QualificationComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Practitioner.QualificationComponent v_Qualification = new Hl7.Fhir.Model.Practitioner.QualificationComponent();
            v_Qualification.DeserializeJson(ref reader, options);
            current.Qualification.Add(v_Qualification);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Qualification.Count == 0)
          {
            current.Qualification = null;
          }
          break;

        case "communication":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Communication = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Communication = new Hl7.Fhir.Model.CodeableConcept();
            v_Communication.DeserializeJson(ref reader, options);
            current.Communication.Add(v_Communication);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Communication.Count == 0)
          {
            current.Communication = null;
          }
          break;

        // Complex: Practitioner, Export: Practitioner, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Practitioner#Qualification into JSON
    /// </summary>
    public static void SerializeJson(this Practitioner.QualificationComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Practitioner#Qualification, Export: QualificationComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

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

      writer.WritePropertyName("code");
      current.Code.SerializeJson(writer, options);

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if (current.Issuer != null)
      {
        writer.WritePropertyName("issuer");
        current.Issuer.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Practitioner#Qualification
    /// </summary>
    public static void DeserializeJson(this Practitioner.QualificationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Practitioner#Qualification
    /// </summary>
    public static void DeserializeJsonProperty(this Practitioner.QualificationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "code":
          current.Code = new Hl7.Fhir.Model.CodeableConcept();
          current.Code.DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          current.Period.DeserializeJson(ref reader, options);
          break;

        case "issuer":
          current.Issuer = new Hl7.Fhir.Model.ResourceReference();
          current.Issuer.DeserializeJson(ref reader, options);
          break;

        // Complex: qualification, Export: QualificationComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class PractitionerJsonConverter : JsonConverter<Practitioner>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(Practitioner).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, Practitioner value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override Practitioner Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        Practitioner target = new Practitioner();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
