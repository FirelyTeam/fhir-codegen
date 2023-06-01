// <auto-generated/>
// Contents of: hl7.fhir.r3.core version: 3.0.2

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using Hl7.Fhir.Introspection;
using Hl7.Fhir.Serialization;
using Hl7.Fhir.Specification;
using Hl7.Fhir.Utility;
using Hl7.Fhir.Validation;

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

namespace Hl7.Fhir.Model
{
  /// <summary>
  /// A homogeneous material with a definite composition
  /// </summary>
  [Serializable]
  [DataContract]
  [FhirType("Substance","http://hl7.org/fhir/StructureDefinition/Substance", IsResource=true)]
  public partial class Substance : Hl7.Fhir.Model.DomainResource
  {
    /// <summary>
    /// FHIR Type Name
    /// </summary>
    public override string TypeName { get { return "Substance"; } }

    /// <summary>
    /// A code to indicate if the substance is actively used
    /// (url: http://hl7.org/fhir/ValueSet/substance-status)
    /// (system: http://hl7.org/fhir/substance-status)
    /// </summary>
    [FhirEnumeration("FHIRSubstanceStatus")]
    public enum FHIRSubstanceStatus
    {
      /// <summary>
      /// The substance is considered for use or reference
      /// (system: http://hl7.org/fhir/substance-status)
      /// </summary>
      [EnumLiteral("active", "http://hl7.org/fhir/substance-status"), Description("Active")]
      Active,
      /// <summary>
      /// The substance is considered for reference, but not for use
      /// (system: http://hl7.org/fhir/substance-status)
      /// </summary>
      [EnumLiteral("inactive", "http://hl7.org/fhir/substance-status"), Description("Inactive")]
      Inactive,
      /// <summary>
      /// The substance was entered in error
      /// (system: http://hl7.org/fhir/substance-status)
      /// </summary>
      [EnumLiteral("entered-in-error", "http://hl7.org/fhir/substance-status"), Description("Entered in Error")]
      EnteredInError,
    }

    /// <summary>
    /// If this describes a specific package/container of the substance
    /// </summary>
    [Serializable]
    [DataContract]
    [FhirType("Substance#Instance", IsNestedType=true)]
    public partial class InstanceComponent : Hl7.Fhir.Model.BackboneElement
    {
      /// <summary>
      /// FHIR Type Name
      /// </summary>
      public override string TypeName { get { return "Substance#Instance"; } }

      /// <summary>
      /// Identifier of the package/container
      /// </summary>
      [FhirElement("identifier", InSummary=true, Order=40)]
      [DataMember]
      public Hl7.Fhir.Model.Identifier Identifier
      {
        get { return _Identifier; }
        set { _Identifier = value; OnPropertyChanged("Identifier"); }
      }

      private Hl7.Fhir.Model.Identifier _Identifier;

      /// <summary>
      /// When no longer valid to use
      /// </summary>
      [FhirElement("expiry", InSummary=true, Order=50)]
      [DataMember]
      public Hl7.Fhir.Model.FhirDateTime ExpiryElement
      {
        get { return _ExpiryElement; }
        set { _ExpiryElement = value; OnPropertyChanged("ExpiryElement"); }
      }

      private Hl7.Fhir.Model.FhirDateTime _ExpiryElement;

      /// <summary>
      /// When no longer valid to use
      /// </summary>
      /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
      [IgnoreDataMember]
      public string Expiry
      {
        get { return ExpiryElement != null ? ExpiryElement.Value : null; }
        set
        {
          if (value == null)
            ExpiryElement = null;
          else
            ExpiryElement = new Hl7.Fhir.Model.FhirDateTime(value);
          OnPropertyChanged("Expiry");
        }
      }

      /// <summary>
      /// Amount of substance in the package
      /// </summary>
      [FhirElement("quantity", InSummary=true, Order=60)]
      [DataMember]
      public Hl7.Fhir.Model.Quantity Quantity
      {
        get { return _Quantity; }
        set { _Quantity = value; OnPropertyChanged("Quantity"); }
      }

      private Hl7.Fhir.Model.Quantity _Quantity;

      public override IDeepCopyable CopyTo(IDeepCopyable other)
      {
        var dest = other as InstanceComponent;

        if (dest == null)
        {
          throw new ArgumentException("Can only copy to an object of the same type", "other");
        }

        base.CopyTo(dest);
        if(Identifier != null) dest.Identifier = (Hl7.Fhir.Model.Identifier)Identifier.DeepCopy();
        if(ExpiryElement != null) dest.ExpiryElement = (Hl7.Fhir.Model.FhirDateTime)ExpiryElement.DeepCopy();
        if(Quantity != null) dest.Quantity = (Hl7.Fhir.Model.Quantity)Quantity.DeepCopy();
        return dest;
      }

      public override IDeepCopyable DeepCopy()
      {
        return CopyTo(new InstanceComponent());
      }

      ///<inheritdoc />
      public override bool Matches(IDeepComparable other)
      {
        var otherT = other as InstanceComponent;
        if(otherT == null) return false;

        if(!base.Matches(otherT)) return false;
        if( !DeepComparable.Matches(Identifier, otherT.Identifier)) return false;
        if( !DeepComparable.Matches(ExpiryElement, otherT.ExpiryElement)) return false;
        if( !DeepComparable.Matches(Quantity, otherT.Quantity)) return false;

        return true;
      }

      public override bool IsExactly(IDeepComparable other)
      {
        var otherT = other as InstanceComponent;
        if(otherT == null) return false;

        if(!base.IsExactly(otherT)) return false;
        if( !DeepComparable.IsExactly(Identifier, otherT.Identifier)) return false;
        if( !DeepComparable.IsExactly(ExpiryElement, otherT.ExpiryElement)) return false;
        if( !DeepComparable.IsExactly(Quantity, otherT.Quantity)) return false;

        return true;
      }

      [IgnoreDataMember]
      public override IEnumerable<Base> Children
      {
        get
        {
          foreach (var item in base.Children) yield return item;
          if (Identifier != null) yield return Identifier;
          if (ExpiryElement != null) yield return ExpiryElement;
          if (Quantity != null) yield return Quantity;
        }
      }

      [IgnoreDataMember]
      public override IEnumerable<ElementValue> NamedChildren
      {
        get
        {
          foreach (var item in base.NamedChildren) yield return item;
          if (Identifier != null) yield return new ElementValue("identifier", Identifier);
          if (ExpiryElement != null) yield return new ElementValue("expiry", ExpiryElement);
          if (Quantity != null) yield return new ElementValue("quantity", Quantity);
        }
      }

      protected override bool TryGetValue(string key, out object value)
      {
        switch (key)
        {
          case "identifier":
            value = Identifier;
            return Identifier is not null;
          case "expiry":
            value = ExpiryElement;
            return ExpiryElement is not null;
          case "quantity":
            value = Quantity;
            return Quantity is not null;
          default:
            return base.TryGetValue(key, out value);
        };

      }

      protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
      {
        foreach (var kvp in base.GetElementPairs()) yield return kvp;
        if (Identifier is not null) yield return new KeyValuePair<string,object>("identifier",Identifier);
        if (ExpiryElement is not null) yield return new KeyValuePair<string,object>("expiry",ExpiryElement);
        if (Quantity is not null) yield return new KeyValuePair<string,object>("quantity",Quantity);
      }

    }

    /// <summary>
    /// Composition information about the substance
    /// </summary>
    [Serializable]
    [DataContract]
    [FhirType("Substance#Ingredient", IsNestedType=true)]
    public partial class IngredientComponent : Hl7.Fhir.Model.BackboneElement
    {
      /// <summary>
      /// FHIR Type Name
      /// </summary>
      public override string TypeName { get { return "Substance#Ingredient"; } }

      /// <summary>
      /// Optional amount (concentration)
      /// </summary>
      [FhirElement("quantity", InSummary=true, Order=40)]
      [DataMember]
      public Hl7.Fhir.Model.Ratio Quantity
      {
        get { return _Quantity; }
        set { _Quantity = value; OnPropertyChanged("Quantity"); }
      }

      private Hl7.Fhir.Model.Ratio _Quantity;

      /// <summary>
      /// A component of the substance
      /// </summary>
      [FhirElement("substance", InSummary=true, Order=50, Choice=ChoiceType.DatatypeChoice)]
      [CLSCompliant(false)]
      [References("Substance")]
      [AllowedTypes(typeof(Hl7.Fhir.Model.CodeableConcept),typeof(Hl7.Fhir.Model.ResourceReference))]
      [Cardinality(Min=1,Max=1)]
      [DataMember]
      public Hl7.Fhir.Model.DataType Substance
      {
        get { return _Substance; }
        set { _Substance = value; OnPropertyChanged("Substance"); }
      }

      private Hl7.Fhir.Model.DataType _Substance;

      public override IDeepCopyable CopyTo(IDeepCopyable other)
      {
        var dest = other as IngredientComponent;

        if (dest == null)
        {
          throw new ArgumentException("Can only copy to an object of the same type", "other");
        }

        base.CopyTo(dest);
        if(Quantity != null) dest.Quantity = (Hl7.Fhir.Model.Ratio)Quantity.DeepCopy();
        if(Substance != null) dest.Substance = (Hl7.Fhir.Model.DataType)Substance.DeepCopy();
        return dest;
      }

      public override IDeepCopyable DeepCopy()
      {
        return CopyTo(new IngredientComponent());
      }

      ///<inheritdoc />
      public override bool Matches(IDeepComparable other)
      {
        var otherT = other as IngredientComponent;
        if(otherT == null) return false;

        if(!base.Matches(otherT)) return false;
        if( !DeepComparable.Matches(Quantity, otherT.Quantity)) return false;
        if( !DeepComparable.Matches(Substance, otherT.Substance)) return false;

        return true;
      }

      public override bool IsExactly(IDeepComparable other)
      {
        var otherT = other as IngredientComponent;
        if(otherT == null) return false;

        if(!base.IsExactly(otherT)) return false;
        if( !DeepComparable.IsExactly(Quantity, otherT.Quantity)) return false;
        if( !DeepComparable.IsExactly(Substance, otherT.Substance)) return false;

        return true;
      }

      [IgnoreDataMember]
      public override IEnumerable<Base> Children
      {
        get
        {
          foreach (var item in base.Children) yield return item;
          if (Quantity != null) yield return Quantity;
          if (Substance != null) yield return Substance;
        }
      }

      [IgnoreDataMember]
      public override IEnumerable<ElementValue> NamedChildren
      {
        get
        {
          foreach (var item in base.NamedChildren) yield return item;
          if (Quantity != null) yield return new ElementValue("quantity", Quantity);
          if (Substance != null) yield return new ElementValue("substance", Substance);
        }
      }

      protected override bool TryGetValue(string key, out object value)
      {
        switch (key)
        {
          case "quantity":
            value = Quantity;
            return Quantity is not null;
          case "substance":
            value = Substance;
            return Substance is not null;
          default:
            return base.TryGetValue(key, out value);
        };

      }

      protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
      {
        foreach (var kvp in base.GetElementPairs()) yield return kvp;
        if (Quantity is not null) yield return new KeyValuePair<string,object>("quantity",Quantity);
        if (Substance is not null) yield return new KeyValuePair<string,object>("substance",Substance);
      }

    }

    /// <summary>
    /// Unique identifier
    /// </summary>
    [FhirElement("identifier", InSummary=true, Order=90)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Identifier> Identifier
    {
      get { if(_Identifier==null) _Identifier = new List<Hl7.Fhir.Model.Identifier>(); return _Identifier; }
      set { _Identifier = value; OnPropertyChanged("Identifier"); }
    }

    private List<Hl7.Fhir.Model.Identifier> _Identifier;

    /// <summary>
    /// active | inactive | entered-in-error
    /// </summary>
    [FhirElement("status", InSummary=true, Order=100)]
    [DeclaredType(Type = typeof(Code))]
    [DataMember]
    public Code<Hl7.Fhir.Model.Substance.FHIRSubstanceStatus> StatusElement
    {
      get { return _StatusElement; }
      set { _StatusElement = value; OnPropertyChanged("StatusElement"); }
    }

    private Code<Hl7.Fhir.Model.Substance.FHIRSubstanceStatus> _StatusElement;

    /// <summary>
    /// active | inactive | entered-in-error
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public Hl7.Fhir.Model.Substance.FHIRSubstanceStatus? Status
    {
      get { return StatusElement != null ? StatusElement.Value : null; }
      set
      {
        if (value == null)
          StatusElement = null;
        else
          StatusElement = new Code<Hl7.Fhir.Model.Substance.FHIRSubstanceStatus>(value);
        OnPropertyChanged("Status");
      }
    }

    /// <summary>
    /// What class/type of substance this is
    /// </summary>
    [FhirElement("category", InSummary=true, Order=110)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.CodeableConcept> Category
    {
      get { if(_Category==null) _Category = new List<Hl7.Fhir.Model.CodeableConcept>(); return _Category; }
      set { _Category = value; OnPropertyChanged("Category"); }
    }

    private List<Hl7.Fhir.Model.CodeableConcept> _Category;

    /// <summary>
    /// What substance this is
    /// </summary>
    [FhirElement("code", InSummary=true, Order=120)]
    [Cardinality(Min=1,Max=1)]
    [DataMember]
    public Hl7.Fhir.Model.CodeableConcept Code
    {
      get { return _Code; }
      set { _Code = value; OnPropertyChanged("Code"); }
    }

    private Hl7.Fhir.Model.CodeableConcept _Code;

    /// <summary>
    /// Textual description of the substance, comments
    /// </summary>
    [FhirElement("description", InSummary=true, Order=130)]
    [DataMember]
    public Hl7.Fhir.Model.FhirString DescriptionElement
    {
      get { return _DescriptionElement; }
      set { _DescriptionElement = value; OnPropertyChanged("DescriptionElement"); }
    }

    private Hl7.Fhir.Model.FhirString _DescriptionElement;

    /// <summary>
    /// Textual description of the substance, comments
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public string Description
    {
      get { return DescriptionElement != null ? DescriptionElement.Value : null; }
      set
      {
        if (value == null)
          DescriptionElement = null;
        else
          DescriptionElement = new Hl7.Fhir.Model.FhirString(value);
        OnPropertyChanged("Description");
      }
    }

    /// <summary>
    /// If this describes a specific package/container of the substance
    /// </summary>
    [FhirElement("instance", InSummary=true, Order=140)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Substance.InstanceComponent> Instance
    {
      get { if(_Instance==null) _Instance = new List<Hl7.Fhir.Model.Substance.InstanceComponent>(); return _Instance; }
      set { _Instance = value; OnPropertyChanged("Instance"); }
    }

    private List<Hl7.Fhir.Model.Substance.InstanceComponent> _Instance;

    /// <summary>
    /// Composition information about the substance
    /// </summary>
    [FhirElement("ingredient", InSummary=true, Order=150)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Substance.IngredientComponent> Ingredient
    {
      get { if(_Ingredient==null) _Ingredient = new List<Hl7.Fhir.Model.Substance.IngredientComponent>(); return _Ingredient; }
      set { _Ingredient = value; OnPropertyChanged("Ingredient"); }
    }

    private List<Hl7.Fhir.Model.Substance.IngredientComponent> _Ingredient;

    public override IDeepCopyable CopyTo(IDeepCopyable other)
    {
      var dest = other as Substance;

      if (dest == null)
      {
        throw new ArgumentException("Can only copy to an object of the same type", "other");
      }

      base.CopyTo(dest);
      if(Identifier != null) dest.Identifier = new List<Hl7.Fhir.Model.Identifier>(Identifier.DeepCopy());
      if(StatusElement != null) dest.StatusElement = (Code<Hl7.Fhir.Model.Substance.FHIRSubstanceStatus>)StatusElement.DeepCopy();
      if(Category != null) dest.Category = new List<Hl7.Fhir.Model.CodeableConcept>(Category.DeepCopy());
      if(Code != null) dest.Code = (Hl7.Fhir.Model.CodeableConcept)Code.DeepCopy();
      if(DescriptionElement != null) dest.DescriptionElement = (Hl7.Fhir.Model.FhirString)DescriptionElement.DeepCopy();
      if(Instance != null) dest.Instance = new List<Hl7.Fhir.Model.Substance.InstanceComponent>(Instance.DeepCopy());
      if(Ingredient != null) dest.Ingredient = new List<Hl7.Fhir.Model.Substance.IngredientComponent>(Ingredient.DeepCopy());
      return dest;
    }

    public override IDeepCopyable DeepCopy()
    {
      return CopyTo(new Substance());
    }

    ///<inheritdoc />
    public override bool Matches(IDeepComparable other)
    {
      var otherT = other as Substance;
      if(otherT == null) return false;

      if(!base.Matches(otherT)) return false;
      if( !DeepComparable.Matches(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.Matches(StatusElement, otherT.StatusElement)) return false;
      if( !DeepComparable.Matches(Category, otherT.Category)) return false;
      if( !DeepComparable.Matches(Code, otherT.Code)) return false;
      if( !DeepComparable.Matches(DescriptionElement, otherT.DescriptionElement)) return false;
      if( !DeepComparable.Matches(Instance, otherT.Instance)) return false;
      if( !DeepComparable.Matches(Ingredient, otherT.Ingredient)) return false;

      return true;
    }

    public override bool IsExactly(IDeepComparable other)
    {
      var otherT = other as Substance;
      if(otherT == null) return false;

      if(!base.IsExactly(otherT)) return false;
      if( !DeepComparable.IsExactly(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.IsExactly(StatusElement, otherT.StatusElement)) return false;
      if( !DeepComparable.IsExactly(Category, otherT.Category)) return false;
      if( !DeepComparable.IsExactly(Code, otherT.Code)) return false;
      if( !DeepComparable.IsExactly(DescriptionElement, otherT.DescriptionElement)) return false;
      if( !DeepComparable.IsExactly(Instance, otherT.Instance)) return false;
      if( !DeepComparable.IsExactly(Ingredient, otherT.Ingredient)) return false;

      return true;
    }

    [IgnoreDataMember]
    public override IEnumerable<Base> Children
    {
      get
      {
        foreach (var item in base.Children) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return elem; }
        if (StatusElement != null) yield return StatusElement;
        foreach (var elem in Category) { if (elem != null) yield return elem; }
        if (Code != null) yield return Code;
        if (DescriptionElement != null) yield return DescriptionElement;
        foreach (var elem in Instance) { if (elem != null) yield return elem; }
        foreach (var elem in Ingredient) { if (elem != null) yield return elem; }
      }
    }

    [IgnoreDataMember]
    public override IEnumerable<ElementValue> NamedChildren
    {
      get
      {
        foreach (var item in base.NamedChildren) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return new ElementValue("identifier", elem); }
        if (StatusElement != null) yield return new ElementValue("status", StatusElement);
        foreach (var elem in Category) { if (elem != null) yield return new ElementValue("category", elem); }
        if (Code != null) yield return new ElementValue("code", Code);
        if (DescriptionElement != null) yield return new ElementValue("description", DescriptionElement);
        foreach (var elem in Instance) { if (elem != null) yield return new ElementValue("instance", elem); }
        foreach (var elem in Ingredient) { if (elem != null) yield return new ElementValue("ingredient", elem); }
      }
    }

    protected override bool TryGetValue(string key, out object value)
    {
      switch (key)
      {
        case "identifier":
          value = Identifier;
          return Identifier?.Any() == true;
        case "status":
          value = StatusElement;
          return StatusElement is not null;
        case "category":
          value = Category;
          return Category?.Any() == true;
        case "code":
          value = Code;
          return Code is not null;
        case "description":
          value = DescriptionElement;
          return DescriptionElement is not null;
        case "instance":
          value = Instance;
          return Instance?.Any() == true;
        case "ingredient":
          value = Ingredient;
          return Ingredient?.Any() == true;
        default:
          return base.TryGetValue(key, out value);
      };

    }

    protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
    {
      foreach (var kvp in base.GetElementPairs()) yield return kvp;
      if (Identifier?.Any() == true) yield return new KeyValuePair<string,object>("identifier",Identifier);
      if (StatusElement is not null) yield return new KeyValuePair<string,object>("status",StatusElement);
      if (Category?.Any() == true) yield return new KeyValuePair<string,object>("category",Category);
      if (Code is not null) yield return new KeyValuePair<string,object>("code",Code);
      if (DescriptionElement is not null) yield return new KeyValuePair<string,object>("description",DescriptionElement);
      if (Instance?.Any() == true) yield return new KeyValuePair<string,object>("instance",Instance);
      if (Ingredient?.Any() == true) yield return new KeyValuePair<string,object>("ingredient",Ingredient);
    }

  }

}

// end of file