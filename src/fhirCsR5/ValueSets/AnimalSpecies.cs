// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This example value set defines a set of codes that can be used to indicate species of animal patients.
  /// </summary>
  public static class AnimalSpeciesCodes
  {
    /// <summary>
    /// goat
    /// </summary>
    public static readonly Coding Goat = new Coding
    {
      Code = "125097000",
      Display = "goat",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// sheep
    /// </summary>
    public static readonly Coding Sheep = new Coding
    {
      Code = "125099002",
      Display = "sheep",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// mule
    /// </summary>
    public static readonly Coding Mule = new Coding
    {
      Code = "132950000",
      Display = "mule",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// goose
    /// </summary>
    public static readonly Coding Goose = new Coding
    {
      Code = "15778005",
      Display = "goose",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// cow
    /// </summary>
    public static readonly Coding Cow = new Coding
    {
      Code = "34618005",
      Display = "cow",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// horse
    /// </summary>
    public static readonly Coding Horse = new Coding
    {
      Code = "388445009",
      Display = "horse",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// duck
    /// </summary>
    public static readonly Coding Duck = new Coding
    {
      Code = "396620009",
      Display = "duck",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// turkey
    /// </summary>
    public static readonly Coding Turkey = new Coding
    {
      Code = "425134008",
      Display = "turkey",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// chicken
    /// </summary>
    public static readonly Coding Chicken = new Coding
    {
      Code = "47290002",
      Display = "chicken",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// donkey
    /// </summary>
    public static readonly Coding Donkey = new Coding
    {
      Code = "85626006",
      Display = "donkey",
      System = "http://snomed.info/sct"
    };

    /// <summary>
    /// Literal for code: Goat
    /// </summary>
    public const string LiteralGoat = "125097000";

    /// <summary>
    /// Literal for code: NoneGoat
    /// </summary>
    public const string LiteralNoneGoat = "http://snomed.info/sct#125097000";

    /// <summary>
    /// Literal for code: Sheep
    /// </summary>
    public const string LiteralSheep = "125099002";

    /// <summary>
    /// Literal for code: NoneSheep
    /// </summary>
    public const string LiteralNoneSheep = "http://snomed.info/sct#125099002";

    /// <summary>
    /// Literal for code: Mule
    /// </summary>
    public const string LiteralMule = "132950000";

    /// <summary>
    /// Literal for code: NoneMule
    /// </summary>
    public const string LiteralNoneMule = "http://snomed.info/sct#132950000";

    /// <summary>
    /// Literal for code: Goose
    /// </summary>
    public const string LiteralGoose = "15778005";

    /// <summary>
    /// Literal for code: NoneGoose
    /// </summary>
    public const string LiteralNoneGoose = "http://snomed.info/sct#15778005";

    /// <summary>
    /// Literal for code: Cow
    /// </summary>
    public const string LiteralCow = "34618005";

    /// <summary>
    /// Literal for code: NoneCow
    /// </summary>
    public const string LiteralNoneCow = "http://snomed.info/sct#34618005";

    /// <summary>
    /// Literal for code: Horse
    /// </summary>
    public const string LiteralHorse = "388445009";

    /// <summary>
    /// Literal for code: NoneHorse
    /// </summary>
    public const string LiteralNoneHorse = "http://snomed.info/sct#388445009";

    /// <summary>
    /// Literal for code: Duck
    /// </summary>
    public const string LiteralDuck = "396620009";

    /// <summary>
    /// Literal for code: NoneDuck
    /// </summary>
    public const string LiteralNoneDuck = "http://snomed.info/sct#396620009";

    /// <summary>
    /// Literal for code: Turkey
    /// </summary>
    public const string LiteralTurkey = "425134008";

    /// <summary>
    /// Literal for code: NoneTurkey
    /// </summary>
    public const string LiteralNoneTurkey = "http://snomed.info/sct#425134008";

    /// <summary>
    /// Literal for code: Chicken
    /// </summary>
    public const string LiteralChicken = "47290002";

    /// <summary>
    /// Literal for code: NoneChicken
    /// </summary>
    public const string LiteralNoneChicken = "http://snomed.info/sct#47290002";

    /// <summary>
    /// Literal for code: Donkey
    /// </summary>
    public const string LiteralDonkey = "85626006";

    /// <summary>
    /// Literal for code: NoneDonkey
    /// </summary>
    public const string LiteralNoneDonkey = "http://snomed.info/sct#85626006";

    /// <summary>
    /// Dictionary for looking up AnimalSpecies Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "125097000", Goat }, 
      { "http://snomed.info/sct#125097000", Goat }, 
      { "125099002", Sheep }, 
      { "http://snomed.info/sct#125099002", Sheep }, 
      { "132950000", Mule }, 
      { "http://snomed.info/sct#132950000", Mule }, 
      { "15778005", Goose }, 
      { "http://snomed.info/sct#15778005", Goose }, 
      { "34618005", Cow }, 
      { "http://snomed.info/sct#34618005", Cow }, 
      { "388445009", Horse }, 
      { "http://snomed.info/sct#388445009", Horse }, 
      { "396620009", Duck }, 
      { "http://snomed.info/sct#396620009", Duck }, 
      { "425134008", Turkey }, 
      { "http://snomed.info/sct#425134008", Turkey }, 
      { "47290002", Chicken }, 
      { "http://snomed.info/sct#47290002", Chicken }, 
      { "85626006", Donkey }, 
      { "http://snomed.info/sct#85626006", Donkey }, 
    };
  };
}
