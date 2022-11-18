using System.Text.RegularExpressions;
using CsvHelper.Configuration.Attributes;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Cities.ValueObjects;

public class CityId : EntityID
{
    public string Id { get; }

    public CityId() 
    {
        
    }
    public CityId(string value)
    {
        if (!Regex.IsMatch(value, "([0-9])|([1-9][0-9]+)"))
        {
            throw new BusinessRuleValidationException("The id of a city can't be negative");
        }
        this.Id = value;
    }

    public override string AsString()
    {
        throw new System.NotImplementedException();
    }
}