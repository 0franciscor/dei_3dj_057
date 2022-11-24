using System.Collections.Generic;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Cities.ValueObjects;

public class CityName : IValueObject
{
    public string Name { get; }

    public CityName(string name)
    {
        
        if (string.IsNullOrEmpty(name))
        {
            throw new BusinessRuleValidationException("The name of the city can´t be null or empty");
        }
        
        if (double.TryParse(name, out _))
        {
            throw new BusinessRuleValidationException("The city name must be a String.");
        }
        
        if (name.Length > 20)
        {
            throw new BusinessRuleValidationException("The city name must be maximum 20 characters");
        }
        
        Name = name;
    }
    
    
    
    protected override IEnumerable<object> GetEqualityComponents()
    {
        yield return Name;
    }
}