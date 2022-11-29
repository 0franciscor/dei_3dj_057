using System.Text.RegularExpressions;
using EletricGo.Domain.Cities.DTO;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Cities;

public class City : Entity<CityId>
{
    public CityName Name;

    public City(){
        
    }
    public City(EntityID id, CityName name)
    {
        Id = id.Value;
        Name = name;
    }

    public City(string city)
    {
        
        if (string.IsNullOrEmpty(city))
        {
            throw new BusinessRuleValidationException("The city can't be null or empty");
        }
        var aux = city.Split(",");
        
        if (!Regex.IsMatch(aux[0], "([0-9])|([1-9][0-9]+)"))
        {
            throw new BusinessRuleValidationException("The Id must be a positive integer"+ aux.ToString());
        }

        this.Id = aux[0];
        Name = new CityName(aux[1]);
        
    }
    
    public City(CityDto dto)
    {
        
        if (!Regex.IsMatch(dto.id, "([0-9])|([1-9][0-9]+)"))
        {
            throw new BusinessRuleValidationException("The Id must be a positive integer");
        }

        this.Id = dto.id;
        Name = new CityName(dto.name);
        
    }

    public CityDto ToCityDto()
    {
        return new CityDto() { id = this.Id, name = this.Name.Name };
    }

    public string AsString()
    {
        return Id + "," + Name.Name;
    }
    
    public CityDto toCityDto()
    {
        return new CityDto() {id = this.Id, name = this.Name.Name};
    }

}