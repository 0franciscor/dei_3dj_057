using System;
using System.Linq;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Infrastructure.Shared;

namespace EletricGo.Infrastructure.Cities;

public class CityRepository : BaseRepository<City, CityId>, ICityRepository
{
    private readonly EletricGoDBContext _context;
    
    public CityRepository(EletricGoDBContext context) : base(context.City)
    {
        _context = context;

    }

    public City FindByCityName(string name)
    {
        try
        {
            
            return _context.City.First(c => c.Name.Name.ToUpper() == name.ToUpper());
        }
        catch (Exception e)
        {
            return null;
        }
        
    }

    public int VerifyNumberOfCitiesInSystem()
    {
        return _context.City.Count();
    }
}