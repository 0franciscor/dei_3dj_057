using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Cities;

public interface ICityRepository : IRepository<City, CityId>
{

    City FindByCityName(string name);
    int VerifyNumberOfCitiesInSystem();

}