using System.Collections.Generic;
using System.Threading.Tasks;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.DTO;

namespace EletricGo.Services.Interfaces;

public interface ICityService
{
    Task ImportCitiesFromCsv(string citiesCsv);
    int NumberOfCities();

    void CityDelete(City city);

    Task<City> CityExists(string name);

    Task<CityDto> CreateCity(CityDto dto);
    Task<List<CityDto>> GetAllCities();
}