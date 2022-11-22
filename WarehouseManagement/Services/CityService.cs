using System;
using System.Threading.Tasks;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.DTO;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;
using EletricGo.Services.Interfaces;

namespace EletricGo.Services;

public class CityService : ICityService
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly ICityRepository _cityRepository;
    private ImportCitiesFromCsvService importCsv;
    
    public CityService(IUnitOfWork unitOfWork, ICityRepository cityRepository)
    {
        _unitOfWork = unitOfWork;
        _cityRepository = cityRepository;
    }
    

    public async Task<CityDto> CreateCity(CityDto dto)
    {
        City city;
        try
        {
            city = new City(dto);
        }
        catch (Exception e)
        {
            
            throw new InvalidOperationException(e.Message);
        }
        
        try
        {
            await _cityRepository.Add(city);
            await _unitOfWork.CommitAsync();
        }
        catch (Exception exp)
        {
            // Log what you need from here.
            throw new InvalidOperationException("There is already a City with this id in the system", exp);
        }

        return city.ToCityDto();
    }

    public Task<City> CityExists(string name)
    {
        return Task.FromResult(_cityRepository.FindByCityName(name));
    }
    
    public async Task<bool> Equals(string cityId, City city)
    {
        var cityAux = await _cityRepository.GetByID(new CityId(cityId));
        if (cityAux != null)
        {
            return city.Equals(cityAux);
        }

        return false;

    }


    public void CityDelete(City city)
    {
        _cityRepository.Delete(city);
    }

    public async Task ImportCitiesFromCsv(string fileName)
    {

        importCsv = new ImportCitiesFromCsvService(fileName);
        var citiesRead = importCsv.GetAllCitiesInCsvFile();

        try
        {
            foreach (var city in citiesRead)
            {
                if (CityExists(city.name).Result == null)
                {
                    await CreateCity(city);
                }
                
            }
        }
        catch (Exception e)
        {

            throw new Exception(e.Message);
        }

    }

    public int NumberOfCities()
    {
        return _cityRepository.VerifyNumberOfCitiesInSystem();
    }




}
