using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.DTO;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;


namespace EletricGo.Services
{
    public class WarehouseService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IWarehouseRepository _warehouseRepository;
        private readonly CityService _cityService;


        public WarehouseService(IUnitOfWork unitOfWork, IWarehouseRepository warehouseRepository, CityService cityService)
        {
            _unitOfWork = unitOfWork;
            _warehouseRepository = warehouseRepository;
            _cityService = cityService;
            
        }

        public async Task<List<WarehouseDto>> GetWarehouses()
        {
            var warehouses = await _warehouseRepository.GetAll();
            return warehouses.Select(x => x.ToWarehouseDto()).ToList();
        }

        public async Task<WarehouseDto> GetWarehouse(WarehouseId id)
        {
            var warehouse = await _warehouseRepository.GetByID(id);
            return warehouse?.ToWarehouseDto();
        }

        /*public async Task<List<WarehouseDto>> GetByDescription(string designation)
        {
            var warehouse = await _warehouseRepository.GetByDescription(designation);

            if (warehouse == null) return null;
            
            List<WarehouseDto> list = null;
            foreach (var wh in warehouse)
            {
                list.Add(wh.ToWarehouseDto());
            }
                
            return list;

        }*/

        public async Task<WarehouseDto> CreateWarehouse(WarehouseDto warehouseDto)
        {
            Warehouse warehouse;
            try
            {
                warehouse = new Warehouse(warehouseDto);
            }
            catch (Exception e)
            {

                throw new InvalidOperationException(e.Message);
            }
            
            try
            {
                await _cityService.ImportCitiesFromCsv("./cities.csv");
                var city = await _cityService.CityExists(warehouseDto.Designation);

                if (city == null)
                {
                    var cityAux = await _cityService.CreateCity(new CityDto()
                        { id = (_cityService.NumberOfCities() + 1).ToString(), name = warehouse.Designation.designation });
                    warehouse.AssociateCityWithWarehouse(new CityId(cityAux.id));
                }
                else
                {
                    warehouse.AssociateCityWithWarehouse(new CityId(city.Id));
                }
                
                await _warehouseRepository.Add(warehouse);
                await _unitOfWork.CommitAsync();
                
            }
            catch (Exception exp)
            {
                // Log what you need from here.
                //throw new InvalidOperationException(exp.Message);
                throw new InvalidOperationException("There is already a warehouse with this id in the system", exp);
            }

            return warehouse.ToWarehouseDto();
        }

        public async Task<WarehouseDto> UpdateWarehouse(WarehouseDto dto)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseId(dto.Id));

            if (warehouse == null)
            {
                return null;
            }

            try
            {
                warehouse.Update(dto);
            }
            catch (Exception e)
            {
                throw new InvalidOperationException(e.Message);
            }

            await _unitOfWork.CommitAsync();
            return warehouse.ToWarehouseDto();
        }

        public async Task<WarehouseDto> DeleteWarehouse(string id)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseId(id));

            if (warehouse == null)
            {
                return null;
            }
            _warehouseRepository.Delete(warehouse);
            await _unitOfWork.CommitAsync();
            return warehouse.ToWarehouseDto();
        }

        public async Task<bool> FindWarehouse(WarehouseDto warehouseDto)
        {
            return await _warehouseRepository.Find(new WarehouseId(warehouseDto.Id));
        }


    }

}

