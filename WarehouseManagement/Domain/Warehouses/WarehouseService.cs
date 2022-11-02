using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;

namespace EletricGo.Domain.Warehouses
{
    public class WarehouseService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IWarehouseRepository _warehouseRepository;
        

        public WarehouseService(IUnitOfWork unitOfWork, IWarehouseRepository warehouseRepository)
        {
            _unitOfWork = unitOfWork;
            _warehouseRepository = warehouseRepository;
        }

        public async Task<List<WarehouseDto>> GetWarehouses()
        {
            var warehouses = await _warehouseRepository.GetAll();
            return warehouses.Select(x => x.ToWarehouseDto()).ToList();
        }

        public async Task<WarehouseDto> GetWarehouse(WarehouseID id)
        {
            var warehouse = await _warehouseRepository.GetByID(id);
            return warehouse?.ToWarehouseDto();
        }

        public async Task<WarehouseDto> CreateWarehouse(WarehouseDto warehouseDto)
        {
            var warehouse = new Warehouse(warehouseDto);
            await _warehouseRepository.Add(warehouse);
            try
            {
                await this._unitOfWork.CommitAsync();
            }
            catch (Exception exp)
            {
                // Log what you need from here.
                throw new InvalidOperationException("Data could not be read", exp);
            }

            return warehouse.ToWarehouseDto();
        }

        public async Task<WarehouseDto> UpdateWarehouse(WarehouseDto dto)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseID(dto.Id));

            if (warehouse == null)
            {
                return null;
            }
            warehouse.Update(dto);
            await this._unitOfWork.CommitAsync();
            return warehouse.ToWarehouseDto();
        }

        public async Task<WarehouseDto> DeleteWarehouse(string id)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseID(id));

            if (warehouse == null)
            {
                return null;
            }
            _warehouseRepository.Delete(warehouse);
            await this._unitOfWork.CommitAsync();
            return warehouse.ToWarehouseDto();
        }
    }    
    
}

