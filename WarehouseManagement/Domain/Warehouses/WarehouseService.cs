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

        public async Task<List<WarehouseDto>> getWarehouses()
        {
            var warehouses = await _warehouseRepository.GetAll();
            return warehouses.Select(x => x.ToWarehouseDto()).ToList();
        }

        public async Task<WarehouseDto> getWarehouse(WarehouseID id)
        {
            var delivery = await _warehouseRepository.GetByID(id);
            return delivery.ToWarehouseDto();
        }

        public async Task<WarehouseDto> createWarehouse(WarehouseDto warehouseDto)
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

        public async Task<WarehouseDto> updateWarehouse(string id, WarehouseDto dto)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseID(id));
            warehouse.Update(dto);
            await this._unitOfWork.CommitAsync();
            return warehouse.ToWarehouseDto();
        }

        public async Task<WarehouseDto> deleteWarehouse(string id)
        {
            var warehouse = await _warehouseRepository.GetByID(new WarehouseID(id));
            _warehouseRepository.Delete(warehouse);
            return warehouse.ToWarehouseDto();
        }
    }    
    
}

