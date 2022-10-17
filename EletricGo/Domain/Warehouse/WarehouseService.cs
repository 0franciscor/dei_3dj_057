using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouse { 

    public class WarehouseService
    {
        private readonly IWarehouseRepository _warehouseRepository;

        public WarehouseService(IWarehouseRepository warehouseRepository)
        {
            _warehouseRepository = warehouseRepository;
        }

        public async Task<List<WarehouseDTO>> GetWarehouses()
        {
            var warehouses = await _warehouseRepository.GetAll();
            return warehouses.Select(x => x.toWarehouseDTO()).ToList();
        }

        public async Task<WarehouseDTO> GetWarehouse(WarehouseID id)
        {
            var warehouse = await _warehouseRepository.Get(id);
            return warehouse.toWarehouseDTO();
        }

        public async Task<WarehouseDTO> CreateWarehouse(WarehouseDTO dto)
        {
            var warehouse = new Warehouse(dto);
            await _warehouseRepository.Add(warehouse);
            return warehouse.toWarehouseDTO();
        }

        public async Task<WarehouseDTO> UpdateWarehouse(WarehouseID id, WarehouseDTO dto)
        {
            var warehouse = await _warehouseRepository.Get(id);
            warehouse.Update(WarehouseDTO);
            await _warehouseRepository.Update(warehouse);
            return warehouse.toWarehouseDTO();
        }

        public async Task<DeliveryPlanDTO> DeleteWarehouse(WarehouseID id)
        {
            var warehouse = await _warehouseRepository.Get(id);
            await _warehouseRepository.Delete(warehouse);
            return warehouse.toWarehouseDTO;
        }
    }

}