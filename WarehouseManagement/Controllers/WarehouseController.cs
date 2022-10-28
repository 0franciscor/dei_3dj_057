using System.Collections.Generic;
using System.Threading.Tasks;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;
using Microsoft.AspNetCore.Mvc;

namespace EletricGo.Controllers
{
    
    [Route("api/warehouses")]
    [ApiController]
    
    public class WarehouseController : ControllerBase
    {
        private readonly WarehouseService _warehouseService;

        public WarehouseController(WarehouseService warehouseService)
        {
            _warehouseService = warehouseService;
        }

        [HttpGet("list")]
        public async Task<ActionResult<List<WarehouseDto>>> Get()
        {
            return await _warehouseService.getWarehouses();
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<WarehouseDto>> GetByID(WarehouseID id)
        {
            return await _warehouseService.getWarehouse(id);
        }

        [HttpPost("createWarehouse")]
        public async Task<ActionResult<WarehouseDto>> Post([FromBody] WarehouseDto dto)
        {
            var warehouse = await _warehouseService.createWarehouse(dto);
            return CreatedAtAction(nameof(GetByID), new { id = warehouse.Id}, warehouse);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<WarehouseDto>> Put(string id, [FromBody] WarehouseDto dto)
        {
            return await _warehouseService.updateWarehouse(id,dto);
        }
        
        [HttpDelete("{id}")]
        public async Task<ActionResult<WarehouseDto>> Delete(string id)
        {
            return await _warehouseService.deleteWarehouse(id);
        }
    
    
    }    
    
}

