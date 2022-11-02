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

        [HttpGet("GetAll")]
        public async Task<ActionResult<List<WarehouseDto>>> Get()
        {
            return await _warehouseService.GetWarehouses();
        }

        [HttpGet("GetByID/{id}")]
        public async Task<ActionResult<WarehouseDto>> GetByID(string id)
        {
            return await _warehouseService.GetWarehouse(new WarehouseID(id));
        }

        [HttpPost("CreateWarehouse")]
        public async Task<ActionResult<WarehouseDto>> Post([FromBody] WarehouseDto dto)
        {
            var warehouse = await _warehouseService.CreateWarehouse(dto);
            return CreatedAtAction(nameof(GetByID), new { id = warehouse.Id}, warehouse);
        }

        [HttpPut("Update")]
        public async Task<ActionResult<WarehouseDto>> Put([FromBody] WarehouseDto dto)
        {
            var updatedObj = await _warehouseService.UpdateWarehouse(dto);

            if (updatedObj == null)
            {
                return NotFound("The Warehouse was not updated.");
            }
            return Ok(updatedObj);
        }
        
        [HttpDelete("Delete/{id}")]
        public async Task<ActionResult<WarehouseDto>> Delete(string id)
        {
            var deletedObject = await _warehouseService.DeleteWarehouse(id);
            
            if (deletedObject == null)
            {
                return NotFound("The requested delete was not performed.");
            }

            return Ok(deletedObject);
            
            
        }
    
    
    }    
    
}

