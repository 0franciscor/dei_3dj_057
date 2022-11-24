using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;
using EletricGo.Services;
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
            List<WarehouseDto> dto = await _warehouseService.GetWarehouses();
            if (dto == null) return NotFound("Warehouses not found");
            
            return dto;
        }

        [HttpGet("GetByID/{id}")]
        public async Task<ActionResult<WarehouseDto>> GetByID(string id)
        {
            var warehouse = await _warehouseService.GetWarehouse(new WarehouseId(id));
            if (warehouse == null) return NotFound("There is no warehouse with this id");

            return Ok(warehouse);
        }
        
        /*[HttpGet("GetByDescription/{id}")]
        public async Task<List<WarehouseDto>> GetByDescription(string id)
        {
            return await _warehouseService.GetByDescription(id);
        }*/

        [HttpPost("CreateWarehouse")]
        public async Task<ActionResult<WarehouseDto>> Post([FromBody] WarehouseDto dto)
        {
            try
            {
                var warehouse = await _warehouseService.CreateWarehouse(dto);
                return Ok(new {warehouse});

            }
            catch (Exception e)
            {
                return Conflict(new {e.Message});
            }
            

        }

        [HttpPut("Update")]
        public async Task<ActionResult<WarehouseDto>> Put([FromBody] WarehouseDto dto)
        {
            WarehouseDto updatedObj;
            try
            {
                updatedObj = await _warehouseService.UpdateWarehouse(dto);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
            

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

        [HttpGet("Exists/{id}")]
        public async Task<ActionResult<bool>> Exists (string id)
        {
            if(await _warehouseService.FindWarehouse(new WarehouseDto{Id = id}))
                return Ok();
            return NotFound("The requested warehouse does not exist.");
        }
        
    
    }    
    
}

