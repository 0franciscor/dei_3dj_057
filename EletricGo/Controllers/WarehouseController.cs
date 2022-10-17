using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouse;

namespace EletricGo.Controllers
{
    
    [Route("api/[controller]")]
    [ApiController]

    public class WarehouseController : ControllerBase
    {
        private readonly WarehouseService _warehouseService;

        public WarehouseController(WarehouseService service)
        {
            _warehouseService = service;
        }

        [HttpGet]
        public async Task<ActionResult<List<WarehouseDTO>>> Get()
        {
            _warehouseService.GetWarehouses() == null ? NotFound() : Ok(_warehouseService.GetWarehouses());
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<WarehouseDTO>> Get(WarehouseID id)
        {
            _warehouseService.GetWarehouse(id) == null ? NotFound() : Ok(_warehouseService.GetWarehouse(id));
        }

        [HttpPost]
        public async Task<ActionResult<WarehouseDTO>> Post([FromBody] WarehouseDTO dto)
        {
            return await _warehouseService.createWarehouse(dto);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<WarehouseDTO>> Put(WarehouseID id, [FromBody] WarehouseDTO dto)
        {
            return await _warehouseService.UpdateWarehouse(id, dto);
        }
        
        [HttpDelete("{id}")]
        public async Task<ActionResult<WarehouseDTO>> Delete(WarehouseID id)
        {
            return await _warehouseService.DeleteWarehouse(id);
        }
        
    }
    
}