using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;


namespace EletricGo.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class TruckController : ControllerBase
    {
        private readonly ITruckService _truckService;

        public TruckController(ITruckService truckService)
        {
            _truckService = truckService;
        }

        [HttpGet]
        public async Task<ActionResult<List<TruckDTO>>> Get()
        {
            return await _truckService.GetTrucks();
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<TruckDTO>> Get(Guid id)
        {
            return await _truckService.GetTruck(id);
        }

        [HttpPost]
        public async Task<ActionResult<TruckDTO>> Post([FromBody] TruckDTO truckDTO)
        {
            return await _truckService.CreateTruck(truckDTO);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<TruckDTO>> Put(Guid id, [FromBody] TruckDTO truckDTO)
        {
            return await _truckService.UpdateTruck(id, truckDTO);
        }

        [HttpDelete("{id}")]
        public async Task<ActionResult<TruckDTO>> Delete(Guid id)
        {
            return await _truckService.DeleteTruck(id);
        }
    }
}