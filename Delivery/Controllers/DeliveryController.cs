using EletricGo.Domain.Deliveries;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace EletricGo.Controllers
{

    [Route("api/deliveries")]
    [ApiController]

    public class DeliveryController : ControllerBase
    {
        private readonly DeliveryService _deliveryService;

        public DeliveryController(DeliveryService service)
        {
            _deliveryService = service;
        }

        [HttpGet("list")]
        public async Task<ActionResult<List<DeliveryDTO>>> Get()
        {
            return await _deliveryService.getDeliveries();
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<DeliveryDTO>> GetByID(DeliveryID id)
        {
            return await _deliveryService.getDelivery(id);
        }

        [HttpPost("createDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Post([FromBody] DeliveryDTO dto)
        {   
            var delivery = await _deliveryService.createDelivery(dto);
            return CreatedAtAction(nameof(GetByID), new { id = delivery.deliveryID}, delivery);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<DeliveryDTO>> Put(string id, [FromBody] DeliveryDTO dto)
        {
            return await _deliveryService.updateDelivery(id, dto);
        }
        
        [HttpDelete("{id}")]
        public async Task<ActionResult<DeliveryDTO>> Delete(string id)
        {
            return await _deliveryService.deleteDelivery(id);
        }
    }
    
}