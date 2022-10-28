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

        [HttpGet("GetAll")]
        public async Task<ActionResult<List<DeliveryDTO>>> GetAll()
        {
            return await _deliveryService.GetDeliveries();
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<DeliveryDTO>> GetByID([FromBody] DeliveryDTO dto)
        {

            var obj = await _deliveryService.GetDelivery(dto);

            if (obj == null)
                return NotFound();

            return obj;
        }

        [HttpPost("createDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Post([FromBody] DeliveryDTO dto)
        {   
            var delivery = await _deliveryService.CreateDelivery(dto);
            return CreatedAtAction(nameof(GetByID), new { id = delivery.deliveryID}, delivery);
        }

        [HttpPut("updateDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Put([FromBody] DeliveryDTO dto)
        {
            return await _deliveryService.UpdateDelivery(dto);
        }

        
        [HttpDelete("{id}")]
        public async Task<ActionResult<DeliveryDTO>> Delete([FromBody] DeliveryDTO dto)
        {
            return await _deliveryService.DeleteDelivery(dto);
        }
    }
    
}