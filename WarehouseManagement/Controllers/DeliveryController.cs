using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
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

        [HttpGet("GetByID")]
        public async Task<ActionResult<DeliveryDTO>> GetByID([FromBody] DeliveryDTO dto)
        {
            try
            {
                var obj = await _deliveryService.GetDelivery(dto);

                if (obj == null)
                {
                    return NotFound("The searched Delivery was not found.");
                }
                return Ok(obj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpPost("createDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Post([FromBody] DeliveryDTO dto)
        {
            try
            {
                var delivery = await _deliveryService.CreateDelivery(dto);

                if (delivery == null)
                {
                    return NotFound("The Delivery was not created.");
                }
                return CreatedAtAction(nameof(GetByID), new { id = delivery.deliveryID }, delivery);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }            
        }

        [HttpPut("updateDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Put([FromBody] DeliveryDTO dto)
        {
            try
            {
                var updatedObj = await _deliveryService.UpdateDelivery(dto);

                if (updatedObj == null)
                {
                    return NotFound("The Delivery was not updated.");
                }
                return Ok(updatedObj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        
        [HttpDelete("{id}")]
        public async Task<ActionResult<DeliveryDTO>> Delete([FromBody] DeliveryDTO dto)
        {
            try
            {
                var deletedObj = await _deliveryService.DeleteDelivery(dto);

                if (deletedObj == null)
                {
                    return NotFound("The requested delete was not performed.");
                }

                return Ok(deletedObj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpGet("Exists")]
        public async Task<ActionResult<bool>> Exists([FromBody] DeliveryDTO dto)
        {
            if (await _deliveryService.FindDelivery(dto))
                return Ok();
            
            return NotFound("The requested Delivery does not exist.");
        }

    }
    
}