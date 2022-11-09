using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System;
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

        [HttpGet("GetByID/{id}")]
        public async Task<ActionResult<DeliveryDTO>> GetByID(string id)
        {
            try
            {
                var obj = await _deliveryService.GetDelivery(new DeliveryID(id));

                if (obj == null)
                {
                    return NotFound("The searched Delivery was not found.");
                }
                return Ok(obj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
        }

        [HttpGet("GetByPeriod")]
        public async Task<ActionResult<List<DeliveryDTO>>> GetByPeriod([FromBody] string receivedDates)
        {
            var parsedDates = receivedDates.Split(",");

            var firstDate = DateTime.Parse(parsedDates[0]);
            var secondDate = DateTime.Parse(parsedDates[1]);

            if (firstDate > secondDate)
                (secondDate, firstDate) = (firstDate, secondDate);

            try
            {
                var obj = await _deliveryService.GetByPeriod(firstDate, secondDate);

                if (obj == null)
                    return NotFound("No deliveries were found in that period.");
                return Ok(obj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
        }

        [HttpPost("CreateDelivery")]
        public async Task<ActionResult<DeliveryDTO>> Post([FromBody] DeliveryDTO dto)
        {
            try
            {
                var delivery = await _deliveryService.CreateDelivery(dto);

                if (delivery == null)
                {
                    return BadRequest("The Delivery was not created.");
                }
                return CreatedAtAction(nameof(GetByID), new { id = delivery.deliveryID }, delivery);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }            
        }

        [HttpPatch("Update")]
        public async Task<ActionResult<DeliveryDTO>> Patch([FromBody] DeliveryDTO dto)
        {
            try
            {
                var updatedObj = await _deliveryService.UpdateDelivery(dto);

                if (updatedObj == null)
                {
                    return BadRequest("The Delivery was not updated.");
                }
                return Ok(updatedObj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
        }
        
        [HttpDelete("Delete/{id}")]
        public async Task<ActionResult<DeliveryDTO>> Delete(string id)
        {
            try
            {
                var deletedObj = await _deliveryService.DeleteDelivery(new DeliveryID(id));

                if (deletedObj == null)
                {
                    return NotFound("The requested delete was not performed.");
                }

                return Ok(deletedObj);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { ex.Message });
            }
        }

        [HttpGet("Exists/{id}")]
        public async Task<ActionResult<bool>> Exists(string id)
        {
            if (await _deliveryService.FindDelivery(new DeliveryID(id)))
                return Ok();
            
            return NotFound("The requested Delivery does not exist.");
        }

    }
    
}