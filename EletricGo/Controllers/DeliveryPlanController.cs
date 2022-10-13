using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Deliveries;

namespace EletricGo.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class DeliveryPlanController : ControllerBase
    {
        private readonly DeliveryPlanService _deliveryPlanService;

        public DeliveryPlanController(DeliveryPlanService deliveryPlanService)
        {
            _deliveryPlanService = deliveryPlanService;
        }

        [HttpGet]
        public async Task<ActionResult<List<DeliveryPlanDTO>>> Get()
        {
            return await _deliveryPlanService.GetDeliveryPlans();
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<DeliveryPlanDTO>> Get(Guid id)
        {
            return await _deliveryPlanService.GetDeliveryPlan(id);
        }

        [HttpPost]
        public async Task<ActionResult<DeliveryPlanDTO>> Post([FromBody] DeliveryPlanDTO deliveryPlanDTO)
        {
            return await _deliveryPlanService.CreateDeliveryPlan(deliveryPlanDTO);
        }

        [HttpPut("{id}")]
        public async Task<ActionResult<DeliveryPlanDTO>> Put(Guid id, [FromBody] DeliveryPlanDTO deliveryPlanDTO)
        {
            return await _deliveryPlanService.UpdateDeliveryPlan(id, deliveryPlanDTO);
        }

        [HttpDelete("{id}")]
        public async Task<ActionResult<DeliveryPlanDTO>> Delete(Guid id)
        {
            return await _deliveryPlanService.DeleteDeliveryPlan(id);
        }
    }
}
