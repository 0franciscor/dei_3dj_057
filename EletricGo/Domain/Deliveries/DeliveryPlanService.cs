using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries{

    public class DeliveryPlanService
    {
        private readonly IDeliveryPlanRepository _deliveryPlanRepository;

        public DeliveryPlanService(IDeliveryPlanRepository deliveryPlanRepository)
        {
            _deliveryPlanRepository = deliveryPlanRepository;
        }

        public async Task<List<DeliveryPlanDTO>> GetDeliveryPlans()
        {
            var deliveryPlans = await _deliveryPlanRepository.GetAll();
            return deliveryPlans.Select(x => x.toDeliveryPlanDTO()).ToList();
        }

        public async Task<DeliveryPlanDTO> GetDeliveryPlan(DeliveryPlanID id)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            return deliveryPlan.toDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> CreateDeliveryPlan(DeliveryPlanDTO deliveryPlanDTO)
        {
            var deliveryPlan = new DeliveryPlan(deliveryPlanDTO);
            await _deliveryPlanRepository.Add(deliveryPlan);
            return deliveryPlan.toDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> UpdateDeliveryPlan(DeliveryPlanID id, DeliveryPlanDTO deliveryPlanDTO)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            deliveryPlan.Update(deliveryPlanDTO);
            await _deliveryPlanRepository.Update(deliveryPlan);
            return deliveryPlan.toDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> DeleteDeliveryPlan(DeliveryPlanID id)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            await _deliveryPlanRepository.Delete(deliveryPlan);
            return deliveryPlan.toDeliveryPlanDTO();
        }
    }

}