using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries{

    public class DeliveryPlanService
    {
        private readonly IDeliveryPlanRepository _deliveryPlanRepository;

        public DeliveryService(IDeliveryPlanRepository deliveryPlanRepository)
        {
            _deliveryPlanRepository = deliveryPlanRepository;
        }

        public async Task<List<DeliveryPlanDTO>> GetDeliveryPlans()
        {
            var deliveryPlans = await _deliveryPlanRepository.GetAll();
            return deliveryPlans.Select(x => x.ToDeliveryPlanDTO()).ToList();
        }

        public async Task<DeliveryPlanDTO> GetDeliveryPlan(Guid id)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            return deliveryPlan.ToDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> CreateDeliveryPlan(DeliveryPlanDTO deliveryPlanDTO)
        {
            var deliveryPlan = new Delivery(deliveryPlanDTO);
            await _deliveryPlanRepository.Add(deliveryPlan);
            return deliveryPlan.ToDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> UpdateDeliveryPlan(Guid id, DeliveryPlanDTO deliveryPlanDTO)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            deliveryPlan.Update(deliveryPlanDTO);
            await _deliveryPlanRepository.Update(deliveryPlan);
            return deliveryPlan.ToDeliveryPlanDTO();
        }

        public async Task<DeliveryPlanDTO> DeleteDeliveryPlan(Guid id)
        {
            var deliveryPlan = await _deliveryPlanRepository.Get(id);
            await _deliveryPlanRepository.Delete(deliveryPlan);
            return deliveryPlan.ToDeliveryPlanDTO();
        }
    }

}