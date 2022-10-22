using EletricGo.Domain.Trucks;

namespace EletricGo.Domain.Deliveries{
    public class DeliveryPlanDTO{
        public Guid deliveryPlanID { get; set; }
        
        public List<DeliveryDTO> deliveries { get; set; }

        public TruckDTO truck { get; set; }

        public DeliveryPlanDTO(Guid deliveryPlanID, List<DeliveryDTO> deliveries, TruckDTO truck){
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = deliveries;
            this.truck = truck;
        }
    }
}