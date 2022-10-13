namespace EletricGo.Domain.Deliveries{
    public class DeliveryPlanDTO{
        public Guid deliveryPlanID { get; set; }
        
        public List<DeliveryDTO> deliveries { get; set; }

        public DeliveryPlanDTO(Guid deliveryPlanID, List<DeliveryDTO> deliveries){
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = deliveries;
        }
    }
}