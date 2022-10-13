namespace EletricGo.Domain.Deliveries{
    public class DeliveryPlanDTO{
        public Guid deliveryID { get; set; }
        
        public List<DeliveryDTO> deliveries { get; set; }

        public DeliveryPlanDTO(Guid deliveryID, List<DeliveryDTO> deliveries){
            this.deliveryID = deliveryID;
            this.deliveries = deliveries;
        }
    }
}