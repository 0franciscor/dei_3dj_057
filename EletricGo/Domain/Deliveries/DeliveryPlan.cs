using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPlan : Entity<DeliveryPlanID>
    {
        
        public DeliveryPlanID deliveryPlanID { get; private set; }
        
        public List<Delivery> deliveries { get; private set; }

       
        public DeliveryPlan(DeliveryPlanID deliveryPlanID, List<Delivery> deliveries)
        {
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = deliveries;
        }

        public DeliveryPlan(DeliveryPlanID deliveryPlanID)
        {
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = new List<Delivery>();
        }

        public DeliveryPlan(DeliveryPlanDTO deliveryPlanDTO)
        {
            this.deliveryPlanID = new DeliveryPlanID(deliveryPlanDTO.deliveryPlanID);
            this.deliveries = new List<Delivery>();
            foreach (DeliveryDTO deliveryDTO in deliveryPlanDTO.deliveries)
            {
                this.deliveries.Add(new Delivery(deliveryDTO));
            }
        }

        public DeliveryPlanDTO toDeliveryPlanDTO()
        {
            return new DeliveryPlanDTO(this.deliveryPlanID.AsGuid(), this.deliveries.Select(x => x.toDeliveryDTO()).ToList());
        }

        public void Update(DeliveryPlanDTO deliveryPlanDTO)
        {
            this.deliveryPlanID = new DeliveryPlanID(deliveryPlanDTO.deliveryPlanID);
            this.deliveries = new List<Delivery>();
            foreach (DeliveryDTO deliveryDTO in deliveryPlanDTO.deliveries)
            {
                this.deliveries.Add(new Delivery(deliveryDTO));
            }
        }

       
    }

}

