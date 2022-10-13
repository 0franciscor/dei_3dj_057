using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPlan : Entity<DeliveryPlanID>
    {
        
        public DeliveryPlanID deliveryPlanID { get; private set; }
        
        public List<Delivery> deliveries { get; private set; }

       
        public DeliveryPlan(DeliveryPlanID deliveryPlanID, List<Delivery> deliveries)
        {
            this.Id = new DeliveryPlanID(Guid.NewGuid());
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = deliveries;
        }

        public DeliveryPlan(DeliveryPlanID deliveryPlanID)
        {
            this.Id = new DeliveryPlanID(Guid.NewGuid());
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = new List<Delivery>();
        }
       
    }

}

