using EletricGo.Domain.Shared;
using EletricGo.Domain.Trucks;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPlan : Entity<DeliveryPlanID>
    {
        
        public DeliveryPlanID deliveryPlanID { get; private set; }
        
        public List<Delivery> deliveries { get; private set; }

        public Truck truck { get; private set; }

       
        public DeliveryPlan(DeliveryPlanID deliveryPlanID, List<Delivery> deliveries, Truck truck)
        {
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = deliveries;
            this.truck = truck;
        }


        public DeliveryPlan(DeliveryPlanID deliveryPlanID, Truck truck)
        {
            this.deliveryPlanID = deliveryPlanID;
            this.deliveries = new List<Delivery>();
            this.truck = truck;
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
            return new DeliveryPlanDTO(this.deliveryPlanID.AsGuid(), this.deliveries.Select(x => x.toDeliveryDTO()).ToList(), this.truck.toTruckDTO());
        }

        public void Update(DeliveryPlanDTO deliveryPlanDTO)
        {
            this.deliveryPlanID = new DeliveryPlanID(deliveryPlanDTO.deliveryPlanID);
            this.deliveries = new List<Delivery>();
            foreach (DeliveryDTO deliveryDTO in deliveryPlanDTO.deliveries)
            {
                this.deliveries.Add(new Delivery(deliveryDTO));
            }
            this.truck = new Truck(deliveryPlanDTO.truck);
        }

       
    }

}

