using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPlan
    {
        private string deliveryID;

        private string truckID;
        

        private DeliveryPlan()
        {
        
        }

        public DeliveryPlan(string deliveryID, string truckID)
        {
            this.deliveryID=deliveryID;
            this.truckID=truckID;
        }

        
        
    }

}
