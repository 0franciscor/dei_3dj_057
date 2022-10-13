namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDTO
    {
        public Guid deliveryID { get; set; }
        public DateTime deliveryDate { get; set; }
        public float loadTime { get; set; }
        public String destiantion { get; set; }
        public float deliveryMass { get; set; }
    
        public DeliveryDTO(Guid deliveryID, DateTime deliveryDate, float loadTime, String destiantion, float deliveryMass)
        {
            this.deliveryID = deliveryID;
            this.deliveryDate = deliveryDate;
            this.loadTime = loadTime;
            this.destiantion = destiantion;
            this.deliveryMass = deliveryMass;
        }
       
    }
    
}