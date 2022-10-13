namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDTO
    {
        public Guid deliveryID { get; set; }
        public DateTime deliveryDate { get; set; }
        public float loadTime { get; set; }
        public float unloadTime { get; set; }
        public String destiantion { get; set; }
        public float deliveryMass { get; set; }
    
        public DeliveryDTO(Guid deliveryID, DateTime deliveryDate, float loadTime, float unloadTime, String destiantion, float deliveryMass)
        {
            this.deliveryID = deliveryID;
            this.deliveryDate = deliveryDate;
            this.unloadTime = loadTime;
            this.destiantion = destiantion;
            this.deliveryMass = deliveryMass;
        }
       
    }
    
}