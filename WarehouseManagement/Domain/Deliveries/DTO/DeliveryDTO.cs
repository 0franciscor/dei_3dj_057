using System;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDTO
    {
        public string deliveryID { get; set; }
        public DateTime deliveryDate { get; set; }
        public float loadTime { get; set; }
        public float unloadTime { get; set; }
        public string destination { get; set; }
        public float deliveryMass { get; set; }

    }    
}