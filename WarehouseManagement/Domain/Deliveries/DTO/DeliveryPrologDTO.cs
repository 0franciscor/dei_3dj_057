using System;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPrologDTO
    {
        public string deliveryID { get; set; }
        public float loadTime { get; set; }
        public float unloadTime { get; set; }
        public string destination { get; set; }
        public float deliveryMass { get; set; }

        public string deliveryDateProlog {get; set;}

    }    
}