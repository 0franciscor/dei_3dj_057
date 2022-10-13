using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class Delivery : Entity<DeliveryID>
    {

        public DeliveryID deliveryID { get; private set; }
        public DeliveryDate deliveryDate { get; private set; }
        public LoadTime loadTime { get; private set; }
        public Destiantion destiantion { get; private set; }
        public DeliveryMass deliveryMass { get; private set; }

        public Delivery(DeliveryID deliveryID, DeliveryDate deliveryDate, LoadTime loadTime, Destiantion destiantion, DeliveryMass deliveryMass)
        {
            this.deliveryID = deliveryID;
            this.deliveryDate = deliveryDate;
            this.loadTime = loadTime;
            this.destiantion = destiantion;
            this.deliveryMass = deliveryMass;
        }

    }    
}

