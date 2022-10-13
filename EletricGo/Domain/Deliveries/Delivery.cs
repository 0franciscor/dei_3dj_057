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

        public Delivery(DeliveryDTO deliveryDTO)
        {
            this.deliveryID = new DeliveryID(deliveryDTO.deliveryID);
            this.deliveryDate = new DeliveryDate(deliveryDTO.deliveryDate);
            this.loadTime = new LoadTime(deliveryDTO.loadTime);
            this.destiantion = new Destiantion(deliveryDTO.destiantion);
            this.deliveryMass = new DeliveryMass(deliveryDTO.deliveryMass);
        }

        public DeliveryDTO toDeliveryDTO()
        {
            return new DeliveryDTO(this.deliveryID.AsGuid(), this.deliveryDate.AsDateTime(), this.loadTime.AsFloat(), this.destiantion.AsString(), this.deliveryMass.AsFloat());
        }

    }    
}

