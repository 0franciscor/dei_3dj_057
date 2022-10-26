using System;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{
    public class Delivery : Entity<DeliveryID>, IAggregateRoot
    { 
        public DeliveryDate deliveryDate { get; private set;}
        public LoadTime loadTime { get; private set;}
        public UnloadTime unloadTime { get; private set;}
        public Destination destination { get; private set;}
        public DeliveryMass deliveryMass { get; private set;}

        public Delivery() { }
        public Delivery(DeliveryDate deliveryDate, LoadTime loadTime, UnloadTime unloadTime, Destination destination, DeliveryMass deliveryMass)
        { 
            this.deliveryDate = deliveryDate;
            this.loadTime = loadTime;
            this.unloadTime = unloadTime;
            this.destination = destination;
            this.deliveryMass = deliveryMass;
        }

        public Delivery(DeliveryDTO deliveryDTO)
        { 
            this.Id = new DeliveryID(Guid.NewGuid()).AsString();
            this.deliveryDate = new DeliveryDate(deliveryDTO.deliveryDate);
            this.loadTime = new LoadTime(deliveryDTO.loadTime);
            this.unloadTime = new UnloadTime(deliveryDTO.unloadTime);
            this.destination = new Destination(deliveryDTO.destination);
            this.deliveryMass = new DeliveryMass(deliveryDTO.deliveryMass);
        }

        public DeliveryDTO toDeliveryDTO()
        {
            return new DeliveryDTO() {deliveryID = new DeliveryID(this.Id), deliveryDate = this.deliveryDate.AsDateTime(), loadTime = this.loadTime.AsFloat(), 
                unloadTime = this.unloadTime.AsFloat(), destination = this.destination.AsString(), deliveryMass = this.deliveryMass.AsFloat() };
        }

        public void update(DeliveryDTO deliveryDTO)
        {
            this.deliveryDate = new DeliveryDate(deliveryDTO.deliveryDate);
            this.loadTime = new LoadTime(deliveryDTO.loadTime);
            this.unloadTime = new UnloadTime(deliveryDTO.unloadTime);
            this.destination = new Destination(deliveryDTO.destination);
            this.deliveryMass = new DeliveryMass(deliveryDTO.deliveryMass);
        }

    }    
}

