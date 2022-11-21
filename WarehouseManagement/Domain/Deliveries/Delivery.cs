using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.ValueObjects;
using System;

namespace EletricGo.Domain.Deliveries
{
    public class Delivery : Entity<DeliveryID>, IAggregateRoot
    {
        public DeliveryDate deliveryDate { get; private set; }
        public LoadTime loadTime { get; private set; }
        public UnloadTime unloadTime { get; private set; }
        public Warehouse destinationWarehouse { get; private set; }
        public string destination { get; private set; }
        public DeliveryMass deliveryMass { get; private set; }
        public Delivery() {}
        public Delivery(string Id, DeliveryDate deliveryDate, LoadTime loadTime, UnloadTime unloadTime, string destination, DeliveryMass deliveryMass)
        {
            this.Id = Id;
            this.deliveryDate = deliveryDate;
            this.loadTime = loadTime;
            this.unloadTime = unloadTime;
            this.destination = destination;
            this.deliveryMass = deliveryMass;
        }

        public Delivery(DeliveryDTO deliveryDTO)
        {
            this.Id = deliveryDTO.deliveryID;
            this.deliveryDate = new DeliveryDate(deliveryDTO.deliveryDate);
            this.loadTime = new LoadTime(deliveryDTO.loadTime);
            this.unloadTime = new UnloadTime(deliveryDTO.unloadTime);
            this.destination = deliveryDTO.destination;
            this.deliveryMass = new DeliveryMass(deliveryDTO.deliveryMass);
        }

        public DeliveryDTO toDeliveryDTO()
        {
            return new DeliveryDTO()
            {
                deliveryID = this.Id,
                deliveryDate = this.deliveryDate.AsDateTime(),
                loadTime = this.loadTime.AsFloat(),
                unloadTime = this.unloadTime.AsFloat(),
                destination = this.destination,
                deliveryMass = this.deliveryMass.AsFloat()
            };
        }

        public void Update(DeliveryDTO deliveryDTO)
        {
            if (deliveryDTO.deliveryDate != default(DateTime))
                this.deliveryDate = new DeliveryDate(deliveryDTO.deliveryDate);

            if (deliveryDTO.loadTime != default(float))
                this.loadTime = new LoadTime(deliveryDTO.loadTime);

            if (deliveryDTO.unloadTime != default(float))
                this.unloadTime = new UnloadTime(deliveryDTO.unloadTime);

            if (deliveryDTO.destination != null)
                this.destination = deliveryDTO.destination;

            if (deliveryDTO.deliveryMass != default(float))
                this.deliveryMass = new DeliveryMass(deliveryDTO.deliveryMass);
        }

        override
        public int GetHashCode()
        {
            return Id.GetHashCode();
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            Delivery delivery = (Delivery)obj;
            return this.Id.Equals(delivery.Id) && this.deliveryDate.Equals(delivery.deliveryDate) && this.loadTime.Equals(delivery.loadTime) && this.unloadTime.Equals(delivery.unloadTime) && this.destination.Equals(delivery.destination) && this.deliveryMass.Equals(delivery.deliveryMass);

        }
    }
}