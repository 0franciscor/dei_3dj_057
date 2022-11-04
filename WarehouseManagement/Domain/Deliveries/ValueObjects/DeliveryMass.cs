using System.Collections.Generic;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryMass : IValueObject
    {
        public float mass { get; }

        public DeliveryMass() { }

        public DeliveryMass(float mass)
        {
            this.mass = mass;
        }

        public float AsFloat()
        {
            return mass;
        }

        override
        public bool Equals(object obj)
        {
            if (obj == null || !(obj is DeliveryMass))
                return false;

            var deliveryMass = (DeliveryMass)obj;
            return this.mass == deliveryMass.mass;
        }

        override
        public int GetHashCode()
        {
            return mass.GetHashCode();
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return mass;
        }
    }
  
}