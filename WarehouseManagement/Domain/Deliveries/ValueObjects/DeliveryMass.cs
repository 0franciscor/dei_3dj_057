using EletricGo.Domain.Shared;
using System.Collections.Generic;

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

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return mass;
        }
    }
  
}