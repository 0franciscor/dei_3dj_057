using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryMass : ValueObject
    {
        private float mass { get; }

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