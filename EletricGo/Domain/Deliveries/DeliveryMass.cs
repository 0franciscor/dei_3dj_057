using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryMass : ValueObject
    {
        private double Mass { get; }

        public DeliveryMass(double mass)
        {
            Mass = mass;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Mass;
        }
    }
  
}